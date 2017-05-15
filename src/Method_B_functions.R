# Function for importing data, return calculation and data wrangling
Returns_calculation <- function (asset_name, index_data)
{
  # Example of asset data
  asset_xts <- xts(get.hist.quote(instrument = paste(asset_name,
                                                     sep = "",
                                                     ".SA"),
                                  start = "2010-01-01",
                                  end = Sys.Date(),
                                  quote="AdjClose",
                                  provider="yahoo",
                                  compression="d",
                                  retclass="zoo"))

  # Calculating asset return
  asset_returns <- Return.calculate(prices = asset_xts , method = "discrete")
  
  # Calculating index return
  index_returns <- xts(Return.calculate(prices = index_data,
                                        method = "discrete"))

  # Merging asset and index returns
  returns <- na.omit(merge(x = asset_returns,
                           y = index_returns,
                           join = "left"))
  
  # Identifying the column with the asset name
  colnames(returns) <- c((paste(asset_name, "return", sep = "_")),
                         "index_return")
  return(returns)
}


# Asset model, check for heteroskedasticity and serial correlation
Model_tests <- function(returns, window) 
{
  # Periods for calculation
  estimation <- first(window, "240 days")
  event <- xts::last(window, "4 days")
  
  # Linear Model for estimation window
  reg <- lm(estimation[,1] ~estimation[,2])
  
  # Durbin's alternative  test for serial correlation
  aux_df <- estimation
  aux_df$res <- reg$residuals
  aux_df$res_lag <- lag(aux_df$res, k = 1) 
  aux_df1 <- as.zoo( na.omit(aux_df))  
  aux_reg_sc <- with ( aux_df1, dynlm(formula = res ~ res_lag + index_return))
  wald_t <- waldtest(aux_reg_sc ,1 , test = "Chisq", vcov = vcovHC)
  
  # Engle LM test for ARCH(1) for heteroskedasticity
  arch_t <- ArchTest(as.zoo(residuals(reg)), lags=1)
  
  # Select model
  if(wald_t$`Pr(>Chisq)`[2] > 0.05 & arch_t$p.value > 0.05) {
    # No serial correlation, no heteroskedasticity
    tests <- c("no_sc_no_hk", NA) 
    
    # Abnormal returs from linear regression
    AR_estimation <- residuals(reg)
    AR_event <- c(event[,1] - 
                    reg$coefficients[1] - 
                    event[,2] * reg$coefficients[2])
    
    out <- list(tests, AR_estimation, AR_event)
    return(out)
    
  } else if(wald_t$`Pr(>Chisq)`[2] > 0.05 & arch_t$p.value <= 0.05) {
    # No serial correlation, heteroskedasticity
    tests <- "no_sc_hk" 
    
    # Linear regression, GARCH(1,1)
    fit.spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                                 garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(0, 0),
                                             include.mean = TRUE,
                                             external.regressors = window[,2]))
    
    # Estimates the model excluding last 11 observations
    fit <- ugarchfit(spec = fit.spec, 
                     data = window[,1], 
                     out.sample=11, 
                     solver = "hybrid") 
    
    # Standardized abnormal returns for estimation window
    AR_estimation <- (estimation[,1]-fitted(fit))/sigma(fit)
    
    # Expected sigma and abnormal return of event window
    expected_sigma <- xts::last(sigma(fit))
    for (nrow in 1:11) {
      expected_sigma_t_plus1 <- sqrt(coef(fit)[3] +
                                     coef(fit)[5] * xts::last(expected_sigma)^2)
      expected_sigma <- rbind(expected_sigma, expected_sigma_t_plus1)
      }
    expected_sigma <- as.numeric(coredata(expected_sigma[9:12]))
    expected_AR <- event[,1] - coef(fit)[1] - coef(fit)[2] * event[,2]
    
    # Standardized abnormal returns for event window
    AR_event <- expected_AR/expected_sigma
    
    # Engle LM test for ARCH(1) for heteroskedasticity
    arch_t_pos <- ArchTest(residuals(fit, standardize=TRUE), lags=1)
    
    # Test results after changing model
    tests[2] <- ifelse (arch_t_pos$p.value <= 0.05, "still hk", "no more hk")
    
    out = list(tests, AR_estimation, AR_event)
    return(out)
    
  } else if(wald_t$`Pr(>Chisq)`[2] <= 0.05 & arch_t$p.value > 0.05) {
    # Serial correlation, no heteroskedasticity
    tests <- "sc_no_hk" 
    
    # ADL model
    input_sc <- estimation
    input_sc$asset_lag <- lag(input_sc[,1], k=1 ) 
    input_sc$index_lag <- lag(input_sc[,2], k=1 ) 
    input_sc <- na.omit(input_sc)
    model_sc <- lm(input_sc[,1] ~ input_sc[,2] + input_sc[,3] + input_sc[,4] )
    
    # Abnormal returns
    AR_estimation <- residuals(model_sc) 
    AR_event <- c(event[,1] - model_sc$coefficients[1] - 
                  event[,2] * model_sc$coefficients[2])
    
    # Durbin's alternative test for serial correlation
    aux_df <- estimation
    aux_df$res <- model_sc$residuals
    aux_df$res_lag <- lag(aux_df$res, k = 1) 
    aux_df <- as.zoo( na.omit(aux_df))  
    aux_reg_sc <- with ( aux_df, dynlm(formula = res ~ res_lag +index_return))
    wald_t_pos <- waldtest(aux_reg_sc ,1 , test = "Chisq", vcov = vcovHC)
    
    # Test results after changing model
    tests[2] <- ifelse (wald_t_pos$`Pr(>Chisq)`[2] <= 0.05, 
                        "still sc", "no more sc")
    
    out <- list(tests, AR_estimation, AR_event)
    return(out)
    
  } else {
    # There is serial correlation and heteroskedasticity
    
    # ADL model
    lag_aux <- lag(returns[,2], 1)
    input_xreg <- na.omit(as.matrix(cbind(window[,2], lag_aux)))
    
    # ADL-GARCH(1,1) model specification 
    fit.spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                                 garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(1, 0),
                                             include.mean = TRUE,
                                             external.regressors = input_xreg))
    
    # Estimates the model excluding last 11 observations
    fit <- ugarchfit( spec = fit.spec, 
                      data = window[,1], 
                      out.sample=11, 
                      solver = "hybrid") 
    
    # Standardized abnormal returns for estimation window
    AR_estimation <- (estimation[,1]-fitted(fit))/sigma(fit)
    
    # Expected sigma and abnormal return of event window
    expected_sigma <- xts::last(sigma(fit))
    for (nrow in 1:11) {
      expected_sigma_t_plus1 <- sqrt(coef(fit)[3] +
                                     coef(fit)[5] * xts::last(expected_sigma)^2)
      expected_sigma <- rbind(expected_sigma,expected_sigma_t_plus1)
    }
    expected_sigma <- as.numeric(coredata(expected_sigma[9:12]))
    expected_AR <- event[,1] - coef(fit)[1] - coef(fit)[2]*event[,2]
    
    # Standardized abnormal returns for event window
    AR_event <- expected_AR/expected_sigma 
    
    # Engle LM test for ARCH(1) for heteroskedasticity
    arch_t_pos <- ArchTest(as.zoo(residuals(fit, standardize = TRUE)), lags = 1)
    tests <- ifelse (arch_t_pos$p.value <= 0.05, "still hk", "no more hk")
    
    # Durbin's alternative  test for serial correlation
    aux_df <- estimation
    aux_df$res <- residuals(fit)
    aux_df$res_lag <- lag(aux_df$res, k = 1) 
    aux_df <- as.zoo( na.omit(aux_df))  
    aux_reg_sc <- with ( aux_df, dynlm(formula = res ~ res_lag +index_return))
    wald_t_pos <- waldtest(aux_reg_sc ,1 , test = "Chisq", vcov = vcovHC)
    
    # Test results after changing model
    tests[2] <- ifelse(wald_t_pos$`Pr(>Chisq)`[2] <= 0.05, 
                       "still sc", "no more sc")
    
    out <- list(tests, AR_estimation, AR_event)
    return(out)
    
  }
  
}


# Function for asset model and CONDITIONAL bootstrap
MethodB_calculation <- function(relevant_date, asset_returns) 
{
  # Extract estimation window and event window data
  prev_days <- xts::last(asset_returns[index(asset_returns) <= relevant_date], 
                         "250 days")
  next_day <- asset_returns[index(asset_returns) > relevant_date]
  
  # Stop function in case there is not enough data to apply model  
  if (nrow(prev_days) < 250 | nrow(next_day) < 1) 
    return(data.frame(Announc_date = relevant_date, 
                      Upper_4CAR = NA ,
                      Lower_4CAR = NA, CAR_4days = 0,
                      Upper_2CAR = NA,Lower_2CAR = NA, CAR_2days = 0,
                      obs1 = "not enough data for model estimation", obs2 = NA,
                      row.names = NULL))
  
  # Estimation window and event window
  asset_returns_window <- rbind(prev_days, next_day[1])
  estimation_window <- first(asset_returns_window, "240 days")
  event_window <- xts::last(asset_returns_window, "4 days")
  
  #Tests of Serial Correlation and Heteroskedasticity
  tests_results <- Model_tests(returns = asset_returns, 
                               window = asset_returns_window)
  
  
  # Calculate abnormal returns for the event window
  abnormal_returns_event <- tests_results[[3]]
  CAR_4days <- sum(abnormal_returns_event)
  CAR_2days <- sum(abnormal_returns_event[1:2])
  
  # Calculate abnormal returns for the estimation window
  abnormal_returns_240 <- tests_results[[2]]
  
  # Bootstrap 4 day CAR
  bootstrap_input <- as.data.frame(abnormal_returns_240)
  bootstrap_event <- replicate (50000,  
                                sample(bootstrap_input[,1], 
                                       4, replace = TRUE)) %>%
    t() %>%
    tbl_df() %>%
    mutate(CAR = V1+V2+V3+V4)
  Upper_cutoff = quantile(bootstrap_event$CAR, 0.995)
  Lower_cutoff = quantile(bootstrap_event$CAR, 0.005)
  
  if(CAR_4days < Lower_cutoff | CAR_4days > Upper_cutoff) {
    # Bootstrap 2 day CAR conditional
    bootstrap_pre_event <- replicate (50000,  
                                      sample(bootstrap_input[,1], 
                                             4, replace = TRUE)) %>%
      t() %>%
      tbl_df() %>%
      mutate(CAR = V1+V2+V3+V4) %>%
      filter(CAR > Upper_cutoff| CAR < Lower_cutoff) %>%
      mutate(CAR2 = V1+V2) 
    Upper_cutoff_event <- quantile(bootstrap_pre_event$CAR2, 0.90)
    Lower_cutoff_event <- quantile(bootstrap_pre_event$CAR2, 0.10)
  } else {
    Upper_cutoff_event <- NA
    Lower_cutoff_event <- NA
  }
  
  # Output
  out = data.frame( Announc_date = relevant_date, 
                    Upper_4CAR = Upper_cutoff, Lower_4CAR = Lower_cutoff, 
                    CAR_4days = CAR_4days, Upper_2CAR = Upper_cutoff_event,
                    Lower_2CAR = Lower_cutoff_event, CAR_2days = CAR_2days,
                    obs1 = tests_results[[1]][1], obs2 = tests_results[[1]][2],
                    row.names = NULL)
  return(out)
}


# Function to calculate all CAR limits for an asset 
MethodB_analysis <- function(asset_returns, announcements)
{
  # Extract the asset's name, company name, announcement dates
  asset_ticker <-strsplit(names(asset_returns)[1], "_")[[1]][1]
  asset_company_name <- substr(asset_ticker, start = 1 , stop = 4 )
  announc_dates <- filter(announcements, Company==asset_company_name)
  
  # Stop function if there are no announcements for this asset
  if (nrow(announc_dates) == 0) 
    return(data.frame(Announc_date = NA,
                      Upper_4CAR = NA ,
                      Lower_4CAR = NA, CAR_4days = 0,
                      Upper_2CAR = NA,Lower_2CAR = NA, CAR_2days = 0,
                      obs1 = "no announcements for this company", obs2 = NA,
                      event = NA, insider = NA, asset = asset_ticker,
                      row.names = NULL))
  
  # Get all CAR limits for all announcements of this asset
  asset_CAR_limits <- Reduce(rbind,
                             lapply(X = unique(announc_dates$News.Date), 
                                    FUN = MethodB_calculation, 
                                    asset_returns = asset_returns)) 
  
  
  # Classify if events/pre-events CAR are relevant  
  CAR_analysis <- tbl_df(asset_CAR_limits) %>%
    mutate(event = factor(ifelse(CAR_4days < Lower_4CAR | 
                                   CAR_4days > Upper_4CAR, 
                                 "relevant", "ok"))) %>%
    mutate(pre_event = factor(ifelse((CAR_2days < Lower_2CAR | 
                                        CAR_4days > Upper_2CAR) & 
                                       CAR_2days*CAR_4days > 0 & 
                                       event == "relevant",
                                     "IPM", "ok")))  %>%
    mutate(asset = asset_ticker)
  return(CAR_analysis)
}
