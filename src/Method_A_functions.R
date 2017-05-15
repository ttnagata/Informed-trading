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


# Function for Market Model and UNCONDITIONAL bootstrap
MethodA_calculation <- function(relevant_date, asset_returns) 
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
                      obs = "not enough data for model estimation",
                      row.names = NULL))
  
  # Estimation window and event window data
  estimation_window <- first(prev_days, "240 days")
  event_window <- xts::last(rbind(prev_days, next_day[1]), "4 days")
  
  
  # Market Model over estimation window
  regression <- lm(estimation_window[,1] ~estimation_window[,2])
  
  # Calculating abnormal returns for the event window
  abnormal_returns_event <- c(event_window[,1] - 
                               regression$coefficients[1] - 
                               event_window[,2] * regression$coefficients[2])
  CAR_4days <- sum(abnormal_returns_event)
  CAR_2days <- sum(abnormal_returns_event[1:2])
  
  #Calculating abnormal returns for the estimation window
  abnormal_returns <- c(estimation_window[,1] - 
                         regression$coefficients[1] - 
                         estimation_window[,2] * regression$coefficients[2])
  colnames (abnormal_returns) <- "abnormal_returns"
  
  #Bootstrap 4 day CAR
  bootstrap_input <- as.data.frame(abnormal_returns)
  bootstrap_event <- replicate (10000,  
                                sample(bootstrap_input$abnormal_returns, 
                                       4, 
                                       replace = TRUE)) %>%
    t() %>%
    tbl_df() %>%
    mutate(CAR = V1+V2+V3+V4)
  Upper_cutoff <- quantile(bootstrap_event$CAR, 0.995)
  Lower_cutoff <- quantile(bootstrap_event$CAR, 0.005)
  
  # Bootstrap 2 day CAR unconditional
  bootstrap_pre_event <- replicate (10000,  
                                    sample(bootstrap_input$abnormal_returns, 
                                           2, 
                                           replace = TRUE)) %>%
    t() %>%
    tbl_df() %>%
    mutate(CAR2 = V1+V2) 
  Upper_cutoff_pre <- quantile(bootstrap_pre_event$CAR2, 0.95)
  Lower_cutoff_pre <- quantile(bootstrap_pre_event$CAR2, 0.05)
  
  # Output
  out <- data.frame(Announc_date = relevant_date, 
                   Upper_4CAR = Upper_cutoff, 
                   Lower_4CAR = Lower_cutoff, 
                   CAR_4days = CAR_4days,
                   Upper_2CAR = Upper_cutoff_pre, 
                   Lower_2CAR = Lower_cutoff_pre, 
                   CAR_2days = CAR_2days,
                   obs = NA,
                   row.names = NULL)
  return(out)
}


# Function to analyse announcements using asset returns
MethodA_analysis <- function(asset_returns, announcements)
{
  # Extract the asset's name from column name, company name, announcement dates
  asset_ticker <- strsplit(names(asset_returns)[1], "_")[[1]][1]                           
  asset_company_name <- substr(asset_ticker, start = 1 , stop = 4 )                                  
  asset_announc_dates <- filter(.data = announcements, 
                                Company == asset_company_name)  
  
  # Stop function if there are no announcements for this asset
  if (nrow(asset_announc_dates) == 0) 
    return(data.frame(Announc_date = NA,
                      Upper_4CAR = NA,
                      Lower_4CAR = NA, CAR_4days = 0,
                      Upper_2CAR = NA, Lower_2CAR = NA, CAR_2days = 0,
                      obs = "no announcements for this company",
                      event = NA, pre_event = NA, asset = asset_ticker,
                      row.names = NULL))
    
  
  # Get all CAR limits for all announcements of this asset
  CAR_limits <- Reduce(rbind, 
                       lapply(X = unique(asset_announc_dates$News.Date), 
                              FUN = MethodA_calculation, 
                              asset_returns = asset_returns ))
  
  # Classify if events/pre-events CAR are relevant 
  CAR_analysis <- tbl_df(CAR_limits) %>%
    mutate(event = factor(ifelse(CAR_4days<Lower_4CAR | CAR_4days>Upper_4CAR, 
                                 "relevant", "ok"))) %>%
    mutate(pre_event = factor(ifelse((CAR_2days<Lower_2CAR |
                                      CAR_4days>Upper_2CAR) &
                                      CAR_2days*CAR_4days>0 &
                                      event=="relevant",
                                     "IPM", "ok")))  %>%
    mutate(asset = asset_ticker)
  return(CAR_analysis)
}


# Function for market model and UNCONDITIONAL bootstrap for method A
Bias_calculation <- function(relevant_date, asset_returns) 
{
  # Extract estimation window and event window data
  prev_days <- xts::last(asset_returns[index(asset_returns)<= relevant_date], 
                    "250 days")
  next_day <- asset_returns[index(asset_returns)> relevant_date]
  
  # Stop function in case there is not enough data to apply model  
  if (nrow(prev_days)<250 | nrow(next_day)<1) 
    return(data.frame(Announc_date = relevant_date,
                      false_sig_announc = NA,
                      false_IPM = NA,
                      obs = "not enough data for model estimation",
                      row.names = NULL))
  
  # Estimation window and event window data
  estimation_window <- first(prev_days, "240 days")
  event_window <- xts::last(rbind(prev_days, next_day[1]), "4 days")
  
  # Market Model over estimation window
  regression <- lm(estimation_window[,1] ~estimation_window[,2])
  
  # Calculate abnormal returns for the estimation window
  abnormal_returns <- c(estimation_window[,1] - 
                          regression$coefficients[1] - 
                          estimation_window[,2] * 
                          regression$coefficients[2])
  colnames (abnormal_returns) <- "abnormal_returns"
  
  # Bootstrap 4 day CAR
  bootstrap_input <- as.data.frame(abnormal_returns)
  bootstrap_event <- replicate (10000,  
                                sample(bootstrap_input$abnormal_returns,
                                       4, replace = TRUE)) %>%
    t() %>%
    tbl_df() %>%
    mutate(CAR = V1+V2+V3+V4)
  Upper_cutoff <- quantile(bootstrap_event$CAR, 0.995)
  Lower_cutoff <- quantile(bootstrap_event$CAR, 0.005)
  
  # Bootstrap 2 day CAR unconditional
  bootstrap_pre_event <- replicate (10000,  
                                    sample(bootstrap_input$abnormal_returns, 
                                           2, replace = TRUE)) %>%
    t() %>%
    tbl_df() %>%
    mutate(CAR2 = V1+V2) 
  Upper_cutoff_event <- quantile(bootstrap_pre_event$CAR2, 0.95)
  Lower_cutoff_event <- quantile(bootstrap_pre_event$CAR2, 0.05)
  
  # Bootstrap bias correction simulation of event
  bootstrap_bias <- replicate (10000,  
                               sample(bootstrap_input$abnormal_returns, 
                                      4, replace = TRUE)) %>%
    t() %>%
    tbl_df() %>%
    mutate(CAR = V1+V2+V3+V4) %>%
    filter(CAR>Upper_cutoff | CAR<Lower_cutoff) %>%
    mutate(CAR2 = V1+V2) 
  
  aux_bias <- bootstrap_bias %>%
    filter(CAR2>Upper_cutoff_event | CAR2<Lower_cutoff_event) %>%
    filter(CAR*CAR2>0)
  fake_sig_announc <- nrow(bootstrap_bias)
  fake_sig_pre <- nrow(aux_bias)
  
  # Output
  out <- data.frame( Announc_date = relevant_date,
                     false_sig_announc = fake_sig_announc,
                     false_IPM = fake_sig_pre,
                     obs = NA
  )
  return(out)
}


# Function to calculate bias correction in method A
Bias_analysis = function(asset_returns, announcements)
{
  # Extract the asset's name, company name, announcement dates
  asset_ticker <-strsplit(names(asset_returns)[1], "_")[[1]][1]                           
  asset_company_name <- substr(asset_ticker, start=1 , stop= 4 )   
  asset_announc_dates <- filter(announcements, 
                               Company==asset_company_name)
  
  # Stop function if there are no announcements for this asset
  if (nrow(asset_announc_dates)==0) 
    return(data.frame(Announc_date = NA,
                      false_sig_announc = NA,
                      false_IPM = NA,
                      obs="no announcements for this company",
                      asset=asset_ticker,
                      row.names = NULL))
  
  
  # Get all CAR limits for all announcements of this asset
  CAR_limits <- Reduce(rbind,
                      lapply(unique(asset_announc_dates$News.Date),
                                    FUN = Bias_calculation, 
                                    asset_returns=asset_returns)) %>%
    mutate(asset=asset_ticker)
  
  return(CAR_limits)
}