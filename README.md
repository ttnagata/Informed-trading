# Informed trading estimation

> Master thesis: Assessment of informed trading in the Brazilian equity market: Market cleanliness methodology
> Author: Tatiana Tiemi Nagata
> Version: 0.1.0

This repository contains the code for the estimation of informed trading used in the master thesis titled: "Assessment of informed trading in the Brazilian equity market: Market cleanliness methodology." The thesis was submitted to the Graduate School of Public Policy, The University of Tokyo, for the degree of Master in Public Policy in August 2017.

The examples described below demonstrate how to calculate the level of informed trading described in the thesis. The R functions for the analysis can be found in the "src" folder.

  
### Example for Method A estimation
```R
# Loading libraries
library(dplyr)
library(PerformanceAnalytics)
library(tseries)

# Create of announcements dataframe
news_data <- data.frame(Company = c("OGXP", "OGXP", "OGXP",
                                    "PETR", "PETR","PETR"),
                        News.Date = as.Date(c("2012-06-28", "2013-10-31",
                                              "2013-12-06", "2014-12-07", 
                                              "2014-11-18", "2015-04-13"),
                                            format = "%Y-%m-%d"))

# Create of vector of asset tickers to be analysed
asset_tickers <- c("PETR3", "OGXP3")

# Create index time series
index_data = get.hist.quote(instrument="^BVSP", start="2010-01-01",
                            end=Sys.Date(), quote="AdjClose",
                            provider="yahoo", compression="d", retclass="zoo")

# Wrangling data: list of asset and index returns
list_returns <- lapply (X = asset_tickers,
                        FUN = Returns_calculation,
                        index_data = index_data)


# Calculation of Method A for retuns data
MethodA_results <- Reduce(rbind,
                          lapply(X = list_returns,
                                 FUN = MethodA_analysis,
                                 announcements = news_data))

# Summary of Method A results
relevant_events = filter(MethodA_results, obs %in% NA) %>%
  mutate(year = format(Announc_date, "%Y")) %>%
  group_by(event, year) %>%
  summarise(total_relevant=n()) %>%
  filter(event != "ok")

relevant_pre_events = filter(MethodA_results, obs %in% NA) %>%
  mutate(year = format(Announc_date, "%Y")) %>%
  group_by(pre_event, year) %>%
  summarise(total_pre_event=n()) %>%
  filter(pre_event != "ok")

summary = full_join(relevant_events, relevant_pre_events, by="year" ) %>%
  mutate(proportion=total_pre_event/total_relevant)

# Bias correction
bias_results <- Reduce(rbind,
                       lapply(X = list_returns,
                              FUN = Bias_analysis,
                              announcements = news_data))


bias_summary <- filter(bias_results, obs %in% NA) %>%
  mutate(year = format(Announc_date, "%Y")) %>%
  group_by(year) %>%
  summarise(mk = sum(false_sig_announc)/10000,
            total_false_IPM = sum(false_IPM)/10000,
            total_announc = n()) 
```
### Example for Method B estimation
```R
# Loading libraries
library(dplyr)
library(xts)
library(lmtest)
library(dynlm)
library(sandwich)
library(FinTS)
library(rugarch)
library(tseries)
library(PerformanceAnalytics)

# Create of announcements dataframe
news_data <- data.frame(Company = c("OGXP", "OGXP", "OGXP",
                                    "PETR", "PETR","PETR"),
                        News.Date = as.Date(c("2012-06-28", "2013-10-31",
                                              "2013-12-06", "2014-12-07", 
                                              "2014-11-18", "2015-04-13"),
                                            format = "%Y-%m-%d"))

# Create of vector of asset tickers to be analysed
asset_tickers <- c("PETR3", "OGXP3")

# Create index time series
index_data = get.hist.quote(instrument="^BVSP", start="2010-01-01",
                            end=Sys.Date(), quote="AdjClose",
                            provider="yahoo", compression="d", retclass="zoo")

# Wrangling data: list of asset and index returns
list_returns <- lapply (X = asset_tickers,
                        FUN = Returns_calculation,
                        index_data = index_data)


# Calculation of Method B for retuns data
MethodB_results <- Reduce(rbind,
                          lapply(X = list_returns,
                                 FUN = MethodB_analysis,
                                 announcements = news_data))

# Summary of Method B results
relevant_events <- MethodB_results %>%
  filter(!(obs1 %in% "not enough data for model estimation")) %>%
  filter(!(obs1 %in% "no announcements for this company")) %>%
  mutate(year = format(Announc_date, "%Y")) %>%
  group_by(event, year) %>%
  summarise(total_relevant=n()) %>%
  filter(event != "ok")

relevant_pre_events <- MethodB_results %>%
  filter(!(obs1 %in% "not enough data for model estimation")) %>%
  filter(!(obs1 %in% "no announcements for this company")) %>%
  mutate(year = format(Announc_date, "%Y")) %>%
  group_by(pre_event, year) %>%
  summarise(total_IPM = n()) %>%
  filter(pre_event != "ok")

summary = full_join(relevant_events, relevant_pre_events, by="year" ) %>%
  mutate(proportion = total_IPM/total_relevant)
```