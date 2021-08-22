############### Transaction usertype data  ###########################
GA_usertype_transformedData <- read_csv("~/GA_usertype_transformedData.csv")
usertypedata_mean <- GA_usertype_transformedData%>%
  group_by(Year, Month)%>%
  summarise(across(everything(), mean))%>%
  ungroup()



######################## Transaction usertype data reactive  ##########################################  
usertype_data_reactive <- reactive({
  transaction_year <- input$trans_year
  transaction_month <- input$trans_month
  
  usertypedata_mean%>%
    filter(Year == transaction_year & Month == transaction_month)
})

################# transaction data wrangling for timeseries analysis  #######################
GA_transactions_monthly <- read_csv("GA_transactions_monthly.csv")
transactions_monthly <- GA_transactions_monthly
transactions_monthly <- transactions_monthly[, c(2, 3:5)]
transactions_monthly_ts <- ts(transactions_monthly, frequency = 12, start = c(2014, 11))


###################### Transaction Exponential smoothing  ##############################
### partition training and testing dataset
transactions_ts_train <- window(transactions_monthly_ts, end = c(2019, 10))
transaction_ts_test <- window(transactions_monthly_ts, start = c(2019, 11))

## Holt-Winters-- refiting model with optimal values for beta and gamma
ets_zzz_opt <- ets(transactions_ts_train[,4], beta = 0.342, gamma = 0.306, model = "ZZZ")
ets_zzz_opt_fort <- forecast(ets_zzz_opt)  

#### fitting damped model  --- the default moldel used was A,Ad,N
ets_zzz_damped <- ets(transactions_ts_train[,4], model = "ZZZ", damped = TRUE)
ets_zzz_fort_damped <- forecast(ets_zzz_damped) 


##################################### full data ###############################################
full_data <- read_csv("~/full_data.csv")
full_data_ts <- ts(full_data, start = c(2014, 11), frequency = 12)
## Partition data into training and testing set
(train_full_data_forecast <- window(full_data_ts, start = c(2014, 11), end = c(2018, 6)))
(test_full_data_forecast <- window(full_data_ts, start = c(2018, 7)))

####################### full data reactive object  ##########################################
full_data_reactive <- reactive({
  year_select <- input$yearsel
  month_select <- input$monthsel
  full_data%>%
    filter(Year == year_select & Month == month_select)
})

