## load libraries
library(readr)
library(forecast)
library(lubridate)
library(tidyverse)
library(fable)

# read data
GA_usertype_transformedData <- read_csv("GA_usertype_transformedData.csv")
View(GA_usertype_transformedData) ## View data

## select columns for transactions
transactions <- GA_usertype_transformedData[, c(2, 3:6)]
View(transactions)
#View(transactions$New_users_transactions + transactions$Returning_users_transactions)

## sum treansactions of new users and returning users (daily)
transactions[, "Total_transactions"] <- (transactions[, 4] + transactions[, 5])

# sum total transactions for each month
transactions_monthly <- transactions%>%
  group_by(Year, Month)%>%
  summarise(Total_Monthly_transactions = sum(Total_transactions))%>%
  ungroup()%>%
  mutate(Monthly_adjusted = Total_Monthly_transactions)



# convert to time series
transactions_monthly_ts <- ts(transactions_monthly, frequency = 12, start = c(2014, 11))

# Adjust for unequal number of days in months by diving total monthly transactions by number of month days
transactions_monthly_ts[,4] = transactions_monthly_ts[,3] / monthdays(transactions_monthly_ts[,2])

## convert ts to df and save
transactions_monthly <- as.data.frame(transactions_monthly_ts)
write.csv(transactions_monthly, file = "GA_transactions_monthly.csv")


autoplot(transactions_monthly_ts[, 4])
checkresiduals(transactions_monthly_ts[,4])

#### decompose timeseries
## additve decompose
autoplot(decompose(transactions_monthly_ts[,4]), type = "additive")

### partition training and testing dataset
transactions_ts_train <- window(transactions_monthly_ts, end = c(2019, 10))
transaction_ts_test <- window(transactions_monthly_ts, start = c(2019, 11))

# Simple Exponential smoothing applied to training set for 5 month forecast
#### Not appropriate method because data has trend and seasonality 
transaction_train_ses <- ses(transactions_ts_train[, 4], h = 10)
autoplot(transaction_train_ses)
accuracy(transaction_train_ses, transaction_ts_test[,4])

### remove trend from data and fit ses
diff_transaction_ts_train <- diff(transactions_ts_train)
# fit ses
diff_ses_train <- ses(diff_transaction_ts_train[,4], h = 10)
autoplot(diff_ses_train) ## plot model
## check accuracy of model on test data
diff_transaction_ts_test <- diff(transaction_ts_test) ## differencing test set
accuracy(diff_ses_train, diff_transaction_ts_test[,4])


## using Holt method of exponential smoothing to forecast trend
holt_transaction_train <- holt(transactions_ts_train[,4], h = 5)  ## model chose  alpha = 0.2291, beta  = 0.0272
autoplot(holt_transaction_train)
# check residuals
checkresiduals(holt_transaction_train)
#### test holt_transaction_train model on test set
accuracy(holt_transaction_train, transaction_ts_test[,4])
summary(holt_transaction_train)
## determing an optimal alpha  (optimal alpha was determined to be 0.457 at which RMSE is 7.25)
holt_alpha <- seq(0.0001, 0.5, by = 0.001)
holt_RMSE <- NA
for(i in seq_along(holt_alpha)){
  holt_transaction_train_loop <- holt(transactions_ts_train[,4], h = 5, alpha = holt_alpha[i])
  holt_RMSE[i] <- accuracy(holt_transaction_train_loop, transaction_ts_test[,4])[2,2]
  
}

result_frame <- data_frame(holt_alpha, holt_RMSE)
holt_apha_min <- filter(result_frame, holt_RMSE == min(holt_RMSE))
ggplot(result_frame, aes(holt_alpha, holt_RMSE)) + geom_line() +
  geom_point(data = holt_apha_min, aes(holt_alpha, holt_RMSE, color = "red"))

## determining an optimal beta value (trend) from Holt model fitted  
#(optimal beta was determined to be 0.223 for which RMSE was 8.36
holt_beta <- seq(0.0001, 0.5, by = 0.001)
holt_beta_RMSE <- NA
for(i in seq_along(holt_beta)){
  holt_transaction_train_loop <- holt(transactions_ts_train[,4], h = 5, beta = holt_beta[i])
  holt_beta_RMSE[i] <- accuracy(holt_transaction_train_loop, transaction_ts_test[,4])[2,2]
  
}
beta_result_frame <- data_frame(holt_beta, holt_beta_RMSE)
holt_beta_min <- filter(beta_result_frame, holt_beta_RMSE == min(holt_beta_RMSE))
ggplot(data = beta_result_frame, aes(holt_beta, holt_beta_RMSE)) + geom_line() +
  geom_point(data = holt_beta_min, aes(holt_beta, holt_beta_RMSE))

#### refit model with optimal beta value
holt_transaction_train_opt <- holt(transactions_ts_train[,4], beta = 0.223, h = 5)
## check accuracy of predition of refit model
accuracy(holt_transaction_train_opt, transaction_ts_test[,4])

## check residuals of holt model with optimal beta --- autocorrelation present
checkresiduals(holt_transaction_train_opt)

## plot of holt model with optimal beta value
autoplot(holt_transaction_train_opt)

#### plot orginal model and optimal mode
orig <- autoplot(holt_transaction_train) +
  ggtitle("Holt model (initial model)") + coord_cartesian(ylim = c(-50, 100))

opti <- autoplot(holt_transaction_train_opt) + 
  ggtitle("Holt model (optimal beta value used)") + coord_cartesian(ylim = c(-50, 100))

grid.arrange(orig, opti, nrow = 1)


######### Holt-Winters method for trend and seasonal data ########## Model selected ANN as default best
ets_ANN <- ets(transactions_ts_train[,4], model = "ZZZ") ## fit exponential smoothing with holt-winter with model unknown
checkresiduals(ets_ANN) ## check residuals
ets_ANN_fort <- forecast(ets_ANN) ## forecast 
accuracy(ets_ANN_fort, transaction_ts_test[,4]) ## check accuracy of prediction on test data
autoplot(ets_ANN) ## plot model
autoplot(ets_ANN_fort) ## plot model with forecast
summary(ets_ANN_fort)
summary(ets_ANN)

###### Determing optimal gamma value for ets_ANN
## default optimal model selected was ANN optimal gamma was 0.306 at which RMSE was 20.2

gamma = seq(0.0001, 0.5, by = 0.001)
RMSE = NA
for(i in seq_along(gamma)){
  ets_zzz <- ets(transactions_ts_train[,4], model = "ZZZ", gamma = gamma[i])
  ets_zzz_fort <- forecast(ets_zzz)
  
  RMSE[i] <- accuracy(ets_zzz_fort, transaction_ts_test[,4])[2,2]
  
  ets_zzz
}
gamma_df <- data_frame(gamma, RMSE)
gamma_min <- filter(gamma_df, RMSE == min(RMSE))
ggplot(gamma_df, mapping = aes(gamma, RMSE)) + geom_line() +
  geom_point(gamma_min, mapping = aes(gamma, RMSE), color = "red")

## dertermining optimal beta value for model = ZZZ  DEFAULT OPTIMAL model chosen is ANN
# optimal beta value is  0.342 
# at which RMSE = 20.2
beta = seq(0.0001, 0.5, 0.001)
RMSE = NA
for(i in seq_along(beta)){
  ets_zzz <- ets(transactions_ts_train[,4], model = "ZZZ", beta = beta[i])
  ets_zzz_fort <- forecast(ets_zzz)
  RMSE[i] <- accuracy(ets_zzz_fort, transaction_ts_test[,4])[2,2]
}
beta_zzz <- data_frame(beta, RMSE)
RMSE_zzz <- filter(beta_zzz, RMSE == min(RMSE))
ggplot(beta_zzz, mapping = aes(beta, RMSE)) + geom_line() +
  geom_point(RMSE_zzz, mapping = aes(beta, RMSE), color = "red")

## Determining alpha value for model = ZZZ 
## Optimal alpha is 0.0171 at which RMSE = -12.9
alpha = seq(0.0001, 0.5, 0.001)
RMSE = NA
for(i in seq_along(alpha)){
  ets_zzz <- ets(transactions_ts_train[,4], alpha = alpha[i])
  ets_zzz_fort <- forecast(ets_zzz)
  RMSE[i] <- accuracy(ets_zzz_fort, transaction_ts_test[,4])
}
alpha_zzz <- data_frame(alpha, RMSE)
RMSE_zzz_min <- filter(alpha_zzz, RMSE == min(RMSE))
ggplot(alpha_zzz, mapping = aes(alpha, RMSE)) + geom_line() +
  geom_point(RMSE_zzz_min, mapping = aes(alpha, RMSE), color = "red") 

## refiting model with optimal values for beta and gamma
ets_zzz_opt <- ets(transactions_ts_train[,4], beta = 0.342, gamma = 0.306, model = "ZZZ")
ets_zzz_opt_fort <- forecast(ets_zzz_opt)  
accuracy(ets_zzz_opt_fort, transaction_ts_test[,4])
autoplot(ets_zzz_opt_fort)
summary(ets_zzz_opt)
checkresiduals(ets_zzz_opt)

#### fitting damped model  --- the default moldel used was A,Ad,N
ets_zzz_damped <- ets(transactions_ts_train[,4], model = "ZZZ", damped = TRUE)
ets_zzz_fort_damped <- forecast(ets_zzz_opt_damped)  
accuracy(ets_zzz_fort_damped, transaction_ts_test[,4])
autoplot(ets_zzz_fort_damped)
summary(ets_zzz_damped)
checkresiduals(ets_zzz_damped)
###### determine optimal beta for damped model  ## optimal beta when damped = 0.192 
# for which RMSE 18.7
beta_damp <- seq(0.0001, 0.5, 0.001)
RMSE_damp <- NA
for(i in seq_along(beta_damp)){
  ets_zzz_damp <- ets(transactions_ts_train[,4], model = "ZZZ", damped = T, beta = beta[i] )
  ets_zzz_damp <- forecast(ets_zzz_damp)
  RMSE_damp[i] <- accuracy(ets_zzz_damp, transaction_ts_test[,4])[2,2]
}
beta_damp_df <- data_frame(beta_damp, RMSE_damp)
RMSE_damp_min <- filter(beta_damp_df, RMSE_damp == min(RMSE_damp))
ggplot(beta_damp_df, mapping = aes(beta_damp, RMSE_damp)) + geom_line() +
  geom_point(RMSE_damp_min, mapping = aes(beta_damp, RMSE_damp), color = "red")

## fit damped model with optimal beta value
ets_zzz_damped_betaopti <- ets(transactions_ts_train[,4], model = "ZZZ", damped = T, beta = 0.192)
ets_zzz_damped_betaopti_fort <- forecast(ets_zzz_damped_betaopti)
accuracy(ets_zzz_damped_betaopti_fort, transaction_ts_test[,4])
summary(ets_zzz_damped_betaopti)
checkresiduals(ets_zzz_damped_betaopti)
autoplot(ets_zzz_damped_betaopti_fort)

## determine optimal value for gamma for damped model  # optimal gamma is 0.201 for which 
# RMSE is 18.4
gammma_damp <- seq(0.0001, 0.5, by = 0.001)
RMSE <- NA
for(i in seq_along(gammma_damp)){
  ets_zzz_damp <- ets(transactions_ts_train[,4], model = "ZZZ", damped = T, gamma = gammma_damp[i])
  ets_zzz_damp_fort <- forecast(ets_zzz_damp)
  RMSE[i] <- accuracy(ets_zzz_damp_fort, transaction_ts_test[,4])[2,2]
}
gammma_damp_df <- data_frame(gammma_damp, RMSE)
RMSE_min <- filter(gammma_damp_df, RMSE == min(RMSE))
ggplot(gammma_damp_df, mapping = aes(gammma_damp, RMSE)) + geom_line() +
  geom_point(RMSE_min, mapping = aes(gammma_damp, RMSE), color = "red")

## refitting with optimal gamma value damped model
ets_zzz_damp_gammaopti <- ets(transactions_ts_train[,4], model = "ZZZ", damped = T, gamma = 0.201)
ets_zzz_damp_gammaopti_fort <- forecast(ets_zzz_damp_gammaopti)
accuracy(ets_zzz_damp_gammaopti_fort, transaction_ts_test[,4])
checkresiduals(ets_zzz_damp_gammaopti)
summary(ets_zzz_damp_gammaopti)
autoplot(ets_zzz_damp_gammaopti_fort)

## refit with optimal beta and gamma value for damped model 
ets_zzz_damp_opti <- ets(transactions_ts_train[,4], model = "ZZZ", damped = T, beta = 0.192, gamma = 0.201)
ets_zzz_damp_opti_fort <- forecast(ets_zzz_damp_opti)
accuracy(ets_zzz_damp_opti_fort, transaction_ts_test[,4])
checkresiduals(ets_zzz_damp_opti_fort)
summary(ets_zzz_damp_opti)
autoplot(ets_zzz_damp_opti_fort)

######### ploting timeseries with exponential smoothing and including damped
autoplot(transactions_ts_train[,4], series = "Average Monthly Transaction data") + 
  autolayer(ets_zzz_opt_fort$mean,  series = "ETS (ANN) forecast") +
  autolayer(ets_zzz_damp_opti_fort$mean, series = "ETS (AAdN) damped forecast") + ylab("Average Monthly transaction") +
  guides(colour = guide_legend(title = "Legend")) + theme(legend.position = "bottom") + ggtitle("Forecasting with Exponential smoothing -- ETS(ANN)")

### fit model with AAA for ets -- default used were alpha = 0.2335, beta  = 0.0288, gamma = 1e-04 
ets_AAA <- ets(transactions_ts_train[,4], model = "AAA")
ets_AAA_fort <- forecast(ets_AAA)
accuracy(ets_AAA_fort, transaction_ts_test[,4])
checkresiduals(ets_AAA_fort)


######### exploration of timeseries
ggAcf(transactions_monthly_ts[,4])
gg_season(transactions_monthly_ts[,4])
ggseasonplot(transactions_monthly_ts[,4], polar = T)

gglagchull(transactions_monthly_ts[,4])
gglagplot(transactions_monthly_ts[,4])
# trypred <- ets(transaction_ts_test[,4], model = "ANN", beta = 0.342, gamma = 0.306)
# View(transaction_ts_test)
# acc_ets <- accuracy(ets_zzz_opt_fort, transaction_ts_test[,4])
