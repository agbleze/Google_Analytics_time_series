### load libraries  ######
library(shiny)
library(readr)
library(forecast)
library(tidyverse)
library(ggplot2)
library(fpp2)
library(lubridate)
library(GGally)
library(dplyr)
library(magrittr)
library(labelled)
library(gtsummary)
library(bfast)
library(ggstatsplot)
library(googleVis)
library(formattable)
library(fontawesome)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {     
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
   
   ################# transaction data wrangling for timeseries analysis  #######################
   GA_transactions_monthly <- read_csv("GA_transactions_monthly.csv")
   transactions_monthly <- GA_transactions_monthly
   transactions_monthly <- transactions_monthly[, c(2, 3:5)]
   transactions_monthly_ts <- ts(transactions_monthly, frequency = 12, start = c(2014, 11))
   
 ######################################  REVENUE UI #############################################  
   output$newuser_revenue <- renderValueBox({
      revenue_selectyear <- input$revenue_selectyear
      revenue_selectmonth <- input$revenue_selectmonth
     
      newuser_revenue_display <- usertypedata_mean%>%
        filter(Year == revenue_selectyear & Month == revenue_selectmonth)%>%
         select(New_Users_Revenue)
      
      valueBox(paste0("$", comma(newuser_revenue_display, digits = 2)), 
               paste0("New Users ( Monthly Average)" ), width = 6, icon = icon("funnel-dollar"))
   })
   
   output$returnuser_revenue <- renderValueBox({
      revenue_selectyear <- input$revenue_selectyear
      revenue_selectmonth <- input$revenue_selectmonth
      
      returninguser_revenue_display <- usertypedata_mean%>%
         filter(Year == revenue_selectyear & Month == revenue_selectmonth)%>%
         select(Returning_Users_Revenue)
      
      valueBox(paste0("$", comma(returninguser_revenue_display, digits = 2)),
               subtitle = "Returning Users ( Monthly Average)", color = "aqua", width = 6, icon = icon("file-invoice-dollar"))
   })
   
   
   output$revenue_timeries <- renderPlot({
       autoplot(full_data_ts[,16])+ ylab("Average Monthly Revenue") + 
         ggtitle("Timeseries of Average Mothly Revenue")
   })
   
   output$revenue_seasonality <- renderPlot({
       (season_revenue_full_data <- ggseasonplot(full_data_ts[,16], year.labels = T, 
                                                 year.label.left = T, year.label.right = T)+ ylab("Average Monthly Revenue") +
           ggtitle(label = "Seasonal Timeseries plot of Average Monthly Revenue")
        )
   })
   
   ## revenue forecast with seasonal naive forecast
   output$seasonal_forecast <- renderPlot({ 
      h = as.numeric(input$revenue_horizon_forecast) 
       revenue_snaive <- snaive(train_full_data_forecast[,16], h = h)
       autoplot(train_full_data_forecast[,16], series = "Data") +
           autolayer(revenue_snaive, series = "Seasonal Naive", PI = T) + ylab("Average Monthly Revenue") +
          guides(colour = guide_legend(title = "Legend", title.position = "top")) + 
          theme(legend.position = "bottom") + ggtitle("Forecast of revenue using on seasonal mean forecasting")
   })
   
   output$revenue_trendSeasonal_forecast <- renderPlot({
      (fitfor <- tslm(full_data_ts[, 16] ~ trend + season))
      forecast(fitfor) %>%
         autoplot() + ylab("Average Monthly Revenue") 
   })
   
   output$regress_model <- renderPlot({
      ggcoefstats(
         x = stats::lm(formula = log(full_data_ts[,16] + 1) ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration +
                          Avg_sessionPer_user + Avg_Pg_session, data = full_data_ts),
         ggtheme = ggplot2::theme_gray(), # changing the default theme
         title = "Regression analysis: Predicting the influence of various KPI on Revenue",
      )      
   })
   
   output$revenue_decompose <- renderPlot({
      (revdecompose_mult <- decompose(full_data_ts[,16], type = "multiplicative") %>%
          autoplot() + ylab("Average Monthly Revenue")) 
   })
   
   output$revenue_stl_forecast <- renderPlot({
      (rev_stlf_meth <-  stlf(full_data_ts[,16]) %>%
          autoplot() + ylab("Average Monthly Revenue"))
      
   })
   
   output$revenue_change_detect <- renderPlot({
      (data_bfast <- bfast::bfast(full_data_ts[,16], decomp = "stl"))
      plot(data_bfast)
   })
   
   output$revenue_bfastmonitor <- renderPlot({
      (data_bfastmonitor <- bfastmonitor(full_data_ts[,16], start = c(20014, 11), plot = T))  
   })
   
   ###################### TRANSACTIONS UI ############################################
   output$newuser_transactions <- renderValueBox({
      newuser_transactions_display <- usertype_data_reactive()%>%
         select(New_users_transactions)
      
      valueBox(value = paste0(comma(newuser_transactions_display, digits = 2)), 
               subtitle = "New Users (Monthly Average)",
               color = "yellow", icon = icon("hand-holding-usd"))
   })
   
   output$returnuser_transactions <- renderValueBox({
      returnuser_transactions_display <- usertype_data_reactive()%>%
         select(Returning_users_transactions)
      
      valueBox(value = paste0(comma(returnuser_transactions_display, digits = 2)), 
               subtitle = "Returning Users (Monthly Average)", color = "yellow", icon = icon("comments-dollar"))
   })
   
   output$transaction_timeseries <- renderPlot({
      autoplot(transactions_monthly_ts[,4]) + ylab("Average Monthly Transactions") + 
         ggtitle("Timeseries of Average Monthly Transactions")
   })
   
   output$transaction_decomp <- renderPlot({
      (transaction_decomposition <- autoplot(decompose(transactions_monthly_ts[,4])))
   })
   
   output$transaction_seasonal <- renderPlot({
      ggseasonplot(transactions_monthly_ts[,4], polar = T) + ggtitle("Seasonal Polar plot of Average Monthly Transactions")
   })
   
   output$transaction_autocorrelation <- renderPlot({
      ggAcf(transactions_monthly_ts[,4]) + ggtitle("Autocorrelation of Average Monthly Transactions")
   })
   
   output$transaction_exponential <- renderPlot({ 
      ######### ploting timeseries with exponential smoothing and including damped
      (autoplot(transactions_ts_train[,4], series = "Average Monthly Transactions data") + 
         autolayer(ets_zzz_opt_fort$mean,  series = "ETS (A,N,N) forecast", PI = T) +
         autolayer(ets_zzz_opt_fort$fitted, series = "ETS (A,N,N) fitted values") +
        # autolayer(ets_zzz_damp_fort$fitted) +
         autolayer(ets_zzz_fort_damped$mean, series = "ETS (A,Ad,N) damped forecast") + ylab("Average Monthly transactions") +
         guides(colour = guide_legend(title = "Legend", title.position = "top")) + 
          theme(legend.position = "bottom") + 
         ggtitle("Forecasting with Exponential smoothing -- ETS(A,N,N), ETS(A,Ad,N)"))
  })
   
   output$ets_residuals <- renderPlot({
      checkresiduals(ets_zzz_opt)
   })
   
  ############################### Business question #########################################
   output$correlation <- renderPlot({
      as.data.frame(full_data_ts[, c(12, 13:19)])%>%
         ggpairs()
   })
   
   ######################## SCENARIO FORECASTING UI #####################################
   output$scenario_forecast <- renderPlot({
      #### regression model of log + 1 Avg_revenue against all predictors without Avg_session
      ### Model 4
      model_tsall_log_butAvgsession <- tslm(log(full_data_ts[,16] + 1) ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration +
                                                Avg_sessionPer_user + Avg_Pg_session, data = full_data_ts)
      
      horizon_forecast = input$horizon_forecast
      h = horizon_forecast
      
      ##### retrieve values for predictors for scenario forecasting
      # determine if scenario is decrease or increase and convert decrease to negatives and increase to positives
      ECR <- if(input$forecast_type_erc == "Decrease"){
         -(input$erc_sce)
      } else{
         input$erc_sce
      }
      
      bounce_rate <- if(input$forecast_type_bounce_rate == "Decrease"){
         -(input$bounce_rate_sce)
      } else{
         input$bounce_rate_sce
      }
      
      Users <- if(input$forecast_type_user == "Decrease"){
         -(input$user_sce)
      } else{
         input$user_sce
      }
      
      session_duration <- if(input$forecast_type_session_dur == "Decrease"){
         -(input$session_dur_sce)
      } else{
         input$session_dur_sce
      }
      
      sessionPer_user <- if(input$forecast_type_session_peruser == "Decrease"){
         -(input$session_peruser_sce)
      } else{
         input$session_peruser_sce
      }
      
      Pg_session <- if(input$forecast_type_pages_persession == "Decrease"){
         -(input$pages_persession_sce)
      } else{
         input$session_peruser_sce
      }
      
      new_data <- data.frame(
         Avg_ECR = rep(ECR, h),
         Avg_bounce_rate = rep(bounce_rate, h),
         Avg_Users = rep(Users, h),
         Avg_session_duration = rep(session_duration, h),
         Avg_sessionPer_user = rep(sessionPer_user, h),
         Avg_Pg_session = rep(Pg_session, h)
      )
      
      ## FORECAST SCENARIO
      # use model_tsall_log_butAvgsession to forecast based on scenario specified
      fcast_scenario <- forecast(model_tsall_log_butAvgsession, newdata = new_data)
      
      # convert forecast to dataframe
      fcast_scenario_dataframe <- data.frame(fcast_scenario)
      
      # backtransform forecast to get actual values of log
      fcast_scenario_dataframe_backtransform <- fcast_scenario_dataframe%>%
         # add column of backtransformed point forecast
         mutate(backtransfrom_Point.forcast = exp(fcast_scenario_dataframe$Point.Forecast))
      
      ## convert forecast to time series
      fcast_scenario_ts <- ts(fcast_scenario_dataframe_backtransform, start = c(2021, 8), frequency = 12)
      
      # Plot timeseries of data
      autoplot(full_data_ts[, 16], series = "Actual Average Monthly Revenue") + 
         # add plot of forecasted values
         autolayer(fcast_scenario_ts[,6], series = "Forecasted Average Monthly Revenue", PI = TRUE) + 
         ylab("Average Monthly Revenue") +
         ggtitle("Forecast of Average Monthly Revenue based on Scenario") + 
         guides(colour = guide_legend(title = "Legend", title.position = "top")) + 
         theme(legend.position = "bottom")
   })
   
   
   ######################### WEB ANALYTICS UI ###########################################
   output$gauge <- renderGvis({
      avgsession <- full_data_reactive()%>%
         select(Avg_session)
      
      Name_col <- ""
      avgsession_display <- cbind(Name_col, avgsession)
      
      googleVis::gvisGauge(avgsession_display, options=list(min=0, max=5000, greenFrom=3000,
                                                          greenTo=5000, yellowFrom=1500, yellowTo=3000,
                                                          redFrom=0, redTo=1500))
   })
   
   output$gauge_users <- renderGvis({     
      Avgusers <- full_data_reactive()%>%
         select(Avg_Users)
      
      Name_col <- ""
      avguser_display <- cbind(Name_col, Avgusers)
      
      gvisGauge(avguser_display, options=list(min=0, max=5000, greenFrom=3000,
                                                       greenTo=5000, yellowFrom=1500, yellowTo=3000,
                                                       redFrom=0, redTo=1500))
   })
   
   output$gauge_Pg_session <- renderGvis({
      Avgpgsession <- full_data_reactive()%>%
         select(Avg_Pg_session)
      
      Name_col <- ""
      avgpgsession_display <- cbind(Name_col, Avgpgsession)
      
      gvisGauge(avgpgsession_display, options=list(min=0, max=10, greenFrom=5,
                                              greenTo=10, yellowFrom=3, yellowTo=6,
                                              redFrom=0, redTo=3))
   })
   
   output$gauge_session_duration <- renderGvis({
      Avgsession_duration <- full_data_reactive()%>%
         select(Avg_session_duration)
      
      Name_col <- ""
      avgsession_duration_display <- cbind(Name_col, Avgsession_duration)
      
      gvisGauge(avgsession_duration_display, options=list(min=0, max=300, greenFrom= 200,
                                                   greenTo= 300, yellowFrom=100, yellowTo= 200,
                                                   redFrom=0, redTo=100))
   })
   
   output$gauge_revenue <- renderGvis({
      Avgrevenue <- full_data_reactive()%>%
         select(Avg_revenue)  
      
      Name_col <- ""
      avgrevenue_display <- cbind(Name_col, Avgrevenue)
      
      gvisGauge(avgrevenue_display, options=list(min=0, max=20000, greenFrom=15000,
                                                          greenTo=20000, yellowFrom=7000, yellowTo=15000,
                                                          redFrom=0, redTo= 7000))
   })
   
   output$gauge_bouncerate <- renderGvis({
      Avgbouncerate <- full_data_reactive()%>%
         select(Avg_bounce_rate)
      
      Name_col <- ""
      avgbouncerate_display <- cbind(Name_col, Avgbouncerate)
      
      gvisGauge(avgbouncerate_display, options=list(min=0, max=60, greenFrom= 40,
                                                          greenTo= 60, yellowFrom= 20, yellowTo= 40,
                                                          redFrom=0, redTo= 20))
   })
   
   output$gauge_sessionPer_user <- renderGvis({
      Avgsessionper_user <- full_data_reactive()%>%
         select(Avg_sessionPer_user)
      
      Name_col <- ""
      avgsessionper_user_display <- cbind(Name_col, Avgsessionper_user)
      gvisGauge(avgsessionper_user_display, options=list(min=0, max=2, greenFrom= 1.2,
                                                          greenTo=2, yellowFrom=0.5, yellowTo= 1.2,
                                                          redFrom=0, redTo=0.5))
   })
   
   output$gauge_erc <- renderGvis({
                     Avgecr <- full_data_reactive()%>%
                        select(Avg_ECR)
                     Name_col <- ""
                     avgecr_display <- cbind(Name_col, Avgecr)
                     
                     gvisGauge(avgecr_display, options=list(min=0, max= 5, greenFrom= 2.7,
                                                            greenTo= 5, yellowFrom= 1.0, yellowTo=2.7,
                                                            redFrom=0, redTo= 1.0))
          
   })
   
   ################ ANIMATIONS #########
   observe(addHoverAnim(session, "returnuser_revenue", "rubberBand"))
   observe(addHoverAnim(session, "newuser_revenue", "rubberBand"))
   observe(addHoverAnim(session, "revenue_timeries", "swing"))
  # observe(addHoverAnim(session, "seas", "pulse"))
   observe(addHoverAnim(session, "revenue_decompose", "pulse"))
   observe(addHoverAnim(session, "revenue_bfastmonitor", "pulse"))
   observe(addHoverAnim(session, "revenue_change_detect", "pulse"))
   observe(addHoverAnim(session, "revenue_stl_forecast", "pulse"))
   observe(addHoverAnim(session, "revenue_trendSeasonal_forecast", "pulse"))
   observe(addHoverAnim(session, "revenue_seasonality", "pulse"))
   observe(addHoverAnim(session, "revenue_seasonality", "pulse"))
   observe(addHoverAnim(session, "regress_model", "pulse"))
   observe(addHoverAnim(session, "seasonal_forecast", "pulse"))
   
})

