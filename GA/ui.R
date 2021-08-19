######################################## Load library  ###############################################
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyanimate)
library(shinydashboardPlus)
library(shinyEffects)
library(shinybusy)
library(shinyBS)
library(magrittr)
library(waiter)
library(fresh)

dashboardPage(skin = "green", 
    dashboardHeader(title = "Google Web Analytics"
                    ),
    dashboardSidebar(
      # menuItem("Google web analytics", badgeColor = "green", icon = icon("dashboard")),
      #                dateRangeInput("date", "Select date", start = "2014-11-01", end = "2021-07-01", min = "2014-11-01",
      #                               max = "2021-07-01"),
                     collapsed = TRUE, disable = TRUE, minified = FALSE 
                     ),
    dashboardBody(#fluidRow(), 
      busy_start_up(loader = spin_epic("semipolar", color = "#99FF66"), text = "Please wait for app to load...",
                    mode = "auto", color = "#FFF", background = "#241632"),
      setZoom(class = "box"), setZoom(class = "gauge"), setZoom("column"), 
      setZoom(id = "newuser_transactions"), setZoom(id = "returnuser_transactions"),
                  tabsetPanel(
                      tabPanel("Web analytics",
                               fluidPage( title = "KPI", withAnim(),
                                         fluidRow(h4("Select year and month to monitor KPI")),
                                         fluidRow(
                                           column(width = 6,
                                             sliderInput("yearsel", label = "Year", min = 2015, max = 2021, value = 2017, sep = ""
                                             )
                                           ),
                                           
                                           column(width = 6,
                                             selectInput("monthsel", label = "Month", choices = c(unique(full_data$Month)))
                                           )
                                         ),
                                       
                                         fluidRow(
                                           column(width = 6, h4("Average number of session on website"),
                                               htmlOutput("gauge")%>%
                                                 withSpinner(type = 8, color = "green", color.background = "#0dc5c1", hide.ui = FALSE)
                                               ),
                                           column(width = 6, h4("Average users on website"),
                                                  withSpinner(type = 8, color = "green", hide.ui = FALSE,
                                                              htmlOutput("gauge_users")
                                                              )
                                                  )
                                           
                                         ),
                                         fluidRow(
                                           column(width = 6, h4("Average number of sessions per user on website"),
                                                  withSpinner(type = 8, color = "green", hide.ui = FALSE, 
                                                              htmlOutput("gauge_sessionPer_user")
                                                              )
                                                  ),
                                           column(width = 6, h4("Average number of pages per session on website"),
                                                  htmlOutput("gauge_Pg_session")%>%
                                                    withSpinner(type = 8, color = "green", hide.ui = FALSE)
                                                  )
                                         ),
                                         fluidRow(
                                           column(width = 6, h4("Average session duration in seconds on website"),
                                                  htmlOutput("gauge_session_duration")%>%
                                                    withSpinner(type = 8, color = "green", hide.ui = FALSE)
                                                  ),
                                           column(width = 6, h4("Average number revenue on website"),
                                                  htmlOutput("gauge_revenue")%>%
                                                    withSpinner(type = 8, color = "green", hide.ui = FALSE)
                                                  )
                                         ),
                                         
                                         fluidRow(
                                           column(width = 6, h4("Average Bounce rate on website"),
                                                  htmlOutput("gauge_bouncerate")%>%
                                                    withSpinner(type = 8, color = "green", hide.ui = FALSE)
                                                  ),
                                           column(width = 6, h4("Average Ecommence Conversion Rate on website"),
                                                  htmlOutput("gauge_erc")%>%
                                                    withSpinner(type = 8, color = "green", hide.ui = FALSE)
                                                  )
                                         )
                                        
                                         )),
                      ##################### Revenue tab  
                      tabPanel("Revenue",
                               fluidPage(title = "KPI",  withAnim(),### KPI Rows
                                         fluidRow(
                                             valueBoxOutput("newuser_revenue"), #%>%
                                              # withSpinner(type = 8, color = "green"),
                                             valueBoxOutput("returnuser_revenue")
                                         ),
                                         fluidRow(
                                           column(width = 6,
                                                  sliderInput("revenue_selectyear", label = "Select year", min = 2015, max = 2021,
                                                              value = 2017, sep = "")
                                           ),
                                           
                                           column(width = 6,
                                                  selectInput("revenue_selectmonth", label = "Select month", choices = c(unique(usertypedata_mean$Month)))
                                           )
                                           ),
                                         
                                         fluidRow(
                                             #column(width = 6,
                                                    box(title = "Revenue timeseries", width = 6,
                                                        collapsible = TRUE,
                                                        plotOutput("revenue_timeries")%>%
                                                          withSpinner(type = 8, color = "green")
                                                       ),
                                                    #),
                                           #  column(width = 6,
                                                    box(title = "Seasonality timeseries plot of Revenue", width = 6,
                                                        collapsible = TRUE,
                                                        plotOutput("revenue_seasonality")%>%
                                                          withSpinner(type = 8, color = "green")
                                                        )
                                                    #)
                                         ),
                                         fluidRow(
                                             
                                             box(title = "Time series decomposition", collapsible = TRUE,
                                                 plotOutput("revenue_decompose")%>%
                                                   withSpinner(type = 8, color = "green")
                                                 ),
                                             
                                             box(title = "Change detection in revenue", width = 6,
                                                 collapsible = TRUE,
                                                 plotOutput("revenue_change_detect")%>%
                                                   withSpinner(type = 8, color = "green")
                                             )
                                         ),
                                               
                                         fluidRow(
                                           box(title = "BfastMonitor", collapsible = TRUE, width = 6,
                                               plotOutput("revenue_bfastmonitor")%>%
                                                 withSpinner(type = 8, color = "green")
                                           ),
                                           box(title = "Regression model", width = 6, collapsible = TRUE,
                                               plotOutput("regress_model")%>%
                                                 withSpinner(type = 8, color = "green")
                                           )
                                         ),
                                         
                                         fluidRow(
                                           box(title = "Revenue forecasting based on trend and seasonality",
                                               collapsible = TRUE,
                                               plotOutput("revenue_trendSeasonal_forecast")%>%
                                                 withSpinner(type = 8, color = "green")
                                           ),
                                           
                                           box(title = "Forecasts of Revenue from STL",collapsible = TRUE,
                                               plotOutput("revenue_stl_forecast")%>%
                                                 withSpinner(type = 8, color = "green")
                                               )
                                         ),
                                         
                                         fluidRow(
                                             column(width = 6,
                                                    selectInput("revenue_horizon_forecast", label = "Select number of months to forecast",
                                                                selected = 24,
                                                                seq(from = 1, to = 24)
                                                                ),
                                                    bsTooltip(title = "time horizon to be used as input", "revenue_horizon_forecast", placement = "right", 
                                                              trigger = "focus")
                                                    ),
                                            # column(width = 6,
                                                    box(title = "Revenue forecasting (seasonal mean)",collapsible = TRUE,
                                                        plotOutput("seasonal_forecast")%>%
                                                          withSpinner(type = 8, color = "green")
                                                        )
                                         ))),
                      ######################### TRANSACTION UI ################################
                      tabPanel(title = "Transactions",
                               fluidRow(
                                 valueBoxOutput("newuser_transactions"), 
                                 
                                 valueBoxOutput("returnuser_transactions") 
                               ),
                               fluidRow(
                                 column(width = 6,
                                        sliderInput("trans_year", h5("Select year"), min = 2015, max = 2021,
                                                    value = 2017, sep = "")
                                        ),
                                 column(width = 6,
                                        selectInput("trans_month", h5("Select month"), 
                                                    choices = c(unique(usertypedata_mean$Month))
                                        )
                              
                               )),
                               fluidRow(
                                   # column(width = 6,
                                           box(title = "Timeseries of Transaction", collapsible = TRUE,
                                               plotOutput("transaction_timeseries")%>%
                                                 withSpinner(type = 8, color = "green")
                                               ),
                                  
                                          box(title = "Timeseries decomposition", collapsible = TRUE,
                                              plotOutput("transaction_decomp")%>%
                                                withSpinner(type = 8, color = "green")
                                              )
                               ),
                               fluidRow(
                                 box(title = "Seasonal plot of monthly transactions", collapsible = TRUE,
                                     plotOutput("transaction_seasonal")%>%
                                       withSpinner(type = 8, color = "green")
                                 ),
                                 box(title = "Autocorrelation of transaction", collapsible = TRUE,
                                     plotOutput("transaction_autocorrelation")%>%
                                       withSpinner(type = 8, color = "green")
                                     )
                               ),
                               fluidRow(
                                          box(title = "Forecasting with Exponential Smoothing", collapsible = TRUE, width = 7,
                                              plotOutput("transaction_exponential")%>%
                                                withSpinner(type = 8, color = "green")
                                              ),
                                          
                                          box(title = "Diagnotics of residuals", collapsible = TRUE, width = 5,
                                            plotOutput("ets_residuals")%>%
                                              withSpinner(type = 8, color = "green")
                                          )
                               )               
                               ),
                      ################### Scenario forecasting UI  #########################################
                      tabPanel(title = "Scenario forecasting for revenue",
                               fluidRow(
                                   column(width = 6,
                                              h4("Set criteria for different scenarios to forecast revenue")
                                          ),
                                   column(width = 6,
                                          sliderInput("horizon_forecast", label = "Number of month(s) to forecast",
                                                      min = 1, max = 24, 
                                                      value = 5,
                                                      step = 1)
                                          )

                               ),
                               
                               fluidRow(
                                 column(width = 6,
                                        selectInput("forecast_type_bounce_rate", "Do you expect an increase or decrease for bounce rate", choices = c("Increase", "Decrease"),
                                                    selected = "Decrease"),
                                        sliderInput("bounce_rate_sce", label = "Bounce rate (%)",
                                                    min = 1, max = 100, 
                                                    value = 3,
                                                    step = 1, width = "50%"),
                                        
                                        selectInput("forecast_type_erc", "Do you expect an increase or decrease for conversion rate", choices = c("Increase", "Decrease"),
                                                    selected = "Increase"),
                                        sliderInput("erc_sce", label = "Ecommerce Conversion rate (%)",
                                                    min = 1, max = 100, 
                                                    value = 2,
                                                    step = 1, width = "50%"),
                                        
                                        selectInput("forecast_type_user", "Do you expect an increase or decrease for Number of Users", choices = c("Increase", "Decrease"),
                                                    selected = "Increase"),
                                        sliderInput("user_sce", label = "Number of users (%)",
                                                    min = 1, max = 100, 
                                                    value = 1,
                                                    step = 1, width = "50%"),
                                        
                                        selectInput("forecast_type_session_dur", "Expected scenario for session duration",
                                                    choices = c("Increase", "Decrease"), selected = "Decrease"),
                                        sliderInput("session_dur_sce", label = "Session duration (%)", min = 1,
                                                    max = 100, value = 5, step = 1, width = "50%"),
                                        
                                        
                                        selectInput("forecast_type_session_peruser",
                                                    label = "Expected scenario for Session per User",
                                                    choices = c("Increase", "Decrease"), selected = "Increase"),
                                        sliderInput("session_peruser_sce", label = "Number of session per User (%)", min = 1,
                                                    max = 100, value = 1, step = 1, width = "50%"),
                                        
                                        
                                        selectInput("forecast_type_pages_persession",
                                                    label = "Expected scenario for Pages per session",
                                                    choices = c("Increase", "Decrease"), selected = "Increase",
                                                    width = "50%"),
                                        sliderInput("pages_persession_sce", label = "Number of Pages per session(%)", min = 1,
                                                    max = 100, value = 1, step = 1, width = "50%")
                                        ),
                                 column(width = 6,
                                       plotOutput("scenario_forecast")%>%
                                   withSpinner(type = 8, color = "green")
                                 )
                               )
                               ),
                      ############################## Business question UI  #########################################
                      tabPanel(title = "Business question",
                               fluidRow(
                                 box(title = "What is the relationship between KPI", width = 12,
                                     collapsible = TRUE,
                                     plotOutput("correlation", height = "600px")%>%
                                       withSpinner(type = 8, color = "green")
                                     )
                               )
                               )
                  ))
)