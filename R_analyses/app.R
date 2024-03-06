library(rsconnect)
#rsconnect::deployApp('~/Desktop/Livestock Forecast Market Price')

# Exploratory data analysis 
#============================================
# Setting the working directory 
#===========================================
#setwd("~/Desktop/Livestock Forecast Market Price")
#============================================

#installing extra packages 

library(tidyverse)
library(ggplot2)
library(readxl)
#=====================================
# Loading the data 

#======================================
Makert_Prices_2022 <- read_excel("Makert Prices 2022.xlsx")
Makert_Prices_2022<-as.data.frame(Makert_Prices_2022)
names(Makert_Prices_2022)
#=====================================


#=====================================
# A plot of the prices of the four livestock 
#=====================================
date <- as.Date(Makert_Prices_2022$Dates)
ggplot(Makert_Prices_2022, mapping=aes(x = date)) +ylab("Livestock prices")+xlab("Dates")+geom_line(aes(y = Cow),color="blue") + 
  ggtitle("Livestock prices")+geom_line(aes(y = Heifer),color="red") + geom_line(aes(y = Steer),color="green")+ geom_line(aes(y = Bull),color="black")

#====================================
# Comparison of Livestock prices by season 
#=====================================
date <- as.Date(Makert_Prices_2022$Dates)
ggplot(Makert_Prices_2022, mapping=aes(x = date)) +ylab("Livestock prices")+xlab("Dates")+geom_line(aes(y = Cow),color="blue") + 
  ggtitle("Livestock prices by season")+geom_line(aes(y = Heifer),color="red") + geom_line(aes(y = Steer),color="green")+ geom_line(aes(y = Bull),color="black")+facet_wrap(~as.factor(Makert_Prices_2022$Seasons) , scales="free",ncol = 1)

#===================================
# Comparison of prices of each livestock by season
#===================================
ggplot(Makert_Prices_2022, mapping=aes(x = date,y=Cow)) +ylab("Cow prices")+xlab("Dates")+geom_line(aes(y = Cow),color="blue")+facet_wrap(~as.factor(Makert_Prices_2022$Seasons) , scales="free",ncol = 1)

ggplot(Makert_Prices_2022, mapping=aes(x = date,y=Heifer)) +ylab("Heifer prices")+xlab("Dates")+geom_line(aes(y = Heifer),color="red")+facet_wrap(~as.factor(Makert_Prices_2022$Seasons) , scales="free",ncol = 1)

ggplot(Makert_Prices_2022, mapping=aes(x = date,y=Steer)) +ylab("Steer prices")+xlab("Dates")+geom_line(aes(y = Steer),color="green")+facet_wrap(~as.factor(Makert_Prices_2022$Seasons) , scales="free",ncol = 1)

ggplot(Makert_Prices_2022, mapping=aes(x = date,y=Bull)) +ylab("Heifer prices")+xlab("Dates")+geom_line(aes(y = Heifer),color="black")+facet_wrap(~as.factor(Makert_Prices_2022$Seasons) , scales="free",ncol = 1)



# Modeling the Livestock prices 

#============================================
# Fitting ARIMA Models to the Livestock prices 
#============================================                    
# Converting the prices to time series objects
library("xts")
attach(Makert_Prices_2022)
data_ts_cow <- xts(Cow, date)        
data_ts_heifer<- xts(Heifer, date)
data_ts_Bull<- xts(Bull, date)
data_ts_Steer<- xts(Steer, date)
#====================================
library(forecast)
# fitting the models for each livestock 
autoArimaModel_cow<-auto.arima(data_ts_cow,seasonal = TRUE)
summary(autoArimaModel_cow)

autoArimaModel_heifer<-auto.arima(data_ts_heifer,seasonal = TRUE)
summary(autoArimaModel_heifer)

autoArimaModel_Bull<-auto.arima(data_ts_Bull,seasonal = TRUE)
summary(autoArimaModel_Bull)

autoArimaModel_Steer<-auto.arima(data_ts_Steer,seasonal = TRUE)
summary(autoArimaModel_Steer)
#=========================================
# Superimposing the fitted and observed values 
# to visualize the fiiting 
#=========================================
fitted_cows_prices<-autoArimaModel_cow$fitted
ggplot(Makert_Prices_2022, mapping=aes(x = date)) +ylab("Cow prices")+xlab("Dates")+geom_line(aes(y = Cow),color="blue")+geom_line(aes(y =fitted_cows_prices),color="black",linetype="dashed")


fitted_heifer_prices<-autoArimaModel_heifer$fitted
ggplot(Makert_Prices_2022, mapping=aes(x = date)) +ylab("Heifer prices")+xlab("Dates")+geom_line(aes(y = Heifer),color="red")+geom_line(aes(y =fitted_heifer_prices),color="black",linetype="dashed")

fitted_steer_prices<-autoArimaModel_Steer$fitted
ggplot(Makert_Prices_2022, mapping=aes(x = date)) +ylab("Steer prices")+xlab("Dates")+geom_line(aes(y = Steer),color="green")+geom_line(aes(y =fitted_steer_prices),color="orange")

fitted_bull_prices<-autoArimaModel_Bull$fitted
ggplot(Makert_Prices_2022, mapping=aes(x = date)) +ylab("Cow prices")+xlab("Dates")+geom_line(aes(y = Bull),color="black")+geom_line(aes(y =fitted_bull_prices),color="orange")

#=====================================
# Forecasting livestock 12 months ahead prices using the fitted models 
#=====================================
predCowprice = forecast(autoArimaModel_cow, h=12)
par(mfrow = c(1, 1),     # Create a single plot
    mar = c(2, 4, 1, 2))   # Set margins (bottom, left, top, right)

plot(predCowprice)
predCowprice$mean

predheiferprice<-forecast(autoArimaModel_heifer)
plot(predheiferprice)
predheiferprice$mean


predbullprice<-forecast(autoArimaModel_Bull)
plot(predbullprice)
predbullprice$mean


predsteerprice<-forecast(autoArimaModel_Steer)
plot(predsteerprice)
predsteerprice$mean

#====================================
# DEPLOYED MODELS TO R SHINY 
#=====================================

Cow_PredPrices<-function(saletime){
  Cowpredictedprice = forecast(autoArimaModel_cow, h=saletime)
  return(Cowpredictedprice)
}
Cow_PredPrices(saletime=5)
plot(Cow_PredPrices(saletime = 5))


Heifer_PredPrices<-function(saletime){
  Heiferpredictedprice = forecast(autoArimaModel_heifer, h=saletime)
  return(Heiferpredictedprice)
}
Heifer_PredPrices(saletime=5)
plot(Heifer_PredPrices(saletime = 5))


Steer_PredPrices<-function(saletime){
  Steerpredictedprice = forecast(autoArimaModel_Steer, h=saletime)
  return(Steerpredictedprice)
}
Steer_PredPrices(saletime=5)
plot(Steer_PredPrices(saletime = 5))


Bull_PredPrices<-function(saletime){
  Bullpredictedprice = forecast(autoArimaModel_Bull, h=saletime)
  return(Bullpredictedprice)
}
Bull_PredPrices(saletime=5)
plot(Bull_PredPrices(saletime = 5))

# Render the R Markdown document
rmarkdown::render("Lena_Task.Rmd", 
                  output_format = rmarkdown::html_document(embed_resources = TRUE, standalone = FALSE))


# Loading the data 
#======================================
Makert_Prices_2022 <- read_excel("Makert Prices 2022.xlsx")
Makert_Prices_2022 <- as.data.frame(Makert_Prices_2022)

# Handle missing or invalid values in the Dates column
if (any(is.na(Makert_Prices_2022$Dates))) {
  # Remove rows with missing values in the Dates column
  Makert_Prices_2022 <- Makert_Prices_2022[complete.cases(Makert_Prices_2022$Dates), ]
}

# Convert Dates column to Date format
Makert_Prices_2022$Dates <- as.Date(Makert_Prices_2022$Dates)

# Create xts objects for each livestock
data_ts_cow <- xts(Makert_Prices_2022$Cow, order.by = Makert_Prices_2022$Dates)        
data_ts_heifer <- xts(Makert_Prices_2022$Heifer, order.by = Makert_Prices_2022$Dates)
data_ts_Bull <- xts(Makert_Prices_2022$Bull, order.by = Makert_Prices_2022$Dates)
data_ts_Steer <- xts(Makert_Prices_2022$Steer, order.by = Makert_Prices_2022$Dates)


#SHINY APP

library(shiny)
library(forecast)
library(xts)

# UI
ui <- fluidPage(
  titlePanel("Livestock Forecast Market Price"),
  
  # Introduction Page
  tabPanel("Introduction",
           fluidRow(
             column(12, 
                    h3("Introduction"),
                    p("This app allows you to forecast the market price of different livestock using time series ARIMA models.")
             )
           )
  ),
  
  # Input Selection Page
  tabPanel("Input",
           fluidRow(
             column(4,
                    h3("Select Livestock Type"),
                    selectInput("livestock", "Livestock Type:",
                                choices = c("Cow", "Heifer", "Steer", "Bull"))
             ),
             column(4,
                    h3("Select Forecast month"),
                    sliderInput("forecast_month", h3("Forecast month"), min=1,max=50, value=1)
             )
           ),
           actionButton("submit", "Submit")
  ),
  
  # Analysis and Output Page
  tabPanel("Output",
           fluidRow(
             column(12,
                    h3("Forecasted Market Price"),
                    plotOutput("forecast_plot"),
                    tableOutput("forecast_table")
             )
           )
  ))
  
# Server logic
server <- function(input, output) {
  forecasted_prices <- reactive({
    # Convert forecast month to forecast horizon
    current_month <- Sys.yearmon()
    forecast_horizon <- as.numeric(input$forecast_month - current_month)
    
    # Load data and fit ARIMA model
    data <- read.csv("Makert Prices 2022.csv")
    data_ts <- xts(data[, input$livestock], order.by = as.Date(data$Dates))
    autoArimaModel <- auto.arima(data_ts, seasonal = TRUE)
    
    # Forecast
    forecast(autoArimaModel, h = forecast_horizon)
  })
  
  
  output$forecast_plot <- renderPlot({
    # Plot forecasted prices
    plot(forecasted_prices(), main = "Forecasted Market Price")
  })
  
  output$forecast_table <- renderTable({
    # Convert forecasted prices to a data frame
    forecast_data <- as.data.frame(forecasted_prices()$mean)
    colnames(forecast_data) <- "Forecasted Price"
    forecast_data
  })
}


# Run the application
  shinyApp(ui = ui, server = server)
  

