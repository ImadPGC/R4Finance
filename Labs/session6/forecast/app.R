# Libraries ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(dplyr)
library(egg)
library(scales)
library(tidyquant)
library(rstanarm)
library(bsts)
library(lubridate)
library(tidybayes)
library(broom)
library(reshape2)
library(mctest)
library(here)
library(forecast)

# Theme -------------------------------------------------------------------

source(here::here('helpers', 'helpers.R'))


# Data --------------------------------------------------------------------

sales  <- readr::read_csv(here::here(
                                     'data',
                                     'apple_rev.csv'))%>% 
          mutate(date =lubridate::mdy(date),
                 sales = as.numeric(sales)) %>% 
         as_data_frame()

gdp <-  tq_get("ND000334Q", 
              get = "economic.data") %>%    
        dplyr::select(price) %>% 
        unlist()

sales$gdp <- c(unlist(gdp[4:40]))

n <- nrow(sales)    

sales$Group <- seq(from = 1,to = n, by = 1)


# User Interface ----------------------------------------------------------


ui <- fluidPage(

    # Application title
    titlePanel("Sales Forecast"),
    theme = shinythemes::shinytheme('sandstone'), #themeselector()


# SidebarPannel -----------------------------------------------------------


    # Sidebar with a slider input for % for model train & Run Buttons 
    sidebarLayout(
        sidebarPanel(
            sliderInput("train",
                        "Select Train group length:",
                        min = .1,
                        max = .90,
                        value = .8),
            actionButton(inputId = 'run',
                         label = 'Run Model')
        ),


# MainPannel --------------------------------------------------------------


        mainPanel(
           tabsetPanel(
               tabPanel('The Problem'),
               tabPanel('Time Series',
                        plotOutput('ts')),
               tabPanel('Regression',
                        plotOutput('reg')),
               tabPanel('Bayesian Regression',
                        plotOutput('breg')),
               tabPanel('ARIMA',
                        plotOutput('arimax')),
               tabPanel('Space State Model',
                        plotOutput('ssm')),
               tabPanel('Model Comparison',
                        'showdown')
           )
        )
    )
)



# Server ------------------------------------------------------------------



server <- function(input, output) {
            
    
    model <- reactive({
        
    sales <- sales %>% 
             mutate( 
                 Group = case_when(Group <= input$train*n ~ 'train',
                                   TRUE ~ 'eval'))
    })#Termina model
    
    output$ts <- renderPlot({
        
        model() %>% 
        gather(variable, value, -c(date,Group)) %>% 
        ggplot(aes(x = date, y = value, col = Group)) +
        geom_line(size = .6) +
        theme_pro() +
        facet_wrap( variable ~. , scales = 'free') +
        scale_y_continuous(labels = scales::dollar_format(prefix = '$')) +
        scale_color_manual(values = c('#79BC42','#2C3744'))+
        labs(title = 'GDp & Apple.Inc Revenue',
             x = 'Date',
             y = 'Sale')
        
       
    }) #Termina ts
    
    observeEvent(input$run, {
        # With progress genera nuestra barra de progreso
        withProgress(
            message = 'Please wait', #Mensaje de progreso
            detail = 'Running Model...',
            value = 0,{
                
                n <- 5
                train <- model() %>% 
                         filter(Group == 'train')
                test <- model() %>% 
                        anti_join(train)
                
                
                #Incremento barra
                incProgress(1/n, detail = 'Ajustando Regresión Lineal') #1
                
                #Regresión Lineal
                model1 <- lm(sales ~ gdp, train)
                coef1 <- tidy(model1)
                aux <- model1 %>% 
                         predict.lm(
                         newdata = test,
                         interval = 'prediction',
                         type = 'response')
                aux <- as_data_frame(aux)
                
                #Guardamos Predicciones
                pred1 <- test %>% 
                        mutate(lm = aux$fit,
                         up_lm = aux$upr,
                         dn_lm = aux$lwr)
                
                #Regression Plot
                output$reg <- renderPlot({
                    
                reg1 <- model() %>% ggplot(aes(x = gdp, y = sales)) +
                            geom_point(aes(shape = Group), size = 1) +
                            stat_smooth(method = "lm", se = FALSE) + 
                            labs(title = 'Linear Regression',
                                 subtitle = 'Full vs Train Fit',
                                 x = 'US GDP',
                                 y = 'Apple Sales') +
                            geom_abline(intercept = coef1$estimate[1], 
                                        slope = coef1$estimate[2], 
                                        col = "#2C3744") +
                            scale_x_continuous(labels = dollar_format(prefix = '$')) +
                            scale_y_continuous(labels = dollar_format(prefix = '$')) +
                            theme_pro()
                
                reg2 <- model() %>% ggplot(aes(x = date, y = sales)) +
                    geom_line(size = 1) +
                    geom_line(aes(y = c(train$sales,pred1$lm)), col = 'blue') + #GGplot solo acepta objetos del mismo tamaño
                    geom_ribbon(aes( 
                        ymin = c(train$sales,pred1$dn_lm),
                        ymax = c(train$sales,pred1$up_lm)),
                        fill = 'grey75',
                        alpha = .3)+
                    scale_y_continuous(labels = dollar_format(prefix = '$')) +
                    labs(title = 'Linear Regression Time Series',
                         subtitle = 'Full vs Train Fit',
                         x = 'Date',
                         y = 'Apple Sales') +
                        theme_pro() 
                
                ggarrange(reg1,reg2, ncol = 1, nrow = 2)
                    
                })#Termina Output Reg
                
                #Incremento barra
                incProgress(1/n, detail = 'Ajustando Regresión Lineal Bayesiana') #2
               
                #Regresión lineal bayesiana
                model2 <- stan_lm(sales ~ gdp, train, prior = NULL, seed = 2016,chains = 1)
                
                incProgress(1/n, detail = 'Ajustando Regresión Lineal Bayesiana, modelo completo') ##
                model2_2 <- stan_lm(sales ~ gdp, model(), prior = NULL, seed = 2016, chains = 1)
                coef2 <- tidy(model2)
                
                # Modelo Completo
                coef2_2 <- tidy(model2_2)
                
                # Guardamos pronóstico
                aux <- model2 %>% 
                       posterior_predict(
                       newdata = test)
                
                aux <- as_data_frame(aux)
                
                pred2 <- test %>% 
                        mutate(blm = colMeans(aux))
                
                aux <- model2 %>% 
                       predictive_interval(
                          newdata = test
                        )
                 
                pred2 <- pred2 %>% 
                        mutate(up_blm = aux[,2],
                               dn_blm = aux[,1])
                
                # Visulización del modelo
                output$breg <- renderPlot({
                    
                breg1 <- model() %>% ggplot(aes(x = gdp, y = sales)) +
                            geom_point(aes(shape = Group), size = 1) +
                            labs(title = 'Bayesian Linear Regression',
                             subtitle = 'Full vs Train Fit',
                             x = 'US GDP',
                             y = 'Apple Sales') +
                            geom_abline(intercept = coef2$estimate[1], 
                                    slope = coef2$estimate[2], 
                                    col = "#2C3744") +
                            geom_abline(intercept = coef2_2$estimate[1], 
                                    slope = coef2_2$estimate[2], 
                                    col = "green") +
                            scale_x_continuous(labels = dollar_format(prefix = '$')) +
                            scale_y_continuous(labels = dollar_format(prefix = '$')) +
                        theme_pro()
                        
                breg2 <- model() %>% ggplot(aes(x = date, y = sales)) +
                            geom_line(size = 1) +
                            geom_line(aes(y = c(train$sales,pred2$blm)), col = 'blue') + #GGplot solo acepta objetos del mismo tamaño
                            geom_ribbon(aes( 
                                ymin = c(train$sales,pred2$dn_blm),
                                ymax = c(train$sales,pred2$up_blm)),
                                fill = 'grey75',
                                alpha = .3)+
                            scale_y_continuous(labels = dollar_format(prefix = '$')) +
                            labs(title = 'Bayesian Linear Regression Time Series',
                                 subtitle = 'Full vs Train Fit',
                                 x = 'Date',
                                 y = 'Apple Sales') +
                            theme_pro() 
                        
                        ggarrange(breg1,breg2, ncol = 1, nrow = 2)
                    
                })#Termina Breg Output
                
                #Incremento barra
                incProgress(1/n, detail = 'Ajustando ARIMAX') #3
                
                #ARIMAX
                model3 <- auto.arima(train$sales, xreg = train$gdp)
                coef3 <- tidy(model3)
                aux <-  summary(forecast(model3,xreg = test$gdp))
                
                # Guardamos Pronostico
                pred3 <- test %>% 
                      mutate(arimax = aux$`Point Forecast`,
                             up_arimax = aux$`Hi 95`,
                             dn_arimax = aux$`Lo 95`)
                
                
                # Visulización del modelo
                output$arimax <- renderPlot({
                    
                    model() %>% ggplot(aes(x = date, y = sales)) +
                        geom_line(size = 1) +
                        geom_line(aes(y = c(train$sales,pred3$arimax)), col = 'blue') + #GGplot solo acepta objetos del mismo tamaño
                        geom_ribbon(aes( 
                                    ymin = c(train$sales,pred3$dn_arimax),
                                    ymax = c(train$sales,pred3$up_arimax)),
                                    fill = 'grey75',
                                    alpha = .3)+
                        scale_y_continuous(labels = dollar_format(prefix = '$')) +
                        labs(title = 'ARIMAX',
                             subtitle = 'Full vs Train Fit',
                             x = 'Date',
                             y = 'Apple Sales') +
                        geom_abline(intercept = coef2$estimate[1], 
                                    slope = coef2$estimate[2], 
                                    col = "#2C3744") +
                        geom_abline(intercept = coef2_2$estimate[1], 
                                    slope = coef2_2$estimate[2], 
                                    col = "green") +
                        theme_pro()
                    
                })#Termina output arimax
                
                #Incremento barra
                incProgress(1/n, detail = 'Ajustando modelo espacio estado') #4
                
                #Modelo Espacio estado
                ss <- AddLocalLinearTrend(list(), train$sales) #tendencia
                ss <- AddSeasonal(ss, model()$sales, nseasons = 4) #Estacionalidad
               
                model4 <- bsts(train$sales ~ train$gdp,
                                   state.specification = ss,
                                   niter = 2000, 
                                   ping=0, seed=2016)
                
                ### Get a suggested number of burn-ins
                burn <- SuggestBurn(0.1, model4)
                
                #predict
                aux <- predict.bsts(model4, 
                                   horizon = length(test$sales),
                                   newdata = test$gdp, 
                                   burn = burn, 
                                   quantiles = c(.025,.975))
                
                pred4 <- test %>% 
                         mutate(ssm = aux$mean,
                                up_ssm = aux[['interval']][2,],
                                dn_ssm = aux[['interval']][1,]
                              )
                
                ### Get the average coefficients when variables were selected (non-zero slopes)
                coef4 <- cbind.data.frame(
                         colMeans(model4$state.contributions[-(1:burn),"trend",]),
                         colMeans(model4$state.contributions[-(1:burn),"regression",]),
                         train$date)  
                
                names(model4) <- c("Trend", "Seasonality" ,"Regression", "Date")
                
                
                # Visulización del modelo
                output$ssm <- renderPlot({
                    
                    model() %>% ggplot(aes(x = date, y = sales)) +
                        geom_line(size = 1) +
                        geom_line(aes(y = c(train$sales,pred4$ssm)), col = 'blue') + #GGplot solo acepta objetos del mismo tamaño
                        geom_ribbon(aes( 
                            ymin = c(train$sales,pred4$dn_ssm),
                            ymax = c(train$sales,pred4$up_ssm)),
                            fill = 'grey75',
                            alpha = .3)+
                        labs(title = 'Space State Model',
                             subtitle = 'Full vs Train Fit',
                             x = 'Date',
                             y = 'Apple Sales') +
                        scale_y_continuous(labels = dollar_format(prefix = '$')) +
                        geom_abline(intercept = coef2$estimate[1], 
                                    slope = coef2$estimate[2], 
                                    col = "#2C3744") +
                        geom_abline(intercept = coef2_2$estimate[1], 
                                    slope = coef2_2$estimate[2], 
                                    col = "green") +
                        theme_pro()
                    
                })#Termina output arimax
                
                
            })#termina with progress
    })#TerminaObserverEvent
    
    
}#Termina Server

# Run the application 
shinyApp(ui = ui, server = server)
