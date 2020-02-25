# Librerias ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(scales)
library(Sim.DiffProc)
library(tidyverse)
library(tidyquant)
library(dplyr)
library(lubridate)
library(scales)
library(DT)
library(readxl)
# Import -------------------------------------------------------------------

tickers <- read_excel(here::here('data', 'tickers.xlsx'))

# Tema --------------------------------------------------------------------

theme_pro <- function(){
    theme_minimal() +
        theme(
            text = element_text(family = "Bookman", color = "gray25"),
            plot.title = element_text(color = "#2C3744", 
                                      size = 18, 
                                      face = "bold"),
            plot.subtitle = element_text(color = "#A6A6A7",
                                         size = 16,
                                         face = "bold"),
            plot.caption = element_text(color = "#A6A6A7",
                                        size = 12,
                                        face = "bold"),
            plot.background = element_rect(fill = "white"),
            plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
            axis.title.x = element_text(color = "#FF7B05",
                                        size = 12,
                                        face = "bold"),
            axis.title.y = element_text(color = "#FF7B05",
                                        size = 12,
                                        face = "bold"),
            axis.text.x = element_text(color = "#531F7E",
                                       face = "bold"),
            axis.text.y = element_text(color = "#531F7E",
                                       face = "bold"),
            axis.line = element_line(color="#A6A6A7"),
            strip.text = element_text(color = "#2C3744",
                                      face = "bold"),
            legend.title = element_text(color ="#A6A6A7",
                                        face = "bold"),
            legend.text = element_text(color = "#2C3744",
                                       face = "bold")
        )
} 


# Dashboard ---------------------------------------------------------------

ui <- dashboardPagePlus(
        dashboardHeaderPlus(title = "GBM"),
        
        dashboardSidebar(
            sidebarMenu(id = 'sidebarmenu',
                menuItem('Explanation', tabName = 'exp', icon=icon("align-center")),
                menuItem('Import', tabName = 'import', icon = icon("file-code")),
                menuItem('Time Series', tabName = 'ts', icon = icon("chart-line")),
                menuItem('Parameters', tabName = "params",icon=icon("calculator")),
                menuItem("Simulation Resuls", tabName = "sim",icon=icon("poll-h")),
                menuItem(sliderInput("h","",min = 10,max = 504,value = 252,step = 10),"Simulation Horizon"),
                menuItem(actionButton("run", "Simulate",icon = icon("laptop-code")))
            )
        ),
        
        dashboardBody(
                tabItems(
                    tabItem(tabName = "exp",
                            fluidPage(
                              
                              fluidRow(
                                withMathJax(includeMarkdown("include.Rmd"))
                              )
                            )
                    ),
                    tabItem(tabName = "import",
                        
                            tabBox(
                                width = 12,
                                title = "",
                                tabPanel("Symbol", selectInput("sym", 
                                                               "Select asset to analyse",
                                                               choices = unique(tickers)
                                                               )
                                         ),
                                tabPanel("Dates", dateRangeInput("date", 
                                                                 "Insert Time Period to analyse",
                                                                 start = Sys.Date() - 250,
                                                                 end = Sys.Date()))
                            )
                            
                            
                    ),
                
                    tabItem(tabName = "ts",
                        plotlyOutput("ts")
                    ),
                    tabItem(tabName = 'params',
                                      valueBoxOutput("s0"),
                                      valueBoxOutput("mu"),
                                      valueBoxOutput("sd")
                    ),
                    tabItem(tabName = 'sim',
                              tabsetPanel(
                                tabPanel(
                                  'Paths',
                                  plotlyOutput("paths")
                                ),
                                tabPanel(
                                  'Histogram',
                                  plotlyOutput("hist")
                               
                              )
                            )
            )
        )
))
        
    

server <- function(input, output) {
    
    import <- reactive({
        
        asset <- input$sym %>% 
                 tq_get(
                     from = input$date[1],
                     to = input$date[2]
                 ) %>% 
                 tq_mutate(select = adjusted,
                           mutate_fun = periodReturn,
                           period = 'daily',
                           type = 'log',
                           col_rename = "return") %>% 
                 select(date,adjusted,return) %>% 
                 drop_na()
        
    })
    
    
    output$ts <- renderPlotly({
        
        import() %>% ggplot(aes(x = date, y = adjusted)) +
                    geom_line( col = "#531F7E") +
                    theme_pro() +
                    scale_y_continuous(labels = dollar_format(prefix = "$")) +
                    labs(title = paste(input$sym, " Time Series",
                                       x = "Date",
                                       y = "Price"))
        
    })
    
   
    
    output$s0 <- renderValueBox({
      valueBox(
        icon = icon("cloudsmith"),
        color = "green",
        "S0",
        paste("$", signif(import()$adjusted[1],5))
      )
    })
    
    output$mu <- renderValueBox({
      valueBox(
        icon = icon("cloudsmith"),
        color = "orange",
        "Mean",
        signif(mean(import()$return),5)
      )
    })
    
    output$sd <- renderValueBox({
      
      valueBox(
        "Sigma",
        color = "yellow",
        icon = icon("cloudsmith"),
        signif(sd(import()$return),5)
      )
    })
    
    
    
    observeEvent(input$run, {
      # wrap the loop execution in withProgress
      withProgress(
        
        message = 'Please wait',
        detail = 'Running Model...',
        value = 0, {
          
          n <- 5
          
          incProgress(1/n, detail = paste("Simulating Trayectories")) #1
          
          sim <- GBM(N = input$h,
                    M = 1000,
                    x0 = import()$adjusted[1],
                    theta = mean(import()$return),
                    sigma = sd(import()$return)
          )
          
          incProgress(1/n, detail = paste("Trayectories simulated"))#2
          
          incProgress(1/n, detail = paste("Generating Trayectories Plot"))#3
          
          sim <- as_data_frame(sim) %>%
            mutate(id = 1:nrow(sim)) 
          
          paths <- sim %>% 
            dplyr::select(id,X1:X1000) %>% 
            gather(Sim, Price, -id) 
          
          
          output$paths <- renderPlotly({
            
            paths %>% ggplot( aes( x = id, y = Price, col = Sim)) +
              geom_line() +
              theme_pro() +
              theme(legend.position = "none",
                    axis.text.x = element_blank()) +
              labs(title = paste("Monte Carlo Stock ", input$sym, " Price Simulation"),
                   subtitle = "Geometric Brownian Motion",
                   caption = paste("1000 Trayectories", input$h, " - 252 Day Horizon"),
                   x = "",
                   y = "") +
              scale_y_continuous(position = "left",
                                 breaks = seq(from = min(import()$adjusted),
                                              to = max(import()$adjusted),
                                              length.out = 20))
          })
          
          incProgress(1/n, detail = paste("Generating Final Price distribution"))#4
          
          output$hist <- renderPlotly({
            
            sim <- sim %>% 
              filter(id == input$h) %>% 
              dplyr::select(-id) %>% 
              gather("Sim", "Price") 
            
            
            r <- range(sim$Price)
            bw <- (r[2] - r[1])/sqrt(nrow(sim))
            
            confidence_interval <- quantile( x = sim$Price, 
                                             probs = c(.025, 0.975))
            upside_prob <- sum(sim$Price >= import()$adjusted[1])/nrow(sim)
            q5 <- mean(sim$Price)
            
            
            # Histogram ---------------------------------------------------------------
            
            sim %>% 
              ggplot(aes(x = Price)) +
              geom_histogram( aes(y = ..density..),binwidth = bw, size = 1,
                              fill = "#79BC42", col = "#531F7E", alpha=0.8) +
              geom_vline(xintercept = q5, size = 1) +
              scale_x_continuous( breaks = seq(from=min(sim$Price), 
                                                to = max(sim$Price), 
                                               length.out = 50 ))+
              geom_text(aes(x= q5 + 25,
                            y=0.1),
                        label=paste("Median", signif(q5,5)), 
                        size =5,
                        col = "#531F7E")+
              theme_pro() +
              labs( title = input$sym,
                    subtitle = "Monte Carlo Price Simulation",
                    caption = paste("90% Confidence interval ", 
                                    "[$",
                                    signif(confidence_interval[1],5),
                                    ", $", 
                                    signif(confidence_interval[2],5),
                                    "]"),
                    x = paste(signif(100*upside_prob,4), "%",
                              " 1Y Upside Probability at current price of $",signif(import()$adjusted[1],5)),
                    y = "") 
          })
          
          incProgress(1/n, detail = paste("Finishing")) #5 
        }
        
      )
    }
    )
}
shiny::shinyApp(ui, server)