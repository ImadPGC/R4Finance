# Libraries ---------------------------------------------------------------
library(readxl)
library(scales)
library(tidyverse)
library(tidyquant)
library(dplyr)
library(xts)
library(lubridate)
library(quantmod)
library(PortfolioAnalytics)

# Import ------------------------------------------------------------------

csv_tesla <- 

sales_excell <- 

hot_stock <- 


# Resumenes ---------------------------------------------------------------

summary(csv_tesla)

glimpse(csv_tesla)

summary(sales_excell)

glimpse(sales_excell)


# Retornos ----------------------------------------------------------------

hot_stock$returns <- as.vector(CalculateReturns(xts()))

# Verbos Dplyr ------------------------------------------------------------

# Select es para columnas

# Filter para renglones

# Group_by & Summarize para resumenes

sales_excell %>% filter(-ok) %>% 
                 mutate() %>% 
                 group_by() %>% 
                 summarize()

# Finance Tidy ------------------------------------------------------------

stock_prices <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") 

stock_prices1 <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "return")

Ra <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

Rb <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

RaRb <- left_join(Ra,Rb,by = c("date" = "date"))

stocks_capm <-  RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)


# Data Join ---------------------------------------------------------------

tesla <- full_join(csv_tesla,sales_excell, by = "Date")

# Data Vis ----------------------------------------------------------------

tesla %>% ggplot(aes()) + 
          geom_line() +
          geom_point() +
          labs() +
          theme_dark()

tesla %>% ggplot(aes()) + 
  geom_line() +
  geom_point() +
  labs() +
  theme_dark()
  
stock_prices %>%  ggplot( mapping = aes(x = 1, y = adjusted)) +
       geom_boxplot() +
       coord_flip() +
       facet_wrap(.~ symbol, scales = "free") +
       theme_light() +
       theme(
         axis.text.y = element_blank()
       ) +
       labs(title = "BoxPlot",
       subtitle = "Stocks",
       x = "",
       y = "Price ",
       caption = "R for finance ITAM")

stock_prices %>% ggplot(aes(x = date, y = adjusted, group = symbol )) +
                 geom_line( aes(col = symbol)) +
                 theme_minimal() +
                 labs(title = "Time Series",
                      subtitle = "Stocks",
                      x = "Date",
                      y = " USD",
                      caption = "R for finance ITAM")

# Regalo ------------------------------------------------------------------

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

# Reto --------------------------------------------------------------------

# Realiza un Histograma de los retornos en el objeto stock prices1
# para tesla
# Los valores en el eje x deben tener el simbolo %
# Usa el theme_pro() que les regalé

