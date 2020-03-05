# Libraries ---------------------------------------------------------------
library(tidyverse) #Tidy data & plots
library(dplyr) #Sintaxis tipo SQL
library(readxl) #Leer excell
library(here) #Encontrar archivos
library(scales) #Formatear gráficas
library(quantmod) #Importar atos finacieros
library(PortfolioAnalytics) #Operar datos financieros
library(tidyquant) #Importar datos financieros fácil y en formato tidy
library(xts) #Operar series de tiempo
library(lubridate) #Ya esta incluida en TidyVerse, trabajar con fechas fácil


# Import ------------------------------------------------------------------

#read_csv
csv_tesla <- 

#read_excel
sales_excell <- 

#Reto importar "TSLA", hint: ?getSymbols (Función de quantmod)
#Importar pib mexicano de la FRED
hot_stock <- 
mxgdp <- 

# Resumenes ---------------------------------------------------------------
#Análisis Exploratorio de datos 101
summary(csv_tesla)
str(csv_tesla)
glimpse(csv_tesla)

summary(sales_excell)
glimpse(sales_excell)


# Retornos ----------------------------------------------------------------

#Problemas de compatibilidad entre objetos
hot_stock$returns <- as.vector(CalculateReturns(xts()))

# Verbos Dplyr ------------------------------------------------------------

# Select es para columnas

# Filter para renglones

# Group_by & Summarize para resumenes

sales_excell %>% filter() %>% 
                 mutate() %>% 
                 summarize()

# Finance Tidy ------------------------------------------------------------
#Devuelve un DF largo
stock_prices_long <- c("AAPL", "GOOG", "NFLX") %>% #Especificamos los activos
  tq_get(get  = "stock.prices", #Es diferente siq ueremos tasas de interés
         from = "2010-01-01",  
         to   = "2015-12-31") 

#Transformar DF wide
stock_prices_wide <- c("AAPL", "GOOG", "NFLX") %>% #Especificamos los activos
  tq_get(get  = "stock.prices", #Es diferente siq ueremos tasas de interés
         from = "2010-01-01",  
         to   = "2015-12-31")%>% 
  dplyr::select(date, symbol, adjusted) %>% 
  spread(symbol,adjusted)

#Eliminamos los problemas de compatibilidad
Ra <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, #Transmute elimina columan adjusted
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
#Set Operation
#right_join
#anti_join
#full_join
RaRb <- left_join(Ra,Rb,by = c("date" = "date"))

stocks_capm <-  RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)


# Data Join ---------------------------------------------------------------

tesla <- full_join(csv_tesla,sales_excell, by = "Date")

# Data Vis ----------------------------------------------------------------

tesla %>% ggplot(aes()) + #Escificar que gráfias
          geom_line() +  #Tipo de gráfica
          geom_point() + #Debe ser compatible
          labs() 

tesla %>% ggplot(aes()) + 
  geom_line() +
  geom_point() +
  labs() +
  theme_dark() #Cambiar tema
  
#Exploratorio de datos 102
stock_prices_long %>%  ggplot( aes(x = 1, y = adjusted)) +#Creamos dummy variable
       geom_boxplot() +
       coord_flip() + #Volteamos ejes, pero solo visualmente
       facet_wrap(.~ symbol, scales = "free") + #Creamos un box plor para cada simbolo
       theme_light() +
       theme(
         axis.text.y = element_blank()
       ) +
       labs(title = "BoxPlot",
       subtitle = "Stocks",
       x = "",
       y = "Price ",
       caption = "R for finance ITAM")

stock_prices_long %>% ggplot(aes(x = date, y = adjusted, group = symbol )) +
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

# Decarga los precio de los ETF's para el IPC, S&P500, y el MSCI
# Calcula sus retornos
# Realiza un histograma de sus retornos con densidad en vez de conteo
# HINT1: Busca en google 'Density Histogram ggplot2'
# Hint2: Usa un DF largo (Lo genera tq por default)
# Los valores en el eje y deben tener el simbolo %
# Hint busca add % to y axix values ggplot2
# Usa el theme_pro() que les regalé


# Reto 2 ------------------------------------------------------------------
data("coronavirus")
covid19 <- coronavirus

#Usa glimpse para ver las variables que contiene tu df
#Has un resumen de cuantos casos confirmados, muertes y curados ha habido a partir
# de febrero para cada país
# Hint: Usa groupb_by con las variables en cuestión y despues summarise y la función sum
# con la variable cases
# Usa filter para filtrar date mayores o iguales al 31 de enero 

# Haz una gráfica de la evolución de los casos confirmados para méxico
# HINT: usa filter con Country.Region junto con type
## la Sintaxis para incluir dos condiciones en un mismo filter es cond1 && cond 2


