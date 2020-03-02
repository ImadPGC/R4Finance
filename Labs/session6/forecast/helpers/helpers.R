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

PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}