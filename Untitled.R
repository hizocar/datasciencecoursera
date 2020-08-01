count <- 0
for (i in hw1_data$Ozone) 
{ if (is.na(i)) count <- count + 1}
library(dplyr)
hw1_data %>% filter(Ozone > 31 & Temp > 90) %>% summarise(mean(Solar.R))

hw1_data %>%  filter( Month == 5) %>% summarise( max(Ozone, na.rm= TRUE))

f <- function(x) {
            g <- function(y) {
              y + z
            }
            z <- 4
            x + g(x)
}
z <- 10
f(3)

x <- 5 
y <- if(x < 3) { NA } else { 10 }

h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

