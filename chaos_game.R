library(tidyverse)
library(gifski)
library(patchwork)

# Chaos game
# https://www.youtube.com/watch?v=kbKtFN71Lfs&ab_channel=Numberphile
# https://www.youtube.com/watch?v=IGlGvSXkRGI&ab_channel=ThinkTwice

n <- 2 * 10^4

# 0 < r < 1 

iterations <- function(df, r, n) {
  
  ref_points <- nrow(df) - 1
  
  for(i in 1:n) {
  
    point <- sample(ref_points, 1)
    
    df <- df %>% 
      bind_rows(tibble(x = r * df$x[point] + (1 - r)*df$x[i], 
                       y = r * df$y[point] + (1 - r)*df$y[i]))
    
  }
  
  df
  
}


# triangle ----------------------------------------------------------------

set.seed(123)

df <- tibble(x = c(0, 1, 0.5), 
             y = c(0, 0, sqrt(.75)))

x_1 <- runif(1)
y_1 <- runif(1, 0, sqrt(.75))

while(y_1 > (sqrt(.75)/.5) * x_1 | y_1 > -(sqrt(.75)/.5)*x_1 + (sqrt(.75)/.5)) y_1 <- runif(1, 0, sqrt(.75))

df <- df %>% 
  bind_rows(tibble(x = x_1, y = y_1))

df <- iterations(df, r = .5, n)

fig <- df[-(1:10),] %>% 
  ggplot(aes(x, y)) + 
    geom_point(size = .1) +
    theme_void()


# square ------------------------------------------------------------------

set.seed(3)

df_square <- tibble(x = c(0, 1, 1, 0, .5), 
                    y = c(0, 0, 1, 1, .5))

df_square <- df_square %>%
  bind_rows(tibble(x = runif(1), y = runif(1)))

df_square <- iterations(df_square, r = 2/3, n)


# animated gif ------------------------------------------------------------

seq <- seq(10, n, 500)

for (i in 1:length(seq)) {
  
  fig_triangle <- df[-(1:10),][1:seq[i],] %>% 
    ggplot(aes(x, y)) + 
    geom_point(size = .05, color = "white") +
    geom_point(data = df[1:3,], aes(x, y), color = "red", size = 2) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "black", colour = "white", linewidth = 1))
  
  fig_square <- df_square[-(1:10),][1:seq[i],] %>% 
    ggplot(aes(x, y)) + 
    geom_point(size = .05, color = "white") +
    geom_point(data = df_square[1:5,], aes(x, y), color = "red", size = 2) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "black", colour = "white", linewidth = 1))
  
  fig <- fig_triangle + fig_square
  
  ggsave(paste0("fig", i, ".png"), fig, height = 4, width = 15, dpi = 150, bg = "black")
  
}

gifski(paste0("fig", 1:length(seq), ".png"), delay = 1/10, width = 1200, height = 600, gif_file = "chaos_game.gif")
unlink(paste0("fig", 1:length(seq), ".png"))