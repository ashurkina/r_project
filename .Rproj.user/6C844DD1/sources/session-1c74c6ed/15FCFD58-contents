library(tidyverse)

# загружаем данные
reviews <- read.csv("reviews.csv")

# добавляем длину отзыва
reviews$review_length <- nchar(reviews$content)

# score как категория
reviews$score <- as.factor(reviews$score)

# удаляем пропуски
reviews <- reviews %>%
  drop_na(score, review_length)

# рисуем график
ggplot(reviews, aes(
  x = score,
  y = review_length,
)) +
  
  geom_boxplot(
    alpha = 0.95,
    linewidth = 2,
    outlier.color = "red",
    outlier.size = 3
  ) +
  
  geom_jitter(
    color = "black",
    width = 0.25,
    alpha = 0.25,
    size = 2
  ) +
  
  scale_fill_manual(values = c(
    "hotpink",
    "cyan",
    "yellow",
    "limegreen",
    "orange"
  )) +
  
  labs(
    title = "CHATGPT APP RATING VS TEXT LENGTH",
    subtitle = "Correlation Between User Ratings and Review Length",
    x = "USER SCORE",
    y = "TEXT LENGTH"
  ) +
  
  theme_minimal(base_size = 18) +
  
  theme(
    panel.background = element_rect(fill = "black"),
    
    plot.background = element_rect(fill = "hotpink"),
    
    panel.grid.major = element_line(
      color = "yellow",
      linewidth = 1.5
    ),
    
    panel.grid.minor = element_line(
      color = "cyan",
      linewidth = 0.7
    ),
    
    text = element_text(
      face = "bold",
      color = "white"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      color = "cyan",
      size = 16
    ),
    
    axis.text.y = element_text(
      color = "yellow",
      size = 16
    ),
    
    axis.title = element_text(
      color = "limegreen",
      size = 22
    ),
    
    plot.title = element_text(
      size = 28,
      color = "limegreen"
    ),
    
    plot.subtitle = element_text(
      size = 16,
      color = "orange"
    ),
    
    legend.position = "bottom"
  )
