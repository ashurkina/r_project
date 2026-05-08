library(tidyverse)

reviews <- read.csv("reviews.csv")

reviews$review_length <- nchar(reviews$content)

reviews$score <- as.factor(reviews$score)

reviews <- reviews %>%
  drop_na(score, review_length)

ggplot(reviews, aes(
  x = score,
  y = review_length,
  fill = score
)) +
  
  geom_boxplot(
    alpha = 0.85,
    outlier.alpha = 0.15,
    width = 0.6
  ) +
  
  geom_jitter(
    width = 0.15,
    alpha = 0.03,
    size = 0.7,
    color = "gray25"
  ) +
  
  scale_fill_brewer(palette = "Blues") +
  
  labs(
    title = "Review Length by ChatGPT App Rating",
    subtitle = "Google Play User Reviews",
    x = "App Rating",
    y = "Review Length (Characters)"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      color = "gray40",
      size = 12
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )

anova_test <- aov(review_length ~ score, data = reviews)

summary(anova_test)