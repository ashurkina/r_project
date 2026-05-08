library(tidyverse)

# load data
reviews <- read.csv("reviews.csv")

# create review length variable
reviews$review_length <- nchar(reviews$content)

# convert score to factor
reviews$score <- as.factor(reviews$score)

# remove missing values
reviews <- reviews %>%
  drop_na(score, review_length)

# highlight groups
reviews <- reviews %>%
  mutate(
    highlight = case_when(
      score == "1" ~ "Negative (1-star)",
      score == "5" ~ "Positive (5-star)",
      TRUE ~ "Neutral (2–4 stars)"
    )
  )

# median values
median_values <- reviews %>%
  group_by(score) %>%
  summarise(
    median_length = median(review_length)
  )

# plot
ggplot(reviews, aes(
  x = score,
  y = review_length,
  fill = highlight
)) +
  
  geom_jitter(
    aes(color = highlight),
    width = 0.08,
    alpha = 0.12,
    size = 0.7
  ) +
  
  geom_boxplot(
    aes(fill = highlight),
    alpha = 0.95,
    outlier.alpha = 0,
    width = 0.42,
    linewidth = 0.7,
    color = "gray30"
  ) +

  
  geom_text(
    data = median_values,
    aes(
      x = as.numeric(score) - 0.28,
      y = median_length,
      label = round(median_length, 0)
    ),
    inherit.aes = FALSE,
    size = 3,
    fontface = "plain",
    color = "gray30"
  ) +
  
  # annotate(
  #   "text",
  #   x = 3.2,
  #   y = 455,
  #   label = "Median review length drops\nfrom 28 characters (1★)\nto 10 characters (5★)",
  #   
  #   size = 5,
  #   color = "gray25",
  #   fontface = "bold",
  #   lineheight = 1.1
  # ) +
  
  # # annotation box
  # annotate(
  #   "label",
  #   x = 3,
  #   y = 430,
  #   label = "Lower ratings, longer reviews.\nUsers who give 1-star reviews write\nsignificantly longer reviews than\nusers who give 5-star reviews.",
  #   size = 5,
  #   fontface = "bold",
  #   color = "#123a8f",
  #   fill = "white",
  #   hjust = 0.5,
  #   lineheight = 1.15
  # ) +
  # 
  # # left arrow
  # annotate(
  #   "curve",
  #   x = 2.3,
  #   y = 420,
  #   xend = 1.15,
  #   yend = 320,
  #   curvature = 0.2,
  #   linewidth = 0.8,
  #   arrow = arrow(length = unit(0.18, "cm")),
  #   color = "#2349b3"
  # ) +
  # 
  # # right arrow
  # annotate(
  #   "curve",
  #   x = 3.7,
  #   y = 420,
  #   xend = 4.95,
  #   yend = 320,
  #   curvature = -0.2,
  #   linewidth = 0.8,
  #   arrow = arrow(length = unit(0.18, "cm")),
  #   color = "#2349b3"
  # ) +
  
  scale_fill_manual(
    values = c(
      "Negative (1-star)" = "#d8e7f5",
      "Positive (5-star)" = "#1f5aa6",
      "Neutral (2–4 stars)" = "gray92"
    ),
    breaks = c(
      "Negative (1-star)",
      "Positive (5-star)"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Negative (1-star)" = "#9fc4ec",
      "Positive (5-star)" = "#2166ac",
      "Neutral (2–4 stars)" = "gray70"
    ),
    breaks = c(
      "Negative (1-star)",
      "Positive (5-star)"
    )
  ) +
  
  # labels
  labs(
    title = "Negative Reviews Tend to Be Longer",
    subtitle = "Based on 50,000 Google Play reviews of the ChatGPT app",
    x = "App Rating",
    y = "Review Length (Characters)",
    caption = "Source: Google Play reviews for the ChatGPT app"
  ) +
  
  # theme
  theme_minimal(base_size = 15, base_family = "Helvetica") +

  theme(
    legend.position = "bottom",
    
    legend.title = element_blank(),
    
    legend.text = element_text(
      size = 11
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 28
    ),
    
    plot.subtitle = element_text(
      size = 16,
      color = "gray40"
    ),
    
    plot.caption = element_text(
      size = 11,
      color = "gray45",
      hjust = 0,
      face = "italic"
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 16
    ),
    
    axis.text = element_text(
      size = 13
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "gray87",
      linewidth = 0.5
    )
  )
