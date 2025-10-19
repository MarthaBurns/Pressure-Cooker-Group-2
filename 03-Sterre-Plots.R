# install.packages("showtext")
# install.packages("ggtext")
# install.packages("xfun")

library(tidyverse)
library(showtext)
library(ggtext)
library(ggplot2)
library(patchwork)

# source("~/Pressure-Cooker-Group-2/01-load-data.r") # Probably has to be adapted to each person

font_add_google("Tenor Sans", "tenor_sans")
showtext_auto()

# Time between events defined as spam
threshold <- 3

### DENSITIES

# Compute densities manually for each event

# Separate standardized tables for hints and evaluations
hints <- consecutive_hints %>%
  mutate(event_type = "HINT") %>%
  rename(time_between = time_between_hints)

evals <- consecutive_evals %>%
  mutate(event_type = "EVALUATE") %>%
  rename(time_between = time_between_evals)

# Combine
combined <- bind_rows(hints, evals)

densities <- combined %>%
  group_by(event_type) %>%
  do({
    dens <- density(.$time_between, from = 0, to = 30)
    data.frame(x = dens$x, y = dens$y, event_type = unique(.$event_type))
  })

# Split into two segments based on threshold
densities <- densities %>%
  mutate(segment = case_when(
    x <= threshold + epsilon ~ "before",
    TRUE ~ "after"
  ))

## Hint density
dens_hint <- density(consecutive_hints$time_between_hints, from = 0, to = 20)
dens_df_hint <- data.frame(x = dens_hint$x, y = dens_hint$y)
# Split
dens_df_hint$segment <- ifelse(dens_df_hint$x <= threshold, "before", "after")

## Eval density
dens_eval <- density(consecutive_evals$time_between_evals, from = 0, to = 60)
dens_df_eval <- data.frame(x = dens_eval$x, y = dens_eval$y)
# Split
dens_df_eval$segment <- ifelse(dens_df_eval$x <= threshold, "before", "after")

### THEME
base_theme <- theme_minimal(base_size = 16)

title_left_theme <- theme(
  plot.title = element_text(
    hjust = 0,         # left align
    vjust = 1,         # slightly higher up
    face = "bold",
    size = 16,
    color = "#383838"
  )
)

### TITLES
# Optional: the thoughts was to add the titles as a textblock to the left
# of the plots. 

# Title block for hint
title_h <- ggplot() +
  annotate("text", x = 0, y = 0.5, label = "Density of Time Between\nConsecutive Hints", 
           hjust = 0, vjust = 0.5, size = 2, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Title block for evaluation
title_e <- ggplot() +
  annotate("text", x = 0, y = 0.5, label = "Density of Time Between\nConsecutive Evaluations", 
           hjust = 0, vjust = 0.5, size = 2, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

### PLOTS

# Plot the density on y axis and x axis as time between consecutive hints
# Hint plot
hint_dist <- ggplot(dens_df_hint, aes(x = x, y = y, fill = segment)) +
  geom_area(data = subset(dens_df_hint, segment == "before"),
            fill = "#648FFF", alpha = 0.7) +
  geom_line(data = subset(dens_df_hint, segment == "before"),
            color = "#648FFF", alpha = 0.7) +
  geom_area(data = subset(dens_df_hint, segment == "after"),
            fill = "#648FFF", alpha = 0.3) +
  geom_line(data = subset(dens_df_hint, segment == "after"),
            color = "#648FFF", alpha = 0.3) +
  geom_vline(xintercept = threshold, linetype = "solid", colour = "#383838", linewidth = 0.6) +
  labs(
    title = "Density of Time Between Consecutive Hint Requests",
    x = "Time Between Hints (seconds)",
    y = "Density"
  ) +
  xlim(c(0, 20)) +
  base_theme +
  title_left_theme

# Hint block
hint_block <- title_h | hint_dist

# Plot the density on y axis and x axis as time between consecutive evaluates
# Eval plot
eval_dist <- ggplot(dens_df_eval, aes(x = x, y = y, fill = segment)) +
  # Area before threshold
  geom_area(data = subset(dens_df_eval, segment == "before"),
            fill = "#FFB000", alpha = 0.7) +
  geom_line(data = subset(dens_df_eval, segment == "before"),
            color = "#FFB000", alpha = 0.7) +
  # Area after threshold
  geom_area(data = subset(dens_df_eval, segment == "after"),
            fill = "#FFB000", alpha = 0.3) +
  geom_line(data = subset(dens_df_eval, segment == "after"),
            color = "#FFB000", alpha = 0.3) +
  geom_vline(xintercept = threshold, linetype = "solid", colour = "#383838", linewidth = 0.6) +
  labs(
    title = "Density of Time Between Consecutive Evaluations",
    x = "Time Between Evaluations (seconds)",
    y = "Density"
  ) +
  xlim(c(0, 40)) +
  base_theme +
  title_left_theme

# Eval block
eval_block <- title_e | eval_dist

### FINAL DENSITY PLOT
dense_plot <- hint_dist / eval_dist + 
  plot_layout(widths = c(0.5, 2))

dense_plot

### SEPARATE BARS PLOT

# THEME
plot2_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Set colors and labels for legend
fill_scale <- scale_fill_manual(
  name = NULL,
  values = c("HINT" = "#648FFF", "EVALUATE" = "#FFB000"),
  labels = c(
    "HINT" = "Spammed\nHint",
    "EVALUATE" = "Spammed\nEvaluation"
  ),
  drop = FALSE
)

# HINT
p_hint <- ggplot(
  subset(second_plot_data, event == "HINT"),
  aes(x = progress_bin, y = percentage, fill = event)
  ) +
  geom_col(linewidth = 0.4) +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    vjust = -0.5,
    size = 3.0,
    color = "#383838"
  ) +
  fill_scale +
  labs(x = "Progress (%)", y = "Percentage of Events") +
  scale_x_discrete(
    labels = c("0–10", "50–60", "90–100")
  ) +
  plot2_theme +
  base_theme

# EVALUATE
p_eval <- ggplot(
  subset(second_plot_data, event == "EVALUATE"),
  aes(x = progress_bin, y = percentage, fill = event)
  ) +
  geom_col(linewidth = 0.4) +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    vjust = -0.5,
    size = 3.0,
    color = "#383838"
  ) +
  fill_scale +
  labs(x = "Progress (%)", y = NULL) +
  scale_x_discrete(
    labels = c("0–10", "10–20", "20–30", "30–40", "40–50", 
               "50–60", "60–70", "70–80", "80–90", "90–100")
  ) +
  plot2_theme +
  base_theme +
  ylim(0,60) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y  = element_blank())

# Combine side by side and collect the single legend
combined_plot <- (p_hint | p_eval) + 
  plot_layout(widths = c(0.33, 1), guides = "collect") &
  theme(
    legend.position="right"
  )

### FINAL PROGRESS PLOT

prog_plot <- combined_plot + 
  plot_annotation(
    title = "Distribution of Spammed Hints and Evaluations Across Progress",
    subtitle = "",
    theme = theme(plot.title = element_text(hjust = 0,
                                            vjust = -2,
                                            face = "bold", 
                                            color = "#383838")
                  # plot.subtitle = element_text(hjust = 0, size = 9)
                  )
  )

prog_plot
# Possible conclusion: 
# Spamming patterns might suggest initial lack of direction and end-of-task disengagement.