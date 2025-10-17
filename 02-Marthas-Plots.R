library(dplyr)
library(ggplot2)

rm(list = ls())
# Takes forever
source("~/Documents/Pressure-Cooker-Group-2/01-load-data.r") # Probably has to be adapted to each person

#  Does using hints improve performance? + Does using evaluates improve performance? + full effect - Martha 
##################################### DATA #####################################
first_plot_data <- full_data %>%
  arrange(desc(row_number())) %>%
  fill(progress, .direction = "down") %>%
  arrange(row_number())

bin_width <- 0.1

first_plot_data <- first_plot_data %>%
  filter(event %in% c("HINT", "EVALUATE")) %>%
  mutate(
    progress_bin = cut(
      progress,
      breaks = seq(0, 1, by = bin_width),
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0(seq(0, 0.95, by = bin_width) * 100, "%–", seq(bin_width, 1, by = bin_width) * 100, "%")
    )
  ) %>%
  group_by(event, progress_bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(event) %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- first_plot_data %>%
  group_by(progress_bin) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>%
  mutate(
    event = "Both",
    percentage = count / sum(count) * 100
  ) %>%
  select(event, progress_bin, count, percentage)

plot_data_with_both <- bind_rows(first_plot_data, combined_data)
#################################### Plots ####################################
ggplot(first_plot_data, aes(x = progress_bin, y = percentage, fill = event)) +
  geom_col(position = "dodge") +
  labs(
    title = "Relative Distribution of Hint and Evaluate Events Across Progress",
    x = "Progress (%)",
    y = "Percentage of Events",
    fill = "Event"
  ) +
  scale_y_continuous(labels = function(x) x / 100) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add overall variable - Not necessary I think. But this graph shows how many less hints are used
ggplot(plot_data_with_both, aes(x = progress_bin, y = percentage, fill = event)) +
  geom_col(position = "dodge") +
  labs(
    title = "Relative Distribution of Hint, Evaluate, and Both Events Across Progress",
    x = "Progress (%)",
    y = "Percentage of Events",
    fill = "Event"
  ) +
  scale_y_continuous(labels = function(x) x / 100) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Added line graph. I don't like it that much, but wanted to try it out
ggplot(first_plot_data, aes(x = progress_bin, y = percentage, group = event, color = event)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Distribution of HINT and EVALUATE Events Across Progress (10% Bins)",
    x = "Progress Bin",
    y = "Percentage of Events",
    color = "Event"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
           

# Does spamming hints improve performance? + Does spamming evaluates improve performance? + full effect - Martha 
##################################### DATA #####################################
second_plot_data <- full_data %>%
  arrange(desc(row_number())) %>%
  fill(progress, .direction = "down") %>%
  arrange(row_number()) %>%
  filter(spam_hint == 1 | spam_eval == 1)

# Bin width
bin_width <- 0.1

second_plot_data <- second_plot_data %>%
  filter(event %in% c("HINT", "EVALUATE")) %>%
  mutate(
    progress_bin = cut(
      progress,
      breaks = seq(0, 1, by = bin_width),
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0(seq(0, 0.95, by = bin_width) * 100, "%–", seq(bin_width, 1, by = bin_width) * 100, "%")
    )
  ) %>%
  group_by(event, progress_bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(event) %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- second_plot_data %>%
  group_by(progress_bin) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(
    event = "Both",
    percentage = count / sum(count) * 100
  ) %>%
  select(event, progress_bin, count, percentage)

plot_data_with_both <- bind_rows(second_plot_data, combined_data)

#################################### Plot ####################################
ggplot(second_plot_data, aes(x = progress_bin, y = percentage, fill = event)) +
  geom_col(position = "dodge") +
  labs(
    title = "Relative Distribution of spammed Hints and Evaluate Events Across Progress",
    x = "Progress (%)",
    y = "Percentage of Events",
    fill = "Event"
  ) +
  scale_fill_manual(
    values = c("HINT" = "#1f77b4", "EVALUATE" = "#ff7f0e"),
    labels = c("HINT" = "Spammed Hint", "EVALUATE" = "Spammed Evaluation")
  ) +
  scale_y_continuous(labels = function(x) x / 100) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot with both variables combined
# Again, probably adding the overall variable is unnecessary
ggplot(plot_data_with_both, aes(x = progress_bin, y = percentage, fill = event)) +
  geom_col(position = "dodge") +
  labs(
    title = "Relative Distribution of Spammed Hint, Evaluate, and Both Events Across Progress",
    x = "Progress (%)",
    y = "Percentage of Events",
    fill = "Event"
  ) +
  scale_fill_manual(
    values = c(
      "HINT" = "#1f77b4",
      "EVALUATE" = "#ff7f0e",
      "Both" = "#2ca02c"  # green for combined
    ),
    labels = c(
      "HINT" = "Spammed Hint",
      "EVALUATE" = "Spammed Evaluation",
      "Both" = "Both Combined"
    )
  ) +
  scale_y_continuous(labels = function(x) x / 100) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











