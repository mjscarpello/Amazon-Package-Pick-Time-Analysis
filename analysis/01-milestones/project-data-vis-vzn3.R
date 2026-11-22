# ============================================
# Title: Milestone 1 Simulation (Workflow Model)
# Author: Matthew Scarpello
# Date: 16 February 2026
# Description:
# Simulated pick-time data reflecting a two-side
# Amazon delivery sorting workflow. This script
# generates the dataset and produces two main
# visualizations for the project:
# 1. Faceted scatterplot with smoothers by bag
# 2. Full-day line plot across pick sequence
#
# Notes:
# - This is a simulation based on real workflow logic
# - Envelope side is modeled as slower and more volatile early
# - Box side is modeled as more stable and generally faster
# ============================================

# ---------------------------
# Libraries
# ---------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---------------------------
# Global options
# ---------------------------
set.seed(310)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
)

# ---------------------------
# User controls
# ---------------------------
n_bags <- 6
min_items_per_side <- 12
max_items_per_side <- 22
max_over_60_per_day <- 3

bag_palette <- c(
  "Bag_1" = "#1b9e77",
  "Bag_2" = "#d95f02",
  "Bag_3" = "#7570b3",
  "Bag_4" = "#e7298a",
  "Bag_5" = "#66a61e",
  "Bag_6" = "#e6ab02"
)

side_palette <- c(
  "Box" = "#F8766D",
  "Envelope" = "#00BFC4"
)

# ---------------------------
# Helper function:
# Simulate one side of one bag
# ---------------------------
simulate_side_times <- function(side, n_items) {
  pos <- 1:n_items
  remaining <- rev(pos)
  
  # progress goes from 1 early in the side to 0 late
  progress <- (remaining - 1) / (n_items - 1)
  
  if (side == "Envelope") {
    # Envelope side:
    # much slower and noisier early, stabilizes late
    base <- 6 + 74 * progress^1.6
    sd_vec <- 3 + 18 * progress
    
    lucky_prob <- 0.05 + 0.03 * progress
    lucky <- rbinom(n_items, 1, lucky_prob)
    lucky_time <- runif(n_items, 0.8, 3.0)
    
    outlier_prob <- 0.03 * progress
    outlier <- rbinom(n_items, 1, outlier_prob)
    outlier_bump <- runif(n_items, 12, 45)
    
  } else if (side == "Box") {
    # Box side:
    # faster and more stable early, converges late
    base <- 6 + 6 * progress^1.3
    sd_vec <- 2 + 7 * progress
    
    lucky_prob <- 0.04 + 0.02 * progress
    lucky <- rbinom(n_items, 1, lucky_prob)
    lucky_time <- runif(n_items, 0.8, 3.0)
    
    outlier_prob <- 0.015 * progress
    outlier <- rbinom(n_items, 1, outlier_prob)
    outlier_bump <- runif(n_items, 8, 30)
  } else {
    stop("side must be either 'Envelope' or 'Box'")
  }
  
  time <- rnorm(n_items, mean = base, sd = sd_vec)
  
  # Replace some values with lucky quick finds
  time <- ifelse(lucky == 1, lucky_time, time)
  
  # Add rare outlier bumps
  time <- time + outlier * outlier_bump
  
  # Last pick on a side is very often one of the fastest
  if (rbinom(1, 1, 0.90) == 1) {
    time[n_items] <- runif(1, 0.8, 2.2)
  }
  
  # Clamp lower bound
  time <- pmax(time, 0.6)
  
  tibble(
    position_in_side = pos,
    remaining_on_side = remaining,
    pick_time_sec = time
  )
}

# ---------------------------
# Build simulated dataset
# ---------------------------
sim_list <- lapply(1:n_bags, function(b) {
  
  env_n <- sample(min_items_per_side:max_items_per_side, 1)
  box_n <- sample(min_items_per_side:max_items_per_side, 1)
  
  env <- simulate_side_times("Envelope", env_n) %>%
    mutate(
      bag_id = paste0("Bag_", b),
      side = "Envelope"
    )
  
  box <- simulate_side_times("Box", box_n) %>%
    mutate(
      bag_id = paste0("Bag_", b),
      side = "Box"
    )
  
  bind_rows(env, box)
})

delivery_sim <- bind_rows(sim_list) %>%
  group_by(bag_id) %>%
  mutate(
    bag_density = n()
  ) %>%
  ungroup()

# ---------------------------
# Create stop index within each side
# ---------------------------
delivery_sim <- delivery_sim %>%
  group_by(bag_id, side) %>%
  arrange(position_in_side, .by_group = TRUE) %>%
  mutate(
    stop_in_bag = row_number()
  ) %>%
  ungroup()

# ---------------------------
# Realism constraint:
# cap number of picks over 60 seconds
# ---------------------------
over_60 <- delivery_sim %>%
  mutate(row_id = row_number()) %>%
  filter(pick_time_sec > 60) %>%
  mutate(
    early_flag = stop_in_bag <= 5,
    priority = (side == "Envelope") * 2 + early_flag
  ) %>%
  arrange(desc(priority), desc(pick_time_sec))

if (nrow(over_60) > max_over_60_per_day) {
  clamp_rows <- over_60$row_id[(max_over_60_per_day + 1):nrow(over_60)]
  delivery_sim$pick_time_sec[clamp_rows] <- runif(length(clamp_rows), 35, 60)
}

# ---------------------------
# Create full-day pick index
# Interleave Envelope then Box within each bag
# ---------------------------
delivery_sim <- delivery_sim %>%
  group_by(bag_id) %>%
  complete(
    stop_in_bag = 1:max(stop_in_bag, na.rm = TRUE),
    side = c("Envelope", "Box")
  ) %>%
  ungroup() %>%
  arrange(
    bag_id,
    stop_in_bag,
    factor(side, levels = c("Envelope", "Box"))
  ) %>%
  mutate(
    day_pick_index = row_number()
  )

# ---------------------------
# Clean plotting data
# ---------------------------
df_plot <- delivery_sim %>%
  filter(!is.na(pick_time_sec)) %>%
  mutate(
    bag_id = factor(bag_id, levels = paste0("Bag_", 1:n_bags)),
    side = factor(side, levels = c("Box", "Envelope"))
  )

# ---------------------------
# Sanity checks
# ---------------------------
cat("Number of picks over 60 seconds:", sum(df_plot$pick_time_sec > 60), "\n")
cat("Total non-missing picks:", nrow(df_plot), "\n")

print(head(df_plot))

# =========================================================
# GRAPH 1:
# Scatterplot of pick time vs remaining packages on that side
# =========================================================
plot_scatter <- ggplot(
  df_plot,
  aes(x = remaining_on_side, y = pick_time_sec, color = side)
) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(
    se = FALSE,
    method = "loess",
    span = 0.9,
    linewidth = 1.2
  ) +
  facet_wrap(~ bag_id, scales = "free_y") +
  scale_x_reverse() +
  scale_color_manual(values = side_palette) +
  labs(
    title = "Amazon Delivery Pick Time by Remaining Packages",
    subtitle = "Each panel represents one bag. Envelope and box sides are shown separately within each bag.",
    x = "Remaining Packages on That Side",
    y = "Pick Time (Seconds)",
    color = "Side"
  ) +
  theme(
    legend.position = "right"
  )

# =========================================================
# GRAPH 2:
# Full-day line plot across pick sequence
# =========================================================
plot_line <- ggplot() +
  # Envelope line
  geom_line(
    data = df_plot %>% filter(side == "Envelope"),
    aes(
      x = day_pick_index,
      y = pick_time_sec,
      group = interaction(bag_id, side),
      color = bag_id
    ),
    linewidth = 0.9,
    alpha = 0.30,
    linetype = "solid"
  ) +
  # Box halo line
  geom_line(
    data = df_plot %>% filter(side == "Box"),
    aes(
      x = day_pick_index,
      y = pick_time_sec,
      group = interaction(bag_id, side)
    ),
    linewidth = 2.8,
    alpha = 1,
    linetype = "dashed",
    color = "white"
  ) +
  # Box emphasized line
  geom_line(
    data = df_plot %>% filter(side == "Box"),
    aes(
      x = day_pick_index,
      y = pick_time_sec,
      group = interaction(bag_id, side),
      color = bag_id
    ),
    linewidth = 1.5,
    alpha = 1,
    linetype = "11"
  ) +
  scale_color_manual(values = bag_palette) +
  labs(
    title = "Amazon Delivery Pick Time Across the Day",
    subtitle = "The full route is shown as a pick sequence. Envelope is lighter, while box is emphasized.",
    x = "Pick Sequence Through Day",
    y = "Pick Time (Seconds)",
    color = "Bag"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = 5,
        linewidth = 1.5,
        alpha = 1
      )
    )
  ) +
  theme(
    legend.position = "right"
  )

# ---------------------------
# Print plots
# ---------------------------
print(plot_scatter)
print(plot_line)

# ---------------------------
# Save plots
# ---------------------------
ggsave(
  filename = "plot_scatter_remaining_vs_picktime.png",
  plot = plot_scatter,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "plot_line_full_day_pick_sequence.png",
  plot = plot_line,
  width = 11,
  height = 6.5,
  dpi = 300
)

# ---------------------------
# Optional summary table
# ---------------------------
summary_by_side <- df_plot %>%
  group_by(side) %>%
  summarise(
    n = n(),
    mean_pick = mean(pick_time_sec),
    median_pick = median(pick_time_sec),
    sd_pick = sd(pick_time_sec),
    max_pick = max(pick_time_sec),
    .groups = "drop"
  )

print(summary_by_side)

summary_by_bag_side <- df_plot %>%
  group_by(bag_id, side) %>%
  summarise(
    n = n(),
    mean_pick = mean(pick_time_sec),
    median_pick = median(pick_time_sec),
    sd_pick = sd(pick_time_sec),
    max_pick = max(pick_time_sec),
    .groups = "drop"
  )

print(summary_by_bag_side)

# =========================================================
# GRAPH 3:
# Histogram of pick times
# =========================================================
plot_hist <- ggplot(df_plot, aes(x = pick_time_sec)) +
  geom_histogram(
    binwidth = 2,
    fill = "#4C78A8",
    color = "white",
    alpha = 0.9
  ) +
  geom_density(
    aes(y = after_stat(count * 2)),
    color = "#E45756",
    linewidth = 1.2,
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Pick Times",
    subtitle = "Most picks are relatively fast, with a smaller number of longer search times creating a right tail.",
    x = "Pick Time (Seconds)",
    y = "Count"
  )

print(plot_hist)

ggsave(
  filename = "plot_hist_pick_time_distribution.png",
  plot = plot_hist,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================================
# GRAPH 4:
# Violin plot comparing pick times by side
# =========================================================


plot_violin_side <- ggplot(
  df_plot,
  aes(x = side, y = pick_time_sec, fill = side)
) +
  geom_violin(
    alpha = 0.75,
    trim = FALSE,
    width = 0.8
  ) +
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = side_palette) +
  labs(
    title = "Distribution of Pick Times by Truck Side",
    subtitle = "Violin shapes show the full distribution while boxplots show median and quartiles.",
    x = "Truck Side",
    y = "Pick Time (Seconds)",
    fill = "Side"
  ) +
  theme(
    legend.position = "none"
  )

print(plot_violin_side)

ggsave(
  filename = "plot_violin_pick_time_by_side.png",
  plot = plot_violin_side,
  width = 8,
  height = 6,
  dpi = 300
)


# =========================================================
# GRAPH 5:
# Average pick time by bag and side
# =========================================================
bag_side_summary <- df_plot %>%
  group_by(bag_id, side) %>%
  summarise(
    mean_pick = mean(pick_time_sec),
    .groups = "drop"
  )

plot_bag_avg <- ggplot(
  bag_side_summary,
  aes(x = bag_id, y = mean_pick, fill = side)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = side_palette) +
  labs(
    title = "Average Pick Time by Bag and Side",
    subtitle = "This view compares average search time across each bag for both workflow sides.",
    x = "Bag",
    y = "Average Pick Time (Seconds)",
    fill = "Side"
  )

print(plot_bag_avg)

ggsave(
  filename = "plot_bar_average_pick_time_by_bag_side.png",
  plot = plot_bag_avg,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================================
# GRAPH 6:
# Equilibrium tracker using cumulative mean
# =========================================================
route_progress <- df_plot %>%
  arrange(day_pick_index) %>%
  mutate(
    cumulative_mean = cummean(pick_time_sec)
  )

overall_mean <- mean(route_progress$pick_time_sec)

plot_equilibrium <- ggplot(route_progress, aes(x = day_pick_index, y = cumulative_mean)) +
  geom_line(color = "#4C78A8", linewidth = 1.2) +
  geom_hline(
    yintercept = overall_mean,
    color = "#E45756",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Equilibrium Tracker Across the Route",
    subtitle = "The cumulative mean shows how overall pick-time behavior stabilizes as the route progresses.",
    x = "Pick Sequence Through Day",
    y = "Cumulative Mean Pick Time (Seconds)"
  )

print(plot_equilibrium)

ggsave(
  filename = "plot_equilibrium_tracker.png",
  plot = plot_equilibrium,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================
# GRAPH 7:
# Stacked average composition by bag
# =========================================================
bag_side_avg <- df_plot %>%
  group_by(bag_id, side) %>%
  summarise(
    avg_pick = mean(pick_time_sec),
    .groups = "drop"
  )

plot_stacked_avg <- ggplot(
  bag_side_avg,
  aes(x = bag_id, y = avg_pick, fill = side)
) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = side_palette) +
  labs(
    title = "Stacked Average Composition by Bag",
    subtitle = "Each bar shows how envelope and box averages combine into the total average workflow burden for a bag.",
    x = "Bag",
    y = "Average Pick Time Contribution (Seconds)",
    fill = "Side"
  )

print(plot_stacked_avg)

ggsave(
  filename = "plot_stacked_average_composition_by_bag.png",
  plot = plot_stacked_avg,
  width = 10,
  height = 6,
  dpi = 300
)



# =========================================================
# GRAPH 6A:
# Slider-style equilibrium tracker
# =========================================================

route_progress <- df_plot %>%
  arrange(day_pick_index) %>%
  mutate(
    cumulative_mean = cummean(pick_time_sec)
  )

overall_mean <- mean(route_progress$pick_time_sec)
final_cum_mean <- tail(route_progress$cumulative_mean, 1)

slider_df <- tibble(
  xmin = min(c(route_progress$cumulative_mean, overall_mean)) - 5,
  xmax = max(c(route_progress$cumulative_mean, overall_mean)) + 5,
  equilibrium = overall_mean,
  current = final_cum_mean,
  y = 1
)

plot_equilibrium_slider <- ggplot(slider_df) +
  
  # slider track
  geom_segment(
    aes(x = xmin, xend = xmax, y = y, yend = y),
    linewidth = 6,
    color = "gray80",
    lineend = "round"
  ) +
  
  # equilibrium marker
  geom_segment(
    aes(x = equilibrium, xend = equilibrium, y = 0.82, yend = 1.18),
    linewidth = 1.4,
    color = "#E45756"
  ) +
  
  # moving ticker
  geom_point(
    aes(x = current, y = y),
    size = 7,
    color = "#4C78A8"
  ) +
  
  # labels
  annotate(
    "text",
    x = overall_mean,
    y = 1.23,
    label = "Equilibrium",
    color = "#E45756",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = final_cum_mean,
    y = 0.75,
    label = paste0("Current: ", round(final_cum_mean, 1), " sec"),
    color = "#4C78A8",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = slider_df$xmin,
    y = 1.1,
    label = "Faster",
    hjust = 0,
    color = "gray30",
    size = 4
  ) +
  annotate(
    "text",
    x = slider_df$xmax,
    y = 1.1,
    label = "Slower",
    hjust = 1,
    color = "gray30",
    size = 4
  ) +
  
  labs(
    title = "Equilibrium Tracker",
    subtitle = "The route's cumulative mean is shown as a moving ticker relative to the overall equilibrium.",
    x = "Cumulative Mean Pick Time (Seconds)",
    y = NULL
  ) +
  coord_cartesian(ylim = c(0.6, 1.35)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(plot_equilibrium_slider)

ggsave(
  filename = "plot_equilibrium_slider.png",
  plot = plot_equilibrium_slider,
  width = 10,
  height = 3.8,
  dpi = 300
)



tolerance <- 2

plot_equilibrium_slider <- ggplot(slider_df) +
  geom_segment(
    aes(x = xmin, xend = xmax, y = y, yend = y),
    linewidth = 6,
    color = "gray85",
    lineend = "round"
  ) +
  annotate(
    "rect",
    xmin = overall_mean - tolerance,
    xmax = overall_mean + tolerance,
    ymin = 0.93,
    ymax = 1.07,
    fill = "#F4A261",
    alpha = 0.5
  ) +
  geom_segment(
    aes(x = equilibrium, xend = equilibrium, y = 0.82, yend = 1.18),
    linewidth = 1.3,
    color = "#E45756"
  ) +
  geom_point(
    aes(x = current, y = y),
    size = 7,
    color = "#4C78A8"
  ) +
  labs(
    title = "Equilibrium Tracker with Stable Zone",
    subtitle = "The highlighted band shows a tolerance range around the route's equilibrium.",
    x = "Cumulative Mean Pick Time (Seconds)",
    y = NULL
  ) +
  coord_cartesian(ylim = c(0.7, 1.3)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )



# =========================================================
# GRAPH 6B:
# Equilibrium dashboard = route history + slider
# =========================================================

library(patchwork)

# Build cumulative route data
route_progress <- df_plot %>%
  arrange(day_pick_index) %>%
  mutate(
    cumulative_mean = cummean(pick_time_sec)
  )

overall_mean <- mean(route_progress$pick_time_sec)
final_cum_mean <- tail(route_progress$cumulative_mean, 1)

# Slider range
slider_min <- floor(min(c(route_progress$cumulative_mean, overall_mean)) - 3)
slider_max <- ceiling(max(c(route_progress$cumulative_mean, overall_mean)) + 3)

# Tolerance zone around equilibrium
tolerance <- 2

# Current status label and color
status_label <- if (final_cum_mean < overall_mean - tolerance) {
  "Currently Faster than Equilibrium"
} else if (final_cum_mean > overall_mean + tolerance) {
  "Currently Slower than Equilibrium"
} else {
  "Currently Within Stable Range"
}

status_color <- if (final_cum_mean < overall_mean - tolerance) {
  "#2A9D8F"   # green-teal
} else if (final_cum_mean > overall_mean + tolerance) {
  "#E76F51"   # warm red-orange
} else {
  "#4C78A8"   # blue
}

# -----------------------------
# TOP PANEL: route history
# -----------------------------
plot_equilibrium_history <- ggplot(route_progress, aes(x = day_pick_index, y = cumulative_mean)) +
  # stable zone
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = overall_mean - tolerance,
    ymax = overall_mean + tolerance,
    fill = "#F4D35E",
    alpha = 0.22
  ) +
  geom_line(
    color = "#4C78A8",
    linewidth = 1.4
  ) +
  geom_hline(
    yintercept = overall_mean,
    color = "#D1495B",
    linewidth = 1.1
  ) +
  geom_point(
    aes(x = max(day_pick_index), y = final_cum_mean),
    color = status_color,
    size = 3.5
  ) +
  annotate(
    "text",
    x = max(route_progress$day_pick_index) * 0.05,
    y = overall_mean + 0.35,
    label = paste0("Equilibrium = ", round(overall_mean, 1), " sec"),
    hjust = 0,
    color = "#D1495B",
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Equilibrium Tracker Across the Route",
    subtitle = "The cumulative mean settles over time, while the shaded band shows the stable range around equilibrium.",
    x = "Pick Sequence Through Day",
    y = "Cumulative Mean Pick Time (Seconds)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# -----------------------------
# BOTTOM PANEL: slider summary
# -----------------------------
slider_df <- tibble(
  xmin = slider_min,
  xmax = slider_max,
  equilibrium = overall_mean,
  current = final_cum_mean,
  y = 1
)

plot_equilibrium_slider2 <- ggplot(slider_df) +
  # left side: faster
  annotate(
    "segment",
    x = slider_min, xend = overall_mean,
    y = 1, yend = 1,
    linewidth = 7,
    color = "#B8E1D9",
    lineend = "round"
  ) +
  # right side: slower
  annotate(
    "segment",
    x = overall_mean, xend = slider_max,
    y = 1, yend = 1,
    linewidth = 7,
    color = "#F3C4B8",
    lineend = "round"
  ) +
  # stable zone
  annotate(
    "rect",
    xmin = overall_mean - tolerance,
    xmax = overall_mean + tolerance,
    ymin = 0.93,
    ymax = 1.07,
    fill = "#F4D35E",
    alpha = 0.85
  ) +
  # equilibrium marker
  geom_segment(
    aes(x = equilibrium, xend = equilibrium, y = 0.78, yend = 1.22),
    linewidth = 1.4,
    color = "#D1495B"
  ) +
  # current ticker
  geom_point(
    aes(x = current, y = y),
    size = 8,
    color = status_color
  ) +
  annotate(
    "text",
    x = overall_mean,
    y = 1.28,
    label = "Equilibrium",
    color = "#D1495B",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = slider_min,
    y = 1.18,
    label = "Faster",
    hjust = 0,
    color = "#2A6F63",
    size = 4
  ) +
  annotate(
    "text",
    x = slider_max,
    y = 1.18,
    label = "Slower",
    hjust = 1,
    color = "#A44A3F",
    size = 4
  ) +
  annotate(
    "text",
    x = final_cum_mean,
    y = 0.74,
    label = paste0(round(final_cum_mean, 1), " sec"),
    color = status_color,
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = (slider_min + slider_max) / 2,
    y = 1.42,
    label = status_label,
    color = status_color,
    fontface = "bold",
    size = 4.2
  ) +
  labs(
    x = "Cumulative Mean Pick Time (Seconds)",
    y = NULL
  ) +
  coord_cartesian(ylim = c(0.65, 1.5)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# -----------------------------
# COMBINE PANELS
# -----------------------------
plot_equilibrium_dashboard <- plot_equilibrium_history / plot_equilibrium_slider2 +
  plot_layout(heights = c(2.2, 1.3))

print(plot_equilibrium_dashboard)

ggsave(
  filename = "plot_equilibrium_dashboard.png",
  plot = plot_equilibrium_dashboard,
  width = 11,
  height = 8,
  dpi = 300
)




# =========================================================
# GRAPH 6C:
# Real-time equilibrium monitor
# Uses rolling mean instead of cumulative mean
# =========================================================

library(dplyr)
library(ggplot2)
library(patchwork)

# -----------------------------
# SETTINGS
# -----------------------------
window_size <- 12
tolerance <- 2

# -----------------------------
# BUILD REAL-TIME METRICS
# -----------------------------
route_progress <- df_plot %>%
  arrange(day_pick_index) %>%
  mutate(
    rolling_mean = sapply(seq_along(pick_time_sec), function(i) {
      start <- max(1, i - window_size + 1)
      mean(pick_time_sec[start:i], na.rm = TRUE)
    })
  )

equilibrium <- mean(route_progress$pick_time_sec, na.rm = TRUE)
current_value <- tail(route_progress$rolling_mean, 1)
current_pick <- max(route_progress$day_pick_index)

difference <- current_value - equilibrium

status_label <- case_when(
  current_value < equilibrium - tolerance ~ "Faster than baseline",
  current_value > equilibrium + tolerance ~ "Slower than baseline",
  TRUE ~ "Within stable range"
)

status_color <- case_when(
  current_value < equilibrium - tolerance ~ "#2A9D8F",
  current_value > equilibrium + tolerance ~ "#E76F51",
  TRUE ~ "#4C78A8"
)

# -----------------------------
# TOP PANEL:
# rolling mean history
# -----------------------------
plot_realtime_history <- ggplot(route_progress, aes(x = day_pick_index, y = rolling_mean)) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = equilibrium - tolerance,
    ymax = equilibrium + tolerance,
    fill = "#F4D35E",
    alpha = 0.20
  ) +
  geom_line(
    color = "#4C78A8",
    linewidth = 1.3
  ) +
  geom_hline(
    yintercept = equilibrium,
    color = "#D1495B",
    linewidth = 1
  ) +
  annotate(
    "point",
    x = current_pick,
    y = current_value,
    color = status_color,
    size = 3.5
  ) +
  annotate(
    "text",
    x = current_pick * 0.05,
    y = equilibrium + tolerance + 0.8,
    label = paste0("Baseline = ", round(equilibrium, 1), " sec"),
    hjust = 0,
    color = "#D1495B",
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Real-Time Workflow Monitor",
    subtitle = paste0(
      "Rolling mean of last ", window_size,
      " picks. The shaded band marks the stable range."
    ),
    x = "Pick Sequence Through Day",
    y = paste0(window_size, "-Pick Rolling Mean (Seconds)")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# -----------------------------
# BOTTOM PANEL:
# simple deviation gauge
# -----------------------------
gauge_min <- floor(min(c(route_progress$rolling_mean, equilibrium)) - 4)
gauge_max <- ceiling(max(c(route_progress$rolling_mean, equilibrium)) + 4)

gauge_df <- tibble(
  y = 1,
  gauge_min = gauge_min,
  gauge_max = gauge_max,
  equilibrium = equilibrium,
  current_value = current_value
)

plot_realtime_gauge <- ggplot(gauge_df) +
  annotate(
    "segment",
    x = gauge_min, xend = equilibrium,
    y = 1, yend = 1,
    linewidth = 7,
    color = "#BFE3DB",
    lineend = "round"
  ) +
  annotate(
    "segment",
    x = equilibrium, xend = gauge_max,
    y = 1, yend = 1,
    linewidth = 7,
    color = "#F3C7BC",
    lineend = "round"
  ) +
  annotate(
    "rect",
    xmin = equilibrium - tolerance,
    xmax = equilibrium + tolerance,
    ymin = 0.93,
    ymax = 1.07,
    fill = "#F4D35E",
    alpha = 0.9
  ) +
  annotate(
    "segment",
    x = equilibrium, xend = equilibrium,
    y = 0.78, yend = 1.22,
    linewidth = 1.3,
    color = "#D1495B"
  ) +
  annotate(
    "point",
    x = current_value,
    y = 1,
    size = 8,
    color = status_color
  ) +
  annotate(
    "text",
    x = equilibrium,
    y = 1.28,
    label = "Baseline",
    color = "#D1495B",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = gauge_min,
    y = 1.16,
    label = "Faster",
    hjust = 0,
    color = "#2A6F63",
    size = 4
  ) +
  annotate(
    "text",
    x = gauge_max,
    y = 1.16,
    label = "Slower",
    hjust = 1,
    color = "#A44A3F",
    size = 4
  ) +
  annotate(
    "text",
    x = current_value,
    y = 0.73,
    label = paste0(round(current_value, 1), " sec"),
    color = status_color,
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = (gauge_min + gauge_max) / 2,
    y = 1.42,
    label = status_label,
    color = status_color,
    fontface = "bold",
    size = 4.3
  ) +
  labs(
    x = "Current Rolling Mean Relative to Baseline",
    y = NULL
  ) +
  coord_cartesian(ylim = c(0.65, 1.5)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# -----------------------------
# OPTIONAL INFO PANEL
# -----------------------------
info_text <- paste0(
  "Current rolling mean: ", round(current_value, 1), " sec\n",
  "Baseline: ", round(equilibrium, 1), " sec\n",
  "Difference: ", ifelse(difference > 0, "+", ""), round(difference, 1), " sec\n",
  "Status: ", status_label
)

plot_info <- ggplot() +
  annotate(
    "label",
    x = 1, y = 1,
    label = info_text,
    hjust = 0,
    vjust = 1,
    size = 4.4,
    fontface = "bold",
    fill = "white",
    color = "gray20",
    label.size = 0.5
  ) +
  xlim(0.9, 2.2) +
  ylim(0.4, 1.1) +
  theme_void()

# -----------------------------
# COMBINE
# -----------------------------
bottom_row <- plot_realtime_gauge | plot_info

plot_realtime_dashboard <- plot_realtime_history / bottom_row +
  plot_layout(heights = c(2.2, 1.2), widths = c(2, 1))

print(plot_realtime_dashboard)

ggsave(
  filename = "plot_realtime_equilibrium_dashboard.png",
  plot = plot_realtime_dashboard,
  width = 12,
  height = 8,
  dpi = 300
)

