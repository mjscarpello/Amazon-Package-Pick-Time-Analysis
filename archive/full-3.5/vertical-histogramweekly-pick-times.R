library(dplyr)
library(ggplot2)
library(tidyr)

# -------------------------------------------------
# Week-to-week mirrored histograms
# Only highlight the 3 largest changes from the previous week
# -------------------------------------------------

weekly_base <- delivery_final %>%
  dplyr::filter(day_num >= 1, day_num <= 28) %>%
  dplyr::mutate(
    week_num = dplyr::case_when(
      day_num <= 7  ~ 1L,
      day_num <= 14 ~ 2L,
      day_num <= 21 ~ 3L,
      TRUE          ~ 4L
    ),
    week = factor(
      paste0("Week ", week_num),
      levels = paste0("Week ", 1:4)
    ),
    pick_bin = floor(pick_time_sec / 2) * 2
  ) %>%
  dplyr::filter(package_type %in% c("Box", "Envelope"))

# Current week counts
weekly_current <- weekly_base %>%
  dplyr::count(week_num, week, package_type, pick_bin, name = "n_current") %>%
  dplyr::mutate(
    plot_current = ifelse(package_type == "Box", -n_current, n_current)
  )

# Previous week counts, shifted forward one panel
weekly_previous <- weekly_base %>%
  dplyr::count(week_num, package_type, pick_bin, name = "n_previous") %>%
  dplyr::mutate(
    week_num = week_num + 1L
  ) %>%
  dplyr::filter(week_num <= 4) %>%
  dplyr::mutate(
    week = factor(paste0("Week ", week_num), levels = paste0("Week ", 1:4)),
    plot_previous = ifelse(package_type == "Box", -n_previous, n_previous)
  )

# Complete grid so missing bins become zero
all_bins <- seq(
  0,
  max(weekly_base$pick_bin, na.rm = TRUE),
  by = 2
)

current_complete <- weekly_current %>%
  dplyr::select(week_num, week, package_type, pick_bin, n_current, plot_current) %>%
  tidyr::complete(
    week_num = 1:4,
    package_type = c("Box", "Envelope"),
    pick_bin = all_bins,
    fill = list(n_current = 0, plot_current = 0)
  ) %>%
  dplyr::mutate(
    week = factor(paste0("Week ", week_num), levels = paste0("Week ", 1:4))
  )

previous_complete <- weekly_previous %>%
  dplyr::select(week_num, week, package_type, pick_bin, n_previous, plot_previous) %>%
  tidyr::complete(
    week_num = 2:4,
    package_type = c("Box", "Envelope"),
    pick_bin = all_bins,
    fill = list(n_previous = 0, plot_previous = 0)
  ) %>%
  dplyr::mutate(
    week = factor(paste0("Week ", week_num), levels = paste0("Week ", 1:4))
  )

# Find the top 3 largest changes within each week panel
change_bins <- current_complete %>%
  dplyr::filter(week_num >= 2) %>%
  dplyr::left_join(
    previous_complete %>%
      dplyr::select(week_num, package_type, pick_bin, n_previous),
    by = c("week_num", "package_type", "pick_bin")
  ) %>%
  dplyr::mutate(
    n_previous = ifelse(is.na(n_previous), 0, n_previous),
    change_abs = abs(n_current - n_previous)
  ) %>%
  dplyr::group_by(week_num) %>%
  dplyr::arrange(dplyr::desc(change_abs), .by_group = TRUE) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::ungroup() %>%
  dplyr::select(week_num, week, package_type, pick_bin, n_previous, plot_current)

# Create the previous-week highlight bars only for those top 3 changed bins
highlight_previous <- change_bins %>%
  dplyr::mutate(
    plot_previous = ifelse(package_type == "Box", -n_previous, n_previous)
  )

# Symmetric axis range
max_count <- max(
  abs(c(current_complete$plot_current, highlight_previous$plot_previous)),
  na.rm = TRUE
)

max_bin <- max(all_bins, na.rm = TRUE)

# Plot
plot_weekly_overlay_top3 <- ggplot() +
  
  # Current week bars
  geom_col(
    data = current_complete,
    aes(x = pick_bin, y = plot_current, fill = package_type),
    width = 1.8,
    alpha = 0.95
  ) +
  
  # Previous week highlight (top 3 changes only)
  geom_col(
    data = highlight_previous,
    aes(
      x = pick_bin,
      y = plot_previous,
      color = "Previous Week (Largest Change)"
    ),
    width = 1.8,
    fill = NA,
    linewidth = 1.3
  ) +
  
  coord_flip() +
  facet_wrap(~ week, ncol = 2) +
  
  scale_y_continuous(
    limits = c(-max_count, max_count),
    labels = \(x) abs(x)
  ) +
  
  scale_x_continuous(
    breaks = seq(0, max_bin, by = 4)
  ) +
  
  
  
  scale_color_manual(
    values = c("Previous Week (Largest Change)" = "#F4C542"),
    name = "Week Comparison:"
  ) +
  
  labs(
    x = "Pick Time (seconds)",
    y = "Number of Packages",
    fill = "Package Type:"
  ) +
  
  annotate(
    "text",
    x = max_bin * 0.82,
    y = -max_count * 0.55,
    label = "Box",
    size = 6,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = max_bin * 0.82,
    y = max_count * 0.55,
    label = "Envelope",
    size = 6,
    fontface = "bold"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey78", linewidth = 0.5),
    panel.grid.major.x = element_line(color = "grey82", linewidth = 0.5)
  )

print(plot_weekly_overlay_top3)