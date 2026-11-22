# ============================================
# Title: Milestone 1 Simulation (Workflow Model)
# Author: Matthew Scarpello
# Date: 16 February 2026
# Description: Simulated pick-time data that
# reflects my two-side sorting workflow and
# prototypes visualization for Milestone 1.
# ============================================

set.seed(310)

library(dplyr)
library(tidyr)

# ---- Controls you can tweak later ----
n_bags <- 6
min_items_per_side <- 12
max_items_per_side <- 22

max_over_60_per_day <- 3  # realism constraint

# ---- Helper: generate pick times using your described behavior ----
simulate_side_times <- function(side, n_items) {
  # position: 1 = first pick on that side, n_items = last pick
  pos <- 1:n_items
  remaining <- rev(pos)
  
  # progress: 1 early -> 0 late
  progress <- (remaining - 1) / (n_items - 1)
  
  if (side == "Envelope") {
    # Envelope: early can be very slow and noisy, late stabilizes strongly
    # Early baseline ~80 sec, late baseline ~6 sec
    base <- 6 + 74 * progress^1.6
    
    # Higher noise early, much lower late
    sd_vec <- 3 + 18 * progress
    
    # Lucky finds (rare)
    lucky_prob <- 0.05 + 0.03 * progress
    lucky <- rbinom(n_items, 1, lucky_prob)
    lucky_time <- runif(n_items, 0.8, 3.0)
    
    # Outliers (rare, mostly early)
    outlier_prob <- 0.03 * progress
    outlier <- rbinom(n_items, 1, outlier_prob)
    outlier_bump <- runif(n_items, 12, 45)
    
  } else {
    # Box side: generally faster than envelopes early, converges late
    # Early baseline ~10-12 sec, late baseline ~6 sec
    base <- 6 + 6 * progress^1.3
    
    # Moderate noise early, low noise late
    sd_vec <- 2 + 7 * progress
    
    # Lucky finds (rare)
    lucky_prob <- 0.04 + 0.02 * progress
    lucky <- rbinom(n_items, 1, lucky_prob)
    lucky_time <- runif(n_items, 0.8, 3.0)
    
    # Outliers (rare, mostly early)
    outlier_prob <- 0.015 * progress
    outlier <- rbinom(n_items, 1, outlier_prob)
    outlier_bump <- runif(n_items, 8, 30)
  }
  
  time <- rnorm(n_items, mean = base, sd = sd_vec)
  
  # Apply lucky finds
  time <- ifelse(lucky == 1, lucky_time, time)
  
  # Apply outlier bumps
  time <- time + outlier * outlier_bump
  
  # Last pick fastest about 9/10 times
  if (rbinom(1, 1, 0.90) == 1) {
    time[n_items] <- runif(1, 0.8, 2.2)
  }
  
  # Clamp floor
  time <- pmax(time, 0.6)
  
  tibble(
    position_in_side = pos,
    remaining_on_side = remaining,
    pick_time_sec = time
  )
}

# ---- Build dataset: bags x sides ----
sim_list <- lapply(1:n_bags, function(b) {
  env_n <- sample(min_items_per_side:max_items_per_side, 1)
  box_n <- sample(min_items_per_side:max_items_per_side, 1)
  
  env <- simulate_side_times("Envelope", env_n) %>%
    mutate(bag_id = paste0("Bag_", b), side = "Envelope")
  
  box <- simulate_side_times("Box", box_n) %>%
    mutate(bag_id = paste0("Bag_", b), side = "Box")
  
  bind_rows(env, box)
})

delivery_sim <- bind_rows(sim_list) %>%
  group_by(bag_id) %>%
  mutate(
    bag_density = n()
  ) %>%
  ungroup()

# ---- Create a true stop index within bag for tandem visualization ----
# stop_in_bag is defined per side (pick 1, pick 2, ... on that side)
delivery_sim <- delivery_sim %>%
  group_by(bag_id, side) %>%
  arrange(position_in_side, .by_group = TRUE) %>%
  mutate(stop_in_bag = row_number()) %>%
  ungroup()

# ---- Realism constraint: cap >60 sec picks to max_over_60_per_day ----
# Keep the worst ones biased toward Envelope early picks, clamp the rest.
over <- delivery_sim %>%
  mutate(row_id = row_number()) %>%
  filter(pick_time_sec > 60) %>%
  mutate(
    early_flag = stop_in_bag <= 5,
    priority = (side == "Envelope") * 2 + (early_flag) * 1
  ) %>%
  arrange(desc(priority), desc(pick_time_sec))

if (nrow(over) > max_over_60_per_day) {
  clamp_rows <- over$row_id[(max_over_60_per_day + 1):nrow(over)]
  delivery_sim$pick_time_sec[clamp_rows] <- runif(length(clamp_rows), 35, 60)
}

# ---- Create tandem-ordered day timeline index ----
# Interleave Envelope then Box at each stop number within each bag.
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
  mutate(day_pick_index = row_number())

# ---- Quick sanity checks ----
# 1) How many picks over 60 seconds?
sum(delivery_sim$pick_time_sec > 60)

# 2) Preview
head(delivery_sim)


#---line plot----------------------------------------

library(ggplot2)
library(dplyr)

df_plot <- delivery_sim %>% filter(!is.na(pick_time_sec))

ggplot() +
  # Envelope: light and thin
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
  
  # Box halo: white dashed line underlay
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
  
  # Box actual: colored fine dashed line on top
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
  
  labs(
    title = "AMAZON Delivery // pick time related to organization; 200 packages (full day)",
    subtitle = "Full day of 200 packages with time to find each; Two sides of van // sides organized by boxes and envelopes",
    x = "Pick Sequence Through Day",
    y = "Pick Time (Seconds)",
    color = "Bag"
  ) +
  
  # 🔹 Force square legend keys cleanly
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = 5,
        shape = 15,
        size = 5,
        alpha = 1
      )
    )
  ) +
  
  theme_minimal()



#=====scatter plot===============================

ggplot(
  delivery_sim %>% filter(!is.na(pick_time_sec)),
  aes(x = remaining_on_side, y = pick_time_sec, color = side)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9) +
  facet_wrap(~ bag_id, scales = "free_y") +
  scale_x_reverse() +
  labs(
    title = "AMAZON Delivery // pick time related to organization",
    subtitle = "Each panel is an individual bag on route; Two sides of van // sides organized by boxes and envelopes",
    x = "Remaining Packages on That Side",
    y = "Pick Time (Seconds)",
    color = "Side"
  ) +
  theme_minimal()

# line plot 2 ------------------------------------

library(ggplot2)
library(dplyr)

df_plot <- delivery_sim %>% filter(!is.na(pick_time_sec))

ggplot() +
  # Envelope first: thin + light
  geom_line(
    data = df_plot %>% filter(side == "Envelope"),
    aes(
      x = day_pick_index,
      y = pick_time_sec,
      group = interaction(bag_id, side),
      color = bag_id
    ),
    linewidth = 0.9,
    alpha = 0.35,
    linetype = "solid"
  ) +
  # Box second: thicker + on top + full opacity
  geom_line(
    data = df_plot %>% filter(side == "Box"),
    aes(
      x = day_pick_index,
      y = pick_time_sec,
      group = interaction(bag_id, side),
      color = bag_id
    ),
    linewidth = 1.6,
    alpha = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Pick Time Across the Day by Bag and Truck Side",
    subtitle = "Box is emphasized by drawing it thicker and on top",
    x = "Pick Sequence Through Day",
    y = "Pick Time (Seconds)",
    color = "Bag"
  ) +
  theme_minimal()



# line plot 3 ------------------------------------

library(ggplot2)
library(dplyr)

df_plot <- delivery_sim %>% filter(!is.na(pick_time_sec))

ggplot() +
  # Envelope: light and thin
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
  # Box halo: white dashed line underlay
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
  # Box actual: colored dashed line on top
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
  labs(
    title = "Pick Time Across the Day by Bag and Truck Side",
    subtitle = "Box is emphasized using a white outline (halo) under the dashed line",
    x = "Pick Sequence Through Day",
    y = "Pick Time (Seconds)",
    color = "Bag"
  ) +
  theme_minimal()



# line plot 4 ------------------------------------

library(ggplot2)
library(dplyr)

df_plot <- delivery_sim %>% filter(!is.na(pick_time_sec))

ggplot() +
  # Envelope: light
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
  # Box line: thicker
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
    linetype = "dashed"
  ) +
  # Box points: small markers to make the path pop
  geom_point(
    data = df_plot %>% filter(side == "Box"),
    aes(
      x = day_pick_index,
      y = pick_time_sec,
      color = bag_id
    ),
    size = 1.4,
    alpha = 1
  ) +
  labs(
    title = "Pick Time Across the Day by Bag and Truck Side",
    subtitle = "Box is emphasized using thicker line plus point markers",
    x = "Pick Sequence Through Day",
    y = "Pick Time (Seconds)",
    color = "Bag"
  ) +
  theme_minimal()

