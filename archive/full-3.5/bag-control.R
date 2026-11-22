# BAG CONTROL

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

set.seed(310)

# -------------------------------------------------
# helper 1: split daily package total across bags
# -------------------------------------------------
split_packages_across_bags <- function(total_packages, n_bags, day_type) {
  
  if (day_type %in% c("heavy", "peak")) {
    weights <- sort(runif(n_bags, 0.85, 1.25), decreasing = TRUE)
  } else if (day_type == "light") {
    weights <- sort(runif(n_bags, 0.90, 1.10), decreasing = TRUE)
  } else {
    weights <- sort(runif(n_bags, 0.88, 1.15), decreasing = TRUE)
  }
  
  raw_alloc <- total_packages * weights / sum(weights)
  bag_packages <- floor(raw_alloc)
  
  remainder <- total_packages - sum(bag_packages)
  
  if (remainder > 0) {
    frac_order <- order(raw_alloc - bag_packages, decreasing = TRUE)
    bag_packages[frac_order[1:remainder]] <- bag_packages[frac_order[1:remainder]] + 1
  }
  
  bag_packages
}

# -------------------------------------------------
# helper 2: convert scoring to default bag difficulty
# -------------------------------------------------
score_to_bag_difficulty <- function(score) {
  case_when(
    score <= 0 ~ "easy",
    score == 1 ~ "medium",
    score >= 2 ~ "hard"
  )
}

# -------------------------------------------------
# build bag control table
# -------------------------------------------------
bag_controls <- daily_controls %>%
  mutate(
    bag_packages = pmap(
      list(total_packages, n_bags, day_type),
      split_packages_across_bags
    )
  ) %>%
  unnest_longer(
    bag_packages,
    indices_to = "bag_order",
    values_to = "bag_packages"
  ) %>%
  group_by(day_num) %>%
  mutate(
    bag_id = sprintf("D%02d_B%02d", day_num, bag_order),
    
    bag_stage = case_when(
      bag_order <= ceiling(n_bags / 3) ~ "early",
      bag_order <= ceiling(2 * n_bags / 3) ~ "middle",
      TRUE ~ "late"
    ),
    
    avg_bag_size_day = total_packages / n_bags,
    
    bag_density_class = case_when(
      bag_packages >= avg_bag_size_day + 2 ~ "dense",
      bag_packages <= avg_bag_size_day - 2 ~ "light",
      TRUE ~ "medium"
    ),
    
    difficulty_score =
      case_when(
        route_difficulty == "hard" ~ 2,
        route_difficulty == "medium" ~ 1,
        route_difficulty == "easy" ~ 0
      ) +
      case_when(
        organization_quality == "low" ~ 1,
        organization_quality == "medium" ~ 0,
        organization_quality == "high" ~ -1
      ) +
      case_when(
        bag_density_class == "dense" ~ 1,
        bag_density_class == "medium" ~ 0,
        bag_density_class == "light" ~ -1
      ) +
      case_when(
        bag_stage == "early" ~ 1,
        bag_stage == "middle" ~ 0,
        bag_stage == "late" ~ -1
      ) +
      case_when(
        month_phase == "volatile_start" ~ sample(c(-1, 0, 1), n(), replace = TRUE, prob = c(0.20, 0.45, 0.35)),
        month_phase == "transition" ~ sample(c(-1, 0, 1), n(), replace = TRUE, prob = c(0.20, 0.55, 0.25)),
        month_phase == "stable_finish" ~ sample(c(-1, 0, 1), n(), replace = TRUE, prob = c(0.30, 0.60, 0.10))
      ),
    
    bag_difficulty = score_to_bag_difficulty(difficulty_score),
    
    envelope_share = case_when(
      bag_difficulty == "easy" ~ 0.68,
      bag_difficulty == "medium" ~ 0.60,
      bag_difficulty == "hard" ~ 0.52
    ),
    
    box_share = 1 - envelope_share,
    
    bag_note = case_when(
      bag_difficulty == "hard" & bag_stage == "early" ~ "dense early bag",
      bag_difficulty == "hard" ~ "difficult bag",
      bag_difficulty == "medium" ~ "standard bag",
      TRUE ~ "smooth bag"
    )
  ) %>%
  ungroup() %>%
  dplyr::select(
    day_date,
    day_num,
    month_phase,
    day_type,
    route_difficulty,
    organization_quality,
    total_packages,
    n_bags,
    bag_order,
    bag_id,
    bag_stage,
    bag_packages,
    bag_density_class,
    bag_difficulty,
    envelope_share,
    box_share,
    bag_note,
    notes
  )

# =========== AFTER BAG CONTROLS

set.seed(311)

bag_controls <- bag_controls %>%
  group_by(day_num) %>%
  mutate(
    bag_exception = case_when(
      month_phase == "volatile_start" & runif(n()) < 0.18 ~ sample(
        c("late_disruption", "middle_smooth", "early_recovery"),
        n(), replace = TRUE
      ),
      month_phase == "transition" & runif(n()) < 0.12 ~ sample(
        c("late_disruption", "middle_smooth"),
        n(), replace = TRUE
      ),
      month_phase == "stable_finish" & runif(n()) < 0.08 ~ sample(
        c("late_disruption", "middle_smooth"),
        n(), replace = TRUE
      ),
      TRUE ~ "none"
    ),
    
    bag_difficulty = case_when(
      bag_exception == "late_disruption" & bag_stage == "late" ~ "hard",
      bag_exception == "middle_smooth" & bag_stage == "middle" ~ "easy",
      bag_exception == "early_recovery" & bag_stage == "early" ~ "medium",
      TRUE ~ bag_difficulty
    ),
    
    bag_note = case_when(
      bag_exception == "late_disruption" & bag_stage == "late" ~ "late bag disruption",
      bag_exception == "middle_smooth" & bag_stage == "middle" ~ "unexpectedly smooth middle bag",
      bag_exception == "early_recovery" & bag_stage == "early" ~ "better-than-expected early bag",
      TRUE ~ bag_note
    ),
    
    envelope_share = case_when(
      bag_difficulty == "easy" ~ 0.68,
      bag_difficulty == "medium" ~ 0.60,
      bag_difficulty == "hard" ~ 0.52
    ),
    
    box_share = 1 - envelope_share
  ) %>%
  ungroup()

# ============= BAG CONTROLS PT3

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

set.seed(312)

# -------------------------------------------------
# helper: only return package-level columns
# -------------------------------------------------
expand_bag_to_packages <- function(bag_packages, envelope_share) {
  
  n_env <- round(bag_packages * envelope_share)
  n_box <- bag_packages - n_env
  
  package_types <- c(rep("Envelope", n_env), rep("Box", n_box))
  package_types <- sample(package_types, length(package_types), replace = FALSE)
  
  tibble(
    package_in_bag = seq_len(bag_packages),
    remaining_in_bag = rev(seq_len(bag_packages)),
    package_type = package_types
  )
}

# -------------------------------------------------
# build package-level structure
# -------------------------------------------------
delivery_structure <- bag_controls %>%
  mutate(
    package_rows = purrr::map2(
      bag_packages,
      envelope_share,
      expand_bag_to_packages
    )
  ) %>%
  tidyr::unnest(package_rows) %>%
  dplyr::select(-envelope_share, -box_share) %>%
  dplyr::group_by(day_num) %>%
  dplyr::mutate(
    day_pick_index = dplyr::row_number(),
    package_id = paste0(bag_id, "_P", sprintf("%03d", package_in_bag))
  ) %>%
  dplyr::ungroup()