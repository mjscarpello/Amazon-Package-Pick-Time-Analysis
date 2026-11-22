ggplot(df_real, aes(x = pick_time_sec)) +
  geom_histogram(binwidth = 1, fill = "#4C78A8", color = "white") +
  labs(
    title = "Observed Pick Time Distribution",
    x = "Pick Time (seconds)",
    y = "Frequency"
  )

set.seed(32)

n_sim <- 250

df_sim <- tibble(
  pick_time_sec = rgamma(
    n_sim,
    shape = shape,
    rate = rate
  )
)

# =============

ggplot() +
  geom_density(
    data = df_real,
    aes(pick_time_sec),
    fill = "red",
    alpha = .3
  ) +
  geom_density(
    data = df_sim,
    aes(pick_time_sec),
    fill = "blue",
    alpha = .3
  ) +
  labs(
    title = "Real vs Simulated Pick Time Distributions"
  )
# ==========================

daily_controls <- daily_controls %>%
  mutate(
    route_difficulty = c(
      "medium","medium","medium","hard","medium","medium","hard","hard","hard","easy",
      "medium","medium","hard","hard","easy","medium","medium","hard","easy","medium",
      "medium","hard","hard","medium","easy","medium","hard","medium","easy","medium","hard"
    ),
    organization_quality = c(
      "medium","low","medium","low","low","medium","medium","low","medium","high",
      "medium","medium","medium","medium","high","medium","high","medium","high","high",
      "high","high","high","high","high","high","high","high","high","high","high"
    ),
    notes = c(
      "normal start, still settling into system",
      "light day but awkward flow and inconsistency",
      "steady workflow with some variability",
      "heavy volume exposed weak organization",
      "lighter route but unexpected inefficiency",
      "standard day, still somewhat uneven",
      "normal volume but harder bag rhythm",
      "heavy route with unstable organization",
      "heavy route, slight recovery compared with earlier days",
      "smoother day with stronger control",
      "light route but not fully stable",
      "standard day with moderate consistency",
      "dense stops, handled better than earlier heavy days",
      "peak volume, but more controlled than first half",
      "normal day with clear organization",
      "lighter route with a few disruptions",
      "steady workflow and better rhythm",
      "dense route, but recovery is improving",
      "standard day with strong control",
      "lighter day with good consistency",
      "stable workflow",
      "heavy volume managed with stronger organization",
      "peak route handled with stability",
      "normal flow and strong routine",
      "light route with efficient search rhythm",
      "steady workflow",
      "dense day, but system remains controlled",
      "standard day with consistent organization",
      "light route and strong rhythm",
      "stable normal day",
      "heavy route finished with strong organization"
    )
  )

