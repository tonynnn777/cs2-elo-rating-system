# Goal: Estimate the probability that a team wins a match based on match context
# (tournament importance and match format)
# Given (team1, team2) and pre-match context
# Predict P(team1 wins the match)
# Elo system: https://en.wikipedia.org/wiki/Elo_rating_system

# -----------------------------
# Load and prepare match data
# -----------------------------

# Read match-level data
data <- read.csv("cs2_results.csv")

# -----------------------------
# Clean and parse match date
# -----------------------------

# Remove leading text from time column
clean_time <- sub("^Results for\\s+", "", data$time)

# Remove ordinal suffixes (e.g., 31st -> 31)
clean_time <- gsub("([0-9]{1,2})(st|nd|rd|th)", "\\1", clean_time)

# Convert to Date
data$match_date <- as.Date(clean_time, format = "%B %d %Y")

# Sort matches chronologically
data <- data[order(data$match_date), ]
row.names(data) <- NULL

# -----------------------------
# Helper functions
# -----------------------------

# Normalize match format:
# bo3 / bo5 kept as-is, map names treated as bo1
normalize_format <- function(shape) {
  s <- tolower(trimws(as.character(shape)))
  ifelse(s %in% c("bo3", "bo5"), s, "bo1")
}

# Match format weight (sqrt rule)
# bo1 = 1
# bo3 = sqrt(3)
# bo5 = sqrt(5)
format_weight <- function(fmt) {
  fmt <- normalize_format(fmt)
  ifelse(fmt == "bo5", sqrt(5),
         ifelse(fmt == "bo3", sqrt(3), 1.0))
}

# Tournament importance weight:
# stars in [0, 5] mapped to [0.8, 1.6]
importance_weight <- function(stars) {
  stars <- as.numeric(stars)
  0.80 + 0.80 * (stars / 5)
}

# Elo win probability (base-10 logistic, scale = s)
elo_prob <- function(r1, r2, s) {
  1 / (1 + 10 ^ ((r2 - r1) / s))
}

# Pre-compute normalized match format
data$format <- normalize_format(data$shape)

# =============================
# Full-sample Elo (diagnostics)
# =============================

K <- 32
initial_elo <- 1500
s <- 450
ratings <- new.env(parent = emptyenv())

# Get current rating for a team (initialize if unseen)
get_rating <- function(team) {
  key <- as.character(team)[1]
  if (!exists(key, envir = ratings, inherits = FALSE)) {
    assign(key, initial_elo, envir = ratings)
  }
  get(key, envir = ratings, inherits = FALSE)
}

# Set updated rating for a team
set_rating <- function(team, value) {
  key <- as.character(team)[1]
  assign(key, value, envir = ratings)
}

# Storage for per-match outputs
n <- nrow(data)
pred_p <- numeric(n)
k_eff  <- numeric(n)
r1_pre <- numeric(n)
r2_pre <- numeric(n)

# -----------------------------
# Elo loop (chronological)
# -----------------------------

for (i in seq_len(n)) {
  t1 <- data$team1[i]
  t2 <- data$team2[i]
  y  <- data$target[i]  # 1 if team1 wins, else 0
  
  # Pre-match ratings
  r1 <- get_rating(t1)
  r2 <- get_rating(t2)
  
  # Pre-match win probability
  p <- elo_prob(r1, r2, s)
  
  # Context-adjusted K (format + tournament importance)
  wf <- format_weight(data$format[i])
  wt <- importance_weight(data$stars_of_tournament[i])
  K_eff <- min(K * wf * wt, 3 * K)
  
  # Elo update
  delta <- K_eff * (y - p)
  set_rating(t1, r1 + delta)
  set_rating(t2, r2 - delta)
  
  # Store pre-match outputs
  pred_p[i] <- p
  k_eff[i]  <- K_eff
  r1_pre[i] <- r1
  r2_pre[i] <- r2
}

# -----------------------------
# Attach results to dataset
# -----------------------------

data$elo_p_team1_win <- pred_p
data$K_eff           <- k_eff
data$elo_team1_pre   <- r1_pre
data$elo_team2_pre   <- r2_pre

# -----------------------------
# Log loss evaluation
# -----------------------------

# Clip probabilities for numerical stability
eps <- 1e-15
p_clip <- pmin(pmax(data$elo_p_team1_win, eps), 1 - eps)

# Per-match log loss
data$logloss <- -(data$target * log(p_clip) +
                    (1 - data$target) * log(1 - p_clip))

# Mean log loss
mean_logloss <- mean(data$logloss, na.rm = TRUE)

# -----------------------------
# Calibration plot (full sample)
# -----------------------------

# Bin predicted probabilities and compare to observed win rate
n_bins <- 10
bins <- cut(
  data$elo_p_team1_win,
  breaks = seq(0, 1, length.out = n_bins + 1),
  include.lowest = TRUE
)

calib <- aggregate(
  cbind(pred = data$elo_p_team1_win, y = data$target),
  by = list(bin = bins),
  FUN = mean
)

plot(
  calib$pred, calib$y,
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "Predicted win probability",
  ylab = "Observed win rate",
  main = "Calibration Curve (Elo)",
  pch = 19
)
abline(0, 1, lty = 2)

# =============================
# Time-based train/test split
# =============================

# Split data by date (train is before split_date, test is on/after split_date)
split_date <- as.Date("2024-08-01")

train_data <- subset(data, match_date < split_date)
test_data  <- subset(data, match_date >= split_date)

# Reset ratings so train/test is clean (no leakage from full-sample run)
ratings <- new.env(parent = emptyenv())

# -----------------------------
# Train phase (build ratings)
# -----------------------------

for (i in seq_len(nrow(train_data))) {
  t1 <- train_data$team1[i]
  t2 <- train_data$team2[i]
  y  <- train_data$target[i]  # 1 if team1 wins, else 0
  
  # Pre-match ratings
  r1 <- get_rating(t1)
  r2 <- get_rating(t2)
  
  # Pre-match win probability
  p <- elo_prob(r1, r2, s)
  
  # Context-adjusted K
  wf <- format_weight(train_data$format[i])
  wt <- importance_weight(train_data$stars_of_tournament[i])
  K_eff <- min(K * wf * wt, 3 * K)
  
  # Elo update
  delta <- K_eff * (y - p)
  set_rating(t1, r1 + delta)
  set_rating(t2, r2 - delta)
}

# -----------------------------
# Test phase (predict -> score -> update)
# -----------------------------

n_test <- nrow(test_data)
test_pred <- numeric(n_test)

for (i in seq_len(n_test)) {
  t1 <- test_data$team1[i]
  t2 <- test_data$team2[i]
  y  <- test_data$target[i]
  
  # Pre-match ratings
  r1 <- get_rating(t1)
  r2 <- get_rating(t2)
  
  # Pre-match prediction (this is what gets evaluated)
  p <- elo_prob(r1, r2, s)
  test_pred[i] <- p
  
  # Update ratings after observing outcome
  wf <- format_weight(test_data$format[i])
  wt <- importance_weight(test_data$stars_of_tournament[i])
  K_eff <- min(K * wf * wt, 3 * K)
  
  delta <- K_eff * (y - p)
  set_rating(t1, r1 + delta)
  set_rating(t2, r2 - delta)
}

# -----------------------------
# Test log loss
# -----------------------------

eps <- 1e-15
p_clip <- pmin(pmax(test_pred, eps), 1 - eps)

test_logloss <- -(test_data$target * log(p_clip) +
                    (1 - test_data$target) * log(1 - p_clip))

mean_test_logloss <- mean(test_logloss, na.rm = TRUE)
mean_test_logloss

# -----------------------------
# Calibration plot (test set)
# -----------------------------

n_bins <- 10
bins <- cut(
  test_pred,
  breaks = seq(0, 1, length.out = n_bins + 1),
  include.lowest = TRUE
)

calib_test <- aggregate(
  cbind(pred = test_pred, y = test_data$target),
  by = list(bin = bins),
  FUN = mean
)

plot(
  calib_test$pred, calib_test$y,
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "Predicted win probability (test)",
  ylab = "Observed win rate (test)",
  main = "Test-Set Calibration Curve (Elo)",
  pch = 19
)
abline(0, 1, lty = 2)

# =============================
# Simple matchup prediction
# =============================

# Predict P(team1 wins) using current ratings state
predict_match <- function(team1, team2) {
  r1 <- get_rating(team1)
  r2 <- get_rating(team2)
  p <- 1 / (1 + 10 ^ ((r2 - r1) / s))
  
  list(
    team1 = team1,
    team2 = team2,
    rating_team1 = r1,
    rating_team2 = r2,
    prob_team1_win = p,
    prob_team2_win = 1 - p
  )
}

predict_match("Natus Vincere", "G2")
