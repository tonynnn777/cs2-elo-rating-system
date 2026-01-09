# Goal: Estimate the probability that a team wins a match based on match context
# (tournament importance and match format)

# Read match-level data from CSV
data <- read.csv("cs2_results.csv")

# Create a column identifying the winning team based on the match outcome label
data$winning_team <- ifelse(data$target == 1, data$team1, data$team2)


