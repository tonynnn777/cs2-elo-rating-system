**CS2 Match Outcome Prediction with Elo Overview**

This project builds an Elo-based rating system to estimate the probability that one CS2 team defeats another. The model produces pre-match win probabilities, incorporates basic match context, and is evaluated using log loss and calibration curves with a time-based train/test split.

The focus is on generating reasonable, well-calibrated probabilities, not just team rankings.

*Methodology*

Teams are initialized with an Elo rating of 1500 and updated chronologically using match outcomes. Win probabilities follow the standard Elo formulation:

P(team1 wins) = 1 / (1 + 10\^((R2 − R1) / s))

where the scale parameter is tuned to s = 450.

Match context affects how strongly ratings update, not the probability formula itself:

Match format (bo1 / bo3 / bo5) increases update size for longer series

Tournament importance (0–5) scales update strength smoothly

*Evaluation*

Model performance is assessed using:

Log loss

Calibration curves (predicted probability vs observed win rate)

To reflect real-world usage, matches are split chronologically:

Training: matches before 2024-08-01

Testing: matches on or after 2024-08-01

Predictions are always made before observing match outcomes, and ratings are updated afterward.

*Results*

Full sample log loss: \~0.656 Out-of-sample test log loss: \~0.642

The model generalizes well to future matches and produces reasonably calibrated probabilities, particularly in the mid-probability range.

*Usage*

After training, the model can be used to predict matchups directly:

predict_match("Natus Vincere", "G2")

This returns the current Elo ratings for both teams and the estimated probability that each team wins.
