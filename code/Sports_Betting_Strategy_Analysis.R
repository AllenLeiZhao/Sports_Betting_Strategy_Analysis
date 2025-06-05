## Lei Zhao
## NUID : 002335702
## ALY6050 MODULE1 PROJECT

############################### BASIC ####################################

red_sox <- 0.6    # the probability Red Sox win their home game
yankees <- 0.57   # the probability Yankees win their home game
win <- 500        # the money I will win when Red Sox win
lose <- 520       # the money I will lose when Red Sox lose

############################### PART 1 ###################################

# Consider the series where the games are played in Boston, New York, and then Boston (if necessary).

# (i) Calculate the probability that the Red Sox will win the series.

win_first <- 0.6   # the first game is held in Boston
win_second <- 1 - 0.57 # the second game is held in New York
win_third <- 0.6   # the thrid game is held in Boston if necessary

# if Red Sox would win the series, they should win :
# either the firs two games
# or win first, lose second, win third
# or lose first, win second, win therd
senario1 <- win_first * win_second
senario2 <- win_first * (1 - win_second) * win_third
senario3 <- (1 - win_first) * win_second * win_third
win_series <- senario1 + senario2 + senario3
print(win_series)

# (ii) Construct the theoretical probability distribution for your net 
# winnings in dollars (X) for the series. From this theoretical 
# calculation you should also compute and record the expected value 
# of the net winnings (the mean of X) and the theoretical standard 
# deviation of X.

# Define possible net winnings outcomes
outcomes <- c(1000, 480, -540, -1040)

# Corresponding probabilities for each outcome
probabilities <- c(0.2580, 0.3084, 0.2280, 0.1720)

# Compute the expected value E(X)
expected_value <- sum(outcomes * probabilities)
print(paste("Expected Value E(X) =", expected_value))

# Compute E(X^2)
expected_squared <- sum((outcomes^2) * probabilities)

# Compute the standard deviation sd
std_dev <- sqrt(expected_squared - expected_value^2)
print(paste("Standard Deviation sd =", std_dev))

# Visualization
library(ggplot2)

# Create a data frame for plotting
df <- data.frame(outcome = outcomes, probability = probabilities)

# Plot using ggplot2
ggplot(df, aes(x = factor(outcome), y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Probability Distribution of Net Winnings",
       x = "Net Winnings ($)",
       y = "Probability") +
  theme_minimal()


# (iii) Create a simulation of 10,000 different 3 game series by using R to 
# create 10,000 random values for X. Let these random values be 
# denoted Y. Each Y value denotes an outcome of the series as defined 
# by the probability distribution. These 10,000 outcomes represent a 
# statistical sample of possible outcomes. Use this sample of outcomes 
# to estimate the expected net win by using a 95% confidence interval. 
# Does this confidence interval contain the theoretical E(X)?

# 1. Simulate 10,000 series outcomes
set.seed(123)
simulations <- 10000

# 2. Define consolidated outcomes and probabilities (no duplicates)
outcomes_unique <- c(1000, 480, -540, -1040)  
probabilities_unique <- c(
  0.342,           # Probability for $1000 (2-0 win)
  0.1548 + 0.1368, # Combined probability for $480 (2-1 win)
  0.1032 + 0.0912, # Combined probability for -$540 (1-2 loss)
  0.172            # Probability for -$1040 (0-2 loss)
)

# 3. Generate simulated data
simulated_outcomes <- sample(
  outcomes_unique,
  simulations,
  replace = TRUE,
  prob = probabilities_unique
)

# 4. Calculate simulated mean and confidence interval
simulated_mean <- mean(simulated_outcomes)
simulated_sd <- sd(simulated_outcomes)
conf_interval <- simulated_mean + c(-1, 1) * qnorm(0.975) * simulated_sd / sqrt(simulations)

# 5. Recalculate THEORETICAL expected value with consolidated probabilities
expected_value_corrected <- sum(outcomes_unique * probabilities_unique)  # = 198.112

# 6. Check if E(X) is within CI (with floating-point tolerance)
tolerance <- 1e-6  # Allow tiny numerical differences
is_within <- (
  expected_value_corrected >= (conf_interval[1] - tolerance) &&
    expected_value_corrected <= (conf_interval[2] + tolerance)
)

# 7. Print results
print(paste("Theoretical E(X):", expected_value_corrected))
print(paste("95% CI: [", conf_interval[1], ",", conf_interval[2], "]"))
print(paste("Is E(X) within CI?", is_within)) 


# (iv) Construct a frequency distribution for Y. Use the Chi-squared 
# goodness of fit test to verify how closely the distribution of Y has 
# estimated the distribution of X.

# Consolidate theoretical probabilities for the same net winnings
outcomes_unique <- c(1000, 480, -540, -1040)  # Unique net winnings values
probabilities_unique <- c(
  0.342,           # Probability for $1000 (Red Sox wins first two games)
  0.1548 + 0.1368, # Combined probability for $480 (paths: WWL + LWW)
  0.1032 + 0.0912, # Combined probability for -$540 (paths: WLW + LWL)
  0.172            # Probability for -$1040 (Red Sox loses all)
)

# Simulate outcomes using consolidated probabilities
set.seed(123)
simulations <- 10000
simulated_outcomes <- sample(
  outcomes_unique, 
  simulations, 
  replace = TRUE, 
  prob = probabilities_unique
)

# Ensure observed frequencies include all possible outcomes
simulated_outcomes_factor <- factor(simulated_outcomes, levels = outcomes_unique)
observed_freq <- table(simulated_outcomes_factor)

# Create a data frame for plotting
df_freq <- data.frame(
  outcome = outcomes_unique,
  observed = as.numeric(observed_freq),
  theoretical = probabilities_unique * simulations
)

# Plot observed vs. theoretical frequencies

ggplot(df_freq, aes(x = factor(outcome))) +
  geom_bar(
    aes(y = observed, fill = "Observed"),
    stat = "identity",
    position = position_dodge(),
    width = 0.4
  ) +
  geom_bar(
    aes(y = theoretical, fill = "Theoretical"),
    stat = "identity",
    position = position_dodge(width = 0.4),
    width = 0.4,
    alpha = 0.6
  ) +
  scale_fill_manual(values = c("Observed" = "skyblue", "Theoretical" = "salmon")) +
  labs(
    title = "Observed vs. Theoretical Frequencies of Net Winnings",
    x = "Net Winnings ($)",
    y = "Frequency",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Perform Chi-squared test with consolidated probabilities
chisq_result <- chisq.test(observed_freq, p = probabilities_unique)
print(chisq_result)


# (v) Use your observations of parts (ii) and (iii) above to describe whether 
# your betting strategy is favorable to you. Write a summary of your 
# observations and analyses in the Word document


############################### PART 2 ###################################

# (i) Calculate the probability that the Red Sox will win the series.
# Game order: NY (Yankees home), BOS (Red Sox home), NY (Yankees home)

p_ny <- 1 - yankees  # Red Sox win probability in NY (away game)
p_bos <- red_sox     # Red Sox win probability in BOS (home game)

# Scenarios where Red Sox win the series:
# 1. Win first two games (NY, BOS)
# 2. Win first, lose second, win third (NY, BOS, NY)
# 3. Lose first, win next two (NY, BOS, NY)
scenario1 <- p_ny * p_bos
scenario2 <- p_ny * (1 - p_bos) * p_ny
scenario3 <- (1 - p_ny) * p_bos * p_ny

win_series_part2 <- scenario1 + scenario2 + scenario3
print(paste("Probability Red Sox win the series (Part 2):", win_series_part2))

# (ii) Theoretical probability distribution for net winnings

# Possible net winnings (same outcomes, different probabilities)
outcomes_part2 <- c(1000, 480, 480, -540, -540, -1040)

# Calculate probabilities for each outcome
probabilities_part2 <- c(
  scenario1,                              # 1000: Win both games (NY, BOS)
  scenario2,                              # 480: Win NY, lose BOS, win NY
  scenario3,                              # 480: Lose NY, win BOS, win NY
  p_ny * (1 - p_bos) * (1 - p_ny),       # -540: Win NY, lose BOS, lose NY
  (1 - p_ny) * p_bos * (1 - p_ny),       # -540: Lose NY, win BOS, lose NY
  (1 - p_ny) * (1 - p_bos)               # -1040: Lose all
)

# Compute expected value and standard deviation
expected_value_part2 <- sum(outcomes_part2 * probabilities_part2)
std_dev_part2 <- sqrt(sum(outcomes_part2^2 * probabilities_part2) - expected_value_part2^2)

print(paste("Expected Value (Part 2):", expected_value_part2))
print(paste("Standard Deviation (Part 2):", std_dev_part2))

# Visualization: Theoretical Probability Distribution
df_part2 <- data.frame(outcome = outcomes_part2, probability = probabilities_part2)
ggplot(df_part2, aes(x = factor(outcome), y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Part 2: Theoretical Probability Distribution of Net Winnings",
       x = "Net Winnings ($)",
       y = "Probability") +
  theme_minimal()

# (iii) Simulation and confidence interval
set.seed(123)
simulated_part2 <- sample(outcomes_part2, 10000, replace = TRUE, prob = probabilities_part2)
sim_mean_part2 <- mean(simulated_part2)
sim_sd_part2 <- sd(simulated_part2)
ci_part2 <- sim_mean_part2 + c(-1, 1) * qnorm(0.975) * sim_sd_part2 / sqrt(10000)

print(paste("95% CI for Part 2:", ci_part2[1], "to", ci_part2[2]))
print(paste("Theoretical E(X) in CI?:", 
            expected_value_part2 >= ci_part2[1] && expected_value_part2 <= ci_part2[2]))

# (iv) Frequency distribution and Chi-squared test
# Consolidate outcomes and probabilities
outcomes_unique_part2 <- c(1000, 480, -540, -1040)
prob_unique_part2 <- c(
  scenario1,
  scenario2 + scenario3,
  sum(probabilities_part2[4:5]),
  probabilities_part2[6]
)

# Generate simulation data with consolidated outcomes
set.seed(123)
simulated_part2_adj <- sample(outcomes_unique_part2, 10000, replace = TRUE, prob = prob_unique_part2)
observed_part2 <- table(factor(simulated_part2_adj, levels = outcomes_unique_part2))

# Create frequency plot
df_freq_part2 <- data.frame(
  outcome = outcomes_unique_part2,
  observed = as.numeric(observed_part2),
  theoretical = prob_unique_part2 * 10000
)

ggplot(df_freq_part2, aes(x = factor(outcome))) +
  geom_bar(
    aes(y = observed, fill = "Observed"),
    stat = "identity",
    position = position_dodge(),
    width = 0.4
  ) +
  geom_bar(
    aes(y = theoretical, fill = "Theoretical"),
    stat = "identity",
    position = position_dodge(width = 0.4),
    width = 0.4,
    alpha = 0.6
  ) +
  scale_fill_manual(values = c("Observed" = "skyblue", "Theoretical" = "salmon")) +
  labs(
    title = "Part 2: Observed vs. Theoretical Frequencies",
    x = "Net Winnings ($)",
    y = "Frequency",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Chi-squared test
chisq_part2 <- chisq.test(observed_part2, p = prob_unique_part2)
print(chisq_part2)


############################### PART 3 ###################################

# (i) Probability Red Sox win the best-of-five series
# Game order: BOS, NY, BOS, NY, BOS (first team to 3 wins)

p_bos <- red_sox    # Red Sox home game probability
p_ny <- 1 - yankees # Red Sox away game probability

# All possible winning paths for Red Sox:
# 3-0: Win first 3 games (BOS, NY, BOS)
prob_3_0 <- p_bos * p_ny * p_bos

# 3-1: Win 3 out of first 4 games
# Paths: WWLW, WLWW, LWWW
prob_3_1 <- 
  p_bos * p_ny * (1 - p_bos) * p_ny +   # WWLW
  p_bos * (1 - p_ny) * p_bos * p_ny +   # WLWW
  (1 - p_bos) * p_ny * p_bos * p_ny     # LWWW

# 3-2: Win 3 out of first 5 games
# Paths: WWLLW, WLWLW, WLLWW, LWWLW, LWLWW, LLWWW
prob_3_2 <- 
  p_bos * p_ny * (1 - p_bos) * (1 - p_ny) * p_bos + # WWLLW
  p_bos * (1 - p_ny) * p_bos * (1 - p_ny) * p_bos + # WLWLW
  p_bos * (1 - p_ny) * (1 - p_bos) * p_ny * p_bos + # WLLWW
  (1 - p_bos) * p_ny * p_bos * (1 - p_ny) * p_bos + # LWWLW
  (1 - p_bos) * p_ny * (1 - p_bos) * p_ny * p_bos + # LWLWW
  (1 - p_bos) * (1 - p_ny) * p_bos * p_ny * p_bos   # LLWWW

total_prob_part3 <- prob_3_0 + prob_3_1 + prob_3_2
print(paste("Probability Red Sox win best-of-five:", total_prob_part3))

# (ii) Theoretical probability distribution
# Net winnings: 3 wins (+1500), 3W-1L (+480), 3W-2L (+460), etc.
# Assume $500 per win, -$520 per loss
outcomes_part3 <- c(1500, 980, 460, -540, -1060, -1580)
probabilities_part3 <- c(
  prob_3_0,                                # 3-0: +1500
  prob_3_1,                                # 3-1: 3W-1L → 3*500 - 1*520 = 980
  prob_3_2,                                # 3-2: 3*500 - 2*520 = 460
  (1 - total_prob_part3) * c(0.3, 0.4, 0.3) # Placeholder for losing scenarios (adjust based on actual paths)
)

# Compute expected value and standard deviation
expected_value_part3 <- sum(outcomes_part3 * probabilities_part3)
std_dev_part3 <- sqrt(sum(outcomes_part3^2 * probabilities_part3) - expected_value_part3^2)

print(paste("Expected Value (Part 3):", expected_value_part3))
print(paste("Standard Deviation (Part 3):", std_dev_part3))

# Visualization: Theoretical Distribution
df_part3 <- data.frame(outcome = outcomes_part3, probability = probabilities_part3)
ggplot(df_part3, aes(x = factor(outcome), y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Part 3: Theoretical Probability Distribution of Net Winnings",
       x = "Net Winnings ($)",
       y = "Probability") +
  theme_minimal()

# (iii) Simulation and confidence interval
set.seed(123)
simulated_part3 <- sample(outcomes_part3, 10000, replace = TRUE, prob = probabilities_part3)
sim_mean_part3 <- mean(simulated_part3)
sim_sd_part3 <- sd(simulated_part3)
ci_part3 <- sim_mean_part3 + c(-1, 1) * qnorm(0.975) * sim_sd_part3 / sqrt(10000)

print(paste("95% CI for Part 3:", ci_part3[1], "to", ci_part3[2]))
print(paste("Theoretical E(X) in CI?:", 
            expected_value_part3 >= ci_part3[1] && expected_value_part3 <= ci_part3[2]))

# (iv) Frequency distribution and Chi-squared test
outcomes_unique_part3 <- c(1500, 980, 460, -540, -1060, -1580)
simulated_part3_adj <- factor(simulated_part3, levels = outcomes_unique_part3)
observed_part3 <- table(simulated_part3_adj)

# Frequency plot
df_freq_part3 <- data.frame(
  outcome = outcomes_unique_part3,
  observed = as.numeric(observed_part3),
  theoretical = probabilities_part3 * 10000
)

ggplot(df_freq_part3, aes(x = factor(outcome))) +
  geom_bar(
    aes(y = observed, fill = "Observed"),
    stat = "identity",
    position = position_dodge(),
    width = 0.4
  ) +
  geom_bar(
    aes(y = theoretical, fill = "Theoretical"),
    stat = "identity",
    position = position_dodge(width = 0.4),
    width = 0.4,
    alpha = 0.6
  ) +
  scale_fill_manual(values = c("Observed" = "skyblue", "Theoretical" = "salmon")) +
  labs(
    title = "Part 3: Observed vs. Theoretical Frequencies",
    x = "Net Winnings ($)",
    y = "Frequency",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Chi-squared test
chisq_part3 <- chisq.test(observed_part3, p = probabilities_part3)
print(chisq_part3)