# Load necessary library
install.packages("tidyverse")
library(tidyverse)

set.seed(42)  # For reproducibility

# Simulate data
n <- 1000  # Number of buildings

# Predictor 1: Year Built
year_built <- sample(1700:2022, n, replace = TRUE)

# Predictor 2: Location Zone (1 = Commercial, 2 = Residential, 3 = Industrial)
location_zone <- sample(1:3, n, replace = TRUE, prob = c(0.3, 0.5, 0.2))

# Predictor 3: Building Usage (1 = Residential, 2 = Office, 3 = Commercial)
building_usage <- sample(1:3, n, replace = TRUE)

# Simulate Number of Floors based on predictors
# Assuming linear relationships and adding random noise
floors <- 2 + (year_built - 1800) * 0.01 + location_zone * 0.5 + building_usage * 0.3 + rnorm(n)

# Create a dataframe
data <- tibble(YearBuilt = year_built,
               LocationZone = factor(location_zone, labels = c("Commercial", "Residential", "Industrial")),
               BuildingUsage = factor(building_usage, labels = c("Residential", "Office", "Commercial")),
               Floors = floors)

# Linear model
model <- lm(Floors ~ YearBuilt + LocationZone + BuildingUsage, data = data)

# Summary of the model
summary(model)

# Perform some diagnostics
par(mfrow=c(2,2))
plot(model)

# Reset par
par(mfrow=c(1,1))

# Predictions for 5 test cases
new_data <- data[sample(nrow(data), 5), ]  # Randomly select 10 buildings from the dataset
predictions <- predict(model, new_data)
predictions


install.packages("ggplot2")
# Load necessary libraries
library(ggplot2)
install.packages("rstanarm")
library(rstanarm)

# Assuming 'data' is your DataFrame containing the 'YearBuilt' and 'Floors' variables

# Step 1: Create the graph with ggplot2
ggplot(data, aes(x = YearBuilt, y = Floors)) +
  geom_point(alpha = 0.6, color = "skyblue") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(title = "Building Heights in London by Construction Year",
       x = "Year Built",
       y = "Number of Floors")

# Step 2: Build the model with rstanarm
# Bayesian linear regression with 'Floors' as the outcome and 'YearBuilt' as the predictor
model <- stan_glm(Floors ~ YearBuilt, data = data, family = gaussian(), prior = normal(0, 2.5), seed = 123)

# Print the summary of the model
print(summary(model))

# Note: The prior here is set to a normal distribution with mean 0 and sd 2.5 as a generic weakly informative prior.
# You might need to adjust this based on your domain knowledge and the scale of your variables.
# Load necessary library
library(tidyverse)


set.seed(123)  # For reproducibility

# Number of observations
n <- 1000

# Simulate independent variables with clearer labels
race <- sample(c("Race_A", "Race_B"), n, replace = TRUE)
gender <- sample(c("Male", "Female"), n, replace = TRUE)

# Convert race and gender to numerical values for the simulation
# Here, we assume Race_A and Male have slight positive effects on voting for Candidate X
race_effect <- ifelse(race == "Race_A", 0.2, -0.1)  # More nuanced effect
gender_effect <- ifelse(gender == "Male", 0.1, -0.05)  # More nuanced effect

# Baseline log odds of voting for Candidate X, reflecting a starting preference level in the population
baseline_log_odds <- -0.3

# Introduce random noise to reflect the imperfect relationship
random_noise <- rnorm(n, mean = 0, sd = 0.5)

# Calculate log odds of vote preference and simulate the outcome
log_odds_vote <- baseline_log_odds + race_effect + gender_effect + random_noise
vote_preference <- rbinom(n, 1, prob = plogis(log_odds_vote))

# Combine into a data frame
data <- tibble(Race = race, Gender = gender, VotePreference = vote_preference)

# View the first few rows of the data frame
head(data)

# Fit a logistic regression model to the data
model <- glm(VotePreference ~ Race + Gender, data = data, family = "binomial")

# Print a summary of the model to see the effects
summary(model)



