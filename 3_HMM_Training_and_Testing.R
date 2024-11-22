# Load necessary libraries
library(tidyverse)
library(depmixS4)
library(arules)

# Load and preprocess data
data <- read.csv("ProjectData_Processed.txt")

# Combine Date and Time columns into a single POSIX datetime column
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

# Drop rows with missing or invalid values in critical columns
data <- data %>%
  filter(!is.na(Global_intensity) & !is.na(Global_reactive_power))

# Split data into training (first 3 years) and testing (4th year)
train_data <- data %>% filter(as.Date(DateTime) < as.Date("2009-01-01"))
test_data <- data %>% filter(as.Date(DateTime) >= as.Date("2009-01-01"))

# Verify non-empty datasets
if (nrow(train_data) == 0 | nrow(test_data) == 0) {
  stop("Filtered datasets are empty. Check your date range or filters.")
}

# Select data for Monday between 2 PM and 5 PM
train_data <- train_data %>%
  filter(weekdays(DateTime) == "Monday" & format(DateTime, "%H") %in% c("14", "15", "16"))
test_data <- test_data %>%
  filter(weekdays(DateTime) == "Monday" & format(DateTime, "%H") %in% c("14", "15", "16"))

# Verify non-empty subsets
if (nrow(train_data) == 0 | nrow(test_data) == 0) {
  stop("No data available for the specified weekday and time range. Adjust the filters.")
}

# Discretize continuous variables into 4 bins
discretize_variable <- function(column) {
  if (length(unique(column)) <= 1) {
    stop("Column has insufficient unique values for discretization.")
  }
  discretize(column, method = "interval", breaks = 4, labels = FALSE)
}

# Apply discretization to the training and testing datasets
train_data <- train_data %>%
  mutate(Global_intensity = discretize_variable(Global_intensity),
         Global_reactive_power = discretize_variable(Global_reactive_power))

test_data <- test_data %>%
  mutate(Global_intensity = discretize_variable(Global_intensity),
         Global_reactive_power = discretize_variable(Global_reactive_power))

# Initialize results dataframe
results <- data.frame(States = integer(), LogLikelihood = numeric(), BIC = numeric())

# Get the number of rows for ntimes
ntimes_train <- nrow(train_data)
ntimes_test <- nrow(test_data)

# Train HMMs with 4 to 20 states (step of 4) and record log-likelihoods and BICs
for (n_states in seq(4, 20, by = 4)) {
  
  # Define HMM model with ntimes
  model <- depmix(list(Global_intensity ~ 1, Global_reactive_power ~ 1), 
                  data = train_data, 
                  nstates = n_states, 
                  family = list(multinomial("identity"), multinomial("identity")), 
                  ntimes = ntimes_train)
  
  # Fit the model
  fitModel <- fit(model, verbose = FALSE)
  
  # Extract log-likelihood and BIC
  log_likelihood <- logLik(fitModel)
  bic_value <- BIC(fitModel)
  
  # Store results
  results <- rbind(results, data.frame(States = n_states,
                                       LogLikelihood = as.numeric(log_likelihood),
                                       BIC = as.numeric(bic_value)))
}

# Print results
print(results)

# Plot results
# Log-Likelihood Plot
ggplot(results, aes(x = States, y = LogLikelihood)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Log-Likelihood vs Number of States",
       x = "Number of States",
       y = "Log-Likelihood") +
  theme_minimal()

# BIC Plot
ggplot(results, aes(x = States, y = BIC)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "BIC vs Number of States",
       x = "Number of States",
       y = "BIC") +
  theme_minimal()

# Determine the best model based on minimum BIC
best_n_states <- results %>% filter(BIC == min(BIC)) %>% pull(States)
cat("Best model selected has", best_n_states, "states based on minimum BIC.\n")

# Fit the best model on the training data
best_model <- depmix(list(Global_intensity ~ 1, Global_reactive_power ~ 1), 
                     data = train_data, 
                     nstates = best_n_states, 
                     family = list(multinomial("identity"), multinomial("identity")), 
                     ntimes = ntimes_train)

fit_best_model <- fit(best_model, verbose = FALSE)

# Get normalized log-likelihood for training data
normalized_training_log_likelihood <- as.numeric(logLik(fit_best_model)) / ntimes_train
cat("Normalized Log-Likelihood for the training data:", normalized_training_log_likelihood, "\n")

# Evaluate the model on test data
best_model_test <- depmix(list(Global_intensity ~ 1, Global_reactive_power ~ 1), 
                          data = test_data, 
                          nstates = best_n_states, 
                          family = list(multinomial("identity"), multinomial("identity")), 
                          ntimes = ntimes_test)

fit_best_model_test <- fit(best_model_test, verbose = FALSE)

# Get normalized log-likelihood for test data
normalized_test_log_likelihood <- as.numeric(logLik(fit_best_model_test)) / ntimes_test
cat("Normalized Log-Likelihood for the test data:", normalized_test_log_likelihood, "\n")
