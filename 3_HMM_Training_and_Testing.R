# Load necessary libraries
library(tidyverse)
library(depmixS4)

# Load and preprocess data
data <- read.csv("TermProjectData.txt")

# Convert date and time columns into a single POSIX datetime column
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S")

# Drop rows with missing values in critical columns
data <- na.omit(data)

# Filter data by date for training (first 3 years) and testing (4th year)
train_data <- data %>% filter(as.Date(DateTime) < as.Date("2009-01-01"))
test_data <- data %>% filter(as.Date(DateTime) >= as.Date("2009-01-01"))

# Select Monday from 2 PM to 5 PM
train_data <- train_data %>%
  filter(weekdays(DateTime) == "Monday" & format(DateTime, "%H") %in% c("14", "15", "16"))
test_data <- test_data %>%
  filter(weekdays(DateTime) == "Monday" & format(DateTime, "%H") %in% c("14", "15", "16"))

# Scale selected columns for consistency across features
scaled_train_data <- scale(train_data[ , c("Global_active_power", "Global_reactive_power", 
                                           "Voltage", "Global_intensity")])
scaled_test_data <- scale(test_data[ , c("Global_active_power", "Global_reactive_power", 
                                         "Voltage", "Global_intensity")])

# Prepare an empty data frame to store model results
results <- data.frame(States = integer(), LogLikelihood = numeric(), BIC = numeric())

# Train HMMs with 4 to 20 states and record log-likelihoods and BICs
for (n_states in seq(4, 20, by = 2)) {
  
  # Define the HMM model
  model <- depmix(list(Global_active_power ~ 1, Global_reactive_power ~ 1, 
                       Voltage ~ 1, Global_intensity ~ 1), 
                  data = as.data.frame(scaled_train_data), 
                  nstates = n_states, 
                  family = list(gaussian(), gaussian(), gaussian(), gaussian()))
  
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

# print the results for the best model
print(results)

# Plot log-likelihood and BIC for model selection
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



# Find the best model based on minimum BIC in the training phase
best_n_states <- results %>% filter(BIC == min(BIC)) %>% pull(States)

# Print the number of states selected
cat("Best model selected has", best_n_states, "states based on minimum BIC.\n")

# Fit the best model on the scaled training data again for reusability
best_model <- depmix(list(Global_active_power ~ 1, Global_reactive_power ~ 1, 
                          Voltage ~ 1, Global_intensity ~ 1), 
                     data = as.data.frame(scaled_train_data), 
                     nstates = best_n_states, 
                     family = list(gaussian(), gaussian(), gaussian(), gaussian()))

# Fit the model on training data
fit_best_model <- fit(best_model, verbose = FALSE)


# Get the log-likelihood for the training data
training_log_likelihood <- logLik(fit_best_model)

# Normalize log-likelihood by the number of observations in the training data
normalized_training_log_likelihood <- as.numeric(training_log_likelihood) / nrow(scaled_train_data)

# Display normalized log-likelihood for the training data
cat("Normalized Log-Likelihood for the training data:", normalized_training_log_likelihood, "\n")


# Predict the log-likelihood on the test data by refitting the model
# Prepare the test data in the required format
test_data_model <- as.data.frame(scaled_test_data)

# Refit the model using the same structure on test data
best_model_test <- depmix(list(Global_active_power ~ 1, Global_reactive_power ~ 1, 
                               Voltage ~ 1, Global_intensity ~ 1), 
                          data = test_data_model, 
                          nstates = best_n_states, 
                          family = list(gaussian(), gaussian(), gaussian(), gaussian()))

# Fit the model on test data
fit_best_model_test <- fit(best_model_test, verbose = FALSE)

# Get the log-likelihood for the test data
test_log_likelihood <- logLik(fit_best_model_test)

# Normalize log-likelihood by the number of observations in the test data
normalized_test_log_likelihood <- as.numeric(test_log_likelihood) / nrow(test_data_model)

# Display normalized log-likelihood for the test data
cat("Normalized Log-Likelihood for the test data:", normalized_test_log_likelihood, "\n")


