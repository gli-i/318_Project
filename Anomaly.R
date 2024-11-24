# Load necessary libraries
library(tidyverse)
library(depmixS4)
library(arules)  # For discretizing continuous variables

# 1. Load and preprocess the data
data <- read.csv("ProjectData_Processed.txt")

# Convert Date and Time columns into a single POSIX datetime column (if needed for later use)
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

# Ensure the Date column is a Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Filter data by date for training (first 3 years) and testing (4th year)
train_data <- data %>% filter(Date < as.Date("2009-01-01"))
test_data <- data %>% filter(Date >= as.Date("2009-01-01"))

# 2. Select Monday data from 12 AM to 11:59 PM (full day)
train_data <- train_data %>%
    filter(weekdays(Date) == "Monday")

# 3. Function to discretize continuous variables into 5 bins
discretize_variable <- function(column) {
    arules::discretize(column, method = "interval", breaks = 5, labels = FALSE)
}

# Apply discretization to training and testing data
train_data <- train_data %>%
    mutate(Global_intensity = discretize_variable(Global_intensity),
           Global_reactive_power = discretize_variable(Global_reactive_power))

test_data <- test_data %>%
    mutate(Global_intensity = discretize_variable(Global_intensity),
           Global_reactive_power = discretize_variable(Global_reactive_power))

# 4. Get the number of rows for ntimes
ntimes_train <- nrow(train_data)
ntimes_test <- nrow(test_data)

# 5. Create subsets from the test data
# Subset 1: From 2009-01-01 to 2009-01-25
subset1 <- test_data %>% filter(Date >= as.Date("2009-01-01") & Date <= as.Date("2009-01-25"))

# Subsets 2 to 9: 5 consecutive weeks (35 days) each
subsets_list <- list()
start_date_next_subset <- as.Date("2009-01-26")  # The start date for the second subset

for (i in 1:8) {
    end_date <- start_date_next_subset + days(34)  # 35 days from the start date
    subsets_list[[i]] <- test_data %>% filter(Date >= start_date_next_subset & Date <= end_date)
    start_date_next_subset <- end_date + days(1)  # Update start date for the next subset
}

# Subset 10: Remaining data
remaining_data <- test_data %>% filter(Date > max(subsets_list[[8]]$Date))

# Combine all subsets into a single list
test_data_split <- c(list(subset1), subsets_list, list(remaining_data))

# 6. Write subsets to CSV files
output_directory <- "test_data_subsets"
dir.create(output_directory, showWarnings = FALSE)

for (i in 1:length(test_data_split)) {
    subset_data <- test_data_split[[i]]
    subset_data$subset <- i  # Add the subset identifier column
    file_name <- paste0(output_directory, "/subset_", i, ".csv")
    write.csv(subset_data, file = file_name, row.names = FALSE)
}

cat("Subsets saved to CSV in directory: ", output_directory, "\n")

# 7. Train the 12-state HMM model on the training data using ntimes
best_n_states <- 12  # From previous work, we already know 12 states is the best

best_model <- depmix(list(Global_intensity ~ 1, Global_reactive_power ~ 1), 
                     data = train_data, 
                     nstates = best_n_states, 
                     family = list(multinomial("identity"), multinomial("identity")),
                     ntimes = ntimes_train)  # Use ntimes for training data

fit_best_model <- fit(best_model, verbose = FALSE)

# 8. Get the log-likelihood for the training data and normalize it
training_log_likelihood <- logLik(fit_best_model)
normalized_training_log_likelihood <- as.numeric(training_log_likelihood) / ntimes_train

cat("Normalized Log-Likelihood for the training data:", normalized_training_log_likelihood, "\n")

# 9. Calculate normalized log-likelihood and deviation for each subset

deviation_list <- list()  # Empty list to store deviations

for (i in 1:length(test_data_split)) {
    # Get the current subset data
    subset_data <- test_data_split[[i]]
    
    # Get the number of rows (ntimes) for the current subset
    ntimes_subset <- nrow(subset_data)
    
    # Apply discretization to the subset data
    subset_data <- subset_data %>%
        mutate(Global_intensity = discretize_variable(Global_intensity),
               Global_reactive_power = discretize_variable(Global_reactive_power))
    
    # Fit the model to the subset data using ntimes
    best_model_subset <- depmix(list(Global_intensity ~ 1, Global_reactive_power ~ 1), 
                                data = subset_data, 
                                nstates = best_n_states, 
                                family = list(multinomial("identity"), multinomial("identity")),
                                ntimes = ntimes_subset)  # Use ntimes for the current subset
    
    fit_best_model_subset <- fit(best_model_subset, verbose = FALSE)
    
    # Get the log-likelihood for the subset data
    subset_log_likelihood <- logLik(fit_best_model_subset)
    
    # Normalize log-likelihood by the number of observations in the subset data
    normalized_subset_log_likelihood <- as.numeric(subset_log_likelihood) / ntimes_subset
    
    # Calculate deviation from the training data log-likelihood
    deviation <- abs(normalized_subset_log_likelihood - normalized_training_log_likelihood)
    
    # Store the deviation in the list
    deviation_list[[i]] <- deviation
}

# Create a data frame of deviations
deviation_data <- data.frame(Subset = 1:length(test_data_split), Deviation = unlist(deviation_list))

# 10. Plot the deviation for each subset
ggplot(deviation_data, aes(x = as.factor(Subset), y = Deviation)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Deviation of Log-Likelihood from Training Data for Each Subset",
         x = "Subset Index", y = "Deviation (Normalized Log-Likelihood)")
