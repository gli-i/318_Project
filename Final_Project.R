library(tidyverse)
library(dplyr)
library(zoo)
library(depmixS4)
library(arules)


library("corrplot")

# install.packages("devtools", dep = TRUE)
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

# IMPORT DATASET
df <- read_csv(
  file = "TermProjectData.txt",
  col_types = list(Date = col_date(format="%d/%m/%Y"))
)
# glimpse(df)

# iterate through every col except for the 1st two, Date & Time
for (colname in colnames(df[, 3:9])){
  # use linear approximation to fill in missing (NA) values
  df[[colname]] <- na.approx(df[[colname]], na.rm = FALSE)
}
# the first value in Global_active_power (col 3) is NA; must manually replace
df[1, 3] = df[2, 3]

# check to make sure no rows have NA values left
stopifnot(nrow(df) == nrow(na.omit(df)))


# STANDARDIZE VALUES + PCA ANALYSIS
df.pca <- prcomp(df[3:9], scale. = TRUE)
summary(df.pca)

# PC1 accounts for the most variation - see which features influence PC1 the most
sort(abs(df.pca$rotation[,1]), decreasing=TRUE)
df.pca$rotation

# Check correlations of variables
df.cor = cor(df[3:9])
df.cor

# show pca plot
print(ggbiplot(pcobj = df.pca,
                 choices = c(1,2),
                 obs.scale = 1, var.scale = 1,  # Scaling of axis
                 varname.size = 5, varname.color = "red",
                 alpha = 0.05) +
        ggtitle("Results of PCA Analysis After Standardization")
)


# CHOOSE SUBSET OF VARIABLES
# keep the date & time columns
df_subset <- df[c("Date", "Time", "Global_intensity", "Global_reactive_power")]

df_subset["Global_intensity"] = scale(df_subset["Global_intensity"])
df_subset["Global_reactive_power"] = scale(df_subset["Global_reactive_power"])

# write to text file
write_csv(df_subset, "ProjectData_Processed.txt", na = "", col_names = TRUE)


# check written dataset

data <- read_csv(
  file = "ProjectData_Processed.txt",
  col_types = list(Date = col_date(format="%Y-%m-%d"))
)
n_distinct(format(data$Date, "%Y"))
glimpse(data)
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
# 1. Load and preprocess the data
data <- read.csv("ProjectData_Processed.txt")

# Convert Date and Time columns into a single POSIX datetime column (if needed for later use)
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

# Ensure the Date column is a Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

test_data <- data %>% filter(Date >= as.Date("2009-01-01"))


# 3. Function to discretize continuous variables into 5 bins
discretize_variable <- function(column) {
    arules::discretize(column, method = "interval", breaks = 5, labels = FALSE)
}


test_data <- test_data %>%
    mutate(Global_intensity = discretize_variable(Global_intensity),
           Global_reactive_power = discretize_variable(Global_reactive_power))


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
