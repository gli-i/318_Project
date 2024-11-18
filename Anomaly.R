# Load necessary libraries
library(tidyverse)

# Load the dataset
test_data <- read.csv("ProjectData_processed.txt")  # Correct file name

# Step 1: Inspect the first few rows to check Date and Time format
head(test_data[, c("Date", "Time")])

# Step 2: Ensure the Date is in the right format (YYYY-MM-DD)
test_data$Date <- as.Date(test_data$Date, format = "%Y-%m-%d")

# Step 3: Sort the data by Date to ensure it starts from 2009-01-01
test_data <- test_data %>% arrange(Date)

# Step 4: Create a reference start date: January 1, 2009
start_date <- as.Date("2009-01-01")

# Step 5: Filter data to only include rows starting from 2009-01-01
test_data <- test_data %>% filter(Date >= start_date)

# Step 6: Create subsets based on the new date ranges

# Subset 1: From 2009-01-01 to 2009-01-25
subset1 <- test_data %>% filter(Date >= as.Date("2009-01-01") & Date <= as.Date("2009-01-25"))

# Subsets 2 to 9: 5 consecutive weeks (35 days) for each
subsets_list <- list()
start_date_next_subset <- as.Date("2009-01-26")  # The start date for the second subset

for (i in 1:8) {
  # Calculate the end date for the current 5-week period (35 days)
  end_date <- start_date_next_subset + days(34)  # 35 days from the start date
  # Filter the data for the current subset
  subsets_list[[i]] <- test_data %>% filter(Date >= start_date_next_subset & Date <= end_date)
  # Update the start date for the next subset (next 5-week period)
  start_date_next_subset <- end_date + days(1)
}

# Subset 10: Remaining data
remaining_data <- test_data %>% filter(Date > max(subsets_list[[8]]$Date))

# Combine all subsets into a single list
test_data_split <- c(list(subset1), subsets_list, list(remaining_data))

# Step 7: Add a 'subset' column to each subset
for (i in 1:length(test_data_split)) {
  test_data_split[[i]]$subset <- i
}

# Step 8: Create a directory to store CSV files (if it doesn't already exist)
output_directory <- "test_data_subsets"
dir.create(output_directory, showWarnings = FALSE)

# Step 9: Write each subset to a CSV file
for (i in 1:length(test_data_split)) {
  subset_data <- test_data_split[[i]]
  
  # Save the subset to CSV file
  file_name <- paste0(output_directory, "/subset_", i, ".csv")
  write.csv(subset_data, file = file_name, row.names = FALSE)
}

