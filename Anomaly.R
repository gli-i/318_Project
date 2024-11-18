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

# Step 6: Calculate the number of subsets (first 9 subsets will be 47,376 rows each)
rows_per_subset <- 47376
num_subsets <- 9
total_rows <- nrow(test_data)

# Ensure there are enough rows
if (total_rows < rows_per_subset * num_subsets) {
  stop("The dataset is too small to fit the requested subset size of 47,376 rows per subset.")
}

# Step 7: Create the subset column and assign values based on row counts
test_data$subset <- NA  # Initialize subset column

# For first 9 subsets (each 47,376 rows)
for (i in 1:num_subsets) {
  start_idx <- (i - 1) * rows_per_subset + 1
  end_idx <- i * rows_per_subset
  # Ensure we only assign to rows that exist
  if (start_idx <= total_rows) {  # Only assign if the index is valid
    end_idx <- min(end_idx, total_rows)  # Avoid exceeding total rows
    test_data$subset[start_idx:end_idx] <- i
  }
}

# For the 10th subset, assign the remaining rows
if ((num_subsets * rows_per_subset + 1) <= total_rows) {
  test_data$subset[(num_subsets * rows_per_subset + 1):total_rows] <- 10
}

# Step 8: Split the data into subsets based on the 'subset' column
test_data_split <- split(test_data, test_data$subset)

# Step 9: Create a directory to store CSV files (if it doesn't already exist)
output_directory <- "test_data_subsets"
dir.create(output_directory, showWarnings = FALSE)

# Step 10: Write each subset to a CSV file
for (i in 1:length(test_data_split)) {
  subset_data <- test_data_split[[i]]
  
  # Save the subset to CSV file
  file_name <- paste0(output_directory, "/subset_", i, ".csv")
  write.csv(subset_data, file = file_name, row.names = FALSE)
}

# After running this, you will have 10 CSV files: subset_1.csv, subset_2.csv, ..., subset_10.csv
