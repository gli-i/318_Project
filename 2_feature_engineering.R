library(tidyverse)
library(dplyr)
library(zoo)

library("corrplot")

# install.packages("devtools", dep = TRUE)
library(devtools)
# install_github("vqv/ggbiplot")


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

# show pca plot
print(ggbiplot(pcobj = df.pca,
                 choices = c(1,2),
                 obs.scale = 1, var.scale = 1,  # Scaling of axis
                 varname.size = 5, varname.color = "red",
                 alpha = 0.05) +
        ggtitle("Results of PCA Analysis After Standardization")
)


# CHOOSE SUBSET OF VARIABLES 
# respectively responsible for 40.7%, 14%, 13.44%, and 11.9% of variance
df_subset <- df[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity")]

# write to text file
write_csv(df, "ProjectData_Processed.txt", na = "", col_names = TRUE)
