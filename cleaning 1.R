# Check column names in both datasets
colnames(selected_data)
colnames(additional_data)
# Inspect the first few rows
head(selected_data)
head(additional_data)
# Check the type of the 'year' column
str(selected_data$year)

str(additional_data$year)
# Check column names and structure of additional_data
# View the structure and column names of additional_data
str(additional_data)
colnames(additional_data)

# Create a sequential year column based on the number of rows
additional_data$year <- seq(from = 1990, by = 1, length.out = nrow(additional_data))
# View the first few rows of additional_data
head(additional_data)
# Merge the datasets
combined_data <- merge(selected_data, additional_data, by = "year")

# Verify the merged data
head(combined_data)





# View the first few rows and column names of additional_data
head(additional_data)
colnames(additional_data)
# Check if there's a date column and inspect its structure
if ("date" %in% colnames(additional_data)) {
  str(additional_data$date)
}
if ("date" %in% colnames(additional_data)) {
  additional_data$year <- as.numeric(format(as.Date(additional_data$date, format = "%Y-%m-%d"), "%Y"))
}
# Assuming data spans from 1990 to 2023 for each row
# This example will replicate years across rows, adjust as needed
n_years <- 1990:2023
additional_data$year <- rep(n_years, length.out = nrow(additional_data))
# Rename or transform existing columns to match the year format
if ("existing_year_col" %in% colnames(additional_data)) {
  additional_data$year <- additional_data$existing_year_col
}
# Ensure that 'year' column exists in both datasets
combined_data <- merge(selected_data, additional_data, by = "year")
# Inspect the first few rows of combined_data
head(combined_data)
# Fit the model to explore combined effects
combined_model <- lm(GDP_growth ~ political_stability * economic_indicators, data = combined_data)
summary(combined_model)


# Check the structure and summary of the combined data
str(combined_data)
summary(combined_data)
# Check for missing values in the combined data
sum(is.na(combined_data))
# Plot GDP growth against political stability and economic indicators
ggplot(combined_data, aes(x = political_stability, y = GDP_growth, color = economic_indicators)) +
  geom_point() +
  labs(title = "GDP Growth vs. Political Stability and Economic Indicators", x = "Political Stability", y = "GDP Growth") +
  theme_minimal()

# Plot time series of the variables
ggplot(combined_data, aes(x = year)) +
  geom_line(aes(y = GDP_growth, color = "GDP Growth")) +
  geom_line(aes(y = political_stability, color = "Political Stability")) +
  geom_line(aes(y = economic_indicators, color = "Economic Indicators")) +
  labs(title = "GDP Growth, Political Stability, and Economic Indicators Over Time", x = "Year", y = "Value") +
  theme_minimal()
# Fit a model to explore combined effects
combined_model <- lm(GDP_growth ~ political_stability * economic_indicators, data = combined_data)
summary(combined_model)

# Check the column names in combined_data
colnames(combined_data)
# Example: Add missing columns to combined_data if you have the data
combined_data$industry_value_added <- NA  # Replace with actual data or merge it
combined_data$imports <- NA  # Replace with actual data or merge it

# Or re-check your data source and make sure these columns are properly merged
## Check for NA values in the relevant columns
summary(combined_data)

# Check for NA values in specific columns
sum(is.na(combined_data$GDP_growth))
sum(is.na(combined_data$political_stability))
sum(is.na(combined_data$economic_indicators))
sum(is.na(combined_data$industry_value_added))
sum(is.na(combined_data$imports))



# Impute missing values with the mean of the respective columns
combined_data$industry_value_added[is.na(combined_data$industry_value_added)] <- mean(combined_data$industry_value_added, na.rm = TRUE)
combined_data$imports[is.na(combined_data$imports)] <- mean(combined_data$imports, na.rm = TRUE)
# Check the number of rows in combined_data
nrow(combined_data)
# Impute missing values with the mean of the respective columns
combined_data$industry_value_added[is.na(combined_data$industry_value_added)] <- mean(combined_data$industry_value_added, na.rm = TRUE)
combined_data$imports[is.na(combined_data$imports)] <- mean(combined_data$imports, na.rm = TRUE)

# Verify that missing values are handled
sum(is.na(combined_data$industry_value_added))
sum(is.na(combined_data$imports))
# Check the structure of combined_data
str(combined_data)
# Explicitly filter out rows with NA values
combined_data_clean <- combined_data %>%
  filter(!is.na(political_stability) & !is.na(economic_indicators) &
           !is.na(industry_value_added) & !is.na(imports))

# Check the number of rows after filtering
nrow(combined_data_clean)

