library(readxl)
library(readxl)
library(tseries)
library(forecast)
library(vars)
library(urca)
library(dynlm)
library(lmtest)
macroeconomic_data <- read_excel("macroeconomic data.xlsx", sheet = "Data")
# Inspect the data
head(macroeconomic_data)
str(macroeconomic_data)




# Filter for Pakistan data
DOC_pakistan <- macroeconomic_data %>% filter(`Country Name` == "Pakistan")

# Filter for GDP data (replace with the exact series name)
DOC_pakistan_GDP <- DOC_pakistan %>% filter(`Series Name` == "GDP (current LCU)")

# Inspect the filtered data
head(DOC_pakistan_GDP)

# Ensure the date column is in Date format
DOC_pakistan_GDP$Date <- as.Date(DOC_pakistan_GDP$Date, format = "%Y-%m-%d") # Adjust format as needed





# Check column names
colnames(DOC_pakistan_GDP)
# Inspect the first few rows
head(DOC_pakistan_GDP)
# Replace 'Date' with the actual column name if different
DOC_pakistan_GDP$Date <- as.Date(DOC_pakistan_GDP$Date, format = "%Y-%m-%d") # Adjust format as needed




# Create a Date object assuming the column contains year and month
DOC_pakistan_GDP$Date <- as.Date(paste0(DOC_pakistan_GDP$YearMonth, "-01"), format = "%Y-%m-%d")
# Create a Date object assuming the column contains year and month
DOC_pakistan_GDP$Date <- as.Date(paste0(DOC_pakistan_GDP$YearMonth, "-01"), format = "%Y-%m-%d")







# Check column names
colnames(DOC_pakistan_GDP)
# Inspect the first few rows of the dataset
head(DOC_pakistan_GDP)




##1. Reshape the Data to Long Format
##You can use the pivot_longer() function from the tidyr package to achieve this:
library(tidyr)

# Reshape the data from wide to long format
DOC_pakistan_GDP_long <- DOC_pakistan_GDP %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "GDP_Value")

# Inspect the reshaped data
head(DOC_pakistan_GDP_long)






# Extract the year from the 'Year' column and convert to a Date object
DOC_pakistan_GDP_long$Year <- as.Date(paste0(substring(DOC_pakistan_GDP_long$Year, 1, 4), "-01-01"))

# Convert the GDP values to numeric (since they might be read as characters)
DOC_pakistan_GDP_long$GDP_Value <- as.numeric(DOC_pakistan_GDP_long$GDP_Value)

# Inspect the cleaned data
head(DOC_pakistan_GDP_long)




library(xts)

# Create time series object
gdp_ts <- xts(DOC_pakistan_GDP_long$GDP_Value, order.by = DOC_pakistan_GDP_long$Year)

# Plot the time series
plot(gdp_ts, main = "GDP Time Series for Pakistan", xlab = "Year", ylab = "GDP (current LCU)")


# Check for missing values
anyNA(gdp_ts)

# If there are missing values, you might want to impute or handle them:
# For simplicity, let's fill NA with the last observation carried forward
gdp_ts <- na.locf(gdp_ts)

# Ensure that the data is numeric
gdp_ts <- as.numeric(gdp_ts)


# Create a time index for your data
time_index <- seq(from = 1990, to = 2023)

# Fit a linear model to analyze the trend
trend_model <- lm(gdp_ts ~ time_index)

# Summary of the trend model
summary(trend_model)

# Plot the GDP data with the trend line
plot(time_index, gdp_ts, type = "l", main = "GDP Time Series with Trend Line", xlab = "Year", ylab = "GDP (current LCU)", col = "blue", lwd = 2)
abline(trend_model, col = "red", lwd = 2)  # Add trend line







library(forecast)

# Fit an ARIMA model
fit <- auto.arima(gdp_ts)

# Summary of the ARIMA model
summary(fit)

# Plot the residuals to check model fit
checkresiduals(fit)

# Forecast the next 10 years (or any other period)
forecasted_values <- forecast(fit, h = 10)

# Plot the forecast
plot(forecasted_values, main = "GDP Forecast for Pakistan", xlab = "Year", ylab = "GDP (current LCU)")


# Plot ACF of residuals
acf(residuals(fit))

# Plot a histogram of residuals
hist(residuals(fit), main = "Residuals of ARIMA Model", xlab = "Residuals", col = "lightblue")





# Example regression model
model1 <- lm(GDP_Growth ~ Political_Stability + Inflation_Rate + Exchange_Rate + Government_Effectiveness, data = dataset)
summary(model1)











any(is.na(DOC_pakistan_GDP$Date))


names(DOC_pakistan_GDP)
library(tidyr)
DOC_pakistan_GDP_long <- DOC_pakistan_GDP %>%
  pivot_longer(
    cols = starts_with("199"),  # Columns that start with '199' (1990 onwards)
    names_to = "Year",          # Name of the new column
    values_to = "GDP"           # Name of the column to store GDP values
  )
DOC_pakistan_GDP_long$Year <- as.numeric(sub("\\s*\\[.*\\]", "", DOC_pakistan_GDP_long$Year))
DOC_pakistan_GDP_long$Date <- as.Date(paste0(DOC_pakistan_GDP_long$Year, "-01-01"))








# Display column names
names(DOC_pakistan_GDP_long)





# Check for duplicate column names
duplicated_names <- names(DOC_pakistan_GDP_long)[duplicated(names(DOC_pakistan_GDP_long))]
print(duplicated_names)
# Rename columns to make them unique
colnames(DOC_pakistan_GDP_long) <- make.names(colnames(DOC_pakistan_GDP_long), unique = TRUE)
library(tidyr)

# Print the column names to inspect them
colnames(DOC_pakistan_GDP_long)
library(dplyr)
library(tidyr)

# Rename columns to ensure uniqueness
colnames(DOC_pakistan_GDP_long) <- make.names(colnames(DOC_pakistan_GDP_long), unique = TRUE)

# Display column names and their counts
table(names(DOC_pakistan_GDP_long))




# Rename columns for simplicity
colnames(DOC_pakistan_GDP_long) <- c(
  "Country_Code", "Country_Name", "Date", "GDP",
  "Series_Code", "Series_Name", "X2000", "X2001", "X2002", "X2003", "X2004",
  "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012",
  "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020",
  "X2021", "X2022", "X2023", "Year"
)

long_data <- long_data %>%
  filter(Value != "..")
long_data <- long_data %>%
  mutate(Value = as.numeric(Value))
# View the first few rows
head(long_data)

# Check the structure of the cleaned data
str(long_data)

















library(dplyr)

# Check the first few rows and column names
head(DOC_pakistan_GDP_long)
colnames(DOC_pakistan_GDP_long)

# Assuming 'Value' column does not exist and you need to pivot longer
# Adjust column names if necessary

# Pivot the data if it hasn't been done already
long_data_clean <- DOC_pakistan_GDP_long %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  filter(Value != "..") %>%
  mutate(Value = as.numeric(Value))

# Check the cleaned data
head(long_data_clean)
str(long_data_clean)
