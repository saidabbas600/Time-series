library(readxl)

# Load the data from the specified sheet
macroeconomic_data <- read_excel("macroeconomic data.xlsx", sheet = "Data")
# Check the first few rows
head(macroeconomic_data)

# Check the structure of the data
str(macroeconomic_data)

# Check the new dataset
head(dataset)
# Check the column names
colnames(macroeconomic_data)






library(tidyr)
library(dplyr)

# Filter for the series of interest
gdp_data <- macroeconomic_data %>% filter(`Series Name` == "GDP (current LCU)")  # Adjust based on actual series names
political_stability_data <- macroeconomic_data %>% filter(`Series Name` == "Political Stability Index")  # Adjust as needed

# Reshape the data from wide to long format
gdp_long <- gdp_data %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Year",
               names_prefix = "",
               values_to = "GDP")

political_stability_long <- political_stability_data %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Year",
               names_prefix = "",
               values_to = "Political_Stability")

# Convert the year column to numeric
gdp_long$Year <- as.numeric(substr(gdp_long$Year, 1, 4))
political_stability_long$Year <- as.numeric(substr(political_stability_long$Year, 1, 4))

# Merge the datasets if needed
dataset <- merge(gdp_long, political_stability_long, by = c("Country Name", "Year"))



str(dataset)

dataset$GDP <- as.numeric(dataset$GDP)
dataset$Political_Stability <- as.numeric(dataset$Political_Stability)
summary(dataset$GDP)
summary(dataset$Political_Stability)






# Check unique values to see if there are non-numeric entries
unique(dataset$GDP)
unique(dataset$Political_Stability)

# Check for NA values
sum(is.na(dataset$GDP))
sum(is.na(dataset$Political_Stability))
# Remove non-numeric values and convert to numeric
dataset$GDP <- as.numeric(gsub("[^0-9.-]", "", dataset$GDP))
dataset$Political_Stability <- as.numeric(gsub("[^0-9.-]", "", dataset$Political_Stability))

# Re-check the data
summary(dataset$GDP)
summary(dataset$Political_Stability)




# Remove rows with missing values in relevant columns
dataset <- na.omit(dataset[, c("GDP", "Political_Stability")])

# Re-check the data
summary(dataset$GDP)
summary(dataset$Political_Stability)


head(dataset$GDP)
head(dataset$Political_Stability)



# Inspect the original data
head(macroeconomic_data)
# Check column names
colnames(macroeconomic_data)

# Check the first few rows of specific columns
head(macroeconomic_data$`1990 [YR1990]`)
head(macroeconomic_data$`1991 [YR1991]`)



# Check the structure of the dataset
str(macroeconomic_data)

# View the first few rows of the dataset
head(macroeconomic_data)

























library(tidyr)
library(dplyr)

# Filter for GDP data
gdp_data <- macroeconomic_data %>%
  filter(`Series Name` == "GDP (current LCU)") %>%  # Adjust based on your exact indicator name
  pivot_longer(cols = starts_with("1990") | starts_with("2000") | starts_with("2010"),
               names_to = "Year",
               values_to = "GDP")

# Clean the GDP column
gdp_data$GDP <- as.numeric(gsub("[^0-9.-]", "", gdp_data$GDP))







# Filter for Political Stability data
political_stability_data <- macroeconomic_data %>%
  filter(`Series Name` == "Political Stability Index") %>%  # Adjust based on your exact indicator name
  pivot_longer(cols = starts_with("1990") | starts_with("2000") | starts_with("2010"),
               names_to = "Year",
               values_to = "Political_Stability")

# Clean the Political Stability column
political_stability_data$Political_Stability <- as.numeric(gsub("[^0-9.-]", "", political_stability_data$Political_Stability))







# Convert Year to numeric
gdp_data$Year <- as.numeric(substr(gdp_data$Year, 1, 4))
political_stability_data$Year <- as.numeric(substr(political_stability_data$Year, 1, 4))

# Merge datasets by Country and Year
dataset <- merge(gdp_data, political_stability_data, by = c("Country Name", "Year"))

# Remove rows with NA values
dataset <- na.omit(dataset[, c("GDP", "Political_Stability")])

# Verify the cleaned dataset
summary(dataset$GDP)
summary(dataset$Political_Stability)






# Inspect the first few rows of the cleaned dataset
head(dataset)




























# Verify the contents and structure of the original dataset
head(macroeconomic_data)
str(macroeconomic_data)



# Check if there are any records for GDP in the original dataset
gdp_check <- macroeconomic_data %>%
  filter(`Series Name` == "GDP (current LCU)")

# Check the result of filtering
head(gdp_check)



# Check if there are any records for Political Stability
political_stability_check <- macroeconomic_data %>%
  filter(`Series Name` == "Political Stability Index")

# Check the result of filtering
head(political_stability_check)























# Example of pivoting and cleaning
gdp_data <- macroeconomic_data %>%
  filter(`Series Name` == "GDP (current LCU)") %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Year",
               values_to = "GDP") %>%
  mutate(GDP = as.numeric(gsub("[^0-9.-]", "", GDP)),
         Year = as.numeric(substr(Year, 1, 4)))

political_stability_data <- macroeconomic_data %>%
  filter(`Series Name` == "Political Stability Index") %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Year",
               values_to = "Political_Stability") %>%
  mutate(Political_Stability = as.numeric(gsub("[^0-9.-]", "", Political_Stability)),
         Year = as.numeric(substr(Year, 1, 4)))








# Merge datasets
dataset <- merge(gdp_data, political_stability_data, by = c("Country Name", "Year"))

# Check if merging produced any results
head(dataset)

# Check for unique values in Series Name
unique(macroeconomic_data$`Series Name`)











library(tidyr)
library(dplyr)

# Check for available series names
print(unique(macroeconomic_data$`Series Name`))

# Filter and reshape GDP data
gdp_data <- macroeconomic_data %>%
  filter(`Series Name` == "GDP (current LCU)") %>%
  pivot_longer(cols = starts_with("1990") | starts_with("2000") | starts_with("2010"),
               names_to = "Year",
               values_to = "GDP") %>%
  mutate(GDP = as.numeric(gsub("[^0-9.-]", "", GDP)),
         Year = as.numeric(substr(Year, 1, 4)))

# Check for any issues with GDP data
head(gdp_data)

# Filter and reshape Political Stability data
political_stability_data <- macroeconomic_data %>%
  filter(`Series Name` == "Political Stability Index") %>%
  pivot_longer(cols = starts_with("1990") | starts_with("2000") | starts_with("2010"),
               names_to = "Year",
               values_to = "Political_Stability") %>%
  mutate(Political_Stability = as.numeric(gsub("[^0-9.-]", "", Political_Stability)),
         Year = as.numeric(substr(Year, 1, 4)))

# Check for any issues with Political Stability data
head(political_stability_data)

# Merge datasets
dataset <- merge(gdp_data, political_stability_data, by = c("Country Name", "Year"))

# Check the merged dataset
head(dataset)

















# Load necessary libraries
library(ggplot2)
library(corrplot)

# Visualize the data
ggplot(dataset, aes(x = Political_Stability, y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "GDP vs. Political Stability",
       x = "Political Stability",
       y = "GDP")

# Calculate and plot correlation matrix
cor_matrix <- cor(dataset[, c("GDP", "Political_Stability")], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)





# Check for missing values in GDP and Political_Stability
sum(is.na(dataset$GDP))
sum(is.na(dataset$Political_Stability))


# Check the data types
str(dataset$GDP)
str(dataset$Political_Stability)

# Load the dataset again
macroeconomic_data <- read_excel("macroeconomic data.xlsx", sheet = "Data")

# Check the structure and first few rows
str(macroeconomic_data)
head(macroeconomic_data)


# Check column names
colnames(macroeconomic_data)

# Extract and inspect relevant columns
dataset <- macroeconomic_data[, c("GDP", "Political_Stability")]

# Inspect the extracted dataset
str(dataset)
head(dataset)
