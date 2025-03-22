# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
library(car)

# Load dataset
dataset <- read.csv("tech_layoffs.csv")
# Convert reported_date to Date format
dataset$reported_date <- as.Date(dataset$reported_date, format="%m/%d/%Y")

# Replace "Unclear" with NA in total_layoffs
dataset$total_layoffs[dataset$total_layoffs == "Unclear"] <- NA

# Convert total_layoffs to numeric
dataset$total_layoffs <- as.numeric(dataset$total_layoffs)

# Replace "Unclear" with NA in impacted_workforce_percentage
dataset$impacted_workforce_percentage[dataset$impacted_workforce_percentage == "Unclear"] <- NA

# Remove percentage signs and spaces *before* conversion
dataset$impacted_workforce_percentage <- gsub("%", "", dataset$impacted_workforce_percentage)
dataset$impacted_workforce_percentage <- gsub("\\s+", "", dataset$impacted_workforce_percentage)  # Remove spaces

# Convert impacted_workforce_percentage to numeric
dataset$impacted_workforce_percentage <- as.numeric(dataset$impacted_workforce_percentage)

# Drop rows where total_layoffs, reported_date, or impacted_workforce_percentage is NA
dataset <- dataset %>% drop_na(total_layoffs, reported_date, impacted_workforce_percentage)

# Create 'Days_since_start' column (time difference in days)
dataset$Days_since_start <- as.numeric(difftime(dataset$reported_date, min(dataset$reported_date), units = "days"))

# Standardization (Z-score Normalization)
dataset$impacted_workforce_percentage <- scale(dataset$impacted_workforce_percentage)
dataset$Days_since_start <- scale(dataset$Days_since_start)

# Summary statistics
summary(dataset$impacted_workforce_percentage)
summary(dataset$Days_since_start)

# Check for remaining NAs
sum(is.na(dataset$impacted_workforce_percentage))  # Should be 0
sum(is.na(dataset$Days_since_start))  # Should be 0

# Ensure the 'industry' column is a factor
dataset$industry <- as.factor(dataset$industry)

# Layoffs trend over time
ggplot(dataset, aes(x=reported_date, y=total_layoffs, color=industry)) +
  geom_line() +
  labs(title="Layoffs Trend in Tech Industry (2022-2023)", x="Reported Date", y="Total Layoffs")


# Remove rows with missing 'industry' data
dataset <- dataset %>% drop_na(industry)

# Top 10 companies with highest layoffs
top_companies <- dataset %>% 
  group_by(company) %>% 
  summarise(total_layoffs = sum(total_layoffs, na.rm=TRUE)) %>% 
  arrange(desc(total_layoffs)) %>% 
  head(10)

dev.off()  # Close any open graphic devices

# Bar plot for top companies
ggplot(top_companies, aes(x=reorder(company, -total_layoffs), y=total_layoffs, fill=company)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Top 10 Companies with Highest Layoffs", x="Company", y="Total Layoffs")

# Correlation Analysis (Only if we have numeric columns)
numeric_data <- dataset %>% select_if(is.numeric)

if (ncol(numeric_data) > 1) {
  corr_matrix <- cor(numeric_data, use="complete.obs")
  corrplot(corr_matrix, method="color")
} else {
  print("Not enough numeric columns for correlation analysis.")
}

#Define Company Size Criteria
dataset <- dataset %>%
  mutate(company_size = ifelse(total_layoffs > 500, "Large", "Startup"))
# Hypothesis Testing
# Example: Are layoffs significantly different between startups and large firms?
large_firms <- dataset %>% filter(company_size == "Large")
startups <- dataset %>% filter(company_size == "Startup")


t.test(large_firms$total_layoffs, startups$total_layoffs, alternative="greater")

# Setting up the Linear Model
# Convert company_size to factor
dataset$company_size <- as.factor(dataset$company_size)

# Create the 'Days_since_start' column
dataset$Days_since_start <- as.numeric(difftime(dataset$reported_date, min(dataset$reported_date), units = "days"))

# Fit the linear regression model
model <- lm(total_layoffs ~ impacted_workforce_percentage + Days_since_start + company_size, data = dataset)

# Summarize the model
summary(model)

# Check for Outliers or Incorrect Scaling
dataset$impacted_workforce_percentage <- scale(dataset$impacted_workforce_percentage)
dataset$Days_since_start <- scale(dataset$Days_since_start)

summary(model)

# Create individual diagnostic plots
plot1 <- plot(model, which = 1)  # Residuals vs Fitted
plot2 <- plot(model, which = 2)  # Normal Q-Q
plot3 <- plot(model, which = 3)  # Scale-Location
plot4 <- plot(model, which = 4)  # Cook's distance

# Check if Days_since_start is numeric in new_data
new_data$Days_since_start <- as.numeric(new_data$Days_since_start)

# Update the new_data with different values
new_data$impacted_workforce_percentage <- c(6, 8)  # Changing impacted workforce percentage
new_data$Days_since_start <- c(150, 800)  # Changing the number of days since the first layoff
new_data$company_size <- c("Large", "Startup")  # Changing company size

# View the updated new_data
print(new_data)
str(new_data)  # Should match the structure of dataset used for training

# Rescale impacted_workforce_percentage using training data's mean and SD
new_data$impacted_workforce_percentage <- scale(new_data$impacted_workforce_percentage, 
                                                center = mean(dataset$impacted_workforce_percentage, na.rm = TRUE), 
                                                scale = sd(dataset$impacted_workforce_percentage, na.rm = TRUE))

# Rescale Days_since_start using training data's mean and SD
new_data$Days_since_start <- scale(new_data$Days_since_start, 
                                   center = mean(dataset$Days_since_start, na.rm = TRUE), 
                                   scale = sd(dataset$Days_since_start, na.rm = TRUE))

# Ensure Days_since_start is numeric after scaling
new_data$Days_since_start <- as.numeric(new_data$Days_since_start)

# Ensure company_size is a factor
new_data$company_size <- factor(new_data$company_size, levels = levels(dataset$company_size))

# View the modified new_data
print(new_data)

# Predict with the model
predicted_layoffs <- predict(model, newdata = new_data)

# View the predictions
print(predicted_layoffs)





