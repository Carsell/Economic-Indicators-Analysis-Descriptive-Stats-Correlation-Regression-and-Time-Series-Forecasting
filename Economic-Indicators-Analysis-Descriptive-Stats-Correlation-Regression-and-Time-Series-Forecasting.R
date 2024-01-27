install.packages('tidyverse')
install.packages("psych")  # for descriptive statistics
install.packages("corrplot")  # for correlation analysis
install.packages("lmtest")  # for hypothesis testing
install.packages("forecast")  # for time series analysis
install.packages("zoo")
install.packages("dplyr")
install.packages("moments")
install.packages("e1071")
install.packages('ggplot2')
install.packages('robustbase')
install.packages('colorspace')
install.packages('reshape2')

library(dplyr)
library(magrittr)
library(tidyr)
library(psych)
library(corrplot)
library(ggplot2)
library(lmtest)
library(e1071)
library(forecast)
library(colorspace)
library(reshape2)
library(car)

# Read the data
data <- read.csv('/Users/carsell/Downloads/P_Data_Extract_From_World_Development_Indicators (2)/Uncleaned_Data_ASDV.csv')

# Drop unnecessary columns
data <- data %>%
  select(-Country.Code, -Time.Code)

# Rename the columns
names(data) <- c("Country.Name", "Year", "Trade_In_Services", "Statistical_Performance_Indicators_SPI_Pillar_3",
                 "Research_And_Development_Expenditure", "Net_Trade_In_Goods_And_Services_BoP_Current", "Merchandise_Trade_Of_GDP",
                 "Employment_To_Population_Ratio","Export_Volume_Index","Import_Volume_Index","Foreign_Direct_Investment_Net_Inflows_Of_GDP",
                 "Foreign_Direct_Investment_Net_Inflows_BoP_Current", "Foreign_Direct_Investment_Net_Outflows_Of_GDP",
                 "Foreign_Direct_Investment_Net_Outflows_BoP_Current")

#Check the first 5 rows of the dataset
head(data)

summary(data)

# Replace all instances of '..' with NA
data[data == ".."] <- NA

# Convert columns to appropriate data types
data <- data %>%
  mutate(Trade_In_Services = as.numeric(Trade_In_Services),
         Statistical_Performance_Indicators_SPI_Pillar_3 = as.numeric(Statistical_Performance_Indicators_SPI_Pillar_3),
         Research_And_Development_Expenditure = as.numeric(Research_And_Development_Expenditure),
         Net_Trade_In_Goods_And_Services_BoP_Current = as.numeric(Net_Trade_In_Goods_And_Services_BoP_Current),
         Merchandise_Trade_Of_GDP = as.numeric(Merchandise_Trade_Of_GDP),
         Employment_To_Population_Ratio = as.numeric(Employment_To_Population_Ratio),
         Export_Volume_Index = as.numeric(Export_Volume_Index),
         Import_Volume_Index = as.numeric(Import_Volume_Index),
         Foreign_Direct_Investment_Net_Inflows_Of_GDP = as.numeric(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
         Foreign_Direct_Investment_Net_Inflows_BoP_Current = as.numeric(Foreign_Direct_Investment_Net_Inflows_BoP_Current),
         Foreign_Direct_Investment_Net_Outflows_Of_GDP = as.numeric(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
         Foreign_Direct_Investment_Net_Outflows_BoP_Current = as.numeric(Foreign_Direct_Investment_Net_Outflows_BoP_Current))

# Group the data by country and replace NA values with the mean per country
data <- data %>%
  group_by(Country.Name) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# View the modified data
View(data)

# Filter out 'Yemen' and 'Haiti' from the dataset
data <- data %>%
  filter(Country.Name != "Yemen, Rep." & Country.Name != "Haiti")

# View the filtered data
View(data)

write.csv(data, 'Cleaned_Data_PBI_new_new.csv', row.names = FALSE)
# Drop unnecessary columns
data_for_decision <- data[, c("Country.Name", "Research_And_Development_Expenditure", "Net_Trade_In_Goods_And_Services_BoP_Current", "Merchandise_Trade_Of_GDP", "Foreign_Direct_Investment_Net_Inflows_Of_GDP")]

# Group the data by country and calculate the mean for each column
mean_data <- data_for_decision %>%
  group_by(Country.Name) %>%
  summarise(Mean_Research_and_Development_Expenditure = mean(Research_And_Development_Expenditure, na.rm = TRUE),
            Mean_Net_Trade_In_Goods_And_Services = mean(Net_Trade_In_Goods_And_Services_BoP_Current, na.rm = TRUE),
            Mean_Merchandise_Trade_Of_GDP = mean(Merchandise_Trade_Of_GDP, na.rm = TRUE),
            Mean_Foreign_Direct_Investment_Net_Inflows = mean(Foreign_Direct_Investment_Net_Inflows_Of_GDP, na.rm = TRUE))

# Classify countries as developing or developed based on mean values
threshold_RnD <- 0.3 # Threshold for Research and Development Expenditure (% of GDP)
threshold_NetTrade <- 0.0 # Threshold for Net Trade in Goods and Services
threshold_MerchTrade <- 20.0 # Threshold for Merchandise Trade of GDP
threshold_FDI <- 0.2 # Threshold for Foreign Direct Investment (FDI) Net Inflows (% of GDP)

mean_data$Classification <- ifelse(mean_data$Mean_Research_and_Development_Expenditure >= threshold_RnD &
                                     mean_data$Mean_Net_Trade_In_Goods_And_Services >= threshold_NetTrade &
                                     mean_data$Mean_Merchandise_Trade_Of_GDP >= threshold_MerchTrade &
                                     mean_data$Mean_Foreign_Direct_Investment_Net_Inflows >= threshold_FDI,
                                   "Developed", "Developing")

# View the classified data
View(mean_data)

# Define the list of developed and developing countries
# Subset for developing countries
developing_countries <- mean_data[mean_data$Classification == "Developing", ]
developed_countries$Country.Name

# Subset for developed countries
developed_countries <- mean_data[mean_data$Classification == "Developed", ]
developing_countries$Country.Name

# Create subsets for developed and developing countries
developed_data <- data %>% filter(Country.Name %in% developed_countries$Country.Name)
developing_data <- data %>% filter(Country.Name %in% developing_countries$Country.Name)

# View the subsets
View(developed_data)
View(developing_data)

developed_data$Country.Name

#*******************************************************************************************************************
#EXPLORATORY DATA ANALYSIS
# Scatter plot: Net Trade in Goods and Services vs. Merchandise Trade of GDP (per country)
ggplot(data, aes(x = Net_Trade_In_Goods_And_Services_BoP_Current, y = Merchandise_Trade_Of_GDP, color = Country.Name)) +
  geom_point() +
  labs(x = "Net Trade in Goods and Services (BoP Current)", y = "Merchandise Trade of GDP", title = "Net Trade in Goods and Services vs. Merchandise Trade of GDP")

# Calculate correlation matrix:
correlation_matrix <- cor(data[, c("Trade_In_Services", "Statistical_Performance_Indicators_SPI_Pillar_3",
                                   "Research_And_Development_Expenditure", "Net_Trade_In_Goods_And_Services_BoP_Current",
                                   "Merchandise_Trade_Of_GDP", "Employment_To_Population_Ratio",
                                   "Export_Volume_Index", "Import_Volume_Index",
                                   "Foreign_Direct_Investment_Net_Inflows_Of_GDP",
                                   "Foreign_Direct_Investment_Net_Inflows_BoP_Current",
                                   "Foreign_Direct_Investment_Net_Outflows_Of_GDP",
                                   "Foreign_Direct_Investment_Net_Outflows_BoP_Current")],
                          use = "complete.obs")

# Create correlation plot
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

View(correlation_matrix)
  
#Trade in Services (% of GDP) over Time - Line Plot:
# Line plot
ggplot(data, aes(x = Year, y = Trade_In_Services, color = Country.Name)) +
  geom_line() +
  labs(x = "Year", y = "Trade in Services (% of GDP)", title = "Trade in Services (% of GDP) over Time") +
  theme_minimal()

# Scatter plot: Employment-to-Population Ratio vs. Trade in Services
ggplot(data, aes(x = Employment_To_Population_Ratio, y = Trade_In_Services, color = Country.Name)) +
  geom_point() +
  labs(x = "Employment-to-Population Ratio", y = "Trade in Services", 
       title = "Employment-to-Population Ratio vs. Trade in Services")

#  Scatter plot: Research and Development Expenditure (% of GDP) vs. Statistical Performance Indicators (SPI)
ggplot(data, aes(x = Research_And_Development_Expenditure, y = Statistical_Performance_Indicators_SPI_Pillar_3, color = Country.Name)) +
  geom_point() +
  labs(x = "Research and Development Expenditure (% of GDP)", y = "Statistical Performance Indicators (SPI) - Pillar 3", 
       title = "Research and Development Expenditure vs. SPI") +
  theme_minimal()

###################################################################################################################

#OBJECTIVE 1
#Select the Indicators for This Objective
data_obj_1 = data[, c("Country.Name", "Year", "Merchandise_Trade_Of_GDP", 
                      "Net_Trade_In_Goods_And_Services_BoP_Current", "Trade_In_Services")]

# Convert numeric columns to numeric data types
numeric_columns <- c("Merchandise_Trade_Of_GDP", "Net_Trade_In_Goods_And_Services_BoP_Current", "Trade_In_Services")
data_obj_1[, numeric_columns] <- sapply(data_obj_1[, numeric_columns], as.numeric)

#Descriptive Analysis
library(moments)

descriptive_stats_developing <- developing_data %>%
  group_by(Country.Name) %>%
  summarize(
    Mean_Merchandise_Trade_Of_GDP = mean(Merchandise_Trade_Of_GDP),
    Median_Merchandise_Trade_Of_GDP = median(Merchandise_Trade_Of_GDP),
    Mode_Merchandise_Trade_Of_GDP = as.numeric(names(which.max(table(Merchandise_Trade_Of_GDP)))),
    SD_Merchandise_Trade_Of_GDP = sd(Merchandise_Trade_Of_GDP ),
    Skewness_Merchandise_Trade_Of_GDP = skewness(Merchandise_Trade_Of_GDP),
    Kurtosis_Merchandise_Trade_Of_GDP = kurtosis(Merchandise_Trade_Of_GDP),
    Mean_Trade_In_Services = mean(Trade_In_Services),
    Median_Trade_In_Services = median(Trade_In_Services),
    Mode_Trade_In_Services = as.numeric(names(which.max(table(Trade_In_Services)))),
    SD_Trade_In_Services= sd(Trade_In_Services),
    Skewness_Trade_In_Services = skewness(Trade_In_Services),
    Kurtosis_Trade_In_Services = kurtosis(Trade_In_Services)
  )

View(descriptive_stats_developing)

descriptive_stats_developed <- developed_data %>%
  group_by(Country.Name) %>%
  summarize(
    Mean_Merchandise_Trade_Of_GDP = mean(Merchandise_Trade_Of_GDP),
    Median_Merchandise_Trade_Of_GDP = median(Merchandise_Trade_Of_GDP),
    Mode_Merchandise_Trade_Of_GDP = as.numeric(names(which.max(table(Merchandise_Trade_Of_GDP)))),
    SD_Merchandise_Trade_Of_GDP = sd(Merchandise_Trade_Of_GDP ),
    Skewness_Merchandise_Trade_Of_GDP = skewness(Merchandise_Trade_Of_GDP),
    Kurtosis_Merchandise_Trade_Of_GDP = kurtosis(Merchandise_Trade_Of_GDP),
    Mean_Trade_In_Services = mean(Trade_In_Services),
    Median_Trade_In_Services = median(Trade_In_Services),
    Mode_Trade_In_Services = as.numeric(names(which.max(table(Trade_In_Services)))),
    SD_Trade_In_Services= sd(Trade_In_Services),
    Skewness_Trade_In_Services = skewness(Trade_In_Services),
    Kurtosis_Trade_In_Services = kurtosis(Trade_In_Services)
  )

View(descriptive_stats_developed)

#Correlation Analysis
correlation_matrix_obj_1 <- cor(data_obj_1[, numeric_columns] <- sapply(data_obj_1[, numeric_columns], as.numeric),
                          use = "complete.obs")

# Create correlation plot
corrplot(correlation_matrix_obj_1, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

#Hypothesis Testing
#Hypothesis 1
# Subset the relevant columns for analysis
subset_data <- data %>%
  select(Trade_In_Services, Merchandise_Trade_Of_GDP)

# Calculate the correlation coefficient
correlation <- cor(subset_data$Trade_In_Services, subset_data$Merchandise_Trade_Of_GDP)

# Perform hypothesis test
p_value <- cor.test(subset_data$Trade_In_Services, subset_data$Merchandise_Trade_Of_GDP)$p.value

# Print the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation, "\n")
cat("p-value:", p_value, "\n")

#HYPOTHESIS 2
#Subset the relevant columns for analysis
subset_data <- data %>%
  select(Foreign_Direct_Investment_Net_Outflows_Of_GDP, Foreign_Direct_Investment_Net_Inflows_Of_GDP)

# Calculate the correlation coefficient
correlation <- cor(subset_data$Foreign_Direct_Investment_Net_Outflows_Of_GDP, subset_data$Foreign_Direct_Investment_Net_Inflows_Of_GDP)

# Perform hypothesis test
p_value <- cor.test(subset_data$Foreign_Direct_Investment_Net_Outflows_Of_GDP, subset_data$Foreign_Direct_Investment_Net_Inflows_Of_GDP)$p.value

# Print the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation, "\n")
cat("p-value:", p_value, "\n")

#Regression Analysis
#First Regression model
# Model 1 (Simple Linear Regression)
lm_model1_a<- lm(Export_Volume_Index ~ Import_Volume_Index, data = data)

# Print the regression results
cat("Regression analysis for Model 1a")
summary(lm_model1_a)

# Assumption 1: Linearity
raintest(lm_model1_a)

# Assumption 2: Independence of residuals
dwtest(lm_model1_a)

# Assumption 3: Homoscedasticity
plot(lm_model1_a, which = 1)

# Assumption 4: Normality of residuals
qqnorm(lm_model1_a$residuals)
qqline(lm_model1_a$residuals)

#Second Regression
# Model 1:
lm_model1_b<- lm(Import_Volume_Index ~ Export_Volume_Index, data = data)

# Print the regression results
cat("Regression analysis for Model 1")
summary(lm_model1_b)

# Model 2: Regression with one independent variable 
lm_model2_b <- lm(Import_Volume_Index ~ Export_Volume_Index + Research_And_Development_Expenditure, data = data)
# Print the regression results
cat("Regression analysis for Model 2")
summary(lm_model2_b)

# Model 3: Regression with one independent variable 
lm_model3_b <- lm(Import_Volume_Index ~ Export_Volume_Index + Research_And_Development_Expenditure + 
                    Merchandise_Trade_Of_GDP , data = data)
# Print the regression results
cat("Regression analysis for Model 3")
summary(lm_model3_b)

# Model 4: Regression with one independent variable
lm_model4_b <- lm(Import_Volume_Index ~ Export_Volume_Index + Research_And_Development_Expenditure + Merchandise_Trade_Of_GDP + 
                      Statistical_Performance_Indicators_SPI_Pillar_3, data = data)
# Print the regression results
cat("Regression analysis for Model 4")
summary(lm_model4_b)

# Model 5:
lm_model5_b <- lm(Import_Volume_Index ~ Export_Volume_Index + Research_And_Development_Expenditure + Merchandise_Trade_Of_GDP + 
                      Statistical_Performance_Indicators_SPI_Pillar_3 + Trade_In_Services, data = data)

# Print the regression results
cat("Regression analysis for Model 5")
summary(lm_model5_b)

# Assumption 1: Linearity
raintest(lm_model5_b)

# Assumption 2: Independence of residuals
dwtest(lm_model5_b)

# Assumption 3: Homoscedasticity
plot(lm_model5_b, which = 1)

# Assumption 4: No multicollinearity
vif(lm_model5_b)

# Assumption 5: Normality of residuals
qqnorm(lm_model5_b$residuals)
qqline(lm_model5_b$residuals)

#Time Series Analysis
# Read the CSV file
data2<- read.csv("/Users/carsell/Downloads/P_Data_Extract_From_World_Development_Indicators (3)/Time_series.csv")

# Drop unnecessary columns
data2 <- data2 %>%
  select(-Country.Code, -Time.Code)

# Rename the columns
names(data2) <- c("Country.Name", "Year", "Foreign_Direct_Investment_Net_Inflows_Of_GDP") 

# Extract the relevant column for FDI
fdi <- data2$Foreign_Direct_Investment_Net_Inflows_Of_GDP
fdi
# Create a time series object
ts_fdi <- ts(fdi, start = c(1973))

# Automatic selection of ARIMA orders using auto.arima
auto_arima_model <- auto.arima(ts_fdi)
summary(auto_arima_model)

# Forecast using auto.arima model
auto_arima_forecast <- forecast(auto_arima_model, h = 10)
print(auto_arima_forecast)

# Visualize forecast for auto.arima model
plot(auto_arima_forecast, main = "Auto ARIMA Forecast", xlab = "Year", ylab = "FDI (% of GDP)")
lines(auto_arima_forecast$mean, col = "blue")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)


checkresiduals(auto_arima_forecast)
##############################################################################################

#OBJECTIVE 2
#Select the Indicators for This Objective
data_obj_2 <- data[, c("Country.Name", "Year", "Employment_To_Population_Ratio", 
                       "Foreign_Direct_Investment_Net_Inflows_Of_GDP", 
                       "Foreign_Direct_Investment_Net_Inflows_BoP_Current",
                       "Foreign_Direct_Investment_Net_Outflows_Of_GDP",
                       "Foreign_Direct_Investment_Net_Outflows_BoP_Current")]

##############################################################################################
# Descriptive Analysis
# Group the data by Country.Name and calculate descriptive statistics
descriptive_stats_developing2 <- developing_data %>%
  group_by(Country.Name) %>%
  summarize(
    Mean_Employment_Ratio = mean(Employment_To_Population_Ratio),
    Median_Employment_Ratio = median(Employment_To_Population_Ratio),
    Mode_Employment_Ratio = as.numeric(names(which.max(table(Employment_To_Population_Ratio)))),
    Skewness_Employment_Ratio = moments::skewness(Employment_To_Population_Ratio),
    Kurtosis_Employment_Ratio = moments::kurtosis(Employment_To_Population_Ratio),
    SD_Employment_Ratio = sd(Employment_To_Population_Ratio),
    Mean_FDI_Net_Inflows_GDP = mean(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Median_FDI_Net_Inflows_GDP = median(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Mode_FDI_Net_Inflows_GDP = as.numeric(names(which.max(table(Foreign_Direct_Investment_Net_Inflows_Of_GDP)))),
    Skewness_FDI_Net_Inflows_GDP = moments::skewness(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Kurtosis_FDI_Net_Inflows_GDP = moments::kurtosis(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    SD_FDI_Net_Inflows_GDP = sd(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Mean_FDI_Net_Outflows_GDP = mean(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    Median_FDI_Net_Outflows_GDP = median(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    Mode_FDI_Net_Outflows_GDP = as.numeric(names(which.max(table(Foreign_Direct_Investment_Net_Outflows_Of_GDP)))),
    Skewness_FDI_Net_Outflows_GDP = moments::skewness(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    Kurtosis_FDI_Net_Outflows_GDP = moments::kurtosis(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    SD_FDI_Net_Outflows_GDP = sd(Foreign_Direct_Investment_Net_Outflows_Of_GDP)
  )

# View the descriptive statistics
View(descriptive_stats_developing2)

descriptive_stats_developed2 <- developed_data %>%
  group_by(Country.Name) %>%
  summarize(
    Mean_Employment_Ratio = mean(Employment_To_Population_Ratio),
    Median_Employment_Ratio = median(Employment_To_Population_Ratio),
    Mode_Employment_Ratio = as.numeric(names(which.max(table(Employment_To_Population_Ratio)))),
    Skewness_Employment_Ratio = moments::skewness(Employment_To_Population_Ratio),
    Kurtosis_Employment_Ratio = moments::kurtosis(Employment_To_Population_Ratio),
    SD_Employment_Ratio = sd(Employment_To_Population_Ratio),
    Mean_FDI_Net_Inflows_GDP = mean(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Median_FDI_Net_Inflows_GDP = median(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Mode_FDI_Net_Inflows_GDP = as.numeric(names(which.max(table(Foreign_Direct_Investment_Net_Inflows_Of_GDP)))),
    Skewness_FDI_Net_Inflows_GDP = moments::skewness(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Kurtosis_FDI_Net_Inflows_GDP = moments::kurtosis(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    SD_FDI_Net_Inflows_GDP = sd(Foreign_Direct_Investment_Net_Inflows_Of_GDP),
    Mean_FDI_Net_Outflows_GDP = mean(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    Median_FDI_Net_Outflows_GDP = median(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    Mode_FDI_Net_Outflows_GDP = as.numeric(names(which.max(table(Foreign_Direct_Investment_Net_Outflows_Of_GDP)))),
    Skewness_FDI_Net_Outflows_GDP = moments::skewness(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    Kurtosis_FDI_Net_Outflows_GDP = moments::kurtosis(Foreign_Direct_Investment_Net_Outflows_Of_GDP),
    SD_FDI_Net_Outflows_GDP = sd(Foreign_Direct_Investment_Net_Outflows_Of_GDP)
  )

# View the descriptive statistics
View(descriptive_stats_developed2)

# Calculate the correlation matrix
correlation_matrix_2 <- cor(data_obj_2[, c(3:7)])

# Create correlation plot
corrplot(correlation_matrix_2, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

############################################################################################################

#OBJECTIVE 3
#Select the Indicators for This Objective

data_obj_3 <- data[, c("Country.Name", "Year", "Statistical_Performance_Indicators_SPI_Pillar_3", 
                       "Import_Volume_Index", 
                       "Export_Volume_Index",
                       "Research_And_Development_Expenditure")]

# Descriptive Analysis for Developing Countries
descriptive_stats_developing3 <- developing_data %>%
  group_by(Country.Name) %>%
  summarize(
    Mean_SPI_Pillar_3 = mean(Statistical_Performance_Indicators_SPI_Pillar_3),
    Median_SPI_Pillar_3 = median(Statistical_Performance_Indicators_SPI_Pillar_3),
    Mode_SPI_Pillar_3 = as.numeric(names(which.max(table(Statistical_Performance_Indicators_SPI_Pillar_3)))),
    Skewness_SPI_Pillar_3 = moments::skewness(Statistical_Performance_Indicators_SPI_Pillar_3),
    Kurtosis_SPI_Pillar_3 = moments::kurtosis(Statistical_Performance_Indicators_SPI_Pillar_3),
    SD_SPI_Pillar_3 = sd(Statistical_Performance_Indicators_SPI_Pillar_3),
    Mean_Import_Volume = mean(Import_Volume_Index),
    Median_Import_Volume = median(Import_Volume_Index),
    Mode_Import_Volume = as.numeric(names(which.max(table(Import_Volume_Index)))),
    Skewness_Import_Volume = moments::skewness(Import_Volume_Index),
    Kurtosis_Import_Volume = moments::kurtosis(Import_Volume_Index),
    SD_Import_Volume = sd(Import_Volume_Index),
    Mean_Export_Volume = mean(Export_Volume_Index),
    Median_Export_Volume = median(Export_Volume_Index),
    Mode_Export_Volume = as.numeric(names(which.max(table(Export_Volume_Index)))),
    Skewness_Export_Volume = moments::skewness(Export_Volume_Index),
    Kurtosis_Export_Volume = moments::kurtosis(Export_Volume_Index),
    SD_Export_Volume = sd(Export_Volume_Index),
    Mean_RnD_Expenditure = mean(Research_And_Development_Expenditure),
    Median_RnD_Expenditure = median(Research_And_Development_Expenditure),
    Mode_RnD_Expenditure = as.numeric(names(which.max(table(Research_And_Development_Expenditure)))),
    Skewness_RnD_Expenditure = moments::skewness(Research_And_Development_Expenditure),
    Kurtosis_RnD_Expenditure = moments::kurtosis(Research_And_Development_Expenditure),
    SD_RnD_Expenditure = sd(Research_And_Development_Expenditure)
  )

View(descriptive_stats_developing3)

# Descriptive Analysis for Developed Countries
descriptive_stats_developed3 <- developed_data %>%
  group_by(Country.Name) %>%
  summarize(
    Mean_SPI_Pillar_3 = mean(Statistical_Performance_Indicators_SPI_Pillar_3),
    Median_SPI_Pillar_3 = median(Statistical_Performance_Indicators_SPI_Pillar_3),
    Mode_SPI_Pillar_3 = as.numeric(names(which.max(table(Statistical_Performance_Indicators_SPI_Pillar_3)))),
    Skewness_SPI_Pillar_3 = moments::skewness(Statistical_Performance_Indicators_SPI_Pillar_3),
    Kurtosis_SPI_Pillar_3 = moments::kurtosis(Statistical_Performance_Indicators_SPI_Pillar_3),
    SD_SPI_Pillar_3 = sd(Statistical_Performance_Indicators_SPI_Pillar_3),
    Mean_Import_Volume = mean(Import_Volume_Index),
    Median_Import_Volume = median(Import_Volume_Index),
    Mode_Import_Volume = as.numeric(names(which.max(table(Import_Volume_Index)))),
    Skewness_Import_Volume = moments::skewness(Import_Volume_Index),
    Kurtosis_Import_Volume = moments::kurtosis(Import_Volume_Index),
    SD_Import_Volume = sd(Import_Volume_Index),
    Mean_Export_Volume = mean(Export_Volume_Index),
    Median_Export_Volume = median(Export_Volume_Index),
    Mode_Export_Volume = as.numeric(names(which.max(table(Export_Volume_Index)))),
    Skewness_Export_Volume = moments::skewness(Export_Volume_Index),
    Kurtosis_Export_Volume = moments::kurtosis(Export_Volume_Index),
    SD_Export_Volume = sd(Export_Volume_Index),
    Mean_RnD_Expenditure = mean(Research_And_Development_Expenditure),
    Median_RnD_Expenditure = median(Research_And_Development_Expenditure),
    Mode_RnD_Expenditure = as.numeric(names(which.max(table(Research_And_Development_Expenditure)))),
    Skewness_RnD_Expenditure = moments::skewness(Research_And_Development_Expenditure),
    Kurtosis_RnD_Expenditure = moments::kurtosis(Research_And_Development_Expenditure),
    SD_RnD_Expenditure = sd(Research_And_Development_Expenditure)
  )

View(descriptive_stats_developed3)

# Calculate correlation matrix:
correlation_matrix_3 <- cor(data[, c("Statistical_Performance_Indicators_SPI_Pillar_3", 
                                   "Import_Volume_Index", 
                                   "Export_Volume_Index",
                                   "Research_And_Development_Expenditure")],
                          use = "complete.obs")

# Create correlation plot
corrplot(correlation_matrix_3, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")