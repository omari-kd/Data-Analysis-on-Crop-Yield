# load data
data <- data.frame(read.csv("./00_raw_data/crop_yield.csv"))
View(data)

# data preprocessing
# clean the data columns using janitor
library(janitor)
data <- data %>% clean_names()

# check for duplicates
sum(duplicated(data))

# check for Na's
colnames(data)[apply(data, 2, anyNA)]

# check the data structure
str(data)
 
# summary of the data
summary(data)

# exploratory data analysis

# Average yield by crop type
data %>% 
  dplyr::group_by(crop) %>%
  dplyr::summarize(avg_yield = mean(yield_tons_per_hectare))
# Maize has the lowest yield


# relationship between yield and other factors
library(ggplot2)
data %>% ggplot(aes(x = rainfall_mm, y = yield_tons_per_hectare
                    , color = crop)) + 
  geom_point() +
  geom_smooth(method = "lm")
# Higher rainfall is generally associated with higher yield


data %>% ggplot(aes(x=temperature_celsius, y = yield_tons_per_hectare,
                     color = crop)) +
  geom_point() + geom_smooth(method = "lm")
# the temperature shows mixed relationship some crops prefer
# warmer conditions whilst others do better in cooler conditions


# other factors
data %>% ggplot(aes(x=soil_type, y=yield_tons_per_hectare,
                    fill=soil_type)) + 
  geom_boxplot()

data %>% ggplot(aes(x=fertilizer_used, y=yield_tons_per_hectare,
                    fill=fertilizer_used)) + 
  geom_boxplot()

# fertilizer has a positive impact on yields

data %>% ggplot(aes(x=irrigation_used, y=yield_tons_per_hectare,
                    fill=irrigation_used)) + 
  geom_boxplot()

# irrigation has a positive impact on yields


# predictive modeling
model <- lm(yield_tons_per_hectare ~ rainfall_mm + 
              temperature_celsius + soil_type + 
              fertilizer_used + irrigation_used +
              days_to_harvest, data = data)
summary(model)

boxplot(yield_tons_per_hectare ~ crop, data=data)

# Check range of yields
range(data$yield_tons_per_hectare)

# Look at correlation between numeric variables
cor(data$rainfall_mm, data$yield_tons_per_hectare)
cor(data$temperature_celsius, data$yield_tons_per_hectare)


# library(randomForest)
rf_model <- randomForest(yield_tons_per_hectare ~ rainfall_mm + temperature_celsius + soil_type + crop + irrigation_used + fertilizer_used, data=data)
importance(rf_model)