setwd("data-science-projects/Data-Analysis-On-Crop-Yield/")
cropData <- data.frame(read.csv("./00_raw_data/crop_yield.csv"))

# clean the columns / variable names
library(janitor)
cropData <- cropData %>% clean_names()
View(cropData)

# check for duplicates
sum(duplicated(cropData))

# check for NA's
colnames(cropData)[apply(cropData, 2, anyNA)]

# check the structure of the data
str(cropData)

# summary of the data 
summary(cropData$yield_tons_per_hectare)
# The summary gives an indication to the distribution of yield in tons
# the dataset
# The yield per ton ranges from -1.148 to 9.963
# 1st Quartile: 25% of the yield is lower than 3.418 tons per hectare
# 3rd Quartile: 75% of the yield is lower than 5.879 tons per hectare
# The data is negatively skewed since the mean(4.649) is less than the
# median(4.652)

# exploratory analysis
library(ggplot2)
cropData %>% ggplot(aes(x = crop, fill = crop)) + geom_bar()+
  ylim(0,200000)+
  labs(title = "A Bar plot of crops grown")

# Data visualization of average yield by crop
library(dplyr)
average_yield <- cropData %>% 
  group_by(crop) %>% 
  summarise(average_yield = mean(yield_tons_per_hectare, na.rm=TRUE))

ggplot(average_yield, aes(x = crop, y = average_yield, fill = crop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%s tons",format(round(average_yield, 3),
                                       big.mark=","))),
            vjust = -0.5, size = 3.5) +
  ylim(0, 10)+
  labs(title = "Average Yield by Crop",
       x = "Crop",
       y = "Average Yield (tons per hectare)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The plot shows a bar chat depicting the average yield per tons per 
# hectare .
# Here is the breakdown of information: 
# Barley: 4.647 tons per hectare
# Cotton: 4.651 tons per hectare
# Maize: 4.641 tons per hectare
# Rice: 4.651 tons per hectare
# Soybean: 4.654 tons per hectare
# Wheat: 4.653 tons per hectare

# The observations made were : 
# Soybean had the highest yield of 4.654 tons per hectare
# Maize had the lowest yield recording 4.641 tons per hectare
# There is less significance jump between the crop yield

# Box plot to visualize the data between fertilizer and yield
cropData %>% ggplot(aes(x = fertilizer_used, y = yield_tons_per_hectare,
                        fill = fertilizer_used))+
  geom_boxplot() +
  labs("Box Plot of Crop Yield by Fertilizer Used")

# The box plot compares crops that used fertilizer, which is indicated
# by true or false
# The Y axis represents the yield in tons per hectare ranging from -1 to 10.0
# The two boxes represents the interquartile range for that fertilizer used
# No application of fertilizer (false) had a low  yield in tons
# The application of fertilizer (true) had a higher yield in tons which indicates
# that crops that were fertilized showed higher yield.
# There is a clear progression in yield with the use of fertilizer application


# Using bar plot
average_rainfall <- cropData %>% 
  group_by(region) %>% 
  summarise(average_rainfall = mean(rainfall_mm, na.rm = T))

 ggplot(average_rainfall, aes(x = region, y = average_rainfall,fill = region)) + 
   geom_bar(stat = "identity") +
   geom_text(aes(label = sprintf("%smm",format(round(average_rainfall, 3),
                                        big.mark=","))),
             vjust = -0.5, size = 3.5)+
   ylim(0, 900) +
   labs(title = "Average rainfall by region")+
   scale_fill_brewer(palette = "Blues")
 
 # The plot shows a bar chart depicting the average rainfall in each region
 # The observation made is as follows 
 # East Region recorded an average rainfall of 549.244mm
 # North Region recorded an average rainfall of 550.793mm
 # South Region recorded an average rainfall of 550.014mm
 # West Region recorded an average rainfall of 549.875mm
 # The North and South region recorded the highest average rainfall of 550mm
 # The lowest average rainfall recorded was the East region(549.244mm)
 # There is not much significance difference in the average rainfall
 
 


