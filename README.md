### Data-Analysis-on-Crop-Yield

This dataset contains agricultural data for 1,000,000 samples aimed at predicting crop yield (in tons per hectare) based on various factors. The dataset can be used for regression tasks in machine learning, especially for predicting crop productivity.

-   Region: The geographical region where the crop is grown (North, East, South, West).

-   Soil_Type: The type of soil in which the crop is planted (Clay, Sandy, Loam, Silt, Peaty, Chalky).

-   Crop: The type of crop grown (Wheat, Rice, Maize, Barley, Soybean, Cotton).

-   Rainfall_mm: The amount of rainfall received in millimeters during the crop growth period.

-   Temperature_Celsius: The average temperature during the crop growth period, measured in degrees Celsius.

-   Fertilizer_Used: Indicates whether fertilizer was applied (True = Yes, False = No).

-   Irrigation_Used: Indicates whether irrigation was used during the crop growth period (True = Yes, False = No).

-   Weather_Condition: The predominant weather condition during the growing season (Sunny, Rainy, Cloudy).

-   Days_to_Harvest: The number of days taken for the crop to be harvested after planting.

-   Yield_tons_per_hectare: The total crop yield produced, measured in tons per hectare.

### Data Preparation

The data was checked for duplicates and for missing values, which there were none.

The objectives were defined below, followed by the formulation of analytic questions to guide the analysis process.

Objectives:

-   Crop Yield Analysis by Soil Type: Identify which soil type had the highest crop yield.

-   Impact of Fertilizer Usage on Crop Yield: Analyse the impact of fertilizer application on crop yield.

-   Effect of Weather Conditions on Crop Yield: Understand how different weather conditions affect crop yield.

-   Influence of Irrigation on Crop Yield: Investigate how irrigation affects crop yield.

-   Seasonal and Regional Variations in Crop Yield: Examine the influence of geographical regions and climate factors on crop yield.

Analytic Questions:

-   What weather conditions (Sunny, Rainy, Cloudy) are most favourable for higher crop yields?

-   Is there a relationship between weather conditions and specific crop types or regions in terms of yield?

-   Do rainy conditions result in a higher or lower yield compared to sunny or cloudy weather?

-   Does the use of irrigation increase the crop yield compared to crops that were not irrigated?

-   Does the use of fertilizer application increase the crop yield compared to crops that were not fertilized?

-   In which regions does irrigation have the most significant effect on yield?

-   How does the impact of irrigation vary across different crop types and soil types?

-   How do crop yields differ by region (North, East, South, West)?

-   What is the relationship between temperature and rainfall with respect to crop yield in different regions?

-   Are certain crops more successful in specific regions and if so, what factors (e.g., temperature, soil type) contribute to this?

### Data Analysis

**What weather conditions (Sunny, Rainy, Cloudy) are most favourable for higher crop yields?**

![Image](https://github.com/user-attachments/assets/c0b83509-1423-43b9-8eab-b6b2dd361b94)

Based on the chart, the total yield for these weather conditions ranges from 0 to approximately 10 tons per hectare. Since the yields for cloudy, rainy and sunny days are almost identical, it seems that the weather conditions didn’t dramatically influence the yield. This could suggest that the crops are adaptable to different weather patterns or that other factors (like soil quality, irrigation, or farming practices) may have a stronger influence on yield than the weather itself.

The fact that all weather conditions show similar yields also points to the possibility that none of the weather conditions had a major negative or positive effect on crop growth during this period. This could be a good sign of overall favourable agricultural conditions.

If this pattern holds across different years or crops, farmers might feel confident in planting regardless of whether the forecast predicts sunny, cloudy or rainy days.

**Is there a relationship between weather conditions and specific crop types or regions in terms of yield?**

![Image](https://github.com/user-attachments/assets/6cfe4126-e87d-4ee7-bbeb-253d30c2b03b)

Barley seems to perform best under cloudy conditions, with other weather conditions (rainy and sunny) yielding slightly lower but still reasonable results. This could indicate that barley is more suited to cooler, overcast conditions, but still does well under a range of weather.

Maize appears to be relatively unaffected by different weather conditions, though sunny weather seems to have a slight positive effect. This suggests maize is resilient and capable of producing stable yields regardless of weather changes.

Rice appears to benefit more from rainy conditions, which is consistent with the fact that rice is typically grown in areas with ample water. The higher yield under rainy weather indicates that water availability is a key factor for optimal rice production.

Like rice, soy beans also seem to benefit from more water (rain), leading to higher yields during rainy weather. This indicates that adequate moisture is essential for maximizing soy bean growth.

Similar to rice and soy beans, wheat seems to thrive with more moisture from rain.

**Do rainy conditions result in a higher or lower yield compared to sunny or cloudy weather?**

![Image](https://github.com/user-attachments/assets/4dda66d1-fca0-4dd0-a1f2-cb83dee2ebca)

The bar chart illustrates the impact of irrigation on crop yield across different crop types. The "False" category represents crops where no irrigation was used, while the "True" category indicates that irrigation was applied, highlighted in blue.

In all crop types shown, the use of irrigation (True) is associated with a noticeable increase in crop yield, as compared to the "False" category, where no irrigation was utilized.

**In which regions does irrigation have the most significant effect on yield?**

![Image](https://github.com/user-attachments/assets/05862c5a-a83c-4ab4-a3aa-133913810237)

The bar chart illustrates the impact of irrigation on crop yield across different regions. The "True" category indicates that irrigation was applied, highlighted in blue. In all regions, the use of irrigation shows a significant positive effect on crop yield, with yields noticeably higher.

**How does the impact of irrigation vary across different crop types and soil types?**

![Image](https://github.com/user-attachments/assets/7d6f5233-d7dd-4b05-b804-6370bea88c57)

**Does the use of fertilizer application increase the crop yield compared to crops that were not fertilized?**

![Image](https://github.com/user-attachments/assets/8639663d-2b01-4e67-8cf6-e3f90e0e6e33)

The bar chart illustrates the impact of fertilizer on crop yield across different crop types. The "False" category represents crops where no fertilizer was applied, while the "True" category indicates that fertilizer was applied, highlighted in pink.

In all crop types shown, the application of fertilizer (True) is associated with a noticeable increase in crop yield, as compared to the "False" category, where no fertilizer was applied.

**How do crop yields differ by region (North, East, South, West)?**

![Image](https://github.com/user-attachments/assets/5b2031e0-0739-4197-ae66-a2a578e730b1)

The chart above illustrates how crop yields differ by region (East, North, South, West). Each region is represented by distinct shades of green. Upon examining the plot, it becomes evident that the North, South and West regions exhibit nearly identical crop yields in tons per hectare.

The East region, while still performing reasonably well, shows a slightly lower yield when compared to the other regions, though this difference is not statistically significant.

**What is the relationship between temperature and rainfall with respect to crop yield in different regions?**

![Image](https://github.com/user-attachments/assets/5b2031e0-0739-4197-ae66-a2a578e730b1)

Temperature and Rainfall (0): The correlation between temperature and rainfall is almost zero (0.0001), which indicates that there's virtually no linear relationship between these two variables. As temperature increases or decreases, rainfall doesn't seem to follow any clear pattern.

Temperature and Yield (0.09): The correlation between temperature and crop yield is weakly positive (0.09). This means that there is a very slight positive relationship between temperature and crop yield, although this correlation is not strong enough to draw definitive conclusions.

Rainfall and Yield (0.8): The correlation between rainfall and crop yield is strong (0.8), suggesting that rainfall has a significant positive effect on crop yield. As rainfall increases, crop yield tends to increase as well, which aligns with the general understanding that more water supports better crop growth.

**Are certain crops more successful in specific regions and if so, what factors (e.g., temperature, soil type) contribute to this?**

![Image](https://github.com/user-attachments/assets/7e25bd9c-b500-4430-b574-c50710d34859)

The chart shows how different crop types perform in various regions:

-   Wheat: Most successful in the East region, where the yield is higher compared to other regions.

-   Barley: Performs best in the North region, with higher yields there.

-   Rice: Shows a significant yield increase in the South, indicating it thrives in this region's conditions.

-   Maize: Exhibits the highest yields in the West region, suggesting it grows particularly well there.

**Further Analysis of Temperature and Soil Type:**\*

Further analysis was conducted to assess the impact of temperature and soil type on crop yield, which might explain why certain crops perform better in specific regions.

**Temperature (temperature_celsius):** The coefficient for temperature is 0.0201, with a very low p-value (\< 2e-16). This indicates a statistically significant positive relationship between temperature and crop yield. For every 1°C increase in temperature, the crop yield increases by approximately 0.02 tons per hectare.

The coefficients for soil types (e.g., Clay, Loam, Peaty, Sandy, Silt) are all not statistically significant, as their p-values are much greater than the typical threshold (0.05):

**- Soil_typeClay:** p-value = 0.1844 (not significant)

**- Soil_typeLoam:** p-value = 0.8228 (not significant)

**- Soil_typePeaty:** p-value = 0.6859 (not significant)

**- Soil_typeSandy:** p-value = 0.4410 (not significant)

**- Soil_typeSilt:** p-value = 0.4448 (not significant)

This suggests that soil type, in this model, does not have a significant impact on crop yield after accounting for temperature and region.

**RegionNorth:** The coefficient for the North region is 0.0087, and the p-value is 0.0704, which is just below the common significance threshold of 0.05. This indicates a marginally significant positive effect on crop yield compared to the reference region (East or South).

**RegionSouth and RegionWest:** Both of these regions have p-values greater than 0.05, indicating that the effects of these regions on crop yield are not statistically significant.

Insights:

-   Temperature has a significant positive impact on crop yield, with higher temperatures associated with higher yields, albeit with a small effect size.

-   Soil Type: Soil type did not show a significant effect on yield in this analysis. While soil characteristics often play an essential role in crop production, other factors (like irrigation, temperature and region) may have a more dominant effect on yield in this case.

-   Region has a small effect, but only the North region shows a marginally significant positive effect on yield.

### Predictive Model

![Image](https://github.com/user-attachments/assets/18effb91-4cbe-41ef-9359-e6b6badd45d5)

-   **Root Node**: The **root node** of the decision tree is the first split and shows the most influential feature in predicting crop yield. In this model, **rainfall** is at the root, meaning it has the **highest information gain** and is the most important factor in determining crop yield.

-   **Subsequent Splits**: The tree continues to split the data further based on other features (like **soil type**, **temperature**, etc.). These subsequent splits provide additional insights into how different combinations of features influence the prediction of crop yield.

-   **Feature Interactions**: The nodes below the root represent interactions between features. Each split shows how different values or conditions of features impact the crop yield prediction, highlighting the **importance of feature interactions** in determining the final prediction.

-   **Decision Making**: The model's decision-making process is based on reducing variance or error at each split. The tree prioritizes the feature that results in the best separation of data, leading to more accurate predictions of crop yield.

**Model Performance**

**R-squared:**

-   **R-squared = 0.8689** means that about **87% of the variability in the target variable (crop yield)** is explained by the features in the decision tree model.

-   The remaining **13%** of the variability could be due to other factors not included in the model, noise in the data, or unexplained relationships.

-   An **R-squared value of 0.87** is typically considered a **good fit**, especially in real-world applications where perfect predictions are rare.

-   The model appears to be doing well in terms of predicting crop yield based on the features provided (like rainfall, temperature, soil type etc)

**Root Mean Squared Error**

-   **RMSE (Root Mean Squared Error)** measures the average magnitude of the errors in the predictions. It gives an idea of how far off the predictions are from the actual values.

-   A **RMSE of 0.613** means that, on average, the predicted crop yield is off by about **0.613 tons per hectare** from the actual crop yield.

-   **Lower RMSE values** indicate that the model is making more accurate predictions.

-   An RMSE of **0.613** could be considered quite good, depending on the range of crop yield in your dataset. If the crop yield varies greatly (e.g., ranging from 0 to 10 tons per hectare), then an RMSE of 0.613 might be a reasonable error.

**Conclusion:**

The analysis of crop yield data across different regions and crop types reveals some valuable insights:

**Regional Crop Preferences:**

-   Wheat performs best in the East, where regional conditions likely favor its growth.

-   Barley yields the highest in the North, indicating it thrives in the conditions there.

-   Rice shows the most significant yield increase in the South, suggesting this region provides optimal conditions for its growth.

-   Maize achieves the highest yield in the West, highlighting regional differences in crop success.

**Temperature's Influence on Crop Yield:**

-   Temperature plays a significant role in increasing crop yield. Higher temperatures generally lead to higher yields, which might explain the regional differences in crop success. For instance, wheat's success in the East region could be due to more favourable temperature conditions.

**Soil Type's Impact:**

-   Soil type did not have a statistically significant effect on crop yield in this analysis. This suggests that while soil type is important, other factors such as temperature and regional conditions (like rainfall and irrigation) may have a stronger impact on crop yield.

**Irrigation's Impact:**

-   The use of irrigation shows a significant positive effect on crop yield, with yields noticeably higher.

**Fertilizer's Impact:**

-   The application of fertilizer (True) is associated with a noticeable increase in crop yield.

**Rainfall's impact:**

-   As rainfall increases, crop yield tends to increase as well, which aligns with the general understanding that more water supports better crop growth.

### source: <https://www.kaggle.com/datasets/samuelotiattakorah/agriculture-crop-yield/data>
