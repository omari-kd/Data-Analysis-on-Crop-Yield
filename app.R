library(shiny)
library(rpart)
library(dplyr)


# Load trained model
tree_model <- readRDS("./05_model/tree_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("🌾 Crop Yield Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region", choices = c("North", "South", "East", "West")),
      selectInput("soil_type", "Soil Type", choices = c("Sandy", "Clay", "Loam", "Silt", "Peaty", "Chalky")),
      selectInput("crop", "Crop", choices = c("Wheat", "Maize", "Rice", "Cotton", "Soybean", "Barley")),
      numericInput("rainfall_mm", "Rainfall (mm)", 100, min = 0),
      numericInput("temperature_celsius", "Temperature (°C)", 25, min = -10, max = 50),
      
      # Updated input values to match training data
      selectInput("fertilizer_used", "Fertilizer Used", choices = c("True", "False")),  # Updated values
      selectInput("irrigation_used", "Irrigation Used", choices = c("True", "False")),  # Updated values
      
      selectInput("weather_condition", "Weather", choices = c("Sunny", "Rainy", "Cloudy")),
      numericInput("days_to_harvest", "Days to Harvest", 120, min = 30, max = 365),
      actionButton("predict_btn", "Predict")
    ),
    
    mainPanel(
      h3("Predicted Yield (tons/hectare):"),
      verbatimTextOutput("prediction")
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$predict_btn, {
    # Ensure that the factor levels match what was used during model training
    new_data <- data.frame(
      region = factor(input$region, levels = c("North", "South", "East", "West")),
      soil_type = factor(input$soil_type, levels = c("Sandy", "Clay", "Loam", "Silt", "Peaty", "Chalky")),
      crop = factor(input$crop, levels = c("Wheat", "Maize", "Rice", "Cotton", "Soybean", "Barley")),
      rainfall_mm = as.numeric(input$rainfall_mm),
      temperature_celsius = as.numeric(input$temperature_celsius),
      
      # Ensure correct factor levels for fertilizer and irrigation
      fertilizer_used = factor(input$fertilizer_used, levels = c("True", "False")),  # Updated levels
      irrigation_used = factor(input$irrigation_used, levels = c("True", "False")),  # Updated levels
      
      weather_condition = factor(input$weather_condition, levels = c("Sunny", "Rainy", "Cloudy")),
      days_to_harvest = as.numeric(input$days_to_harvest)
    )
    
    # Predict with the trained model
    prediction <- predict(tree_model, new_data)
    
    # Output the prediction
    output$prediction <- renderText({
      round(prediction, 2)
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)