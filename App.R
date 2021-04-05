library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

# Load in datasets and order if required
# Order by average combined math & RLA score
race = read_csv("./data/race.csv") %>% 
  arrange(desc(Average_Score)) %>% 
  mutate_at(vars(State), list(~factor(., levels=unique(.))))

# Order by average combined math & RLA score
gender = read_csv("./data/gender.csv") %>% 
  arrange(desc(Average_Score)) %>% 
  mutate_at(vars(State), list(~factor(., levels=unique(.))))

socioeconomic = read_csv("./data/socioeconomic.csv")

state_data = read_csv("./data/state_data.csv")

# Put two plots side-by-side with the inputs on the bottom
ui <- fluidPage(
  
  splitLayout(
    plotlyOutput(outputId = "mapplot"),
    
    plotlyOutput(outputId = "scatterplot")
  ),
  
  splitLayout(
    radioButtons(inputId = "test",
                 label = "Test:",
                 choices = c("Math", "RLA")
    ),
    
    radioButtons(inputId = "plot",
                label = "Factor",
                choices = c("Race", "Gender", "Socioeconomic")
    )
  )
)

server <- function(input, output) {
  
  # Create a choropleth map of the 50 states
  output$mapplot <- renderPlotly({
    # Get geo encoding
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    
    # Plot math scores
    if (input$test == "Math") {
      # Fill in map with math scores
      plot_geo() %>%
        add_trace(
          data = state_data,
          z = state_data$gcs_mn_avg_mth_ol,
          colorscale="Viridis",
          zmin = 4.47,
          zmax = 6.73,
          # Create tooltip
          text = paste("%White:", state_data$perwht,
                       "\n%Hispanic:", state_data$perhsp,
                       "\n%Black:", state_data$perblk,
                       "\n%Asian:", state_data$perasn,
                       "\nSES:", state_data$sesavgall),
          span = I(0),
          locations = state_data$stateabb, 
          locationmode = 'USA-states'
        ) %>%
        config(displayModeBar = FALSE) %>% 
        layout(geo = g, dragmode = FALSE, xaxis = list(fixedrange = TRUE),
               yaxis = list(fixedrange = TRUE))
    # Plot RLA scores
    } else {
      # Fill in map with RLA scores
      plot_geo() %>%
        add_trace(
          data = state_data,
          z = state_data$gcs_mn_avg_rla_ol,
          colorscale="Viridis",
          zmin = 4.47,
          zmax = 6.73,
          # Create tooltip
          text = paste("%White:", state_data$perwht,
                       "\n%Hispanic:", state_data$perhsp,
                       "\n%Black:", state_data$perblk,
                       "\n%Asian:", state_data$perasn,
                       "\nSES:", state_data$sesavgall),
          span = I(0),
          locations = state_data$stateabb, 
          locationmode = 'USA-states'
        ) %>%
        config(displayModeBar = FALSE) %>% 
        layout(geo = g, dragmode = FALSE, xaxis = list(fixedrange = TRUE),
               yaxis = list(fixedrange = TRUE))
    }
  })
  
  # Create the scatterplot of test scores
  output$scatterplot <- renderPlotly({
    
    # Plot scores by race
    if (input$plot == "Race") {
      # Plot math scores
      if (input$test == "Math") {
        plot = ggplot(race, aes(x = Math_Score, y = State, col = Race)) + 
          xlim(2.929, 9.03) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal()
      # Plot RLA scores
      } else {
        plot = ggplot(race, aes(x = RLA_Score, y = State, col = Race)) +
          xlim(2.929, 9.03) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal()
      }
    # Plot scores by Gender
    } else if(input$plot == "Gender") {
      # Plot math scores
      if (input$test == "Math") {
        plot = ggplot(gender, aes(x = Math_Score, y = State, col = Gender)) +
          xlim(4.022, 7.241) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal()
      # Plot RLA scores
      } else {
        plot = ggplot(gender, aes(x = RLA_Score, y = State, col = Gender)) +
          xlim(4.022, 7.241) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal()
      }
    # Plot scores by socioeconomic status
    } else {
      plot = ggplot(socioeconomic, aes(x = Socioeconomic_Status, y = Score, name = State, col = Test)) +
        geom_point(alpha = 0.6, size = 5) +
        geom_hline(yintercept = 5.5, alpha = 0.5, col = "red") +
        theme_minimal() +
        labs(
          x = "Average Socioeconomic Status",
          y = "Average Test Score",
          color = "Test")
    }
    
    ggplotly(plot, height = 400) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
}

shinyApp(ui = ui, server = server)
