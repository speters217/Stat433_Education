library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

# Load in datasets and order if required
# Order by average combined math & RLA score
race = read_csv("./data/race.csv") %>% 
  arrange(desc(Total_Score)) %>% 
  mutate_at(vars(State), list(~factor(., levels=unique(.))))

# Order by average combined math & RLA score
gender = read_csv("./data/gender.csv") %>% 
  arrange(desc(Total_Score)) %>% 
  mutate_at(vars(State), list(~factor(., levels=unique(.))))

socioeconomic = read_csv("./data/socioeconomic.csv")

state_data = read_csv("./data/state_data.csv")

# Put two plots side-by-side with the inputs on the bottom
ui <- fluidPage(
  
  HTML("<br><br><br>"),
  
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:inherit;",
      
      p("Select options to change the inputs of the plots."),
      
      radioButtons(inputId = "test",
                   label = "Test:",
                   choices = c("Math", "RLA")
      ),
      
      radioButtons(inputId = "plot",
                   label = "Factor",
                   choices = c("Race", "Gender", "Socioeconomic")
      )
    ),
    
    mainPanel(
      plotlyOutput(outputId = "mapplot"),
      
      p("States are filled by their average value for the selected test. Hover over a state to see its exact test 
        score as well as socioeconomic score and racial composition. Note that data for Native Americans were 
        removed due to insufficient state data."),
      
      plotlyOutput(outputId = "scatterplot"),
      
      HTML("<br><br><br><br><br><br>"),
      
      p("State test scores are plotted for the selected test and factor. Hover over a point 
        to see its exact values. Click on values in the legend to display data for only certain groups. 
        The red bar represents the expected average score of the standardized test (5.5).")
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
      z = state_data$gcs_mn_avg_mth_ol
    # Plot RLA scores
    } else {
      z = state_data$gcs_mn_avg_rla_ol
    }
    
    # Fill in map with desired scores
    plot_geo() %>%
      add_trace(
        data = state_data,
        z = z,
        colorscale="Viridis",
        zmin = 4.47,
        zmax = 6.73,
        # Create tooltip
        text = paste("SES: ", round(state_data$sesavgall, 3),
                     "\nWhite:\t", 100 * round(state_data$perwht, 3), "%",
                     "\nHspn:\t", 100 * round(state_data$perhsp, 3), "%",
                     "\nBlack:\t", 100 * round(state_data$perblk, 3), "%",
                     "\nAsian:\t", 100 * round(state_data$perasn, 3), "%",
                     sep = ""),
        span = I(0),
        locations = state_data$stateabb, 
        locationmode = 'USA-states'
      ) %>%
      config(displayModeBar = FALSE) %>% 
      layout(geo = g, dragmode = FALSE, xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE),
             title = "Scores, SES, and Racial Composition of States")
    
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
          theme_minimal() +
          labs(
            title = "Math Scores by State and Race"
          )
      # Plot RLA scores
      } else {
        plot = ggplot(race, aes(x = RLA_Score, y = State, col = Race)) +
          xlim(2.929, 9.03) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal() +
          labs(
            title = "RLA Scores by State and Race"
          )
      }
    # Plot scores by Gender
    } else if(input$plot == "Gender") {
      # Plot math scores
      if (input$test == "Math") {
        plot = ggplot(gender, aes(x = Math_Score, y = State, col = Gender)) +
          xlim(4.022, 7.241) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal() +
          labs(
            title = "Math Scores by State and Gender"
          )
      # Plot RLA scores
      } else {
        plot = ggplot(gender, aes(x = RLA_Score, y = State, col = Gender)) +
          xlim(4.022, 7.241) +
          geom_point(alpha = 0.6, size = 5) +
          geom_vline(xintercept = 5.5, alpha = 0.5, col = "red") +
          theme_minimal() +
          labs(
            title = "RLA Scores by State and Gender"
          )
      }
    # Plot scores by socioeconomic status
    } else {
      plot = ggplot(socioeconomic, aes(x = Socioeconomic_Status, y = Score, col = Test, name = State)) +
        geom_point(alpha = 0.6, size = 5) +
        geom_hline(yintercept = 5.5, alpha = 0.5, col = "red") +
        theme_minimal() +
        labs(
          title = "Socioeceonomic Score vs. Test Score by State",
          x = "Average Socioeconomic Status",
          y = "Average Test Score",
          color = "Test")
    }
    
    ggplotly(plot, height = 500, tooltip = c("x", "y", "State")) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
}

shinyApp(ui = ui, server = server)
