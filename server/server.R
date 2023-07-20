# server/server.R
# Load the most common libraries used with suppressed startup messages
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(arrow))


server <- function(input, output) {
  # Source the functions
  source('scripts/final_functions.R')
  
  # Reactive expression to load and filter the data based on selected event_ID
  data_reactive <- reactive({
    # Read the data with the status complete processing, establish the correct frames, select important columns and filter those frames 
    # where there is a lobster and an urchin and Reshape the data from long to wide format
    data <- read_and_join_all_parquet_files('/Users/s2985905/Dropbox/GithubRepos/utas-lobster-predation/data/merged') %>%
      filter(event_ID == input$selected_event) %>%
      corrected_frames() %>%
      select(event_ID, category_id, correct_frame, cx, cy) %>%
      group_by(event_ID, correct_frame) %>%
      filter(all(c(2, 0) %in% category_id)) %>%
      pivot_wider(names_from = category_id, values_from = c(cx, cy), values_fn = list) %>%
      unnest(cols = c(cx_2, cy_2, cx_0, cy_0))
    
    return(data)
  })

  # Create the boxplot based on the reactive data
  output$boxplot_correct_frames <- renderPlot({
    data <- data_reactive()
    
    # Generate the boxplot of the number of correct_frames per category_id
    ggplot(data, aes(x = as.factor(category_id), y = correct_frame)) +
      geom_boxplot() +
      labs(x = "Category ID", y = "Number of Correct Frames", title = "Boxplot of Correct Frames per Category ID")
  })

  # Observe the selected event_ID in the sidebar and update the plot accordingly
  observe({
    data <- data_reactive()
    
    # Update the boxplot based on the selected event_ID
    output$boxplot_correct_frames <- renderPlot({
      data_filtered <- data %>%
        filter(event_ID == input$selected_event)
      
      ggplot(data_filtered, aes(x = as.factor(category_id), y = correct_frame)) +
        geom_boxplot() +
        labs(x = "Category ID", y = "Number of Correct Frames", title = "Boxplot of Correct Frames per Category ID")
    })
  })
}
