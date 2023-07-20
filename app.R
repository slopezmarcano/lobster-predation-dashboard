library(shiny)
library(shinydashboard)
library(arrow)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Event ID Dashboard"),
  dashboardSidebar(
    # A select input to select the event_id
    selectInput("event_id", "Select Event ID", choices = c("1_N1","1_N11","1_N12","1_N13","1_N14","1_N15","1_N4","1_N5","1_N6","1_N7","1_N8","1_N9","2_N1","2_N10",
    "2_N11","2_N12","2_N13","2_N14","2_N15","2_N3","2_N4","2_N5","2_N6","2_N8","2_N9","3_N1","3_N10","3_N11","3_N13","3_N15","3_N3","3_N4","3_N5","3_N6","3_N7","3_N8","3_N9",
    "4_N1","4_N10", "4_N12", "4_N13", "4_N15", "4_N3", "4_N4", "4_N5", "4_N6","4_N7","4_N8","4_N9","5_N10", "5_N11", "5_N12", "5_N13","5_N14","5_N15","5_N2","5_N4","5_N5", "7_N10"))
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("count_plot", height = 300)),
      box(plotOutput("motif_count_plot", height = 300))
    ),
    plotOutput("interaction_plot", height = 600)
  )
)

server <- function(input, output) {
  source('scripts/final_functions.R')
  
  # Create a reactive expression to filter data based on the selected event_id
  filtered_data <- reactive({
    req(input$event_id) # Ensure an event_id is selected before proceeding
    
    # Read the data with the selected event_ID, establish the correct frames, select important columns
      data1 <- read_and_join_all_parquet_files('/Users/s2985905/Dropbox/GithubRepos/utas-lobster-predation/data/merged') %>%
              filter(event_ID==input$event_id) %>%
              corrected_frames() %>%
              select(event_ID, category_id, correct_frame, cx, cy) 

      # Determine the min and max interaction frames for the selected event_ID
      frames <- data1 %>%
              group_by(event_ID, correct_frame) %>%
              filter(all(c(2, 0) %in% category_id)) %>%
              select(-cx, -cy, -category_id) %>%
              distinct() %>%
              ungroup() %>%
              mutate(frame_diff = correct_frame - lag(correct_frame)) %>%
              mutate(frame_diff = ifelse(is.na(frame_diff), 0, frame_diff)) %>%
              mutate(group = cumsum(frame_diff > 100)) %>%
              group_by(event_ID, group) %>%
              summarise(min_frame = min(correct_frame), max_frame = max(correct_frame))

      # For all interaction groups or motifs in the selected event_ID, establish a consistent time sequence.
      data3 <- left_join(data1, frames, by = "event_ID", multiple = "all") %>%
        filter(between(correct_frame, min_frame, max_frame)) %>%
        select(-min_frame, -max_frame) %>%
        group_by(event_ID, group, subgroup = round(correct_frame / n(), 1)) %>%
        add_count() %>%
        ungroup() %>%
        group_by(event_ID, group) %>%
        mutate(correct_subgroup = dense_rank(subgroup) - 1)
        

    return(data3)
  })
  
  # Create the plot based on the reactive filtered data
  output$interaction_plot <- renderPlot({
    # Access the filtered data using filtered_data()
    filtered_data <- filtered_data()
    
    # Perform ggplot and plot the cx and cy coordinates, color by correct_subgroup, and shape by category_id
    ggplot(filtered_data %>% filter(n >=20), aes(x = cx, y = cy, color = correct_subgroup, shape = as.factor(category_id))) +
    geom_point(
    size = 4,
    alpha = 1) +
    scale_color_gradient2(low = "#c50404", mid="#ffc251", high = "#1d891d") +
    #theme_minimal()+
    facet_wrap(~ group, ncol = 4)+
    #scale_x_continuous(limits = c(min(data4$cx, na.rm = TRUE), max(data4$cx, na.rm = TRUE))) +
    #scale_y_continuous(limits = c(min(data4$cy, na.rm = TRUE), max(data4$cy, na.rm = TRUE))) +
    labs(title= "Behavioural Motifs for the Event_ID", shape='Species', color = "Time Sequence") +
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    scale_shape_manual(values = c(0, 2), labels = c("Urchin", "Lobster"))
  })

output$count_plot <- renderPlot({
  # Access the filtered data using filtered_data()
  filtered_data <- filtered_data()

  # Perform ggplot and show the count of each category_id for the selected event_ID across the correct_frame
  ggplot(filtered_data %>% group_by(category_id, correct_frame) %>% count() %>% filter(category_id==0), aes(x = correct_frame, y = n, color = as.factor(category_id))) +
  geom_line(size = 2, position = position_dodge(1)) +
  labs(title= "Urchin Counts Across Frames of the Event_ID" , color = "Species") +
  xlab("Frame Number") +
  ylab("Species") +
  scale_y_continuous(limits = c(0,3))+
  scale_color_manual(values = c("#037563"), labels = c("Urchin"))
})

output$motif_count_plot <- renderPlot({
  # Access the filtered data using filtered_data()
  filtered_data <- filtered_data()

  # Perform ggplot and show the count of frames for each motif in the selected event_ID
  ggplot(filtered_data %>% group_by(group) %>% count(), aes(x = factor(group), y = n)) +
  geom_segment(aes(x = factor(group), xend = factor(group), y = 0, yend = n), color = "black") +
  geom_point(size = 4, color = "black") +
  labs(title = "Number of Frames Per Motif", color = "Species") +
  xlab("Motif Number") +
  ylab("Number of Frames") +
  coord_flip()
})

}

shinyApp(ui, server)
