#--Libraries--------------------------------------------------------------------#
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(tidyverse))
#suppressPackageStartupMessages(library(ggridges))

#-- UI -------------------------------------------------------------------------#
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "🦞Lobster Predation Behaviour"),
  dashboardSidebar(
    # Add a text box with key explanation
    div(style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px;",
        p("Welcome to the Rock Lobster Predation Behaviour Dashboard!"),
        #p("Use the sidebar menu to navigate between different sections."),
        p("Select a tank from the radio buttons and an event ID from the dropdown."),
        #p("Explore the data and visualizations for each section.")
    ),

    # add an about tab
    #sidebarMenu(
      #menuItem("About", tabName = "about", icon = icon("info-circle")),
      #menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar"))
    #),

    # Add a radio button for tank selection
    radioButtons("tank_selection", "Select Tank",
                 choices = c("Tank 1", "Tank 2", "Tank 3", "Tank 4", "Tank 5", "Tank 6", "Tank 7"),
                 selected = "Tank 1"
    ),
    selectInput("event_id", "Select Event ID", NULL)
  ),
  dashboardBody(
    # About tab content
    fluidRow(
            box(
              title = "About",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              #h1("💡About"),
              p("This research project focuses on understanding the predation behavior between southern rock lobsters and sea urchins in Tasmanian waters, 
                which are experiencing significant warming and species redistribution. The long-spined sea urchin, Centrostephanus rodgersii, 
                has extended its range in Tasmanian coastal waters, leading to over-grazing and unproductive barren habitats."),
              p("Rock lobsters are important predators of sea urchins, with the eastern rock lobster being a significant predator of C. rodgersii in its natural range.However, eastern rock lobsters are uncommon in Tasmania. This research aims to test the relative predation of urchins by the southern rock lobster."),
              p("This dashboard allows you to explore the data collected from the experiments and visualize the results."),
              p("Data collection + Science by @jesmith5 and Data Science by @slopezmarcano from University of Tasmania and Griffith University; Australia.")
              # Add an image for the "About" section that is located in the assets folder
              # img(src = "https://www.dpi.nsw.gov.au/__data/assets/image/0018/117540/lobster.jpg", height = 200, width = 200, align = "center")
            )
),
    # Add a tab for all the plots
            fluidRow(
              box(width = 12, title = 'Descriptive Statistics', collapsible = TRUE, status = "success",
              infoBoxOutput("max_urchin_count_output", width = 4),
              infoBoxOutput("median_urchin_count_output", width = 4),
              infoBoxOutput("possible_predation_output", width = 4))),
            fluidRow(
              column(width=12, h4("Predation Motifs in Event_ID")),
              box(collapsible = TRUE, status = "success",  title="Time when Motifs ocurred",  plotOutput("time_motif_plot", height = 300)),
              box(collapsible = TRUE, status = "success", title="Number of Motifs in Event_ID", plotOutput("motif_count_plot", height = 300))),
            fluidRow(
              column(width=12, h4("Behavioural Sequences Across Predation Motifs > 20 frames")),
              column(width=12, plotOutput("interaction_plot", height = 600)))))


#-- Server ---------------------------------------------------------------------#
server <- function(input, output, session) {

  # Observe the tank selection and update the event_id choices accordingly
  observe({
    selected_tank <- input$tank_selection
    event_ids <- switch(selected_tank,
      "Tank 1" = c("1_N1", "1_N4", "1_N5", "1_N6", "1_N7", "1_N8", "1_N9", "1_N11", "1_N12", "1_N13", "1_N14", "1_N15"),
      "Tank 2" = c("2_N1", "2_N3", "2_N4", "2_N5", "2_N6", "2_N8", "2_N9", "2_N10", "2_N11", "2_N12", "2_N13", "2_N14", "2_N15"),
      "Tank 3" = c("3_N1", "3_N3", "3_N4", "3_N5", "3_N6", "3_N7", "3_N8", "3_N9", "3_N10", "3_N11", "3_N13", "3_N15"),
      "Tank 4" = c("4_N1", "4_N3", "4_N4", "4_N5", "4_N6", "4_N7", "4_N8", "4_N9", "4_N10", "4_N12", "4_N13", "4_N15"),
      "Tank 5" = c("5_N2", "5_N4", "5_N5", "5_N6", "5_N7", "5_N8", "5_N9", "5_N10", "5_N11", "5_N12", "5_N13", "5_N14", "5_N15"),
      "Tank 6" = c("6_N3", "6_N4", "6_N5", "6_N6", "6_N7", "6_N8", "6_N9", "6_N10", "6_N11", "6_N12", "6_N14", "6_N15"), 
      "Tank 7" = c("7_N3", "7_N5","7_N6", "7_N7", "7_N9", "7_N10", "7_N11", "7_N12", "7_N13", "7_N14", "7_N15"))
    updateSelectInput(session, "event_id", choices = event_ids)
  })

  # Load the functions
  #source('scripts/final_functions.R')
  
  # Load the ggpplot themes
  #source('scripts/templates/yyy_theme_setup.R')
  
  # Suppress the dplyr summarise() messages
  options(dplyr.summarise.inform = FALSE)


  # Create a reactive expression to filter data based on the selected event_id
  filtered_data <- reactive({
    req(input$event_id) # Ensure an event_id is selected before proceeding
    
    # Read the data with the selected event_ID
      data1 <- read_parquet('www/data.parquet') %>%
              filter(event_ID==input$event_id)

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
      data3 <- data1 %>% 
        left_join(frames, by = "event_ID", relationship='many-to-many')  %>%
        filter(between(correct_frame, min_frame, max_frame)) %>%
        select(-min_frame, -max_frame) %>%
        group_by(event_ID, group, subgroup = round(correct_frame / n(), 1)) %>%
        add_count() %>%
        ungroup() %>%
        group_by(event_ID, group) %>%
        mutate(correct_subgroup = dense_rank(subgroup) - 1) %>%
        mutate(minute = correct_frame * 0.001389)
        

    return(data3)
  })
  
  # Create the plot based on the reactive filtered data
  output$interaction_plot <- renderPlot({
    ggplot(filtered_data() %>% filter(n >=20), aes(x = cx, y = cy, color = correct_subgroup, shape = as.factor(category_id))) +
    geom_point(
    size = 4,
    alpha = 1) +
    scale_color_gradient2(low = "#c50404", mid = "#e9c46a", high = "#2a9d8f") +
    theme_light()+
    facet_wrap(~ group, ncol = 4)+
    labs(shape='Species', color = "Time Sequence") +
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    scale_shape_manual(values = c(0, 2), labels = c("Urchin", "Lobster"))
  })

output$max_urchin_count_output <- renderInfoBox({
    infoBox("Urchin MaxN", filtered_data() %>% filter(category_id == '0') %>% group_by(correct_frame) %>% count() %>% pull(n) %>% max(), icon = icon("calculator"), color="purple")
  })

output$median_urchin_count_output <- renderInfoBox({
    infoBox("Urchin Median", filtered_data() %>% filter(category_id == '0') %>% group_by(correct_frame) %>% count() %>% pull(n) %>% median(), icon = icon("calculator"), color="purple")
  })

output$possible_predation_output <- renderInfoBox({
  infoBox("Possible Predation", ifelse(filtered_data() %>% group_by(group) %>% summarise(n = n()) %>% nrow() > 5, "Yes", "No") , icon = icon("utensils"), color="purple")
})

output$motif_count_plot <- renderPlot({
  ggplot(filtered_data() %>% group_by(group) %>% count(), aes(x = factor(group), y = n)) +
  geom_segment(aes(x = factor(group), xend = factor(group), y = 0, yend = n), color = "#631fa6") +
  geom_point(size = 4, color = "#631fa6") +
  labs(color = "Species") +
  xlab("Motif Number") +
  ylab("Number of Frames") +
  coord_flip()+
  theme_light()
})

output$time_motif_plot <- renderPlot({
  ggplot(filtered_data(), aes(x = factor(group), y = minute)) + 
    geom_boxplot(coef = 0, outlier.shape = NA, alpha = 0.3, fill = "#631fa6") +
    theme(legend.position = "none") +
    xlab("Motif Number") +
    ylab("Start and End (minutes)") +
    theme_light()
})

}

runApp(shinyApp(ui, server))
