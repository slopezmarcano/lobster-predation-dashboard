# ui/ui.R

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))

ui <- fluidPage(
  headerPanel("Shiny App with Video and ggplot"),
  
  # Control Center (Left Column)
  sidebarLayout(
    sidebarPanel(
      # Add input elements (e.g., selectInput, sliderInput) for user interaction
      selectInput("selected_event", "Select Event ID", choices = c),
      # Add more inputs if needed
    ),
    
    # Video and ggplot Display (Right Column)
    mainPanel(
      fluidRow(
        column(12, 
               # Add HTML or Shiny tags to display the video here
               # Example:
               # tags$video(src = "videos/video1.mp4", type = "video/mp4", width = "100%", controls = TRUE)
        )
      ),
      fluidRow(
        column(12, 
               # Display the boxplot of correct_frames per category_id
               plotOutput("boxplot_correct_frames")
        )
      )
    )
  )
)

