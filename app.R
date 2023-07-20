# Load the shiny library
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))

# Load the UI and server scripts within their own environment
local({
  source("/Users/s2985905/Dropbox/GithubRepos/lobster-predation-dashboard/ui/ui.R", encoding = "UTF-8", local = TRUE)
  source("/Users/s2985905/Dropbox/GithubRepos/lobster-predation-dashboard/server/server.R", encoding = "UTF-8", local = TRUE)
})

# Run the app
shinyApp(ui = ui, server = server)

source('app.R')

