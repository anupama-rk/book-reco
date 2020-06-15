####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
source("test.R")



  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Book Crossing",
      tabPanel("Rate a book",
               sidebarPanel(
                 tags$h3("Rate now for better experience:"),
                 selectInput("selection", "Choose a book:",
                             choices =booklist),
                 sliderInput(inputId = "rates",
                             label = "rating",
                             min = 1,
                             max = 10,
                             value = 5),
                 actionButton("update", "Rate"),
                 
               ), # sidebarPanel
               mainPanel(
                            h3("Thank you for rating the book. "),
                            
                            h4("You have rated"),
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("About", "R markdown about the project"),
      tabPanel("Popular Books"),
      tabPanel("Top Rated"),
      tabPanel("Recommendations", "Top 5 recommendations based on your hsitory")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
