library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))

# Define UI for application that plots features of movies 
ui <- fluidPage(
    
    # Sidebar layout with a input and output definitions 
    sidebarLayout(
        
        # Inputs
        sidebarPanel(
            
            # Select variable for y-axis
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"), 
                        selected = "audience_score"),
            
            # Select variable for x-axis
            selectInput(inputId = "x", 
                        label = "X-axis:",
                        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"), 
                        selected = "critics_score"),
            
            # Select variable for color
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = c("Title type" = "title_type", 
                                    "Genre" = "genre", 
                                    "MPAA rating" = "mpaa_rating", 
                                    "Critics rating" = "critics_rating",
                                    "Audience rating" = "audience_rating"),
                        selected = "mpaa_rating"),
            
            # Set alpha level
            sliderInput(inputId = "alpha",
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            # Show dat table
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE)
        ),
        
        # Outputs
        mainPanel(
            # Show the plots
            plotOutput(outputId = "scatterplot"),
            plotOutput(outputId = "densityplot", height = 200),
            
            # Show the data table
            DT::dataTableOutput(outputId = "moviestable")
        )
    )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
    
    # Create scatterplot object the plotOutput function is expecting
    output$scatterplot <- renderPlot({
        ggplot(data = movies, aes_string(x = input$x, y = input$y,
               color = input$z)) +
            geom_point(alpha = input$alpha)
    })
    
    # create densityplot
    output$densityplot <- renderPlot({
        ggplot(data = movies, aes_string(x = input$x)) +
            geom_density(alpha = input$alpha)
    })
    
    # Print data table if checked
    output$moviestable <- DT::renderDataTable({
        if(input$show_data){
            DT::datatable(data = movies %>% select(1:7),
                          options = list(pageLength = 10),
                          rownames = FALSE)
        }
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
