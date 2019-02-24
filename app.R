library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))
all_studios <- sort(unique(movies$studio))

min_date <- min(movies$thtr_rel_date)
max_date <- max(movies$thtr_rel_date)

# Define UI for application that plots features of movies 
ui <- fluidPage(
    
    # Sidebar layout with a input and output definitions 
    sidebarLayout(
        
        # Inputs
        sidebarPanel(
            
            HTML(paste0("What date range would you like to display? Pick between ", min_date, " and ", max_date, ".")),
            
            br(), br(),
            
            # Date input
            dateRangeInput(inputId = "date",
                      label = "Select dates:",
                      start = "2013-01-01", 
                      end = "2014-01-01",
                      startview = "year",
                      min = min_date, max = max_date),
            
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
            
            # Pick a studio
            selectInput(inputId = "studio",
                        label = "Select studio:",
                        choices = all_studios,
                        selected = "20th Century Fox", 
                        multiple = TRUE,
                        selectize = TRUE),
            
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
            
            # Show the text correlation
            textOutput(outputId = "correlation"), 
            
            # Show the data table
            DT::dataTableOutput(outputId = "moviestable")
            
            
        )
    )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
    
    # Create scatterplot object the plotOutput function is expecting
    output$scatterplot <- renderPlot({
        req(input$date)
        movies_selected_date <- movies %>%
            mutate(thtr_rel_date = as.Date(thtr_rel_date)) %>%
            filter(thtr_rel_date >= input$date[1] & thtr_rel_date <= input$date[2])
        
        ggplot(data = movies_selected_date, aes_string(x = input$x, y = input$y,
               color = input$z)) +
            geom_point(alpha = input$alpha)
    })
    
    # create densityplot
    output$densityplot <- renderPlot({
        req(input$date)
        movies_selected_date <- movies %>%
            mutate(thtr_rel_date = as.Date(thtr_rel_date)) %>%
            filter(thtr_rel_date >= input$date[1] & thtr_rel_date <= input$date[2])
        
        ggplot(data = movies_selected_date, aes_string(x = input$x)) +
            geom_density(alpha = input$alpha)
    })
    
    # Print data table if checked
    output$moviestable <- DT::renderDataTable({
        req(input$studio)
        movies_from_selected_studios <- movies %>%
            filter(studio %in% input$studio)
        
        req(input$date)
        movies_selected_date <- movies_from_selected_studios %>%
            mutate(thtr_rel_date = as.Date(thtr_rel_date)) %>%
            filter(thtr_rel_date >= input$date[1] & thtr_rel_date <= input$date[2])
        
        DT::datatable(data = movies_selected_date,
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
    
    # Render correlation
    output$correlation <- renderText({ 
        r <- round(cor(movies[, input$x], movies[, input$y], use = "pairwise"), 3)
        paste0("Correlation = ", r, ". Note: If the relationship between the two variables is not linear, 
                the correlation coefficient will not be meaningful")
        })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
