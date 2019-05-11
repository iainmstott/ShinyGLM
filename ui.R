# Define UI for application that draws a histogram
ui <- fluidPage(

    # THEME
    theme = shinytheme("paper"),

#    shinyjs::useShinyjs(),

# Head
    tags$head(
              
# # favicon         
#          tags$link(rel="icon", href="favicon.ico", type="image/x-icon"),
#          tags$link(rel="shortcut icon", href="favicon.ico", type="image/x-icon"),
# # CSS
#          includeCSS("CSS/main.css")

    ),

## TITLE PANEL #################################################################

    navbarPage("BGY2010M", collapsible = TRUE, selected = "DATA",
        tags$div(id = "tagline",
            HTML("This Shiny app is designed to help novice learners understand 
            the nature of data, visualising it, and analysing it using 
            generalised linear models. Data visualisation uses the 
            <a href='https://ggplot2.tidyverse.org/' target=_blank>ggplot2</a>
            package by Hadley Wickham and Winston Chang. I recommend 
            <a href='https://global.oup.com/academic/product/getting-started-with-r-9780198787846' target=_blank>Getting Started with R</a>
            by Andrew Beckerman, Dylan Childs and Owen Petchey as an accompanying 
            textbook. Source code for this project is on my
            <a href='https://github.com/iainmstott' target=_blank>GitHub</a>"
            ),
            p()
        ),
        br(),


## USER GUIDE PANEL ############################################################

        tabPanel("USER GUIDE",
            h3("USER GUIDE"),
            HTML("This page walks you through the content and useage of each panel."
            ),
            br(), br(), br(),
            h3("HEADER 1"),
            HTML("Text 1"),
            br(),
            br(),
            h4("HEADER 2"),
            HTML("Text 2"
            ),
            h4("HEADER 3"),
            HTML("Text 3"
            ),
            br(), br(), br(), br()
        ),


## DATA PANEL ##################################################################

        tabPanel("DATA",
            sidebarLayout(
#...............................................................................
                sidebarPanel(width = 4,
                    # Choose data
                    selectInput(inputId = "SelectedData", label = "Choose a data source:",
                                choices = "Games", selected = "Games",
                                multiple = FALSE, selectize = TRUE),
                    HTML("HERE GO MORE OPTIONS")
                ),
#...............................................................................
                mainPanel(
                    tabsetPanel(type = "tabs", selected = "TAB1",
                        # Tab1
                        tabPanel("TAB1",
                            HTML("HERE GOES SOME GRAPHS AND STUFF")
                        ),
                        tabPanel("TAB2",
                            HTML("HERE GOES MORE GRAPHS AND STUFF")
                        )
                    )
                )
            )
        )


## DATA VISUALISATION PANEL ####################################################

        tabPanel("DATA VISUALISATION",
            sidebarLayout(
#...............................................................................
                sidebarPanel(width = 4,
                    # Choose data
                    selectInput(inputId = "SelectedData", label = "Choose a data source:",
                                choices = "Games", selected = "Games",
                                multiple = FALSE, selectize = TRUE),
                    HTML("HERE GO MORE OPTIONS")
                ),
#...............................................................................
                mainPanel(
                    tabsetPanel(type = "tabs", selected = "TAB1",
                        # Tab1
                        tabPanel("TAB1",
                            HTML("HERE GOES SOME GRAPHS AND STUFF")
                        ),
                        tabPanel("TAB2",
                            HTML("HERE GOES MORE GRAPHS AND STUFF")
                        )
                    )
                )
            )
        )


## DATA ANALYSIS PANEL #########################################################

        navbarMenu("DATA ANALYSIS",

##_GAUSSIAN_DATA________________________________________________________________

            tabPanel("GAUSSIAN",
                sidebarLayout(
#...............................................................................
                    sidebarPanel(width = 4,
                        # Choose data
                        selectInput(inputId = "SelectedData", label = "Choose a data source:",
                                    choices = "Games", selected = "Games",
                                    multiple = FALSE, selectize = TRUE),
                        HTML("HERE GO MORE OPTIONS")
                    ),
#...............................................................................
                    mainPanel(
                        tabsetPanel(type = "tabs", selected = "TAB1",
                            # Tab1
                            tabPanel("TAB1",
                                HTML("HERE GOES SOME GAUSSIAN STUFF")
                            ),
                            tabPanel("TAB2",
                                HTML("HERE GOES MORE GAUSSIAN STUFF")
                            )
                        )
                    )
                )
            )

##_POISSON_DATA_________________________________________________________________

            tabPanel("POISSON",
                sidebarLayout(
#...............................................................................
                    sidebarPanel(width = 4,
                        # Choose data
                        selectInput(inputId = "SelectedData", label = "Choose a data source:",
                                    choices = "Games", selected = "Games",
                                    multiple = FALSE, selectize = TRUE),
                        HTML("HERE GO MORE OPTIONS")
                    ),
#...............................................................................
                    mainPanel(
                        tabsetPanel(type = "tabs", selected = "TAB1",
                            # Tab1
                            tabPanel("TAB1",
                                HTML("HERE GOES SOME POISSON STUFF")
                            ),
                            tabPanel("TAB2",
                                HTML("HERE GOES MORE POISSON STUFF")
                            )
                        )
                    )
                )
            )

##_BINOMIAL_DATA________________________________________________________________

            tabPanel("BINOMIAL",
                sidebarLayout(
#...............................................................................
                    sidebarPanel(width = 4,
                        # Choose data
                        selectInput(inputId = "SelectedData", label = "Choose a data source:",
                                    choices = "Games", selected = "Games",
                                    multiple = FALSE, selectize = TRUE),
                        HTML("HERE GO MORE OPTIONS")
                    ),
#...............................................................................
                    mainPanel(
                        tabsetPanel(type = "tabs", selected = "TAB1",
                            # Tab1
                            tabPanel("TAB1",
                                HTML("HERE GOES SOME BINOMIAL STUFF")
                            ),
                            tabPanel("TAB2",
                                HTML("HERE GOES MORE BIOMIAL STUFF")
                            )
                        )
                    )
                )
            )
        )
    )
)
