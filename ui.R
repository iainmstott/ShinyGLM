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
# # Prettify
#         tags$script(src="https://cdn.jsdelivr.net/gh/google/code-prettify@master/loader/run_prettify.js?lang=r"),
# CSS
        includeCSS("CSS/atelier-sulphurpool-light.css"),
        includeCSS("CSS/main.css"),
        includeCSS("CSS/prism.css"),
# JS
        tags$script(src = "prism.js", type = "text/javascript"),
        tags$script(src = "prism.r.js", type = "text/javascript")
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
                    selectInput(
                        inputId = "dataSelect", 
                        label = "Choose a data source:",
                        choices = datanames, selected = "games",
                        multiple = FALSE, selectize = TRUE
                        ),
                    actionButton(inputId = "refreshData", label = "REFRESH DATA"),
                    br(),
                    br(),
                    radioButtons(
                        inputId = "rowsBy", 
                        label = "Choose rows by...",
                        choices = list(
                            "Row numbers" = "rowNum", 
                            "Row names" = "rowNam", 
                            "Variables" = "rowVar"
                        ),
                        selected = "rowNum"
                    ),

### *** updateSelectInput to deal with this;    ½ both rows and columns
                    conditionalPanel(
                        condition = "input.rowsBy == 'rowNum'",
                        sliderInput(
                            inputId = "rowNumFilter", 
                            label = "Choose rows:",
                            value = c(1, 10),
                            min = 1, max = 10, step = 1
                        )
                   ),
                    #  conditionalPanel(
                    #     condition = "input.rowsBy == 'Variable names'",
                    #     selectInput(
                    #         inputId = "rowFilter", 
                    #         label = "Choose variables:",
                    #         choices = rowFilterValues,
                    #         multiple = TRUE, selectize = TRUE)
                    # ),
                    HTML("HERE GO MORE OPTIONS"),
                    br(),
                    br(),
                    radioButtons(
                        inputId = "colsBy", 
                        label = "Choose columns by...",
                            choices = list(
                                     "Column numbers" = "colNum", 
                                     "Variable names" = "colVar"
                                 ),
                        selected = "colNum"
                    ),
                    br(),
                    conditionalPanel(
                        condition = "input.colsBy == 'colNum'",
                        sliderInput(
                            inputId = "colNumFilter", 
                            label = "Choose columns:",
                            value = c(1, 4),
                            min = 1, max = 4, step = 1
                        )
                    ),
                    # conditionalPanel(
                    #     condition = "input.colsBy == 'Variable names'",
                    #     selectInput(
                    #         inputId = "colVarFilter", 
                    #         label = "Choose variables:",
                    #         choices = colVarFilterValues,
                    #         multiple = TRUE, selectize = TRUE
                    #     ),
                    # ),
                    # br(),
                    br(),


                    HTML("HERE GO MORE OPTIONS")
                ),
#...............................................................................
                mainPanel(
                    tabsetPanel(type = "tabs", selected = "OUTPUT",
                        # Tab1
                        tabPanel("OUTPUT",
                            DT::dataTableOutput("renderData"),
                            br(),
                            br()
                        ),
                        tabPanel("CODE", 
                            tags$div(id = "dataCode",
                                htmlOutput("renderCode")
                            ),
                            br(),
                            br()
                        )
                    )
                )
            )
        ),


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
        ),


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
            ),

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
            ),

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











# TEST HTML CODE
#                                 pre(class="prettyprint lang-r",
#                                     HTML(
# 'function (A, vector = "n", bound = NULL, return.N = FALSE)
# {
#     if (any(length(dim(A)) != 2, dim(A)[1] != dim(A)[2]))
#         stop("A must be a square matrix")
#     order <- dim(A)[1]
#     if (!isIrreducible(A)) {
#         warning("Matrix is reducible")
#     }
#     else {
#         if (!isPrimitive(A))
#             warning("Matrix is imprimitive")
#     }
#     M <- A
#     eigvals <- eigen(M)$values
#     lmax <- which.max(Re(eigvals))
#     lambda <- Re(eigvals[lmax])
#     A <- M/lambda
#     if (vector[1] == "n") {
#         if (!any(bound == "upper", bound == "lower"))
#             stop("Please specify bound=\"upper\", bound=\"lower\" or specify vector")
#         if (bound == "upper") {
#             reac <- norm(A)
#             if (return.N) {
#                 N <- reac * lambda
#                 return(list(reac = reac, N = N))
#             }
#             else {
#                 return(reac)
#             }
#         }
#         if (bound == "lower") {
#             reac <- .minCS(A)
#             if (return.N) {
#                 N <- reac * lambda
#                 return(list(reac = reac, N = N))
#             }
#             else {
#                 return(reac)
#             }
#         }
#     }
#     else {
#         if (!is.null(bound))
#             warning("Specification of vector overrides calculation of bound")
#         n0 <- vector
#         vector <- n0/sum(n0)
#         reac <- sum(A %*% vector)
#         if (return.N) {
#             N <- reac * sum(n0) * lambda
#             return(list(reac = reac, N = N))
#         }
#         else {
#             return(reac)
#         }
#     }
# }'
#                                     )
#                                 )