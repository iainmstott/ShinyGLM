# Define UI for application that draws a histogram
ui <- fluidPage(

### SET UP #####################################################################
## THEME .......................................................................
# theme = shinytheme("paper"),

## HEAD ........................................................................


    tags$head(

## FAVICON .....................................................................

            # Favicon (not used)
            # tags$link(rel="icon", href="favicon.ico", type="image/x-icon"),
            # tags$link(rel="shortcut icon", href="favicon.ico", type="image/x-icon"),

## CSS .........................................................................

        # not quite sure what this is
        includeCSS("CSS/atelier-sulphurpool-light.css"),
        # My edits
        includeCSS("CSS/main.css"),
        # Prism (for code syntax highlighting)
        includeCSS("CSS/prism.css"),


## JS ..........................................................................

        # Prettify (not used)
        #  tags$script(src="https://cdn.jsdelivr.net/gh/google/code-prettify@master/loader/run_prettify.js?lang=r"),

        # Prism & Prism for R
        tags$script(src = "prism.js", type = "text/javascript"),
        tags$script(src = "prism.r.js", type = "text/javascript")
    ),


### TITLE PANEL ################################################################

navbarPage("ShinyGLM",
    collapsible = TRUE, selected = "USER GUIDE",
    position = "fixed-top",

### USER GUIDE PANEL ###########################################################

        tabPanel("USER GUIDE", style = "padding-top:10px;",
            sidebarLayout(
## SIDEBAR PANEL (CHOOSE DATA) .................................................
                sidebarPanel(width = 4, style = "padding-bottom: 90px",
                    tags$h4("USER GUIDE"),
                    HTML("This page navigates you through the shiny app. The 
                          very first thing you need to do though, is choose the 
                          data with which you want to work."),
                    hr(),
                    # Choose data source
                    selectInput(
                        inputId = "dataSelect", 
                        label = "Choose data to work with:",
                        choices = datanames, selected = "games",
                        multiple = FALSE, selectize = TRUE
                    ),
                    tags$ul(id = "dataButton",
                        tags$li(
                            actionButton(inputId = "refreshData",
                                         label = "REFRESH DATA")
                        ),
                        tags$li(
                            # text to advise on logical stuff to choose by variable 
                            # values
                            HTML("Beware: refreshing the data means losing all
                                  other current app settings!")
                        )
                    ),
                    hr(),
                    # Choose theme
                    div(
                        themeSelectorHack()
                    )
                ),
                mainPanel(
                    ## Heading
                    # stuff about the app
                    tags$div(id = "tagline",
                        br(),
                        tags$em(
                            HTML("This Shiny app is designed to help novice learners understand 
                                                the nature of data, visualising it, and analysing it using 
                                                generalised linear models. The app uses the 
                                                <a href='https://www.tidyverse.org/packages/' target=_blank>tidyverse</a>
                                                and is intended as a loose accompaniment to the excellent
                                                <a href='https://global.oup.com/academic/product/getting-started-with-r-9780198787846' target=_blank>Getting Started with R (second edition)</a>
                                                textbook by Andrew Beckerman, Dylan Childs and Owen Petchey (2017). 
                                                Source code for this project is on my
                                                <a href='https://github.com/iainmstott' target=_blank>GitHub</a>.")
                        )
                    ),
                    br(),
                    h4("USER GUIDE"),
                    HTML("This page walks you through the content and useage of each panel.
                          This Shiny app is all about DATA. So, first
                          choose the data you want to work with, using the drop-down menu
                          on this page. The app is designed to follow the 'proper' analytical process.
                          That is: tidy your data,
                          plot your data, fit a model, evaluate the model fit, present your
                          results. See the 'Getting Started with R' book for more detail."
                    ),
                    br(), br(),
                    HTML("<strong>The app pages are listed below. These need to 
                          be completed in order.</strong>"
                    ),
                    br(), br(), br(),
                    h5("DATA"),
                    HTML("Having chosen the data you want to work with, you can see
                          it in the DATA tab. In this tab you should select the rows
                          and columns of data you want to work with, and add a variable 
                          if you need to."
                    ),
                    br(), br(),
                    h5("DATA VISUALISATION"),
                    HTML("Once you've tidied up your data and got everything you want
                          to work with, it's time for DATA VISUALISATION. It's important
                          you should have some idea of what your data looks like, and 
                          whether it seems to fit your hypotheses, before you fit a model."
                    ),
                    br(),
                    HTML("<strong>ONE VARIABLE:</strong>
                          This tab is for visualising the distributions of your variables:
                          histograms and probability density of continuous variables, and 
                          numbers of observations for categorical variables."
                    ),
                    br(),
                    HTML("<strong>TWO VARIABLES:</strong>
                          This tab is for visualising how variables relate to one 
                          another: scatterplots, box plots, violin plots, 
                          mean &plusmn; standard error."
                    ),
                    br(), br(),
                    h5("DATA ANALYSIS"),
                    HTML("Having had a look at the data, you're ready for DATA ANALYSIS. 
                          Choose the response variable and up to two 
                          explanatory variables, and refresh the model. The tabs show
                          the MODEL FIT and the RESULTS. There are 3 types of analysis."
                    ),
                    br(),
                    HTML("<strong>GAUSSIAN:</strong>
                          Analyse Gaussian (normally-distributed) data. The response variable is
                          usually continuous data which isn't bounded."
                    ),
                    br(),
                    HTML("<strong>POISSON:</strong>
                          Analyse Poisson data. The response variable is usually integer (count) 
                          data which is bounded below at zero."
                    ),
                    br(),
                    HTML("<strong>BINOMIAL:</strong>
                          Analyse binomial data. The response variable is usually specified as two integer
                          columns representing 'successes' (an event occuring), and 
                          'failures' (an event not occuring), where the total number of 
                          'trials' (successes + failures) can be different on each row. 
                          This page therefore asks for the user to choose two response
                          variables."
                    ),
                    br(), br(), br(), br()
                )
            )
        ),


### DATA PANEL #################################################################


        tabPanel("DATA", style = "padding-top:10px;",
            sidebarLayout(
## SIDEBAR PANEL (CHOOSE DATA) .................................................
                sidebarPanel(width = 4,
                    tags$h5("DATA"),
                    HTML("On this page, you choose the data to work with by removing
                    rows and columns you don't want, and adding new variables you need."),
                    hr(),
                    tags$h5("SUBSET DATA"),
                    HTML("Subsetting the data is the process of removing 
                            rows and columns that you don't need."),
                    hr(),
                    # Choose rows by either numbers or variable values
                    radioButtons(
                        inputId = "rowsBy", 
                        label = "ROWS | Choose by...",
                        choices = list(
                            "Row numbers" = "rowNum", 
                            "Variable values" = "rowVar"
                        ),
                        selected = "rowNum"
                    ),
                    # Choose by row numbers: conditional panel
                    conditionalPanel(
                        condition = "input.rowsBy == 'rowNum'",
                        sliderInput(
                            inputId = "rowNumFilter", 
                            label = "Choose rows:",
                            value = c(1, 1),
                            min = 1, max = 1, step = 1
                        )
                    ),
                    # choose by variable values: conditional panel
                    conditionalPanel(
                        condition = "input.rowsBy == 'rowVar'",
                        textInput(
                            inputId = "rowVarFilter", 
                            label = "Type expressions below using logical operators:",
                            placeholder = "e.g. continuous > 0 & categorical == 'A'"),
                        tags$ul(id = "rowVarButton",
                            tags$li(
                                # text to advise on logical stuff to choose by variable 
                                # values
                                HTML("Use variable names and values from the selected 
                                    data source. See 'Getting Started with R' 
                                    pp.63-65 for help.")
                            ),
                            tags$li(
                                actionButton(inputId = "refreshRowVar", label = "REFRESH")
                            )
                        )
                    ),
                    br(),
                    # choose columns by either numbers or variable names
                    radioButtons(
                        inputId = "colsBy", 
                        label = "COLUMNS | Choose by...",
                            choices = list(
                                    "Column numbers" = "colNum", 
                                    "Variable names" = "colVar"
                                ),
                        selected = "colNum"
                    ),
                    # choose columns by numbers (conditional panel)
                    conditionalPanel(
                        condition = "input.colsBy == 'colNum'",
                        sliderInput(
                            inputId = "colNumFilter", 
                            label = "Choose columns:",
                            value = c(1, 1),
                            min = 1, max = 1, step = 1
                        )
                    ),
                    # choose columns by names (conditional panel)
                    conditionalPanel(
                        condition = "input.colsBy == 'colVar'",
                        selectInput(
                            inputId = "colVarFilter", 
                            label = "Choose variables:",
                            choices = "A",
                            multiple = TRUE, selectize = TRUE
                        )
                    ),
                    br(),
                    hr(),
                    tags$h5("MANIPULATE DATA"),
                    HTML("Manipulating data means to add to, rearrange and 
                            calculate quantities from the data."),
                    hr(),
                    # Add a variable to the data (mutate / group_by)
                    checkboxInput(
                        inputId = "addVar",
                        label = "ADD A VARIABLE...",
                        value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.addVar == true",
                        # selectInput(
                        #     inputId = "addVarFilter", 
                        #     label = "Choose variable:",
                        #     choices = "A",
                        #     multiple = FALSE, selectize = TRUE
                        # ),
                        textInput(
                            inputId = "addVarName", 
                            label = "Name of new variable:",
                            placeholder = "e.g. logC"
                        ),
                        textInput(
                            inputId = "addVarValue", 
                            label = "Value of new variable:",
                            placeholder = "e.g. log(continuous)"
                        ),
                        tags$ul(id = "addVarButton",
                            tags$li(
                                # text to advise on choosing variables & functions
                                HTML("Use variable names and values from the selected 
                                    data source. See 'Getting Started with R' 
                                    p.67 for help.")
                            ),
                            tags$li(
                                actionButton(inputId = "refreshAddVar", label = "ADD VARIABLE")
                            )
                        )
                    ),
                    br(),
                    hr()
                ),
## MAIN PANEL (DATA TABLE AND CODE) ............................................
                mainPanel(
                    # Display outputs for data selection
                    tabsetPanel(type = "tabs", selected = "DATA FRAME",
                        # Data frame as table
                        tabPanel("DATA FRAME",
                            DT::dataTableOutput("renderData"),
                            br(),
                            br()
                        ),
                        # code used to produce data frame
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

        tabPanel("DATA DISTRIBUTIONS", style = "padding-top:10px;",
            sidebarLayout(
#...............................................................................
                sidebarPanel(width = 4,
                    tags$h5("DATA VISUALISATION / one variable"),
                    HTML("This page uses the data you've subsetted and manipulated
                    on the previous page to make single variable plots. Anything 
                    you do on this page won't affect the data you've chosen, but
                    if you change what you've selected on the DATA page, it will 
                    affect the plots on this page."),
                    hr(),
                    selectInput(
                        inputId = "dataViz1VarFilter", 
                        label = "Choose a variable to plot:",
                        choices = "A",
                        multiple = FALSE, selectize = TRUE
                    ),
                    br(),
                    sliderInput(
                        inputId = "dataViz1XLim", 
                        label = "X axis limits:",
                        value = c(1, 1),
                        min = 1, max = 1, step = 1
                    ),
                    numericInput(
                        inputId = "dataViz1Bin",
                        label = "Choose the number of bins to plot:",
                        min = 3, max = 100, step = 1, value = 10
                    ),
                    br(),
                    textInput(
                        inputId = "dataViz1Fill", 
                        label = "Type a colour name!",
                        value = "steelblue4"),
                    HTML("The <a href='https://bit.ly/1lE3ouh' target=blank>R colour guide</a> may help."),
                    br(),
                    br(),
                    checkboxInput(
                        inputId = "dataViz1Density",
                        label = "Add a density plot?",
                        value = FALSE
                    ),
                    br(),
                    radioButtons(
                        inputId = "dataViz1Theme", 
                        label = "Choose a theme for the plot:",
                        choices = list(
                            "minimal" = "minimal", 
                            "grey" = "grey",
                            "classic" = "classic",
                            "void" = "void"
                        ),
                        selected = "minimal"
                    ),
                    br(),
                    hr()
                ),

#...............................................................................
                mainPanel(
                    tabsetPanel(type = "tabs", selected = "PLOT",
                        tabPanel("PLOT",
                            br(),
                            plotOutput(outputId = "oneVarPlot")
                        ),
                        tabPanel("CODE",
                            tags$div(id = "dataViz1Code",
                                htmlOutput("renderDataViz1Code")
                            ),
                            br(),
                            br()
                        )
                    )
                )
            )
        ),
        tabPanel("DATA VISUALISATION", style = "padding-top:10px;",
            sidebarLayout(
#...............................................................................
                sidebarPanel(width = 4,
                    tags$h5("DATA VISUALISATION / two variables"),
                    HTML("This page uses the data you've subsetted and manipulated
                    on the previous page to make two-variable plots. Anything 
                    you do on this page won't affect the data you've chosen, but
                    if you change what you've selected on the DATA page, it will 
                    affect the plots on this page."),
                    hr(),
                    selectInput(
                        inputId = "dataViz2YVarFilter", 
                        label = "Choose a DEPENDENT (y) variable to plot",
                        choices = "A",
                        multiple = FALSE, selectize = TRUE
                    ),
                    sliderInput(
                        inputId = "dataViz2YLim", 
                        label = "Y axis limits",
                        value = c(1, 1),
                        min = 1, max = 1, step = 1
                    ),
                    selectInput(
                        inputId = "dataViz2XVarFilter", 
                        label = "Choose an INDEPENDENT (x) variable to plot",
                        choices = "A",
                        multiple = FALSE, selectize = TRUE
                    ),
                    selectInput(
                        inputId = "dataViz2ZVarFilter", 
                        label = "Choose a GROUPING variable to plot",
                        choices = "A",
                        multiple = FALSE, selectize = TRUE
                    ),
                    br(),
                    radioButtons(
                        inputId = "dataViz2Plot", 
                        label = "Plot TYPE:",
                        choices = list(
                            "points" = "points", 
                            "boxplot" = "boxplot",
                            "violin" = "violin",
                            "barplot" = "barplot"
                        ),
                        selected = "points"
                    ),
                    br(),
                    textInput(
                        inputId = "dataViz2Color", 
                        label = "COLOR name (points & lines)",
                        value = "steelblue4"),
                    conditionalPanel(
                        condition = "input.dataViz2ZVarFilter !== 'None'",
                        HTML("If a grouping variable is being used, specify the name of a 
                        <a href='https://www.datanovia.com/en/wp-content/uploads/dn-tutorials/ggplot2/figures/0101-rcolorbrewer-palette-rcolorbrewer-palettes-1.png' target=blank>
                        COLOR BREWER PALETTE
                        </a> instead (under COLOR for points; under FILL for 
                        boxplots, violin plots & barplots). This gives multiple
                        colors for the multiple groups. If the palette name
                        is invalid, everything will be a default green.
                        </br></br>")
                    ),
                    textInput(
                        inputId = "dataViz2Fill", 
                        label = "FILL color name (boxes & bars)",
                        value = "lightblue"),
                    HTML("The <a href='https://bit.ly/1lE3ouh' target=blank>R colour guide</a> may help."),
                    br(),
                    br(),
                    br(),
                    textInput(
                        inputId = "dataViz2Ylab", 
                        label = "Y label",
                        value = "",
                        placeholder = "y label"),
                    textInput(
                        inputId = "dataViz2Xlab", 
                        label = "X label",
                        value = "",
                        placeholder = "x label"),
                    br(),
                    radioButtons(
                        inputId = "dataViz2Theme", 
                        label = "Choose a THEME for the plot:",
                        choices = list(
                            "minimal" = "minimal", 
                            "grey" = "grey",
                            "classic" = "classic",
                            "void" = "void"
                        ),
                        selected = "minimal"
                    ),
                    br(),
                    hr()
                ),
### ***
#...............................................................................
                mainPanel(
                    tabsetPanel(type = "tabs", selected = "PLOT",
                        tabPanel("PLOT",
                            br(),
                            plotOutput(outputId = "twoVarPlot")
                        ),
                        tabPanel("CODE",
                            tags$div(id = "dataViz2Code",
                                htmlOutput("renderDataViz2Code")
                            ),
                            br(),
                            br()
                        )
                    )
                )
            )
        ),


## DATA ANALYSIS PANEL #########################################################

        navbarMenu("DATA ANALYSIS",

##_GAUSSIAN_DATA________________________________________________________________

            tabPanel("GAUSSIAN", style = "padding-top:10px;",
                sidebarLayout(
#...............................................................................
                    sidebarPanel(width = 4,
                        tags$h5("DATA ANALYSIS / Gaussian"),
                        HTML("This page uses the variables you've plotted on
                        the 'Data Visualisation / two variables' page in a 
                        Gaussian linear model. Anything you do on this page 
                        won't affect the data you've chosen, or the variables 
                        you've plotted, but if you change what you've selected 
                        on the DATA page, or change the variables you plot, it 
                        will affect what's displayed here."),
                        hr(),
                        htmlOutput("GyVarText"),
                        htmlOutput("GxVarText"),
                        htmlOutput("GzVarText"),
                        br(),
                        conditionalPanel(
                            condition = "input.dataViz2ZVarFilter !== 'None'",
                            checkboxInput(
                                inputId = "Ginteraction",
                                label = "Include an INTERACTION term?",
                                value = FALSE
                            )
                        ),
                        br(),
                        HTML("Note: it's best to specify a model using e.g. "),
                        htmlOutput("GrenderExampleCode"),
                        HTML("instead of <b>y</b>, <b>x1</b> and <b>x2</b>. 
                        It's just easier to take the latter approach here because 
                        of the way the app works!")
                    ),
#...............................................................................
                    mainPanel(
                        tabsetPanel(type = "tabs", selected = "MODEL FIT",
                            tabPanel("MODEL FIT",
                                br(),                                
                                plotOutput(outputId = "Gcheckplots")
                            ),
                            tabPanel("MODEL RESULTS",
                                br(),
                                h5("Model summary (parameters)"),
                                verbatimTextOutput("Gglm"),
                                br(),
                                # checkboxInput(
                                #     inputId = "GLinPred",
                                #     label = "Add linear predictor to plot?",
                                #     value = FALSE
                                # ),
                                # br(),
                                h5("Anova table (significance)"),
                                verbatimTextOutput("Gglm_anova"),
                                HTML("
                                    The F tests associated with this anova table 
                                    are only valid for balanced experimental 
                                    designs. It is usually better to compare nested
                                    models to one another, e.g. compare: </br>
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1 * x2</span> to
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1 + x2</span>; </br>
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1 + x2</span> to
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1</span> or 
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x2</span>; </br>
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1</span> or
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x2</span> to
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ 1</span>.</br>
                                "),
                                br(),
                                h5("AIC"),
                                verbatimTextOutput("Gglm_AIC"),
                                br()
                            ),
                            tabPanel("CODE",
                                tags$div(id = "GglmCode",
                                    htmlOutput("renderGglmCode")
                                ),
                                br(),
                                br()
                            )
                        )
                    )
                )
            ),

##_POISSON_DATA_________________________________________________________________

            tabPanel("POISSON", style = "padding-top:10px;",
                sidebarLayout(
#...............................................................................
                    sidebarPanel(width = 4,
                        tags$h5("DATA ANALYSIS / Poisson"),
                        HTML("This page uses the variables you've plotted on
                        the 'Data Visualisation / two variables' page in a 
                        Poisson linear model. Anything you do on this page 
                        won't affect the data you've chosen, or the variables 
                        you've plotted, but if you change what you've selected 
                        on the DATA page, or change the variables you plot, it 
                        will affect what's displayed here."),
                        hr(),
                        htmlOutput("PyVarText"),
                        htmlOutput("PxVarText"),
                        htmlOutput("PzVarText"),
                        br(),
                        conditionalPanel(
                            condition = "input.dataViz2ZVarFilter !== 'None'",
                            checkboxInput(
                                inputId = "Pinteraction",
                                label = "Include an INTERACTION term?",
                                value = FALSE
                            )
                        ),
                        br(),
                        HTML("Note: it's best to specify a model using e.g. "),
                        htmlOutput("PrenderExampleCode"),
                        HTML("instead of <b>y</b>, <b>x1</b> and <b>x2</b>. 
                        It's just easier to take the latter approach here because 
                        of the way the app works!")
                    ),
#...............................................................................
                    mainPanel(
                        tabsetPanel(type = "tabs", selected = "MODEL FIT",
                            tabPanel("MODEL FIT",
                                br(),                                
                                plotOutput(outputId = "Pcheckplots")
                            ),
                            tabPanel("MODEL RESULTS",
                                br(),
                                h5("Model summary (parameters)"),
                                verbatimTextOutput("Pglm"),
                                br(),
                                # checkboxInput(
                                #     inputId = "GLinPred",
                                #     label = "Add linear predictor to plot?",
                                #     value = FALSE
                                # ),
                                # br(),
                                h5("Anova table (significance)"),
                                verbatimTextOutput("Pglm_anova"),
                                HTML("
                                    The F tests associated with this anova table 
                                    are only valid for balanced experimental 
                                    designs. It is usually better to compare nested
                                    models to one another, e.g. compare: </br>
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1 * x2</span> to
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1 + x2</span>; </br>
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1 + x2</span> to
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1</span> or 
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x2</span>; </br>
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x1</span> or
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ x2</span> to
                                    <span style='font-family: Consolas, Monospace; background-color: #F2F2F2;'>
                                    y ~ 1</span>.
                                    </br>
                                "),
                                br(),
                                h5("AIC"),
                                verbatimTextOutput("Pglm_AIC"),
                                br()
                            ),
                            tabPanel("CODE",
                                tags$div(id = "PglmCode",
                                    htmlOutput("renderPglmCode")
                                ),
                                br(),
                                br()
                            )
                        )
                    )
                )
            ),

##_BINOMIAL_DATA________________________________________________________________

            tabPanel("BINOMIAL", style = "padding-top:10px;",
                sidebarLayout(
#...............................................................................
                    sidebarPanel(width = 4,
                        # Choose data
                        HTML("HERE GO MORE OPTIONS")
                    ),
#...............................................................................
                    mainPanel(
                        tabsetPanel(type = "tabs", selected = "MODEL FIT",
                            # Tab1
                            tabPanel("MODEL FIT",
                                br(),
                                HTML("MODEL FIT COMING SOON...")
                            ),
                            tabPanel("MODEL RESULTS",
                                br(),
                                HTML("MODEL RESULTS COMING SOON...")
                            ),
                            tabPanel("CODE",
                                br(),
                                HTML("CODE COMING SOON...")
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