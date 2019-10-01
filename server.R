# Define server logic required to draw a plot
server <- function(input, output, session) {



    ### DATA VALUES ################################################################

    ### Update data. This is done using the " REFRESH DATA" button. On the 
    ### 'DATA SUBSETTING' tab. When the data is refreshed, all the settings for 
    ### selecting rows and columns of the data is lost.
    dataSelected <- reactive({
        input$refreshData
        data[[isolate(input$dataSelect)]]
    })

    ## ROWS
    # get max rows each time the data is changed
    rowNumMax <- reactive({
        dat <- dataSelected()
        dim(dat)[1]
    })

    ## COLUMNS
    # get max cols each time the data is changed
    colNumMax <- reactive({
        dat <- dataSelected()
        dim(dat)[2]
    })
    # names of the columns
    colNames <- reactive({
        dat <- dataSelected()
        names(dat)
    })


    ### DATA #######################################################################


    ## SUBSETTING ..................................................................

    ### Selecting rows...
    ## number slider selector: the values for this are saved even when switching
    ## between selecting rows by numbers vs variable names
    # update the rows number slider if data is changed
    observe({
        nRows <- rowNumMax()
        updateSliderInput(session, "rowNumFilter",
            max = nRows, value = c(1, nRows)
        )
    })
    # range of row numbers chosen (debounce)
    rowNumRange <- reactive({
        input$rowNumFilter
    })
    rowNums <- rowNumRange %>% debounce(50)
    # Sequence of numbers to select rows (max and min may be different or the 
    # same)
    rowNumFilterOut <- reactive({
        rowNumFilterOut <- seq(rowNums()[1], rowNums()[2], 1)
        rowNumFilterOut
    })

    ## variable selector: (settings also saved for switching between 
    ## using numbers vs variable values)
    rowVars <- reactive({
        rRV <- input$refreshRowVar
        isolate(input$rowVarFilter)
    })

    ### Selecting columns...
    ## number slider selector (settings also saved for switching between 
    ## using numbers vs variable names)
    # update the cols number slider if data is changed
    observe({
        nCols <- colNumMax()
        updateSliderInput(session, "colNumFilter",
            max = nCols, value = c(1, nCols)
        )
    })
    # range of column numbers chosen (debounce)
    colNumRange <- reactive({
        input$colNumFilter
    })
    colNums <- colNumRange %>% debounce(50)
    # Sequence of columns (max & min can be the same if one column selected)
    colNumFilterOut <- reactive({
        colNumFilterOut <- seq(colNums()[1], colNums()[2], 1)
        colNumFilterOut
    })

    ## Selecting by variable (settings also saved for switching between 
    ## using numbers vs variable names)
    # update the cols variables SelectInput if data is changed
    observe({
        colNames <- colNames()
        updateSelectInput(session, "colVarFilter",
            choices = colNames, selected = colNames
        )
    })
    # column variables chosen (debounce)
    colVarSelect <- reactive({
        input$colVarFilter
    })
    colVars <- colVarSelect %>% debounce(50)


    ## MANIPULATION ................................................................

    ### ADD VARIABLE
    ## Update groupAddVar dropdown with the variables in the data
    observe({
        if (input$colsBy %in% "colNum") {
            cols <- colNames()[colNumFilterOut()]
        }
        # columns by names
        if (input$colsBy %in% "colVar") {
            cols <- colVars()
        }
        dat <- dataSelected()[, cols]
        whichCatVars <- !sapply(dat, is.numeric)
        catVars <- cols[whichCatVars]
        if (any(whichCatVars)) {
            groupChoices <- c("None", catVars)
            updateSelectInput(session, "groupAddVar",
                              choices = groupChoices)
        }
        if (!any(whichCatVars)) {
            groupChoices <- "None"
            updateSelectInput(session, "groupAddVar",
                              choices = groupChoices)
        }
    })

    ## Update the add column name
    colAddName <- reactive({
        rAV <- input$refreshAddVar
        isolate(input$addVarName)
    })

    ## Update the add column value
    colAddVal <- reactive({
        rAV <- input$refreshAddVar
        aVV <- isolate(input$addVarValue)
        aVVnoSpace <- gsub(" ", "", aVV)
        aVVnoSpace
    })



    ### Subsetting the data...
    ## filter the data based on chosen rows and columns
    dataOut <- reactive({
        # rows by numbers
        if (input$rowsBy %in% "rowNum") {
            rowFilter <- rowNumFilterOut()
            dFr <- dataSelected() %>% slice(rowFilter)
        }
        # rows by variables
        if (input$rowsBy %in% "rowVar") {
            rowFilter <- rowVars()
            if (rowFilter %in% "") {
                dFr <- dataSelected()
            }
            if (!(rowFilter %in% "")) {
                dFr <- dataSelected() %>%
                           filter(eval(parse(text = rowFilter)))
            }
        }
        # columns by numbers
        if (input$colsBy %in% "colNum") {
            colFilter <- colNumFilterOut()
            dFc <- dFr %>% select(colFilter)
        }
        # columns by names
        if (input$colsBy %in% "colVar") {
            colFilter <- colVars()
            dFc <- dFr %>% select(colFilter)
        }
        # add any variables
        if (input$addVar %in% FALSE) {
            dFa <- dFc
        }
        if (input$addVar %in% TRUE) {
            aVN <- colAddName()
            aVV <- colAddVal()
            if (all(c(aVN, aVV) %in% "")) {
                dFa <- dFc
            }
            if (!all(c(aVN, aVV) %in% "")) {
                newvar0 <- with(dFc, eval(parse(text = aVV)))
                newvar <- round(newvar0, 9)
                dFa <- mutate(dFc, !!aVN := newvar)
            }
        }
        # if (input$arrData %in% TRUE) {

        # }
        list(dFr = dFr, dFc = dFc, dFa = dFa)
    })







    ## RENDERING ...................................................................

    ## render the data for display
    output$renderData <- DT::renderDataTable({
        dOut <- dataOut()
        dT <- datatable(dOut$dFa,
                            options = list(dom = "ft", pageLength = -1),
                            rownames = FALSE, filter = "none")
        dT
    }, na = "NA")


    ## CODE ........................................................................
    ### Create a long string...
    displayDataCode <- reactive({
        # HTML bookend (start)
        begin <- "<pre><code class = 'language-r'> \n"
        ## reading in the data
        dataName <- isolate(input$dataSelect)
        read <- paste("# read in data", "\n",
                      "data <- read_csv('",
                      dataName,
                      "'.csv)", "\n\n",
                      sep = "")
        ## choosing the rows and columns
        # rows by numbers
        if (input$rowsBy %in% "rowNum") {
            rowRange <- ifelse(diff(rowNums()) %in% 0,
            # one row only (simpler code displayed)
                paste("# define row range", "\n",
                      "rows <- ",
                      rowNums()[1], "\n\n",
                      sep = ""
                ),
            # more than one row (more complicated code displayed)
                paste("# define row range", "\n",
                      "rows <- c(",
                      paste(rowNums(), collapse = ", "), ")", "\n\n",
                      sep = ""
                ))
            # code for rows (one, or many)
            rowSubset <- ifelse(diff(rowNums()) %in% 0,
                "    slice(rows)",
                "    slice( seq(rows[1], rows[2], 1) )"
            )
            # if all rows are selected, remove code (not necessary
            # as all rows are selected by default)
            if (all(rowNums() == c(1, rowNumMax()))) {
                rowRange <- ""
                rowSubset <- ""
            }
        }
        # rows by variable values
        if (input$rowsBy %in% "rowVar") {
            rowRange <- ""
            rowSubset <- ifelse(rowVars() %in% "",
                "",
                paste("    filter( ", rowVars(), " )", sep = "")
            )
        }

        # columns by numbers
        if (input$colsBy %in% "colNum") {
            colRange <- ifelse(diff(colNums()) %in% 0,
            # one column only (simpler code displayed)
                paste("# define column range", "\n",
                      "cols <- ",
                      colNums()[1], "\n\n",
                      sep = ""
                ),
            # more than one column (more complicated code displayed)
                paste("# define column range", "\n",
                      "cols <- c(",
                      paste(colNums(), collapse = ", "), ")", "\n\n",
                      sep = ""
                )
            )
            # code to select columns (one, or many)
            colSubset <- ifelse(diff(colNums()) %in% 0,
                "        select(cols) \n",
                "        select( seq(cols[1], cols[2], 1) ) \n"
            )
            # if all columns are selected, remove code (not necessary
            # as all columns are selected by default)
            if (all(colNums() == c(1, colNumMax()))) {
                colRange <- ""
                colSubset <- ""
            }
        }
        # columns by names
        if (input$colsBy %in% "colVar") {
            colRange <- ifelse(length(colVars()) %in% 1,
            # one column only (simpler code displayed)
                paste("# define column range", "\n",
                      "cols <- '",
                      colVars(), "'", "\n\n",
                      sep = ""
                ),
            # more than one column (more complicated code displayed)
                paste("# define column range", "\n",
                      "cols <- c('",
                      paste(colVars(), collapse = "', '"), "')", "\n\n",
                      sep = ""
                )
            )
            colSubset <- "        select(cols) \n"
            if (all(colNames() %in% colVars())) {
                # all columns (no code displayed)
                colRange <- ""
                colSubset <- ""
            }
        }
        dataNext <- "dataSubset"
        if (rowSubset %in% "") {
            rowPipe <- ""
        }
        if (!(rowSubset %in% "")) {
            if (colSubset %in% "") rowPipe <- "\n"
            if (!(colSubset %in% "")) rowPipe <- " %>%\n"
        }
        ifelse(all(c(rowSubset, colSubset) %in% ""), " %>%\n", "\n")
        # display subset data only if a variable isn't added
        if (input$addVar == TRUE) dataSubsetDisplay <- ""
        if (input$addVar == FALSE) dataSubsetDisplay <- "# display the data\ndataSubset\n\n"
        # paste together the subsetting code
        dataSubset <- paste0("# subset the data", "\n",
                             "dataSubset <- data %>%", "\n",
                             rowSubset, rowPipe, colSubset, "\n",
                             dataSubsetDisplay)
        # is the entire data table selected?
        dOut <- dataOut()
        allDat <- all(dim(dOut$dFc) == dim(dataSelected()))
        # if the entire data table is selected and no variables added, show simpler code
        if (allDat) {
            dataSubset <- paste("# display the data", "\n",
                                "data", "\n\n",
                                sep = "")
            dataNext <- "data"
            # if a variable is added, don't display this data
            if(input$addVar == TRUE) dataSubset <- ""
        }
        # adding variables
        aVN <- colAddName()
        aVV <- colAddVal()
        dataAddVar <- ifelse(all(c(aVN, aVV) %in% ""),
                              "",
                              paste0("# add a variable\ndataAddVar <- mutate(",
                                     dataNext, ", ", aVN, " = ", aVV, ")\n\n",
                                     "# display the data\ndataAddVar\n\n"))
        if (input$addVar == FALSE) dataAddVar <- ""
        if (!all(c(aVN, aVV) %in% "")) dataNext <- "dataMutate"
        # HTML bookend (end)
        end <- "</code></pre>"
        # add all the code together including HTML bookends
        paste(begin, read,
              rowRange, colRange,
              dataSubset,
              dataAddVar,
              end,
              sep = "")
    })
    ## render the code for display using Prism to syntax highlight with 
    ## CSS and javascript
    output$renderCode <- renderUI({
        prismCodeBlock(displayDataCode())
    })




    ### ONE VARIABLE PLOTS #########################################################


    ## SELECTING VARIABLE ..........................................................

    ## data to use with one-variable graphs (only continuous variables)
    dataViz1Data <- reactive({
        dat <- dataOut()$dFa
        whichNumVars <- sapply(dat, is.numeric)
        datNum <- dat[, whichNumVars]
        datNum
    })

    ## variable names in the chosen data
    dataViz1ColNames <- reactive({
        datNum <- dataViz1Data()
        names(datNum)
    })

    ## update variable selectInput to match chosen data
    observe({
        dataViz1ColNames <- dataViz1ColNames()
        if (input$dataViz1VarFilter == "A") select <- dataViz1ColNames[1]
        if (input$dataViz1VarFilter != "A") select <- input$dataViz1VarFilter
        if (!(input$dataViz1VarFilter %in% dataViz1ColNames())) select <- dataViz1ColNames()[1]
        updateSelectInput(session, "dataViz1VarFilter",
            choices = dataViz1ColNames, selected = select
        )
    })

    # column variable chosen (debounce)
    dataViz1Var <- reactive({
        if (input$dataViz1VarFilter %in% dataViz1ColNames()) {
            var <- input$dataViz1VarFilter
        }
        if (!(input$dataViz1VarFilter %in% dataViz1ColNames())) {
            var <- dataViz1ColNames()[1]
        }
        var
    })

    dataViz1VarLim <- reactive({
        dat <- dataViz1Data()
        var <- dataViz1Var()
        range(dat[!is.na(dat[, var]), var])
    })

    observe({
        if (input$dataViz1VarFilter == "A") {
            updateSliderInput(session, "dataViz1XLim",
                min = 1, max = 1, value = c(1, 1))
        }
        if (input$dataViz1VarFilter != "A") {
            range <- dataViz1VarLim()
            width <- range[2] - range[1]
            updateSliderInput(session, "dataViz1XLim",
                min = (range[1] - width), max = (range[2] + width),
                value = c(range[1], range[2])
            )
        }
    })

    dataViz1XLimSelect <- reactive({
        input$dataViz1XLim
    })
    dataViz1XLim <- dataViz1XLimSelect %>% debounce(250)

    # column variable chosen (debounce)
    dataViz1BinSelect <- reactive({
        input$dataViz1Bin
    })
    dataViz1Bin <- dataViz1BinSelect %>% debounce(250)

    # column variable chosen (debounce)
    dataViz1FillSelect <- reactive({
        input$dataViz1Fill
    })
    dataViz1Fill <- dataViz1FillSelect %>% debounce(1000)

    # SINGLE VARIABLE PLOT
    dataViz1Plot <- reactive({
        # if the variable is categorical, stacked barplot
        dat <- dataViz1Data()
        var <- dataViz1Var()
        bin <- dataViz1Bin()
        fill <- dataViz1Fill()
        lim <- dataViz1XLim()
        hist <- ggplot(dat, aes_string(x = var)) +
                geom_histogram(aes(y = ..density..), bins = bin,
                               fill = fill) +
                xlim(lim[1], lim[2])
        if (input$dataViz1Density == TRUE) hist <- hist + geom_density(color = "white", fill = "darkgrey", alpha = 0.5)
        if (input$dataViz1Theme == "minimal") hist <- hist + theme_minimal()
        if (input$dataViz1Theme == "grey") hist <- hist + theme_gray()
        if (input$dataViz1Theme == "classic") hist <- hist + theme_classic()
        if (input$dataViz1Theme == "void") hist <- hist + theme_void()
        hist
    })


    ## RENDERING ...............................................................

    # render the plot for display
    output$oneVarPlot <- renderPlot({
        dataViz1Plot()
    })

    ## CODE ....................................................................

    displayDataViz1Code <- reactive({
        # HTML bookend (start)
        begin <- "<pre><code class = 'language-r'> \n"

        # which data frame are we working with?
        allDat <- all(dim(dataOut()$dFa) == dim(dataSelected()))
        if (allDat) dat <- "data"
        if (!allDat) dat <- "dataSubset"
        if (input$addVar == TRUE) dat <- "dataAddVar"

        ## build the histogram
        dataAddVarPlot1 <- paste0(
            "# initialise the plot with the data and aesthetics (aes)\n",
            "hist <- ggplot(", dat,
            ", aes(x = ", dataViz1Var(), ")) +\n",
            "    # plot a histogram (geom)\n",
            "    geom_histogram(aes(y = ..density..), bins = ",
            dataViz1Bin(), ", fill = ", dataViz1Fill(), ") +\n",
            "    # tweak the aes\n",
            "    xlim(", dataViz1XLim()[1], ", ", dataViz1XLim()[2], ") +\n",
            "    # change the theme\n",
            "    theme_", input$dataViz1Theme, "()\n\n",
            "# and plot!\n",
            "hist")

        # add density if requested
        if (input$dataViz1Density == TRUE) dens <- " + geom_density(color = 'white', fill = 'darkgrey', alpha = 0.5)\n"
        if (input$dataViz1Density == FALSE) dens <- "\n"
        # HTML bookend (end)
        end <- "</code></pre>"
        # add together
        paste0(begin, dataAddVarPlot1, dens, end)
    })

    ## render the code for display using Prism to syntax highlight with 
    ## CSS and javascript
    output$renderDataViz1Code <- renderUI({
        prismCodeBlock(displayDataViz1Code())
    })
    reactive({
        dim(dataViz1Data())
        dim(dataSelected())
    })
}



# ggplot(N, aes(x = "", y = N, fill = group)) +
#     geom_bar(stat = "identity") +
#     scale_fill_brewer(palette = "Set1") +
#     theme_minimal()

