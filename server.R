# Define server logic required to draw a plot
server <- function(input, output, session) {

### DATA TAB ###################################################################


    ### Update data. This is done using the " REFRESH DATA" button. When the 
    ### data is refreshed, all the settings for selecting rows and columns of 
    ### the data is lost.
    dataSelected <- reactive({
        input$refreshData
        data[[isolate(input$dataSelect)]]
    })


    ### Selecting rows...
    ## number slider selector: the values for this are saved even when switching
    ## between selecting rows by numbers vs variable names
    # get max rows each time the data is changed
    rowNumMax <- reactive({
        dat <- dataSelected()
        dim(dat)[1]
    })
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
    # get max cols each time the data is changed
    colNumMax <- reactive({
        dat <- dataSelected()
        dim(dat)[2]
    })
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
    # names of the columns
    colNames <- reactive({
        dat <- dataSelected()
        names(dat)
    })
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


    ### Subsetting the data...
    ## filter the data based on chosen rows and columns
    dataFilter <- reactive({
        # rows by numbers
        if(input$rowsBy %in% "rowNum") {
            rowFilter <- rowNumFilterOut()
            dFr <- dataSelected() %>% slice(rowFilter)
        }
        # rows by variables
        if(input$rowsBy %in% "rowVar") {
            rowFilter <- rowVars()
            if(rowFilter %in% "") {
                dFr <- dataSelected()
            }
            if(!(rowFilter %in% "")) {
                dFr <- dataSelected() %>% 
                           filter(eval(parse(text = rowFilter)))
            }        
        }
        # columns by numbers
        if(input$colsBy %in% "colNum") {
            colFilter <- colNumFilterOut()
            dF <- dFr %>% select(colFilter)
        }
        # columns by names
        if(input$colsBy %in% "colVar") {
            colFilter <- colVars()
            dF <- dFr %>% select(colFilter)
        }
        dF
    })
    ## render the data for display
    output$renderData <- DT::renderDataTable({
        datatable(dataFilter(), 
                  options = list(dom = "ft", pageLength = -1),
                  rownames = FALSE)
    })

    ### Displaying the code in the 'CODE' tab...
    ### Create a long string...
    displayDataCode <- reactive({
        # HTML bookend (start)
        begin <- "<pre><code class = 'language-r'> \n"
        ## reading in the data
        dataName <- isolate(input$dataSelect)
        read <- paste("# read in data", "\n",
                      "data <- read_csv('", 
                      dataName,
                      "')", "\n\n",
                      sep = "")
        ## choosing the rows and columns
        # rows by numbers
        if(input$rowsBy %in% "rowNum"){
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
                "    slice(rows) \n",
                "    slice( seq(rows[1], rows[2], 1) ) \n"
            )
            # if all rows are selected, remove code (not necessary
            # as all rows are selected by default)
            if(all(rowNums() == c(1, rowNumMax()))) {
                rowRange <- ""
                rowSubset <- ""
            }
        }
        # rows by variable values
        if(input$rowsBy %in% "rowVar"){
            rowRange <- ""
            rowSubset <- ifelse(rowVars() %in% "",
                "",
                paste("    filter( ", rowVars(), " ) \n", sep = "")
            )
        }

        # columns by numbers
        if(input$colsBy %in% "colNum"){
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
            if(all(colNums() == c(1, colNumMax()))) {
                colRange <- ""
                colSubset <- ""
            }
        }
        # columns by names
        if(input$colsBy %in% "colVar"){
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
            colSubset <- "        subset(cols) \n"
            if(all(colNames() %in% colVars())) {
                # all columns (no code displayed)
                colRange <- ""
                colSubset <- ""
            }
        }
        # paste together the subsetting code
        dataSubset <- paste("# subset the data", "\n",
                            "dataSubset <- data %>% ", "\n",
                            rowSubset, colSubset, "\n",
                            "# display the data", "\n",
                            "dataSubset", "\n\n",
                            sep = "")
        # if the entire data table is selected, show simpler code
        if(all(dim(dataFilter()) == dim(dataSelected()))){
            dataSubset <- paste("# display the data", "\n",
                                "data", "\n\n",
                                sep = "")
        }
        # HTML bookend (end)
        end <- "</code></pre>"
        # add all the code together including HTML bookends
        paste(begin, read, 
              rowRange, colRange, 
              dataSubset, end,
              sep = "")
    })
    ## render the code for display using Prism to syntax highlight with 
    ## CSS and javascript
    output$renderCode <- renderUI({
        prismCodeBlock(displayDataCode())
    })

} # end the data tab
