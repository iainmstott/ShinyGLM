# Define server logic required to draw a plot
server <- function(input, output, session) {

    dataSelected <- reactive({
        input$refreshData
        data[[isolate(input$dataSelect)]]
    })

    rowNumMax <- reactive({
        dat <- dataSelected()
        dim(dat)[1]
    })
    observe({
        nRows <- rowNumMax()
        updateSliderInput(session, "rowNumFilter",
            max = nRows, value = c(1, nRows)
        )
    })
    rowNumFilterOut <- reactive({
        rowNumRange <- input$rowNumFilter
        rowNumFilter <- seq(rowNumRange[1], rowNumRange[2], 1)
        seq(1, rowNumMax(), 1) %in% rowNumFilter
    })
    rowNumFilterDebounce <- rowNumFilterOut %>% debounce(50)

    colNumMax <- reactive({
        dat <- dataSelected()
        dim(dat)[2]
    })
    observe({
        nCols <- colNumMax()
        updateSliderInput(session, "colNumFilter",
            max = nCols, value = c(1, nCols)
        )
    })
    colNumFilterOut <- reactive({
        colNumRange <- input$colNumFilter
        colNumFilter <- seq(colNumRange[1], colNumRange[2], 1)
        seq(1, colNumMax(), 1) %in% colNumFilter
    })
    colNumFilterDebounce <- colNumFilterOut %>% debounce(200)

    dataFilter <- reactive({
        if(input$rowsBy %in% "rowNum") {
            rowFilter <- rowNumFilterDebounce()
        }
        if(input$colsBy %in% "colNum") {
            colFilter <- colNumFilterDebounce()
        }
        dataSelected()[rowFilter, colFilter]
    })

    output$renderData <- DT::renderDataTable({
        datatable(dataFilter(), 
                  options = list(dom = "ft", pageLength = -1),
                  rownames = FALSE)
    })

    codeOut <- 
"<pre><code class = 'language-r'>
abc <- function(a, b, c){
    if(all(is.numeric(a), is.numeric(b), is-numeric(c)))
    out <- (a + b + c)
    out
}
a <- runif(10)
b <- rnorm(10, 0, 1)
c <- rpois(10, 2)
abc(a, b, c)

</code></pre>"
    output$renderCode <- renderUI({
        prismCodeBlock(codeOut)
    })

}
