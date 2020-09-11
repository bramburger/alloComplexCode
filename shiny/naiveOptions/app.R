library("shiny")

## Define UI for app that draws a histogram ----
ui <- fluidPage(
    ## App title ----
    titlePanel("Hello Shiny!"),
    ## Sidebar layout with input and output definitions ----
    sidebarLayout(
        ## Sidebar panel for inputs ----
        sidebarPanel(
            numericInput(inputId= "nbatches",
                         label="Number of Batches",
                         value=1,
                         min=1,
                         step=1,
                         max=100),
            lapply(seq(20), function(i) {
                numericInput(inputId= paste0("group", i),
                             label=paste0("Number of Subjects in Group ", i),
                             value=0,
                             min=0,
                             step=1,
                             max=100)
            })
        ),

        ## Main panel for displaying outputs ----
        mainPanel(
            htmlOutput("samplesize"),

            htmlOutput("lefttobatch"),

            textOutput("defsimple"),

            textOutput("numberofoptions"),

        )
    )
)

## Define server logic required to draw a histogram ----
server <- function(input, output) {

    output$samplesize <- renderText({
        positive <- which(sapply(seq(20), function (i) {
            input[[paste0("group", i)]] %% input$nbatches }) > 0)
        batchstring <-
            if (any(positive)) {
                sampsizes <- sapply(positive, function (i)
                    input[[paste0("group", i)]] %% input$nbatches)
                batchsizes <- rep(sum(sampsizes) %/% input$nbatches,
                                  input$nbatches)
                if (sum(sampsizes) %% input$nbatches)
                    batchsizes[seq(sum(sampsizes) %% input$nbatches)] <-
                        batchsizes[seq(sum(sampsizes) %% input$nbatches)] + 1
                paste("Batch sizes are:",
                      paste(batchsizes, collapse=" "), "<br>")
            } else {
                ""
            }
        sampstring <- paste("Total sample size is",
                            sum(sapply(seq(20), function (i)
                                input[[paste0("group", i)]])), "<br>")
        HTML(paste(sampstring, batchstring))
    })

    output$defsimple <- renderText({
        positive <- which(sapply(seq(20), function (i) {
            input[[paste0("group", i)]] %% input$nbatches }) > 0)
        if (length(positive) == 0) {
            "No subjects, very simple."
        } else if (length(positive) == 1) {
            "Only one group, very simple."
        } else if (sum(sapply(positive, function (i)
            input[[paste0("group", i)]] %% input$nbatches)) <= input$nbatches) {
            "Add one random subject to all batches until you run out of subjects. Very easy."
        } else {
            "LALALALALA I have to think, sorry."
        }
    })

    output$lefttobatch <- renderUI({
        positive <- which(sapply(seq(20), function (i) {
            input[[paste0("group", i)]] }) > 0)
        HTML(paste("SUBJECTS LEFT TO BATCH AFTER PREALLOCATION:",
              paste(lapply(positive, function (i)
                  paste("group", i, " : ",
                        input[[paste0("group", i)]] %% input$nbatches)
                  ), collapse= "<br>"), sep="<br>"))
    })


    output$numberofoptions <- renderText({
        positive <- which(sapply(seq(20), function (i) {
            input[[paste0("group", i)]] %% input$nbatches }) > 0)
        sampsizes <- sapply(positive, function (i)
            input[[paste0("group", i)]] %% input$nbatches)
        batchsizes <- rep(sum(sampsizes) %/% input$nbatches, input$nbatches)
        if (sum(sampsizes) %% input$nbatches)
            batchsizes[seq(sum(sampsizes) %% input$nbatches)] <-
                batchsizes[seq(sum(sampsizes) %% input$nbatches)] + 1
        paste("Number of possible allocations",
              allocations(sampsizes, batchsizes))
    })

}

allocations <- function (sampsizes, batchsizes) {
    sampsizes <- sampsizes[sampsizes > 0]
    torem <- which(sampsizes == length(batchsizes))
    if (length(torem) > 0) {
        sampsizes <- sampsizes[-torem]
        batchsizes <- batchsizes - length(torem)
    }
    if (all(batchsizes == 0)) return(1)
    sum(unlist(apply(gtools::combinations(length(sampsizes), batchsizes[1]), 1,
              function (x) {
                  aaa <- sampsizes
                  aaa[x] <- aaa[x]-1
                  allocations(aaa, batchsizes[-1])
              })))
}

shinyApp(ui = ui, server = server)
