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
            htmlOutput("allOutput")
        )
    )
)

## Define server logic required to draw a histogram ----
server <- function(input, output, session) {

    output$allOutput <- renderText({
        positive <- which(sapply(seq(20), function (i) {
            input[[paste0("group", i)]] %% input$nbatches }) > 0)
        totalSampleSize <- sum(sapply(seq(20), function (i)
            input[[paste0("group", i)]]))

        sampsizes <-
            if (any(positive)) {
                sapply(positive, function (i)
                    input[[paste0("group", i)]] %% input$nbatches)
            } else {
                0
            }
        batchsizes <- rep(sum(sampsizes) %/% input$nbatches, input$nbatches)
        if (sum(sampsizes) %% input$nbatches > 0)
            batchsizes[seq(sum(sampsizes) %% input$nbatches)] <-
                batchsizes[seq(sum(sampsizes) %% input$nbatches)] + 1
        numberPossibleAllocations <-
            if (sum(batchsizes) > 0) {
                allocations(sampsizes, batchsizes)
            } else {
                0
            }

        sampstring <- paste("Total sample size is",
                            totalSampleSize,
                            "<br>")
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
                "No subjects, so batches are empty.<br>"
            }

        subjectsToBatch <-
            if (any(positive)) {
                paste("<b>Subjects left to batch after preallocation:</b>",
                      paste(lapply(positive, function (i)
                          paste("group", i, " : ",
                                input[[paste0("group", i)]] %% input$nbatches)
                          ), collapse= "<br>"), sep="<br>")
            } else {
                "<br>"
            }

        defsimple <-
            if (length(positive) == 0) {
                "No subjects, very simple."
            } else if (length(positive) == 1) {
                "Only one group, very simple."
            } else if (totalSampleSize <= input$nbatches) {
                "Add one random subject to all batches until you run out of subjects. Very easy."
            } else {
                "LALALALALA I have to think, sorry."
            }

        numberOfOptions <-
            paste("Number of possible allocations",
                  numberPossibleAllocations)

        matricesOfOptions <-
            if (numberPossibleAllocations == 0 ||
                numberPossibleAllocations > 1000) {
                ""
            } else {
                genOptionsStrings(sampsizes, batchsizes)
            }

        HTML(paste(sampstring, batchstring, subjectsToBatch,
                   defsimple, numberOfOptions, matricesOfOptions,
                   sep="<br>"))
    })

    session$onSessionEnded(function() {
        stopApp()
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

genMat <- function (sampsizes, batchsizes, allocations) {
    if (length(batchsizes) == 0) {
        print(allocations)
        return(allocations)
    }

    allAllocated <- colSums(allocations) == sampsizes
    detAllocate <-
        sampsizes - colSums(allocations) == length(batchsizes)
    toSample <- seq_along(sampsizes)[!(allAllocated | detAllocate)]
    if (length(toSample) > 0) {
        tryCombs <-
            gtools::combinations(length(toSample),
                                 batchsizes[1] - sum(detAllocate),
                                 toSample)
        for (g in which(detAllocate)) {
            tryCombs <- cbind(tryCombs, g)
        }
    } else {
        tryCombs <- matrix(detAllocate, nrow=1)
    }

    apply(tryCombs, 1, function (r) {
        cComb <- rep(0, length(sampsizes))
        cComb[c(r)] <- 1
        newAlloc <- rbind(allocations, cComb)
        univar <- all(sampsizes - colSums(newAlloc) <=
                      length(batchsizes) - 1)
        if (univar) {
            genMat(sampsizes, batchsizes[-1], newAlloc)
        } else {
            NULL
        }
    })
}

genOptions <- function (sampsizes, batchsizes) {
    gens <- genMat(sampsizes, batchsizes,
                   matrix(0, nrow=0, ncol=length(sampsizes)))
    genRelist <- matrix(unlist(gens),
                        nrow=length(sampsizes)*length(batchsizes))
    lapply(seq_len(ncol(genRelist)), function (i) {
        matrix(genRelist[, i], ncol=length(sampsizes))
    })
}

genOptionsStrings <- function (sampsizes, batchsizes) {
    gens <- genOptions(sampsizes, batchsizes)
    retString <- ""
    ## paste(lapply(gens, function (g) print(xtable::xtable(g, auto=TRUE),
    ##                                       type="html")),
    ##       collapse="<br><br>")

    ## paste(lapply(gens, function (g) paste(unlist(g))), collapse="<br><br>")

    paste(lapply(gens, function (mat) {
        paste(apply(mat, 1, function (r) {
            paste(unlist(r), collapse="\t")
        }), collapse="<br>")
    }), collapse="<br><br>")
}



## asdf <- genMat(c(2,2,2), c(1,1,1,1,1,1), matrix(0, nrow=0, ncol=3))
## apply(matrix(unlist(asdf), nrow=18), 2, function (ccc) {
##     matrix(ccc, ncol=3)})

## asdff <- matrix(unlist(asdf), nrow=3*6)
## lapply(seq_len(ncol(asdff)), function (i) {
##     matrix(asdff[, i], ncol=3)
## })


## gtools::combinations(3, 2, LETTERS[1:3])



shinyApp(ui = ui, server = server)
