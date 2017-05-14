library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggdendro)
library(igraph)
library(rmutil)
library(mc2d)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)

server <- function(input, output, clientData, session) {
  values <- reactiveValues()
  
  inFile <- reactive(input$datafile)
  
  subsetCols <- c()
  subsetRows <- c()
  subsetColsFiltered <- c()
  subsetRowsFiltered <- c()
  
  values$n <- 0
  values$m <- 0
  values$num.filters <- c()
  values$num.sorts <- c()
  values$uploaded <- FALSE
  values$generated <- FALSE

  observeEvent(input$datafile, {
    values$df <- read.csv(inFile()$datapath, header = input$header, sep = input$sep)
    values$uploaded <- TRUE
    values$generated <- FALSE
  })
  
  getFullTable <- reactive({
    values$colnames <- colnames(values$df)
    values$df
  })
  
  getTable <- reactive({
    df <- subset(values$df[values$df[, "del"] == 0, ], select=-del)
    values$colnames <- colnames(df)
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    updateSelectInput(session, "elementNames", choices=c("№", colnames(df)))
    updateTextInput(session, "subsetCols", "")
    updateTextInput(session, "subsetRows", "")
    updateTextInput(session, "subsetColsFiltered", "")
    updateTextInput(session, "subsetRowsFiltered", "")
    df
  })
  
  getSubTable <- reactive({
    df <- getTable()[unlist(getSubsetRows()), unlist(getSubsetCols()), drop=FALSE]
    for(i in values$num.filters) {
      if(!is.null(input[[paste0("val", i)]]) && input[[paste0("val", i)]] != "") {
        df <- switch(input[[paste0("op", i)]],
                     "=" = df[df[, input[[paste0("col", i)]]] == input[[paste0("val", i)]],],
                     "<" = df[df[, input[[paste0("col", i)]]] <  input[[paste0("val", i)]],],
                     ">" = df[df[, input[[paste0("col", i)]]] >  input[[paste0("val", i)]],]
          )
      }
    }
    for(i in values$num.sorts) {
      if(!is.null(input[[paste0("sortcol", i)]])) {
        df <- switch(input[[paste0("asc", i)]],
                     "по возрастанию" = df[order(df[[paste0(input[[paste0("sortcol", i)]])]]), ],
                     "по убыванию" = df[order(-df[[paste0(input[[paste0("sortcol", i)]])]]), ]
        )
      }
    }
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    df
  })
  
  getFilteredSubTable <- reactive({
    getSubTable()[unlist(getSubsetRowsFiltered()), unlist(getSubsetColsFiltered()), drop=FALSE]
  })
  
  getSubTableRowNames <- reactive({
    if(input$elementNames == "№") {
      rownames(getTable()[unlist(getSubsetRows()), ,])
    } else {
      res <- getTable()[unlist(getSubsetRows()), c(input$elementNames)]
      return(res)
    }
  })
  
  validateRange <- function(s) {
    if(is.null(s)) {
      return(TRUE)
    }
    s <- gsub(" ", "", s, fixed = TRUE)
    if(s == "") {
      return(TRUE)
    }
    all(sapply(strsplit(s, ",")[[1]], function(x) {
      ifelse(grepl("-", x), 
             {
               l <- strsplit(x, "-")[[1]]
               if(length(l) > 2) {
                 return(FALSE)
               }
               !is.na(as.integer(l[1])) && !is.na(as.integer(l[2]))
             }, !is.na(as.integer(x)))
    }))
  }
  
  getSubsetCols <- reactive({
    validate(need(validateRange(input$subsetCols), "Столбцы выбраны неправильно"))
    s <- gsub(" ", "", input$subsetCols, fixed = TRUE)
    if(s == "") {
      return(colnames(getTable()))
    }
    
    lapply(strsplit(s, ",")[[1]], function(x) {
      ifelse(grepl("-", x), 
             {
               l <- strsplit(x, "-")
               list(as.integer(l[[1]][[1]]):as.integer(l[[1]][[2]]))
             }, as.integer(x))
      })
  })
  
  getSubsetRows <- reactive({
    validate(need(validateRange(input$subsetRows), "Строки выбраны неправильно"))
    s <- gsub(" ", "", input$subsetRows, fixed = TRUE)
    if(s == "") {
      return(rownames(getTable()))
    }
    lapply(strsplit(s, ",")[[1]], function(x) {
      ifelse(grepl("-", x), 
             {
               l <- strsplit(x, "-")
               list(as.integer(l[[1]][[1]]):as.integer(l[[1]][[2]]))
             }, as.integer(x))
    })
  })
  
  getSubsetColsFiltered <- reactive({
    validate(need(validateRange(input$subsetColsFiltered), "Столбцы выбраны неправильно"))
    s <- gsub(" ", "", input$subsetColsFiltered, fixed = TRUE)
    if(s == "") {
      return(colnames(getSubTable()))
    }
    
    lapply(strsplit(s, ",")[[1]], function(x) {
      ifelse(grepl("-", x), 
             {
               l <- strsplit(x, "-")
               list(as.integer(l[[1]][[1]]):as.integer(l[[1]][[2]]))
             }, as.integer(x))
    })
  })
  
  getSubsetRowsFiltered <- reactive({
    validate(need(validateRange(input$subsetRowsFiltered), "Строки выбраны неправильно"))
    s <- gsub(" ", "", input$subsetRowsFiltered, fixed = TRUE)
    if(s == "") {
      return(rownames(getSubTable()))
    }
    lapply(strsplit(s, ",")[[1]], function(x) {
      ifelse(grepl("-", x), 
             {
               l <- strsplit(x, "-")
               list(as.integer(l[[1]][[1]]):as.integer(l[[1]][[2]]))
             }, as.integer(x))
    })
  })
  
  output$maintable <- DT::renderDataTable({
    DT::datatable(getTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  output$fulltable <- DT::renderDataTable({
    DT::datatable(getFullTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  output$outInfoColumn <- renderText({
    validate(
      need(values$df, "Generate or upload data")
    )
    arr <- getFilteredSubTable()[, input$infoColumn]
    sprintf("Среднее: %f, стандартное отклонение: %f", mean(arr), sd(arr))
    })
  
  output$densityPlot <- renderPlotly({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    
    validate(
      need(as.numeric(getFilteredSubTable()[, input$infoColumn]), "Выберите числовой столбец")
    )
    
    x <- getFilteredSubTable()[, input$infoColumn]
    fit <- density(x)
    
    plot_ly(x = x, type = "histogram", name = "Гистограмма") %>% 
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Плотность") %>% 
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$dendrogram <- renderPlot({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    dd <- as.dendrogram(getHC())
    df <- dendro_data(dd)
    ggdendrogram(df)
  })
  
  getDist <- reactive({
    dist(getFilteredSubTable())
  })
  
  getHC <- reactive({
    hclust(getDist(), method=input$clusteringMethod)
  })
  
  getClusters <- reactive({
    hc <- getHC()
    clusters <- cutree(hc, k = input$numClusters)
    clusters
  })
  
  output$pca <- renderPlotly({
    validate(
      need(values$df, "Generate or upload data")
    )
    validate(
      need(is.numeric(as.matrix(getFilteredSubTable())), "Матрица содержит нечисловой столбец")
    )
    validate(
      need(ncol(getFilteredSubTable()) > 2, "Количество столбцов недостаточно")
    )
    pca <- princomp(getFilteredSubTable(), cor = TRUE)
    clusters <- getClusters()
    df <- data.frame(pca$scores, "cluster" = factor(clusters))
    df <- transform(df, cluster_name = paste("Cluster", clusters))
    plot_ly(df, x = ~Comp.1, y = ~Comp.2, z = ~Comp.3, type = "scatter3d", mode = "markers",
            color = ~cluster_name, marker = list(symbol = 'circle', size = 4)) %>%
      layout(title="Анализ главных компонент",
             scene = list(xaxis = list(title="PC1"),
                          yaxis = list(title="PC2"),
                          zaxis = list(title="PC3")))
  })
  
  output$graph <- renderPlotly({
    validate(
      need(values$df, "Generate or upload data")
    )
    g <- graph.empty(input$numClusters, directed = FALSE)
    L <- layout.circle(g)
    clusters <- getClusters()
    counts <- as.vector(table(clusters))
    total <- sum(counts)
    sizes <- c(counts / total) * 100 + 70
    labels <- sapply(1:input$numClusters, function(x) {
      long.string <- paste(getSubTableRowNames()[clusters == x], collapse=", ")
      paste(strwrap(long.string, width = 70), collapse="<br>")
    })
    network <- plot_ly(x = ~L[,1], y = ~L[,2], mode = "markers", type = "scatter", 
                       text = labels, hoverinfo = "text",
                       symbol = 'circle',
                       marker = list(size = sizes),
                       color = 1:input$numClusters, colors = "Set1")
    axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    p <- layout(
      network,
      xaxis = axis,
      yaxis = axis
    )
    p
  })
  
  getCorrMatrix <- reactive({
    cor(getFilteredSubTable())
  })
  
  getCovMatrix <- reactive({
    cov(getFilteredSubTable())
  })
  
  output$corrMatrix <- renderTable({
    validate(
      need(values$df, "Generate or upload data")
    )
    validate(
      need(is.numeric(as.matrix(getFilteredSubTable())), "Матрица содержит нечисловой столбец")
    )
    getCorrMatrix()
  })
  
  output$covMatrix <- renderTable({
    validate(
      need(values$df, "Generate or upload data")
    )
    validate(
      need(is.numeric(as.matrix(getFilteredSubTable())), "Матрица содержит нечисловой столбец")
    )
    getCovMatrix()
  })
  
  output$downloadData <- downloadHandler(
    filename = "subset.csv",
    content = function(file) {
      write.table(getFilteredSubTable(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadGenerated <- downloadHandler(
    filename = "generated_dataset.csv",
    content = function(file) {
      write.table(getTable(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadCorr <- downloadHandler(
    filename = "corr.csv",
    content = function(file) {
      write.table(getCorrMatrix(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadCov <- downloadHandler(
    filename = "cov.csv",
    content = function(file) {
      write.table(getCovMatrix(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadDist <- downloadHandler(
    filename = "adjacency.csv",
    content = function(file) {
      write.table(as.matrix(getDist()), file, sep=",", row.names = FALSE)
    }
  )
  
  output$subtable <- DT::renderDataTable({
    validate(
      need(values$df, "Generate or upload data")
    )
    DT::datatable(getFilteredSubTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               ordering=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  observeEvent(input$addFilter, {
    values$n <- values$n + 1
    n <- values$n
    values[[paste0("selected_col", n)]] <- 1
    values[[paste0("selected_op", n)]] <- "="
    values[[paste0("selected_val", n)]] <- ""
    
    values$num.filters <- c(values$num.filters, n)
    output[[paste0("col", n)]] <- renderUI({
      selectizeInput(paste0("col", n), "", choices = values$colnames, 
                     selected = values[[paste0("selected_col", n)]])
    })
    output[[paste0("op", n)]] <- renderUI({
      selectizeInput(paste0("op", n), "", multiple = F, choices = list("=", "<", ">"), 
                     selected = values[[paste0("selected_op", n)]])
    })
    output[[paste0("val", n)]] <- renderUI({
      textInput(paste0("val", n), "", value = values[[paste0("selected_val", n)]])
    })
    output[[paste0("remove", n)]] <- renderUI({
      actionButton(paste0("remove", n), "x", class="btn-danger")
    })
    
    observeEvent(input[[paste0("remove", n)]], {
      values$num.filters <- values$num.filters[-which(values$num.filters == n)]
    })
    observe({
      values[[paste0("selected_col", n)]] <- input[[paste0("col", n)]]
      values[[paste0("selected_op", n)]] <- input[[paste0("op", n)]]
      values[[paste0("selected_val", n)]] <- input[[paste0("val", n)]]
    })
  })
  
  output$filter <- renderUI({
    if(length(values$num.filters) > 0) {
      lapply(values$num.filters, function(i) {
        fluidRow(
          column(4, uiOutput(paste0("col", i))),
          column(3, uiOutput(paste0("op", i))),
          column(3, uiOutput(paste0("val", i))),
          column(2, uiOutput(paste0("remove", i)))
        )
      })
    }
  })
  
  observeEvent(input$addSort, {
    values$m <- values$m + 1
    m <- values$m
    values[[paste0("selected_sortcol", m)]] <- 1
    values[[paste0("selected_asc", m)]] <- "по возрастанию"
    
    values$num.sorts <- c(values$num.sorts, m)
    output[[paste0("sortcol", m)]] <- renderUI({
      selectizeInput(paste0("sortcol", m), "", choices = values$colnames, 
                     selected = values[[paste0("selected_sortcol", m)]])
    })
    output[[paste0("asc", m)]] <- renderUI({
      selectizeInput(paste0("asc", m), "", multiple = F, choices = list("по возрастанию", "по убыванию"), 
                     selected = values[[paste0("selected_asc", m)]])
    })
    output[[paste0("removesort", m)]] <- renderUI({
      actionButton(paste0("removesort", m), "x", class="btn-danger")
    })
    
    observeEvent(input[[paste0("removesort", m)]], {
      values$num.sorts <- values$num.sorts[-which(values$num.sorts == m)]
    })
    observe({
      values[[paste0("selected_sortcol", m, sep="")]] <- input[[paste0("sortcol", m)]]
      values[[paste0("selected_asc", m, sep="")]] <- input[[paste0("asc", m)]]
    })
  })
  
  output$sort <- renderUI({
    if(length(values$num.sorts) > 0) {
      lapply(values$num.sorts, function(i) {
        fluidRow(
          column(5, uiOutput(paste0("sortcol", i))),
          column(5, uiOutput(paste0("asc", i))),
          column(2, uiOutput(paste0("removesort", i)))
        )
      })
    }
  })
  
  values$numBlobs <- 0
  values$numBlackholes <- 0
  
  insertedBlobs <- c()
  insertedBlackholes <- c()
  
  output$generated <- reactive({
    values$generated
  })
  
  outputOptions(output, "generated", suspendWhenHidden=FALSE)
  
  observeEvent(input$insertBlob, {
    values$generated <- FALSE
    btn <- input$insertBlob
    id <- paste0("blob", btn)
    values[[paste0(id, "_M")]] <- input$M
    insertUI(
      selector = "#generatorPanel",
      ui = tags$div(
        wellPanel(
          h4(paste0("Параметры набора ", btn, ":")),
          numericInput(paste0(id, "_N"), "N", 10, min=1, max=1000),
          lapply(1:input$M, function(i) {
            fluidRow(
              column(3, selectInput(paste0(id, "_distr", i), label = paste0("X", i),
                        choices = list("Нормальное" = "normal",
                                       "Пуассона" = "Poisson",
                                       "Лапласа" = "Laplace",
                                       "Равномерное" = "uniform",
                                       "Треугольное" = "triangular",
                                       "Биномиальное" = "binomial"
                                       ),
                        selected = "normal")),
              uiOutput(paste0(id, "_distr_params", i))
            )
          }),
          actionButton(paste0("deleteBlob", btn), "Удалить", class="btn-danger")
        ),
        id = id
        )
    )
    
    lapply(1:input$M, function(i) {
      output[[paste0(id, "_distr_params", i)]] <- renderUI({
        switch(input[[paste0(id, "_distr", i)]],
               normal = fluidRow(
                 column(2, numericInput(paste0(id, "_distr", i, "_mean"), "mean", 0)),
                 column(2, numericInput(paste0(id, "_distr", i, "_sd"), "sd", 1, min=0))
               ),
               Poisson = fluidRow(
                 column(2, numericInput(paste0(id, "_distr", i, "_lambda"), "lambda", 1, min=0))
               ),
               Laplace = fluidRow(
                 column(2, numericInput(paste0(id, "_distr", i, "_m"), "m", 0)),
                 column(2, numericInput(paste0(id, "_distr", i, "_s"), "s", 0))
               ),
               uniform = fluidRow(
                 column(2, numericInput(paste0(id, "_distr", i, "_min"), "min", 0)),
                 column(2, numericInput(paste0(id, "_distr", i, "_max"), "max", 1))
               ),
               triangular = fluidRow(
                 column(2, numericInput(paste0(id, "_distr", i, "_min"),  "min", -1)),
                 column(2, numericInput(paste0(id, "_distr", i, "_mode"), "mode", 0)),
                 column(2, numericInput(paste0(id, "_distr", i, "_max"),  "max", 1))
               ),
               binomial = fluidRow(
                 column(2, numericInput(paste0(id, "_distr", i, "_size"), "size", 1, min=1)),
                 column(2, numericInput(paste0(id, "_distr", i, "_prob"), "prob", 0.5, min=0, max=1))
               )
               )
      })
    })
    
    insertedBlobs <<- c(insertedBlobs, id)
    
    observeEvent(input[[paste0("deleteBlob", btn)]], {
      values$generated <- FALSE
      removeUI(
        selector = paste0("#", id)
      )
      insertedBlobs <<- insertedBlobs[-which(insertedBlobs == id)]
    })
  })
  
  observeEvent(input$insertBlackhole, {
    values$generated <- FALSE
    btn <- input$insertBlackhole
    id <- paste0("blackhole", btn)
    values[[paste0(id, "_M")]] <- input$M
    
    insertUI(
      selector = "#generatorPanel",
      ui = tags$div(
        wellPanel(
          h4(paste0("Центр дыры ", btn, ":")),
          lapply(1:input$M, function(i) {
            numericInput(paste0(id, "_x", i), paste0("X", i), 0)
          }),
          numericInput(paste0(id, "_r"), "Радиус", 1),
          numericInput(paste0(id, "_v"), "Скорость затухания", 0.5),
          actionButton(paste0("deleteBlackhole", btn), "Удалить", class="btn-danger")
        ),
        id = id
        )
    )
    insertedBlackholes <<- c(insertedBlackholes, id)
    
    observeEvent(input[[paste0("deleteBlackhole", btn)]], {
      values$generated <- FALSE
      removeUI(
        selector = paste0("#", id)
      )
      insertedBlackholes <<- insertedBlackholes[-which(insertedBlackholes == id)]
    })
  })
  
  validateGenerator <- function() {
    ok <- TRUE
    if(length(insertedBlobs) == 0) {
      showNotification(paste0("Не указано ни одного набора данных"), type="error")
      ok <- FALSE
    }
    for(id in insertedBlobs) {
      if(values[[paste0(id, "_M")]] != input$M) {
        showNotification(paste0("Количество измерений в наборе ", substring(id, 5), " неверно"), type="error")
        ok <- FALSE
      }
    }
    for(id in insertedBlackholes) {
      if(values[[paste0(id, "_M")]] != input$M) {
        showNotification(paste0("Количество измерений в дыре ", substring(id, 10), " неверно"), type="error")
        ok <- FALSE
      }
    }
    return(ok)
  }
  
  bell <- function(x, alpha) {
    return((4 * x * (1 - x))**alpha)
  }
  
  observeEvent(input$generate, {
    if(validateGenerator()) {
      values$generated <- TRUE
      values$uploaded <- FALSE
      df <- data.frame()
      for(id in insertedBlobs) {
        l <- lapply(1:input$M, function(i) {
          switch(input[[paste0(id, "_distr", i)]],
                 normal = rnorm(n = input[[paste0(id, "_N")]],
                                mean = input[[paste0(id, "_distr", i, "_mean")]],
                                sd = input[[paste0(id, "_distr", i, "_sd")]]),
                 Poisson = rpois(n = input[[paste0(id, "_N")]],
                                 lambda = input[[paste0(id, "_distr", i, "_lambda")]]),
                 Laplace = rlaplace(n = input[[paste0(id, "_N")]],
                                    m = input[[paste0(id, "_distr", i, "_m")]],
                                    s = input[[paste0(id, "_distr", i, "_s")]]),
                 uniform = runif(n = input[[paste0(id, "_N")]],
                                 min = input[[paste0(id, "_distr", i, "_min")]],
                                 max = input[[paste0(id, "_distr", i, "_max")]]),
                 triangular = rtriang(n = input[[paste0(id, "_N")]],
                                        min = input[[paste0(id, "_distr", i, "_min")]],
                                        mode = input[[paste0(id, "_distr", i, "_mode")]],
                                        max = input[[paste0(id, "_distr", i, "_max")]]),
                 binomial = rbinom(n = input[[paste0(id, "_N")]],
                                   size = input[[paste0(id, "_distr", i, "_size")]],
                                   prob = input[[paste0(id, "_distr", i, "_prob")]]
                                   )
          )
        })
        tdf <- do.call(cbind.data.frame, l)
        colnames(tdf) <- sapply(1:input$M, function(i) paste0("X", i))
        df <- rbind(df, tdf)
      }
      df["del"] <- rep(0, nrow(df))

      for(id in insertedBlackholes) {
        for(j in 1:nrow(df)) {
          dist <- sum((unlist(subset(df[j, ], select=-del)) - unlist(lapply(1:input$M, function(i) as.double(input[[paste0(id, "_x", i)]]))))**2)
          if(dist < input[[paste0(id, "_r")]]**2) {
            p <- bell(dist / (2 * input[[paste0(id, "_r")]]) + 0.5, input[[paste0(id, "_v")]])
            if(rbinom(1, 1, p)) {
              df[j, "del"] <- 1
            }
          }
        }
      }
      df <- cbind("id" = seq.int(nrow(df)), df)
      values$df <- df
    }
  })
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "Clustering",
    tabPanel("Генерация",
             sidebarPanel(
               numericInput("M", "M", 1, min=1, max=10),
               tags$hr(),
               actionButton("insertBlob", "Добавить набор"),
               actionButton("insertBlackhole", "Добавить дыру"),
               tags$hr(),
               fluidRow(
                 column(4, actionButton("generate", "Сгенерировать", class="btn-info")),
                 column(4, conditionalPanel(condition = "output.generated", p("Сгенерировано!"))),
                 column(4, downloadButton("downloadGenerated", "Скачать", class="btn-success"))
               ),
               tags$hr(),
               fileInput("datafile", "или выбрать файл для загрузки",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values",
                           "text/tab-separated-values",
                           "text/plain",
                           ".csv",
                           ".tsv"
                         ),
                         buttonLabel="Загрузить",
                         placeholder="Файл не выбран"
               ),
               checkboxInput("header", "Заголовок", TRUE),
               radioButtons("sep", "Разделитель:",
                            c("Запятая"=",",
                              "Точка с запятой"=";",
                              "Табуляция"="\t"),
                            ","),
               tags$hr(),
               p("Для тестирования можете загрузить примеры файлов .csv or .tsv:",
                 a(href = "mtcars.csv", "mtcars.csv"), "или",
                 a(href = "pressure.tsv", "pressure.tsv")
               )
             ),
             mainPanel(
               tags$div(id = "generatorPanel")
             )
    ),
    tabPanel("Полная таблица",
      mainPanel(
        DT::dataTableOutput("fulltable")
      )
    ),
    tabPanel("Таблица",
             sidebarPanel(
               p("Номера строк и столбцов следует указывать через запятую, ",
                 "интервалы можно указывать через тире. Например: 1,2,3-10,15-17,20"),
               h5("Столбцы подбазы"),
               textInput("subsetCols", label=""),
               h5("Строки подбазы"),
               textInput("subsetRows", label=""),
               tags$hr(),
               uiOutput("filter"),
               actionButton("addFilter", "Добавить фильтр"),
               tags$hr(),
               uiOutput("sort"),
               actionButton("addSort", "Добавить сортировку"),
               tags$hr(),
               h5("Столбцы подбазы после сортировки"),
               textInput("subsetColsFiltered", label=""),
               h5("Строки подбазы после сортировки"),
               textInput("subsetRowsFiltered", label=""),
               downloadButton("downloadData", "Скачать csv", class="btn-success")
             ),
             mainPanel(
               h2("Исходная таблица"),
               DT::dataTableOutput("maintable"),
               tags$hr(),
               h2("Подбаза"),
               DT::dataTableOutput("subtable")
             )
    ),
    tabPanel("Информация",
             sidebarPanel(
               selectInput("infoColumn", "Выберите столбец", choices=NULL, selectize=TRUE)
             ),
             mainPanel(
               verbatimTextOutput("outInfoColumn"),
               h4("График плотности:"),
               plotlyOutput("densityPlot")
             )
    ),
    tabPanel("Матрицы",
             mainPanel(
               h4("Матрица корреляции:"),
               tableOutput("corrMatrix"),
               downloadButton("downloadCorr", "Скачать csv", class="btn-success"),
               tags$hr(),
               h4("Матрица ковариации:"),
               tableOutput("covMatrix"),
               downloadButton("downloadCov", "Скачать csv", class="btn-success")
             )
    ),
    tabPanel("Кластеризация",
             sidebarPanel(
               radioButtons("clusteringMethod", "Метод кластеризации", 
                            choices=c("одиночной связи"="single",
                                      "средней связи"="average",
                                      "полной связи"="complete"
                                      ),
                            selected = c("single")),
               numericInput("numClusters", "Количество кластеров", value = 3, min = 1),
               tags$hr(),
               selectizeInput(
                 "elementNames", "Использовать в качестве названия", choices = NULL, multiple = FALSE
               ),
               tags$hr(),
               downloadButton("downloadDist", "Скачать матрицу смежности", class="btn-success")
             ),
             mainPanel(
               h4("Дендрограмма:"),
               plotOutput("dendrogram"),
               tags$hr(),
               h4("Анализ главных компонент:"),
               plotlyOutput("pca"),
               h4("Кластеры"),
               plotlyOutput("graph")
             )
    )
  )
)

shinyApp(ui = ui, server = server)
