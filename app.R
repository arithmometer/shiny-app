library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggdendro)
library(igraph)
library(rmutil)
library(mc2d)
library(ggfortify)
library(Rtsne)
library(Rssa)

source("k_means_mahalanobis.R")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)

server <- function(input, output, clientData, session) {
  inFile <- reactive(input$datafile)
  tsFile <- reactive(input$timeseriesfile)
  
  subsetCols <- c()
  subsetRows <- c()
  subsetColsFiltered <- c()
  subsetRowsFiltered <- c()

  values <- reactiveValues()
  values$uploaded <- FALSE
  values$tsuploaded <- FALSE
  values$generated <- FALSE
  values$decimated <- FALSE
  values$generatedL <- FALSE
  values$processedL <- FALSE
  values$insertedFilters <- c()
  values$insertedSorts <- c()

  observeEvent(input$datafile, {
    values$df <- read.csv(inFile()$datapath, header = input$header, sep = input$sep)
    if(!("del" %in% colnames(values$df))) {
      values$df["del"] <- 0
    }
    values$uploaded <- TRUE
    values$generated <- FALSE
    updateNumericInput(session, "LN", value=nrow(values$df))
  })
  
  observeEvent(input$timeseriesfile, {
    values$ts <- read.csv(tsFile()$datapath, header = input$tsheader, sep = input$tssep)
    values$tsuploaded <- TRUE
  })
  
  getFullTable <- reactive({
    values$colnames <- colnames(values$df)
    values$df
  })
  
  getTable <- reactive({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    df <- values$df
    if("del" %in% colnames(df)) {
      df <- subset(df[df[, "del"] == 0, ], select=-del)
    }
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
    for(id in values$insertedFilters) {
      if(!is.null(input[[paste0(id, "val")]]) && input[[paste0(id, "val")]] != "") {
        df <- switch(input[[paste0(id, "op")]],
                     "=" = df[df[, input[[paste0(id, "col")]]] == input[[paste0(id, "val")]],],
                     "<" = df[df[, input[[paste0(id, "col")]]] <  input[[paste0(id, "val")]],],
                     ">" = df[df[, input[[paste0(id, "col")]]] >  input[[paste0(id, "val")]],]
          )
      }
    }
    for(id in values$insertedSorts) {
      if(!is.null(input[[paste0(id, "sortcol")]])) {
        df <- switch(input[[paste0(id, "asc")]],
                     "по возрастанию" = df[order(df[[paste0(input[[paste0(id, "sortcol")]])]]), ],
                     "по убыванию" = df[order(-df[[paste0(input[[paste0(id, "sortcol")]])]]), ]
        )
      }
    }
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    df
  })
  
  getFilteredSubTable <- reactive({
    wn <- getSubTable()[unlist(getSubsetRowsFiltered()), unlist(getSubsetColsFiltered()), drop=FALSE]
    wn[ , !names(wn) %in% c(input$elementNames)]
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
    validate(
      need(nrow(getTable()) > 0, "Нет ни одной строки")
    )
    DT::datatable(getTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  output$fulltable <- DT::renderDataTable({
    validate(
      need(nrow(getTable()) > 0, "Нет ни одной строки")
    )
    DT::datatable(getFullTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  output$outInfoColumn <- renderText({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    arr <- getFilteredSubTable()[, input$infoColumn]
    sprintf("Среднее: %f, стандартное отклонение: %f", mean(arr), sd(arr))
    })
  
  output$densityPlot <- renderPlotly({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    
    validate(
      need(is.numeric(getFilteredSubTable()[, input$infoColumn]), "Выберите числовой столбец")
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
    validate(
      need(input$clusteringMethod != "kmeans", "Недоступно для k-means")
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
    if(input$clusteringMethod == "kmeans") {
      distFunc <- switch(input$kmeansmetric,
                         "euclid"=distEuclid,
                         "mahalanobis"=distMahalanobis)
      clusters <- KMeans(as.matrix(getFilteredSubTable()), input$numClusters, distFunc = )$class
    } else {
      hc <- getHC()
      clusters <- cutree(hc, k = input$numClusters)
    }
    updateSelectInput(session, "clusterNumber", choices = 1:input$numClusters)
    clusters
  })
  
  output$metric <- renderUI({
    if(input$clusteringMethod == "kmeans") {
      radioButtons("kmeansmetric", "Метрика",
                   choices=c("евклидова"="euclid",
                             "Махаланобиса"="mahalanobis"),
                   selected = c("euclid"))
    }
  })
  
  getPCA <- reactive({
    pca <- princomp(getFilteredSubTable(), cor = TRUE)
    comps <- colnames(pca$scores)
    updateSelectInput(session, "pc1", "PC1", choices=comps, selected=comps[1])
    updateSelectInput(session, "pc2", "PC2", choices=comps, selected=comps[2])
    updateSelectInput(session, "pc3", "PC3", choices=comps, selected=comps[3])
    pca
  })
  
  output$pca <- renderPlotly({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    validate(
      need(is.numeric(as.matrix(getFilteredSubTable())), "Матрица содержит нечисловой столбец")
    )
    validate(
      need(ncol(getFilteredSubTable()) > 2, "Количество столбцов недостаточно")
    )
    pca <- getPCA()
    clusters <- getClusters()
    df <- data.frame(pca$scores, "cluster" = factor(clusters))
    df <- transform(df, cluster_name = paste("Cluster", clusters))
    plot_ly(df, x = ~df[, input$pc1], y = ~df[, input$pc2], z = ~df[, input$pc3], type = "scatter3d", mode = "markers",
            color = ~cluster_name, marker = list(symbol = 'circle', size = 4)) %>%
      layout(title="Анализ главных компонент",
             scene = list(xaxis = list(title=input$pc1),
                          yaxis = list(title=input$pc2),
                          zaxis = list(title=input$pc3)))
  })
  
  output$graph <- renderPlotly({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
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
      need(values$df, "Сгенерируйте или загрузите данные")
    )
    validate(
      need(is.numeric(as.matrix(getFilteredSubTable())), "Матрица содержит нечисловой столбец")
    )
    getCorrMatrix()
  })
  
  output$covMatrix <- renderTable({
    validate(
      need(values$df, "Сгенерируйте или загрузите данные")
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
      write.table(values$df, file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadGenerated2 <- downloadHandler(
    filename = "generated_dataset.csv",
    content = function(file) {
      write.table(values$df, file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadDecimated <- downloadHandler(
    filename = "generated_dataset.csv",
    content = function(file) {
      write.table(values$df, file, sep=",", row.names = FALSE)
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
      need(values$df, "Сгенерируйте или загрузите данные")
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
    btn <- input$addFilter
    id <- paste0("filter", btn)
    insertUI(
      selector = "#filterPanel",
      ui = tags$div(
        wellPanel(
          fluidRow(column(4, selectizeInput(paste0(id, "col"), "", choices = values$colnames)),
                   column(3, selectizeInput(paste0(id, "op"), "", multiple = F, choices = list("=", "<", ">"))),
                   column(4, textInput(paste0(id, "val"), "")),
                   column(1, actionButton(paste0(id, "delete"), "x", class="btn-danger"))
          )
        ),
        id = id
      )
    )
    
    values$insertedFilters <<- c(values$insertedFilters, id)
    
    observeEvent(input[[paste0(id, "delete")]], {
      removeUI(
        selector = paste0("#", id)
      )
      values$insertedFilters <- values$insertedFilters[-which(values$insertedFilters == id)]
    })
  })
  
  observeEvent(input$addSort, {
    btn <- input$addSort
    id <- paste0("sort", btn)
    insertUI(
      selector = "#sortPanel",
      ui = tags$div(
        wellPanel(
          fluidRow(column(4, selectizeInput(paste0(id, "sortcol"), "", choices = values$colnames)),
                   column(6, selectizeInput(paste0(id, "asc"), "", multiple = F, choices = list("по возрастанию", "по убыванию"))),
                   column(1, actionButton(paste0(id, "delete"), "x", class="btn-danger"))
          )
        ),
        id = id
      )
    )
    
    values$insertedSorts <<- c(values$insertedSorts, id)
    
    observeEvent(input[[paste0(id, "delete")]], {
      removeUI(
        selector = paste0("#", id)
      )
      values$insertedSorts <- values$insertedSorts[-which(values$insertedSorts == id)]
    })
  })

  insertedBlobs <- c()
  insertedBlackholes <- c()
  
  output$generated <- reactive({
    values$generated
  })
  
  output$decimated <- reactive({
    values$decimated
  })
  
  output$processedL <- reactive({
    values$processedL
  })
  
  outputOptions(output, "generated", suspendWhenHidden=FALSE)
  outputOptions(output, "decimated", suspendWhenHidden=FALSE)
  outputOptions(output, "processedL", suspendWhenHidden=FALSE)
  
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
          numericInput(paste0(id, "_N"), "Количество точек", 10, min=1, max=1000),
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
          actionButton(paste0(id, "delete"), "Удалить", class="btn-danger")
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
    
    observeEvent(input[[paste0(id, "delete")]], {
      values$generated <- FALSE
      removeUI(
        selector = paste0("#", id)
      )
      insertedBlobs <<- insertedBlobs[-which(insertedBlobs == id)]
    })
  })
  
  observeEvent(input$insertBlackhole, {
    # values$generated <- FALSE
    btn <- input$insertBlackhole
    id <- paste0("blackhole", btn)
    values[[paste0(id, "_M")]] <- input$M
    
    insertUI(
      selector = "#decimationPanel",
      ui = tags$div(
        wellPanel(
          h4(paste0("Центр дыры ", btn, ":")),
          lapply(1:input$M, function(i) {
            numericInput(paste0(id, "_x", i), paste0("X", i), 0)
          }),
          numericInput(paste0(id, "_r"), "Радиус", 1),
          numericInput(paste0(id, "_v"), "Скорость затухания", 0.5),
          actionButton(paste0(id, "delete"), "Удалить", class="btn-danger")
        ),
        id = id
        )
    )
    insertedBlackholes <<- c(insertedBlackholes, id)
    
    observeEvent(input[[paste0(id, "delete")]], {
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
    return(ok)
  }
  
  validateDecimator <- function() {
    ok <- TRUE
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

      df <- cbind("id" = seq.int(nrow(df)), df)
      updateNumericInput(session, "LN", value=nrow(df))
      values$df <- df
    }
  })
  
  observeEvent(input$decimate, {
    if(validateDecimator()) {
      for(id in insertedBlackholes) {
        for(j in 1:nrow(values$df)) {
          d <- sum((unlist(subset(values$df[j, ], select=-del)) - unlist(lapply(1:input$M, function(i) as.double(input[[paste0(id, "_x", i)]]))))**2)
          if(d < input[[paste0(id, "_r")]]**2) {
            p <- bell(sqrt(d) / (2 * input[[paste0(id, "_r")]]) + 0.5, input[[paste0(id, "_v")]])
            if(rbinom(1, 1, p)) {
              values$df[j, "del"] <- 1
            }
          }
        }
      }
      values$decimated <- TRUE
    }
  })
  
  observeEvent(input$generateL, {
    x <- getFilteredSubTable()
    M <- ncol(x)
    l <- lapply(1:M, function(i) {
      runif(n = input$LN, min = min(x[, i]), max = max(x[, i]))
    })
    df <- do.call(cbind.data.frame, l)
    colnames(df) <- sapply(1:M, function(i) paste0("X", i))
    df["del"] <- 0
    values$L <- df
    values$generatedL <- TRUE
  })
  
  dst2 <- function(x, y) {
    sum((x - y)**2)
  }
  
  observeEvent(input$removeM, {
    x <- getFilteredSubTable()
    for(i in 1:nrow(x)) {
      for(j in 1:nrow(values$L)) {
        d <- dst2(unlist(subset(values$L[j, ], select=-del)), unlist(x[i, ]))
        if(d < input$eps**2) {
          values$L[j, "del"] <- 1
        }
      }
    }
    values$processedL <- TRUE
  })
  
  output$downloadL <- downloadHandler(
    filename = "manifold_L.csv",
    content = function(file) {
      write.table(values$L, file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadCluster <- downloadHandler(
    filename = paste0("cluster", input$clusterNumber, ".csv"),
    content = function(file) {
      write.table(getFilteredSubTable()[getClusters() == input$clusterNumber, ], file, sep=",", row.names = FALSE)
    }
  )

  output$manifold <- renderPlot({
    if((ncol(getFilteredSubTable()) != 2) || (input$visualisation != "dd")) {
      set.seed(42)
      df <- unique(getFilteredSubTable())
      tsne_out <- Rtsne(df, perplexity=input$perplexity)
      dat <- data.frame(X1=tsne_out$Y[, 1], X2=tsne_out$Y[, 2])
      g <- ggplot(dat, aes(x=X1, y=X2)) + 
        geom_point(color="blue")
      if(values$generatedL) {
        dfL <- unique(values$L[values$L["del"] == 0, ])
        tsne_L <- Rtsne(dfL, perplexity=input$perplexity)
        dat2 <- data.frame(X1=tsne_L$Y[, 1], X2=tsne_L$Y[, 2])
        if(nrow(dat2) > 0) {
          g <- g + geom_point(dat2, mapping = aes(x=X1, y=X2, color="red"))
        }
      }
    } else {
      dat <- data.frame(X1=getFilteredSubTable()[, 1], X2=getFilteredSubTable()[, 2])
      g <- ggplot(dat, aes(x=X1, y=X2)) +
        geom_point(color="blue")
      if(values$generatedL) {
        dfL <- values$L[values$L["del"] == 0, ]
        dat2 <- data.frame(X1=dfL[, 1], X2=dfL[, 2])
        if(nrow(dat2) > 0) {
          g <- g + geom_point(dat2, mapping = aes(x=X1, y=X2, color="red"))
        }
      }
    }
    g
  })
  
  observeEvent(input$ssaDecompose, {
    validate(
      need(values$ts, "Загрузите временной ряд")
    )
    values$s <- ssa(values$ts, L = input$ssaL, neig = input$ssaN)
  })
  
  output$ts <- renderPlotly({
    validate(
      need(values$ts, "Загрузите временной ряд")
    )
    plot_ly(y=values$ts[, 2], type="scatter", mode="lines")
  })
  
  output$wcor <- renderPlot({
    validate(
      need(values$s, "Выполните разложение ряда")
    )
    plot(wcor(values$s))
  })
  
  output$scree <- renderPlot({
    validate(
      need(values$s, "Выполните разложение ряда")
    )
    plot(values$s)
  })
  
  output$paired <- renderUI({
    validate(
      need(values$s, "Выполните разложение ряда")
    )
    do.call(tabsetPanel, lapply(0:((input$ssaN + 19) %/% 20 - 1), function(i) {
      last <- i * 20 + 20
      if(i * 20 + 20 >= input$ssaN) {
        last <- input$ssaN - 1
      }
      output[[paste0("paired_", i)]] <- renderPlot(plot(values$s, type = "paired", idx = (i * 20 + 1):last))
      tabPanel(paste0(i * 20 + 1, "-", i * 20 + 20), plotOutput(paste0("paired_", i)))
    }))
  })
  
  output$visualisationChoice <- renderUI({
    if(ncol(getFilteredSubTable()) == 2) {
      radioButtons("visualisation", "Тип визуализации",
                   choices=c("t-SNE"="tsne",
                             "2D"="dd"),
                   selected = c("tsne"))
    }
  })
  
  output$perplexity <- renderUI({
    if((ncol(getFilteredSubTable()) != 2) || (input$visualisation != "dd")) {
      numericInput("perplexity", "Перплексия t-SNE", value=10)
    }
  })
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "Clustering",
    tabPanel("Генерация",
             sidebarPanel(
               numericInput("M", "Размерность M", 1, min=1, max=10),
               tags$hr(),
               actionButton("insertBlob", "Добавить набор"),
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
    tabPanel("Прореживание",
             sidebarPanel(
             actionButton("insertBlackhole", "Добавить дыру"),
             tags$hr(),
             fluidRow(
               column(4, actionButton("decimate", "Проредить", class="btn-info")),
               column(4, conditionalPanel(condition = "output.decimated", p("Прорежено!"))),
               column(4, downloadButton("downloadDecimated", "Скачать", class="btn-success"))
             )
           ),
           mainPanel(
             tags$div(id = "decimationPanel")
           )
    ),
    tabPanel("Полная таблица",
      mainPanel(
        DT::dataTableOutput("fulltable"),
        downloadButton("downloadGenerated2", "Скачать", class="btn-success")
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
               selectizeInput("elementNames", "Использовать в качестве названия", choices = NULL, multiple = FALSE),
               tags$hr(),
               actionButton("addFilter", "Добавить фильтр"),
               tags$div(id = "filterPanel"),
               tags$hr(),
               actionButton("addSort", "Добавить сортировку"),
               tags$div(id = "sortPanel"),
               tags$hr(),
               h5("Столбцы подбазы после фильтрации"),
               textInput("subsetColsFiltered", label=""),
               h5("Строки подбазы после фильтрации"),
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
    tabPanel("Многообразие",
             sidebarPanel(
               uiOutput("visualisationChoice"),
               uiOutput("perplexity"),
               tags$hr(),
               fluidRow(
                 column(4, numericInput("LN", "N", 100)),
                 column(7, actionButton("generateL", "Добавить множество L", class="btn-info"))
               ),
               tags$hr(),
               fluidRow(
                 column(4, numericInput("eps", "Epsilon", 1)),
                 column(7, actionButton("removeM", "Удалить множество M", class="btn-danger"))
               ),
               conditionalPanel(condition = "output.processedL", 
                                downloadButton("downloadL", "Скачать множество L", class="btn-success")
                                )
             ),
             mainPanel(
               plotOutput("manifold")
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
                                      "полной связи"="complete",
                                      "k-means"="kmeans"
                                      ),
                            selected = c("single")),
               uiOutput("metric"),
               numericInput("numClusters", "Количество кластеров", value = 3, min = 1),
               tags$hr(),
               h4("Главные компоненты"),
               fluidRow(column(4, selectInput("pc1", "PC1", choices=c())),
                        column(4, selectInput("pc2", "PC2", choices=c())),
                        column(4, selectInput("pc3", "PC3", choices=c()))),
               tags$hr(),
               downloadButton("downloadDist", "Скачать матрицу смежности", class="btn-success"),
               tags$hr(),
               fluidRow(column(5, selectInput("clusterNumber", "Номер кластера", choices=c())),
                        column(4, downloadButton("downloadCluster", "Скачать кластер", class="btn-success")))
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
    ),
    tabPanel("SSA",
             sidebarPanel(
               fileInput("timeseriesfile", "Выбрать файл для загрузки",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values",
                           "text/tab-separated-values",
                           "text/plain",
                           ".csv",
                           ".tsv"
                         ),
                         buttonLabel="Загрузить ряд",
                         placeholder="Файл не выбран"
               ),
               tags$hr(),
               checkboxInput("tsheader", "Заголовок", TRUE),
               radioButtons("tssep", "Разделитель:",
                            c("Запятая"=",",
                              "Точка с запятой"=";",
                              "Табуляция"="\t"),
                            ","),
               tags$hr(),
               numericInput("ssaL", "Длина окна L", value=100, min=1),
               numericInput("ssaN", "Количество компонент разложения", value=50, min=1),
               actionButton("ssaDecompose", "Выполнить разложение", class="btn-info"),
               tags$hr(),
               p("Для тестирования можете загрузить пример файла",
                 a(href = "wines.csv", "wines.csv")
               )
             ),
             mainPanel(
               h4("График временного ряда"),
               plotlyOutput("ts"),
               h4("График w-корреляций"),
               plotOutput("wcor"),
               h4("График собственных чисел"),
               plotOutput("scree"),
               h4("Парные графики собственных векторов"),
               uiOutput("paired")
             )
    )
  )
)

shinyApp(ui = ui, server = server)
