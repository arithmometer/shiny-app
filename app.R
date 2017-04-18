library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggdendro)
library(igraph)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)

server <- function(input, output, clientData, session) {
  values <- reactiveValues()
  
  inFile <- reactive(input$datafile)
  
  subsetCols <- c()
  subsetRows <- c()
  
  getTable <- reactive({
    df <- read.csv(inFile()$datapath, header = input$header,
             sep = input$sep)
    values$colnames <- colnames(df)
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    updateSelectInput(session, "elementNames", choices=c("№", colnames(df)))
    updateTextInput(session, "subsetCols", "")
    updateTextInput(session, "subsetRows", "")
    df
  })
  
  getSubTable <- reactive({
    df <- getTable()[unlist(getSubsetRows()), unlist(getSubsetCols()), drop=FALSE]
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    df
  })
  
  getSubTableRowNames <- reactive({
    if(input$elementNames == "№") {
      rownames(getTable()[unlist(getSubsetRows()), ,])
    } else {
      res <- getTable()[unlist(getSubsetRows()), c(input$elementNames)]
      write.csv(str(res), 'debug.csv')
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
  
  output$maintable <- DT::renderDataTable({
    if (is.null(inFile())) {
      return(NULL)
    }
    DT::datatable(getTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  output$outInfoColumn <- renderText({
    if (is.null(inFile())) {
      return(NULL)
    }
    arr <- getSubTable()[, input$infoColumn]
    sprintf("Среднее: %f, стандартное отклонение: %f", mean(arr), sd(arr))
    })
  
  output$densityPlot <- renderPlotly({
    if (is.null(inFile())) {
      return(NULL)
    }
    
    validate(
      need(as.numeric(getSubTable()[, input$infoColumn]), "Выберите числовой столбец")
    )
    
    x <- getSubTable()[, input$infoColumn]
    fit <- density(x)
    
    plot_ly(x = x, type = "histogram", name = "Гистограмма") %>% 
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Плотность") %>% 
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$dendrogram <- renderPlot({
    dd <- as.dendrogram(getHC())
    df <- dendro_data(dd)
    ggdendrogram(df)
  })
  
  getDist <- reactive({
    dist(getSubTable())
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
      need(is.numeric(as.matrix(getSubTable())), "Матрица содержит нечисловой столбец")
    )
    pca <- princomp(getSubTable(), cor = TRUE)
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
    cor(getSubTable())
  })
  
  getCovMatrix <- reactive({
    cov(getSubTable())
  })
  
  output$corrMatrix <- renderTable({
    validate(
      need(is.numeric(as.matrix(getSubTable())), "Матрица содержит нечисловой столбец")
    )
    getCorrMatrix()
  })
  
  output$covMatrix <- renderTable({
    validate(
      need(is.numeric(as.matrix(getSubTable())), "Матрица содержит нечисловой столбец")
    )
    getCovMatrix()
  })
  
  output$downloadData <- downloadHandler(
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_subset", ".csv", sep = ""),
    content = function(file) {
      write.table(getSubTable(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadCorr <- downloadHandler(
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_corr", ".csv", sep = ""),
    content = function(file) {
      write.table(getCorrMatrix(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadCov <- downloadHandler(
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_cov", ".csv", sep = ""),
    content = function(file) {
      write.table(getCovMatrix(), file, sep=",", row.names = FALSE)
    }
  )
  
  output$downloadDist <- downloadHandler(
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_adjacency", ".csv", sep = ""),
    content = function(file) {
      write.table(as.matrix(getDist()), file, sep=",", row.names = FALSE)
    }
  )
  
  output$subtable <- DT::renderDataTable({
    if (is.null(inFile())) {
      return(NULL)
    }
    DT::datatable(getSubTable(),
                  options=list(pageLength=10, 
                               lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE,
                               ordering=FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
  })
  
  values$n <- 0
  values$m <- 0
  values$num.filters <- c()
  values$num.sorts <- c()
  
  observeEvent(input$addFilter, {
    values$n <- values$n + 1
    n <- values$n
    values[[paste("selected_col", n, sep="")]] <- 1
    values[[paste("selected_op", n, sep="")]] <- "="
    values[[paste("selected_val", n, sep="")]] <- ""
    
    values$num.filters <- c(values$num.filters, n)
    output[[paste("col", n, sep="")]] <- renderUI({
      selectizeInput(paste("col", n, sep=""), "", choices = values$colnames, 
                     selected = values[[paste("selected_col", n, sep="")]])
    })
    output[[paste("op", n, sep="")]] <- renderUI({
      selectizeInput(paste("op", n, sep=""), "", multiple = F, choices = list("=", "<", ">"), 
                     selected = values[[paste("selected_op", n, sep="")]])
    })
    output[[paste("val", n, sep="")]] <- renderUI({
      textInput(paste("val", n, sep=""), "", value = values[[paste("selected_val", n, sep="")]])
    })
    output[[paste("remove", n, sep="")]] <- renderUI({
      actionButton(paste("remove", n, sep=""), "x")
    })
    
    observeEvent(input[[paste("remove", n, sep="")]], {
      values$num.filters <- values$num.filters[-which(values$num.filters == n)]
    })
    observe({
      values[[paste("selected_col", n, sep="")]] <- input[[paste("col", n, sep="")]]
      values[[paste("selected_op", n, sep="")]] <- input[[paste("op", n, sep="")]]
      values[[paste("selected_val", n, sep="")]] <- input[[paste("val", n, sep="")]]
    })
  })
  
  output$filter <- renderUI({
    if(length(values$num.filters) > 0) {
      lapply(values$num.filters, function(i) {
        fluidRow(
          column(4, uiOutput(paste("col", i, sep=""))),
          column(3, uiOutput(paste("op", i, sep=""))),
          column(3, uiOutput(paste("val", i, sep=""))),
          column(2, uiOutput(paste("remove", i, sep="")))
        )
      })
    }
  })
  
  observeEvent(input$addSort, {
    values$m <- values$m + 1
    m <- values$m
    values[[paste("selected_sortcol", m, sep="")]] <- 1
    values[[paste("selected_asc", m, sep="")]] <- "по возрастанию"
    
    values$num.sorts <- c(values$num.sorts, m)
    output[[paste("sortcol", m, sep="")]] <- renderUI({
      selectizeInput(paste("sortcol", m, sep=""), "", choices = values$colnames, 
                     selected = values[[paste("selected_sortcol", m, sep="")]])
    })
    output[[paste("asc", m, sep="")]] <- renderUI({
      selectizeInput(paste("asc", m, sep=""), "", multiple = F, choices = list("по возрастанию", "по убыванию"), 
                     selected = values[[paste("selected_asc", m, sep="")]])
    })
    output[[paste("removesort", m, sep="")]] <- renderUI({
      actionButton(paste("removesort", m, sep=""), "x")
    })
    
    observeEvent(input[[paste("removesort", m, sep="")]], {
      values$num.sorts <- values$num.sorts[-which(values$num.sorts == m)]
    })
    observe({
      values[[paste("selected_sortcol", m, sep="")]] <- input[[paste("sortcol", m, sep="")]]
      values[[paste("selected_asc", m, sep="")]] <- input[[paste("asc", m, sep="")]]
    })
  })
  
  output$sort <- renderUI({
    if(length(values$num.sorts) > 0) {
      lapply(values$num.sorts, function(i) {
        fluidRow(
          column(5, uiOutput(paste("sortcol", i, sep=""))),
          column(5, uiOutput(paste("asc", i, sep=""))),
          column(2, uiOutput(paste("removesort", i, sep="")))
        )
      })
    }
  })
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "Clustering",
    tabPanel("Данные",
             sidebarPanel(
               fileInput('datafile', 'Выбрать файл для загрузки',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )
               ),
               checkboxInput('header', 'Заголовок', TRUE),
               radioButtons('sep', 'Разделитель:',
                            c('Запятая'=',',
                              'Точка с запятой'=';',
                              'Табуляция'='\t'),
                            ','),
               tags$hr(),
               p('Для тестирования можете загрузить примеры файлов .csv or .tsv:',
                 a(href = 'mtcars.csv', 'mtcars.csv'), 'или',
                 a(href = 'pressure.tsv', 'pressure.tsv')
               )
             ),
             mainPanel(
               DT::dataTableOutput('maintable')
               )
    ),
    tabPanel("Подбаза",
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
               downloadButton('downloadData', 'Скачать csv')
             ),
             mainPanel(
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
               downloadButton('downloadCorr', 'Скачать csv'),
               tags$hr(),
               h4("Матрица ковариации:"),
               tableOutput("covMatrix"),
               downloadButton('downloadCov', 'Скачать csv')
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
                 'elementNames', 'Использовать в качестве названия', choices = NULL, multiple = FALSE
               ),
               tags$hr(),
               downloadButton('downloadDist', 'Скачать матрицу смежности')
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
