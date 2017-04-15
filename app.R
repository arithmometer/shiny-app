library(shiny)
library(shinythemes)
library(plotly)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)

server <- function(input, output, clientData, session) {
  inFile <- reactive(input$datafile)
  
  subsetCols <- c()
  subsetRows <- c()
  
  getTable <- reactive({
    df <- read.csv(inFile()$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    updateSelectizeInput(session, "sortColumns", choices=colnames(df))
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    updateTextInput(session, "subsetCols", "")
    updateTextInput(session, "subsetRows", "")
    df
  })
  
  getSubTable <- reactive({
    df <- getTable()[unlist(getSubsetRows()), unlist(getSubsetCols())]
    df
  })
  
  getSubsetCols <- reactive({
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
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json'),
                               order = lapply(input$sortColumns, function(x) list(which(colnames(getTable()) == x), 'asc'))
                  ))
  })
  
  output$debug <- reactive({
    input$infoColumn
  })
  
  output$outInfoColumn <- renderPrint({
    if (is.null(inFile())) {
      return(NULL)
    }
    arr <- getTable()[, input$infoColumn]
    sprintf("Среднее: %f, стандартное отклонение: %f", mean(arr), sd(arr))
    })
  
  output$densityPlot <- renderPlotly({
    if (is.null(inFile())) {
      return(NULL)
    }
    
    x <- getTable()[, input$infoColumn]
    fit <- density(x)
    
    plot_ly(x = x, type = "histogram", name = "Гистрограмма") %>% 
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Плотность") %>% 
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$dendrogram <- renderPlot({
    hc <- hclust(dist(getSubTable()), method=input$clusteringMethod)
    plot(hc)
  })
  
  getCorrMatrix <- reactive({
    cor(getSubTable())
  })
  
  getCovMatrix <- reactive({
    cov(getSubTable())
  })
  
  output$corrMatrix <- renderTable({
    getCorrMatrix()
  })
  
  output$covMatrix <- renderTable({
    getCovMatrix()
  })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_subset", ".csv", sep = ""),
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(getSubTable(), file, sep=",",
                  row.names = FALSE)
    }
  )
  
  output$downloadCorr <- downloadHandler(
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_corr", ".csv", sep = ""),
    content = function(file) {
      write.table(getCorrMatrix(), file, sep=",",
                  row.names = FALSE)
    }
  )
  
  output$downloadCov <- downloadHandler(
    filename = paste(substr(inFile(), 1, nchar(inFile())-4), "_cov", ".csv", sep = ""),
    content = function(file) {
      write.table(getCovMatrix(), file, sep=",",
                  row.names = FALSE)
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
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
                  ))
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
               #               radioButtons('quote', 'Кавычки',
               #                            c('Отсутствуют'='',
               #                              'Двойные кавычки'='"',
               #                              'Одинарные кавычки'="'"),
               #                            '"'),
               tags$hr(),
               selectizeInput(
                 'sortColumns', 'Сортировать по', choices = NULL, multiple = TRUE
               ),
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
               textInput("subsetCols", "Столбцы подбазы:"),
               textInput("subsetRows", "Строки подбазы:"),
               # textInput("n", "N"),
               tags$hr(),
               downloadButton('downloadData', 'Скачать csv')
             ),
             mainPanel(
               DT::dataTableOutput("subtable")
             )
    ),
    tabPanel("Информация",
             sidebarPanel(
               selectInput("infoColumn", "Выберите столбец", choices=NULL, selectize=TRUE),
               numericInput("bins", "Количество разбиений гистограммы", value=10)
             ),
             mainPanel(
               verbatimTextOutput("outInfoColumn"),
               h4("График плотности:"),
               plotlyOutput("densityPlot")
             )
    ),
    tabPanel("Матрицы",
             # sidebarPanel(),
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
                            choices=c("полной связи"="complete",
                                      "одиночной связи"="single",
                                      "средней связи"="average"
                                      ))
             ),
             mainPanel(
               h4("Дендрограмма:"),
               plotOutput("dendrogram")
             )
    ),
    tabPanel("Анализ главных компонент",
             sidebarPanel(),
             mainPanel()
    )
  )
)

shinyApp(ui = ui, server = server)
