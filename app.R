library(shiny)
library(shinythemes)
library(ggplot2)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)

server <- function(input, output, clientData, session) {
  getTable <- reactive({
    df <- read.csv(input$datafile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    updateSelectizeInput(session, "sortColumns", choices=colnames(df))
    updateSelectInput(session, "infoColumn", choices=colnames(df))
    df
  })
  
  output$maintable <- DT::renderDataTable({
    inFile <- input$datafile
    if (is.null(inFile)) {
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
  
  output$outInfoColumn <- renderPrint({
    inFile <- input$datafile
    if (is.null(inFile)) {
      return(NULL)
    }
    arr <- getTable()[, input$infoColumn]
    sprintf("Среднее: %f, стандартное отклонение: %f", mean(arr), sd(arr))
    })
  
  output$densityPlot <- renderPlot({
    inFile <- input$datafile
    if (is.null(inFile)) {
      return(NULL)
    }
    
    ggplot(getTable(), aes(x=input$infoColumn)) + geom_density()
  })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$datafile, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      # write.table(subtable, file, sep = sep,
      #            row.names = FALSE)
    }
  )
  
  output$subtable <- DT::renderDataTable({
    inFile <- input$datafile
    if (is.null(inFile)) {
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
               textInput("subsetCols", "Столбцы подбазы:"),
               textInput("subsetRows", "Строки подбазы:"),
               tags$hr(),
               downloadButton('downloadData', 'Download')
             ),
             mainPanel(
               DT::dataTableOutput('subtable')
             )
    ),
    tabPanel("Информация",
             sidebarPanel(
               selectInput('infoColumn', 'Выберите столбец', choices=NULL, selectize=TRUE)
             ),
             mainPanel(
               verbatimTextOutput('outInfoColumn'),
               plotOutput("densityPlot")
             )
    ),
    tabPanel("Корреляция",
             sidebarPanel(),
             mainPanel()
    ),
    tabPanel("Кластеризация",
             sidebarPanel(),
             mainPanel()
    )
  )
)

shinyApp(ui = ui, server = server)
