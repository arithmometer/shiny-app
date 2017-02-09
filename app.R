library(shiny)
library(shinythemes)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)
df <- NULL
col.names <- c()

server <- function(input, output) {
  getTable <- reactive({
    df <<- read.csv(input$datafile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    col.names <<- colnames(df)
    df
  })
  
  output$maintable <- DT::renderDataTable({
    inFile <- input$datafile
    isolate(inFile)
    if (is.null(inFile))
      return(NULL)
    DT::datatable(getTable(),
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
                 'sortColumns', 'Сортировать по', choices = colnames(df), multiple = TRUE
               ),
               tags$hr(),
               p('Для тестирования можете загрузить примеры файлов .csv or .tsv:',
                 a(href = 'mtcars.csv', 'mtcars.csv'), 'или',
                 a(href = 'pressure.tsv', 'pressure.tsv')
               )
             ),
             mainPanel(
               DT::dataTableOutput('maintable'))
    ),
    tabPanel("Информация",
             sidebarPanel(),
             mainPanel()
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
