library(shiny)
library(shinythemes)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 1024MB.
options(shiny.maxRequestSize = 1024*1024^2)

server <- function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$datafile
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$mytable1 <- DT::renderDataTable({
    inFile <- input$datafile
    DT::datatable(read.csv(inFile$datapath, header = input$header,
                           sep = input$sep, quote = input$quote))
  })
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "Кластеризация",
    tabPanel("Раздел 1",
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
               tags$hr(),
               checkboxInput('header', 'Header', TRUE),
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ','),
               radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
               tags$hr(),
               p('If you want a sample .csv or .tsv file to upload,',
                 'you can first download the sample',
                 a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
                 a(href = 'pressure.tsv', 'pressure.tsv'),
                 'files, and then try uploading them.'
               ),
               tags$hr(),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Deafult actionButton:"),
               actionButton("action", "Search"),
               
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Вкладка 1",
                          h4("Таблица"),
                          tableOutput("contents"),
                          
                          h4("Пока что это приложение ничего не делает"),
                          # verbatimTextOutput("text output"),
                          h1("Заголовок 1"),
                          h2("Заголовок 2"),
                          h3("Заголовок 3"),
                          h4("Заголовок 4"),
                          h5("Заголовок 5")
                 ),
                 tabPanel("Вкладка 2",
                          DT::dataTableOutput('mytable1')),
                 tabPanel("Вкладка 3")
               )
             )
    ),
    tabPanel("Раздел 2"),
    tabPanel("Раздел 3")
  )
)

shinyApp(ui = ui, server = server)
