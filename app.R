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
    "Clustering",
    tabPanel("Navbar 1",
             sidebarPanel(
               fileInput('datafile', 'Choose file to upload',
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
                 tabPanel("Tab 1",
                          h4("Table"),
                          tableOutput("contents"),
                          
                          h4("This sample is doing nothing"),
                          verbatimTextOutput("text output"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
		tabPanel("Tab 2",
			DT::dataTableOutput('mytable1')),
		tabPanel("Tab 3")
               )
             )
    ),
    tabPanel("Navbar 2"),
    tabPanel("Navbar 3")
  )
)

shinyApp(ui = ui, server = server)
