library(aRchery)
library(shiny)
source("R/gather-data.R")
options(shiny.maxRequestSize=3000*1024^2)

ui <- fluidPage(
    titlePanel("Archery v0.0.2"),

    sidebarLayout(
        sidebarPanel(
            fileInput("files", label = "Upload files", multiple = TRUE),
            downloadButton("downloadData")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("MainData")           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    mainData <- reactive({
        req(input$files)
        data <- read_archery_files(input$files$datapath)
        data
    })
    output$MainData <- DT::renderDataTable({
        req(mainData())
        datatable(mainData())
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('archery-data-', Sys.Date(), '.xlsx', sep='')
        },
        content = function(con) {
            
            openxlsx::write.xlsx(mainData(), con)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
