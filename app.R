library(aRchery)
library(shiny)
library(DT)
library(openxlsx)
library(dragulaR)
library(readr)
library(lubridate)
options(shiny.maxRequestSize=3000*1024^2)

source(dir("modules/", full.names = TRUE))

ui <- fluidPage(
    titlePanel("Archery v0.0.2"),

    sidebarLayout(
        sidebarPanel(
            fileInput("files", label = "Upload files", multiple = TRUE),
            selectInput("Dates", label = "Selected dates", choices = NULL, multiple = TRUE),
            downloadButton("downloadData")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(title = "All data",DT::dataTableOutput("MainData")),
                tabPanel(title = "Compare targets by days", compareTargetsModuleUI("TargetsByDay")),
                tabPanel(title = "Daily plot", plotOutput("DailyPlot", width = "100%", height = 600)),
                tabPanel(title = "Scale curve", plotOutput("ScaleCurve", width = "100%", height = 600))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

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
    
    
    ###### dates
    
    observe({
        req(mainData())
        choices <- sort(unique(mainData()[["Date"]]))
        selected <-
            na.omit(unique(c(
                choices[1], 
                choices[length(choices) / 2], 
                choices[length(choices)]))
            )
        updateSelectInput("Dates",
                          session = session,
                          choices = choices,
                          selected = selected)
    })
    
    selectedDates <- reactive({
        input$Dates
    })
    
    ## compareTargetsModule
    callModule(compareTargetsModule, id = "TargetsByDay", mainData = mainData, dates = selectedDates)
    
    output$ScaleCurve <- renderPlot({
        req(mainData(), selectedDates())
        dtAll <- mainData()[as.character(mainData()[["Date"]]) %in% selectedDates(), ]
        aRchery::plot_scale_curves(dtAll, "Date")
    })
    
    output$DailyPlot <- renderPlot({
        req(mainData())
        aRchery::plot_daily_total_points(mainData())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
