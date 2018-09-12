library(pacman)
library(aRchery)
pacman::p_load(shiny)
pacman::p_load(DT)
pacman::p_load(openxlsx)
pacman::p_load(dragulaR)
pacman::p_load(readr)
pacman::p_load(lubridate)
pacman::p_load(dplyr)
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
                tabPanel(title = "Scale curve", plotOutput("ScaleCurve", width = "100%", height = 600)),
                tabPanel(title = "Daily shot series", 
                    selectInput(
                        "NSeries",
                        label = "Number of shots in group",
                        choices = c(1,  2,  3,  4,  5,  6, 10, 12, 15, 20, 30), 
                        selected = 12),    
                    plotOutput("FncBoxPlot", width = "100%", height = 600)),
                tabPanel(title = "Daily median shot", plotOutput("DailyMedians", width = "100%", height = 600))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    mainData <- reactive({
        req(input$files)
        data <- aRchery::read_archery_files(input$files$datapath)
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
        aRchery::plot_daily_total_points(mainData(), size = 2)
    })
    
    output$FncBoxPlot <- renderPlot({
        req(mainData())
        
        dtMatrix12 <- fnc_by_n_shots(mainData(), as.numeric(input$NSeries))
        plot_fnc_boxplot(dtMatrix12)
    })
    
    output$DailyMedians <- renderPlot({
        req(mainData())
        aRchery::plot_medians_polygon(mainData())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
