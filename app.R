source("app_functions.R")
library(shiny)
library(rsconnect)

# ui
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"), # set dark theme

  # layout with 2 tabs
  tabsetPanel(  
    
    # tab 1 - comparisons by weather variable
    tabPanel("Comparing Weather Data", 
      h2("Comparing Weather Data at Different Sites across the UK"),
      p("The data in this app has been obtained from the Met Office Integrated Data Archive System, for weather stations at 20 different sites across the UK, over the period from 1st January to 30th November 2022. In this first tab, you can explore the different weather variables across the different sites, during this period."),

      # first sidebar layout
      sidebarLayout(
        sidebarPanel(
          # Sites selector
          selectizeInput(
            "sitesSelected", # var name in server
            "Select up to five Sites to compare", # label in UI
            choices = sites$Site_Name,
            selected = c("Heathrow", "Aldergrove", "Sumburgh"),
            multiple = TRUE,
            options = list(maxItems = 5)
          ),
          # Weather variable
          radioButtons(
            "weatherVar", # var name in server
            "Weather Variable", #label in UI
            selected= "Air temperature",
            choices = c("Air temperature", "Wind speed", "Relative humidity", "Visibility")
          ),
          # Time display
          radioButtons(
            "timeDisplay", # var name in server
            "Time Display", #label in UI
            selected="Calendar Time",
            choices = c("Calendar Time", "Day of week", "Hour of day")
          ),
          # Aggregation
          radioButtons(
            "aggregation", # var name in server
            "Aggregation", #label in UI
            selected = "Hourly (no aggregation)",
            choices = c("Hourly (no aggregation)", "Daily", "Monthly")
          ),
          # Summary statistic
          conditionalPanel(
            condition = "input.aggregation != `Hourly (no aggregation)`", # don't show when no aggregation
            radioButtons(
              "summaryStat", # var name in server
              "Summary Statistic", #label in UI
              selected = "Mean",
              choices = c("Mean", "Min", "Max") # TODO: add more?
            )
          ),
        ),
        # main plot panel
        mainPanel(
          plotOutput("summaryPlot"), # output$summaryPlot
        )
      ),
      
      # second sidebar layout
      sidebarLayout(
        # map plot
        sidebarPanel(
          h4("Locations of selected sites:"),
          plotOutput("mapPlot"), # output$mapPlot
          
          # download panel
          conditionalPanel(
            condition = "input.sitesSelected !== null && input.sitesSelected.length > 0", # hide download buttons if no sites selected
            hr(),
            h4("Download data"),
            downloadButton("downloadSevenDayTable", "Download daily mean values for last 7-days (Excel)"),
            downloadButton("downloadReport", "Download report for selected variables (Word document)"),
          ),
        ),
        # seven day table
        mainPanel(
          uiOutput("sevenDaysTable"), # output$sevenDaysTable
        )
      )
    ),

    # tab 2 - Hutton Criteria
    tabPanel("Hutton Criteria",
      h2("Hutton Criteria"),
      p("The Hutton Criteria occurs for a particular day when both criteria are met:"),
      tags$ol(
        tags$li("The two previous days have a minimum temperature of at least 10\u00B0C"), 
        tags$li("The two previous days have at least six hours of relative humidity in each day of 90% or higher")
      ),
      
      # hutton sidebar layout
      sidebarLayout(
        sidebarPanel(
          # Sites selector
          selectizeInput(
            "huttonSitesSelected", # var name in server
            "Select a site", # label in UI
            choices = sites$Site_Name,
            selected = c("Abbotsinch"),
            multiple = FALSE
          ),
          # Month selector
          selectizeInput(
            "huttonMonthSelected", # var name in server
            "Month", #label in UI
            selected = "Jan",
            choices = c("Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11) # TODO: add "Dec" & handle no data
          ),
          
          # Hutton summary plot
          hr(),
          h3("Blight risk by site"),
          h5("Proportion of days where Hutton Criteria Met at each site between 1st Jan - 30th Nov 2022"),
          plotOutput("huttonSummaryPlot", height = 650)
        ),
        # main panel
        mainPanel(
          # detail by month
          uiOutput("huttonDetailByMonth"), # output$huttonDetailByMonth
        )
      ),
    )
  )
)


# server
server <- shinyServer(function(input, output, session){
  
  # get value of timeDisplay radio buttons  
  observe({
    time.display <- input$timeDisplay
    time.opt <- case_when(
      time.display == "Calendar Time" ~ list(c("Hourly (no aggregation)", "Daily", "Monthly")), # possible aggregations
      time.display == "Day of week" ~ list(c("Hourly (no aggregation)", "Daily")),
      time.display == "Hour of day" ~ list(c("Hourly (no aggregation)"))
    )
    updateRadioButtons(
      session,
      "aggregation", # update aggregation radio options based on selected time.display
      choices = time.opt[[1]]
    )
  })
  
  # main plot
  output$summaryPlot <- renderPlot({ 
    plot.chart(input$sitesSelected, input$weatherVar, input$summaryStat, input$aggregation, input$timeDisplay)
  })
  # map plot  
  output$mapPlot <- renderPlot({
    plot.map(input$sitesSelected)
  })
  # seven days daily mean table
  output$sevenDaysTable <- renderUI({
    if(!is.null(input$sitesSelected)){
      htmltools_value(seven.days.table(input$sitesSelected))
    }
  })
  # hutton table - detail by month
  output$huttonDetailByMonth <- renderUI({
    htmltools_value(
      hutton.detail.table(input$huttonSitesSelected, input$huttonMonthSelected)
    )
  })
  # hutton summary chart - all sites
  output$huttonSummaryPlot <- renderPlot({
    hutton.summary.plot()
  })
  # download seven day table
  output$downloadSevenDayTable <- downloadHandler(
    filename = "last-7-day-daily-means.csv",
    content = function(file) {
      seven.day.data <- seven.days.table.data(input$sitesSelected)
      write.csv(seven.day.data, file, row.names=FALSE)
    }
  )
  # download markdown report
  output$downloadReport <- downloadHandler(
    filename = "weather-comparison.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(),"Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)

      # save current plots to RData files
      build.report.plots(input$sitesSelected, input$weatherVar, input$summaryStat, input$aggregation, input$timeDisplay)
      
      # render markdown
      rmarkdown::render("Report.Rmd",
        output_format = "word_document",
        output_file = file,
        params = list(
          selected.site.string = comma.separated.string(input$sitesSelected),
          weather.var = input$weatherVar,
          summary.stat = input$summaryStat,
          aggregation = input$aggregation,
          time.display = input$timeDisplay
        ),
        envir = new.env(parent = globalenv()),clean=F,encoding="utf-8"
      )
    }
  )
}) 


# create app
shinyApp(ui = ui, server = server)