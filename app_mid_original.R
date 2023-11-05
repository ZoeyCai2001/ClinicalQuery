#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.

source("ct-util_mid_original.R")
max_num_studies = 10000

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      selectInput("source_class",
                   label = h3("Sponsor Type"),
                   choices = list("All" = "no_filter",
                                  "Federal" = "FED",
                                  "Individual" = "INDIV",
                                  "Industry" = "INDUSTRY",
                                  "Network" = "NETWORK",
                                  "NIH" = "NIH",
                                  "Other" = "OTHER",
                                  "Other Government" = "OTHER_GOV",
                                  "Unknow" = "Unknown"
                   ),
                   selected = 1),
      dateRangeInput("start_date_range", "Start Date Range:", 
                     start = Sys.Date() - 30, end = Sys.Date()),
      dateRangeInput("completion_date_range", "Completion Date Range:", 
                     start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        tabPanel("Conditions",plotOutput("conditions_plot")),
        tabPanel("Word Cloud",plotOutput("wordcloud_plot")),
        tabPanel("World Map",plotOutput("country_plot"))
      ),
      DT::dataTableOutput("trial_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    if(input$source_class != "no_filter"){
      ret = ret |>
        filter(source_class %in% !!input$source_class)
    }else{
      ret = ret
    }
    if (!is.null(input$start_date_range)) {
      start_date <- as.Date(input$start_date_range[1])
      end_date <- as.Date(input$start_date_range[2])
      ret = ret |>
        filter(start_date >= start_date & start_date <= end_date)
    }
    
    if (!is.null(input$completion_date_range)) {
      start_date_c <- as.Date(input$completion_date_range[1])
      end_date_c <- as.Date(input$completion_date_range[2])
      ret = ret |>
        filter(completion_date >= start_date_c & completion_date <= end_date_c)
    }
    ret |>
      head(max_num_studies) |>
      collect()
  })
  
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })

  output$concurrent_plot = renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") +
      theme_bw()
  })

  output$conditions_plot = renderPlot({
    get_studies() |>
      plot_conditions_histogram()
  })

  output$trial_table =  DT::renderDataTable({
    df = get_studies() |>
      table_with_link()
    DT::datatable(df[drop = FALSE], escape = FALSE)
  })
  
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(get_studies())
      })
    })
  })
  
  
  output$wordcloud_plot <- renderPlot({
    v <- terms()
    wordcloud(names(v), v, scale=c(4,0.5),
              min.freq = input$freq, max.words=input$max,
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$country_plot <-renderPlot({
    get_studies() |>
      plot_countries()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
