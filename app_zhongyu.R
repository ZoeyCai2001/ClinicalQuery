#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# 0. Midterm scheduling

# 1. Clean up the table column names X
# 2. Allow multiple brief title keywords X
# 3. Create a histogram of the phase
# 4. Organize files.
# 5. Fix the Phase plot
# 6. Plot the concurrent studies (adding a feature/capability).

# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.

source("ct-util_zhongyu.R")
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
                   selected = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        tabPanel("Conditions",plotOutput("conditions_plot"))
      ),
      dataTableOutput("trial_table")
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

  output$trial_table = renderDataTable({
    get_studies() |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
