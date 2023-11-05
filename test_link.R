library(shiny)
library(DT)
con = dbConnect(
  duckdb(
    file.path("..", "..", "2023-09-27", "ctgov.duckdb"),
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
conditions = tbl(con, "conditions")
countries = tbl(con, "countries")
links = tbl(con, "links")
countries = countries %>% rename(country = name)
x= studies |> 
  left_join(conditions, by = "nct_id") |> 
  left_join(countries, by = "nct_id") |>
  left_join(links, by = "nct_id") |>
  head(100)

df = table_with_link(x)

ui <- fluidPage(
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  
  output$table <- DT::renderDataTable({
    DT::datatable(df[drop = FALSE], escape = FALSE)
  })
  
}

shinyApp(ui, server)

