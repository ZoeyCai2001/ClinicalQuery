source("ct-util_mid.R")
#Select rows by keywords and sponsor
#Save selected rows as ret
source_class = 
ret = studies |> 
  query_kwds("pembrolizumab", "brief_title") |>
  filter(source_class == "individual") |>
  collect()

#Date Range Selection
start_date_range = c("2000-1-1","2050-12-31")
completion_date_range = c("2000-1-1","2050-12-31")

if (!is.null(start_date_range)) {
  start_date_s <- as.Date(input$start_date_range[1])
  end_date_s <- as.Date(input$start_date_range[2])
  ret = ret |>
    filter(start_date >= start_date_s & start_date <= end_date_s)
}
if (!is.null(completion_date_range)) {
  start_date_c <- as.Date(completion_date_range[1])
  end_date_c <- as.Date(completion_date_range[2])
  ret = ret |>
    filter(completion_date >= start_date_c & completion_date <= end_date_c)
}

#Word Cloud
termmatrix = ret |> getTermMatrix()
freq = 10
max = 100
wordcloud(names(termmatrix), termmatrix, scale=c(4,0.5),
          min.freq = freq, max.words=max,
          colors=brewer.pal(8, "Dark2"))
  
#World Map
ret |> plot_countries()

#American Map
ret |> plot_states_us()

#Top 10 Investigated Diseases
ret |> pie_chart_mesh()

#Link to Website
ret |> table_with_link()