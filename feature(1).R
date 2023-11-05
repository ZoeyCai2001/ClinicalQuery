library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(tm)
library(wordcloud)
library(memoise)
library(rworldmap)
#mapDevice('x11')
library(DT)
library(leaflet)
library(scales)

# Create the connection to a database and "studies" and "sponsors" tables.

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
keywords = tbl(con, "keywords") %>% rename(keyword = name)
facilities = tbl(con, "facilities") %>% rename(country_facility = country)
countries = countries %>% rename(country = name)
browse_conditions = tbl(con, "browse_conditions")

studies = studies |> 
  left_join(conditions, by = "nct_id") |> 
  left_join(countries, by = "nct_id") |>
  left_join(links, by = "nct_id") |>
  left_join(facilities, by = "nct_id") |>
  left_join(keywords, by = "nct_id")|>
  left_join(browse_conditions, by = "nct_id")


#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE, 
                       start_date_range = NULL, completion_date_range = NULL) {
  
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query)) 
  return(d)
}

# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.

phase_all = studies |>
  select(phase) |>
  distinct() |>
  collect()

plot_phase_histogram = function(x) {
  x = x |>
    select(phase) |>
    collect()
  x = x |> bind_rows(phase_all)
  x$phase[is.na(x$phase)] = "NA"
  x = x|>
    group_by(phase) |>
    summarize(n = n()-1)
  
  ggplot(x, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

plot_conditions_histogram = function(x){
  x = x |>
    select(downcase_name) |>
    group_by(downcase_name) |>
    summarize(n = n()) |>
    arrange(-n) |>
    collect()
  
  sum = sum(x$n)
  
  nrows = x |>
    mutate(percentage = n/sum) |>
    filter(percentage > 0.001) |>
    nrow()
  
  n = min(50,nrows)
  
  x = x |>
    head(n)
  
  x$downcase_name[is.na(x$downcase_name)] = "NA"
  
  ggplot(x, aes(x = downcase_name, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Conditions") +
    ylab("Count")
}

#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  # Get all of the unique dates.
  all_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}


plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}

getTermMatrix <- memoise(function(x) {
  
  x = x |> collect() |> select(keyword) |> as.character()
  myCorpus = Corpus(VectorSource(x))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "small","type", "and", "but",
                      "study","patients", "total", "computed", "outcome", "moderate"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

plot_countries = function(x){
  x = x |> collect() |>
    select(country) |>
    group_by(country) |>
    summarise(n = n()) |>
    as.data.frame()
  spdf <- joinCountryData2Map(x, joinCode="NAME", nameJoinColumn="country")
  mapCountryData(spdf, nameColumnToPlot="n", catMethod="fixedWidth",colourPalette = "YlGnBu",
                 mapTitle = "Number of Studies Worldwide" )
}



table_with_link = function(x){
  x = x |> collect()
  x$link <- paste0("<a href='", x$url, "' target='_blank'>", x$nct_id, "</a>")
  x = x |> as.data.frame()
  x = x |>
    select(link, brief_title, start_date, completion_date) |>
    rename(`NCT ID` = link, `Brief Title` = brief_title,
           `Start Date` = start_date, `Completion Date` = completion_date) |>
    distinct()
}



# plot a color coded map of US
plot_states_us = function(x){
  x = x |> collect() |>
    filter(country_facility == "United States")|>
    select(state) |>
    group_by(state) |>
    summarise(n = n()) |>
    as.data.frame()
  
  x$state <- tolower(x$state)
  # Get the map data
  states_map <- map_data("state")
  # Merge your data with the map data
  map_data <- merge(states_map, x, by.x = "region", by.y = "state", all.x = TRUE)

  ggplot() +
    geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = n), 
                 color = "white", size = 0.1) +
    coord_fixed(1.3) + 
    scale_fill_gradient(low = "white", high = "red", 
                        na.value = "grey90", name = "Frequency") +
    theme_minimal() +
    labs(title = "Frequency Distribution Across U.S. States",
         caption = "Each state is color-coded based on the frequency value.")
  }
  
  
  
# create a pie chart that visualizes only the top 10 "mesh_term" categories 
pie_chart_mesh = function(x){
  x = x |> collect() |>
  select(downcase_mesh_term) |>
    group_by(downcase_mesh_term) |>
    summarise(n = n()) |>
    as.data.frame()
  
  # Ordering the data by the 'n' column in descending order and getting the top 10
  plot1 <- head(x[order(-x$n),], 10)
  # Define colors
  colors <- rainbow(length(plot1$n))
  # Creating the pie chart
  pie_chart <- pie(plot1$n, labels = plot1$downcase_mesh_term, 
                   main = "Top 10 investigated diseases", 
                   col = colors,
                   clockwise = TRUE)
  
  # Adding percentages
  pie_labels <- round(100 * plot1$n / sum(plot1$n), 1)
  pie_labels <- paste(pie_labels, "%", sep="")
  
  # Adding the legend immediately after the pie chart with corresponding colors
  legend("topright", legend = pie_labels, fill = colors, bty = "n", title = "Percentages")
}
  
  










