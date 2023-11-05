# Load required libraries
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
library(DT)

# Create a database connection and access tables
con = dbConnect(
  duckdb(
    file.path("..", "..", "2023-09-27", "ctgov.duckdb"),
    read_only = TRUE
  )
)

# Check if tables exist in the database
if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}

# Access tables from the database
studies = tbl(con, "studies")
conditions = tbl(con, "conditions")
conditions = conditions |>
  group_by(nct_id) %>%
  summarize(conditions = str_flatten(downcase_name, ", "))
countries = tbl(con, "countries")
links = tbl(con, "links") %>% 
  distinct(nct_id, .keep_all = TRUE)
countries = countries %>% 
  rename(country = name) %>% 
  distinct(nct_id, .keep_all = TRUE)
facilities = tbl(con, "facilities") %>% 
  rename(country_facility = country) %>%
  distinct(nct_id, .keep_all = TRUE)
browse_conditions = tbl(con, "browse_conditions") %>%
  distinct(nct_id, .keep_all = TRUE)

# Access and process the "keywords" table
# Group by nct_id and turn the one nct_id-keyword pair into nct_id-all keywords pair
keywords = tbl(con, "keywords")
keywords =  keywords |>
  group_by(nct_id) %>%
  summarize(keyword = str_flatten(name, ", "))

# Join various tables together
studies = studies |> 
  left_join(conditions, by = "nct_id") |> 
  left_join(countries, by = "nct_id") |>
  left_join(links, by = "nct_id") |>
  left_join(facilities, by = "nct_id") |>
  left_join(browse_conditions, by = "nct_id") |>
  left_join(keywords, by = "nct_id")

# Define a function to query keywords from a database table
#' @title Query keywords from a database table.
#' @description The function is to select rows from a table with keywords.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE, start_date_range = NULL, completion_date_range = NULL) {
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
}

#Find all the phases in studies table
phase_all = studies |>
  select(phase) |>
  distinct() |>
  collect()

#' @title Plot a histogram of different phases
#' @description The function is to plot a histogram of phases of selected table with a fixed x-axis.
#' @param x the database table.
plot_phase_histogram = function(x) {
  x = x |>
    select(phase) |>
    collect()
  #Bind all phases to the selected table to make sure every phase will appear at least once.
  x = x |> bind_rows(phase_all)
  x$phase[is.na(x$phase)] = "NA"
  x = x|>
    group_by(phase) |>
    summarize(n = n()-1)
  #Plot the pahse histogram
  ggplot(x, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

#' @title Plot a histogram of conditions
#' @description The function is to plot a histogram of conditions of selected studies. 
#' The number of conditions is the smaller one of 50 and number of conditions with percentage>0.0001.
#' @param x the database table.
#' @return a histogram of conditions of selected studies
plot_conditions_histogram = function(x){
  x = x |>
    select(conditions) |>
    collect() |>
    separate_rows(conditions, sep = ", ") |>
    group_by(conditions) |>
    summarize(n = n()) |>
    filter(nchar(conditions) < 30) |>
    arrange(-n) |>
    collect()
  #Calculate the sum of all conditions(studies)
  sum = sum(x$n)
  #Calculate the persentage of each condition
  nrows = x |>
    mutate(percentage = n/sum) |>
    filter(percentage > 0.001) |>
    nrow()
  #Let n be the smaller of 50 and number of conditions with percentage>0.0001.
  n = min(50,nrows)
  #Select top n in x
  x = x |>
    head(n)
  #Plot the conditions histgram
  ggplot(x, aes(x = conditions, y = n)) +
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

#' @title Get matrix of terms
#' @description The function is to get the keywords frequency of selected studies.
#' @param d the studies to get the number of keywords for.
#' @return a matrix containing words existed in the keyword column of x and their frequency.
getTermMatrix <- memoise(function(x) {
  
  x = x |> collect() |> select(keyword) |> as.character()
  myCorpus = Corpus(VectorSource(x))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but","study","patients"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

#' @title Plot Countries
#' @description The function is to get the number of studies in different countries 
#' and visualize in a world map.
#' @param d the studies to get the world map for.
plot_countries = function(x){
  x = x |> collect() |>
    select(country) |>
    group_by(country) |>
    summarise(n = n()) |>
    as.data.frame()
  spdf <- joinCountryData2Map(x, joinCode="NAME", nameJoinColumn="country")
  mapCountryData(spdf, nameColumnToPlot="n", catMethod="fixedWidth",colourPalette = "white2Black", mapTitle = "Number of Studies Worldwide" )
}

#' @title Table with Links
#' @description The function is to get a table of selected studies' 
#' nct_id(with link),brief title, start date, and completion date.
#' @param d the studies to get the table.
#' @return table containing nct_id(with link),brief title, start date, and completion date.
table_with_link = function(x){
  x = x |> collect()
  x$link <- paste0("<a href='", x$url, "' target='_blank'>", x$nct_id, "</a>")
  x = x |> as.data.frame() |>
    mutate(link = case_when(
      is.na(url) ~ nct_id,
      TRUE ~ link
    ))
  x |>
    select(link, brief_title, start_date, completion_date) |>
    rename(`NCT ID` = link, `Brief Title` = brief_title,
           `Start Date` = start_date, `Completion Date` = completion_date)
}

#' @title Plot states
#' @description The function is to get the number of studies in different states 
#' and visualize in an US map.
#' @param d the studies to get the US map for.
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


#' @title  Pie Chart
#' @description The function is to get a pie chart of top 10 mesh_terms.
#' @param d the studies to get the top 10 mesh_terms for.
pie_chart_mesh = function(x){
  x = x |> collect() |>
    select(downcase_mesh_term) |>
    group_by(downcase_mesh_term) |>
    summarise(n = n()) |>
    filter(!is.na(downcase_mesh_term)) |>
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