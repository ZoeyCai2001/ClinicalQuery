

library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)


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
studies = studies |> left_join(conditions, by = "nct_id")

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).


query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE, start_date_range = NULL, completion_date_range = NULL) {
  
  # Filter by start date range 
  if (!is.null(start_date_range)) {
    d <- d %>% filter(start_date >= start_date_range[1] & start_date <= start_date_range[2])
  }
  # Filter by completion date range 
  if (!is.null(completion_date_range)) {
    d <- d %>% filter(completion_date >= completion_date_range[1] & completion_date <= completion_date_range[2])
  }
  
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
    select(name) |>
    group_by(name) |>
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
  
  x$name[is.na(x$name)] = "NA"
  
  ggplot(x, aes(x = name, y = n)) +
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





