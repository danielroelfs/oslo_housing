### SCRAPE FINN ADS FOR BUILD LISTINGS ########################

#-- Libraries -------------------------

library(tidyverse)
library(rvest)

#-- Load data ------------------------

list_url <- "https://www.finn.no/realestate/homes/search.html?is_new_property=false&location=0.20061&page=1&price_collective_from=1500000&sort=PUBLISHED_DESC"

#-- Get number of ads ------------------------

n_ads <- read_html(list_url) %>% 
  html_nodes(".u-strong") %>%
  html_text() %>% 
  str_remove_all("[[:blank:]]") %>% 
  parse_number(trim_ws = TRUE) %>% 
  unique()

n_ads_per_page <- read_html(list_url) %>% 
  html_nodes(".ads__unit") %>% 
  as.character() %>% 
  str_extract_all("\\d{9}") %>% 
  map(., 1) %>% 
  unlist() %>% 
  length()

n_pages <- ceiling(n_ads / n_ads_per_page)

#-- Scrape individual ad function ------------------------

# Define scrape function
scrape_ad <- function(ad_url, verbose = TRUE, new_building = FALSE) {
  
  url_text <- read_html(ad_url)
  ad_id <- parse_number(str_extract(ad_url, "\\d+"))
  
  # Get location
  loc <- url_text %>% 
    html_nodes(".u-t3.u-display-block") %>% 
    html_text()
  
  if (is_empty(loc)) {
    loc <- ""
  }
  
  # Get title
  title <- url_text %>% 
    html_nodes(".u-t2") %>% 
    html_text()
  
  # Get address
  address <- url_text %>% 
    html_nodes(".u-caption") %>% 
    html_text() %>% 
    unlist() %>% 
    .[1]
  
  # Get price
  price <- url_text %>% 
    html_nodes(".u-t3") %>% 
    html_text() %>% 
    .[str_detect(.," kr")] %>% 
    str_squish() %>% 
    str_remove_all(" ") %>% 
    parse_number()
  
  # Collect key info
  info_name <- url_text %>% 
    html_nodes("dt") %>% 
    html_text() %>% 
    str_squish()
  
  last_name <- which(info_name == "Formuesverdi")
  
  info_num <- url_text %>% 
    html_nodes("dd") %>% 
    html_text() %>% 
    str_squish()
  
  descriptive_vars <- suppressWarnings(
    tibble(name = info_name, num = info_num) %>% 
      mutate(num = str_remove_all(num, " ")) %>% 
      slice(seq(last_name)) %>% 
      mutate(num_parsed = parse_number(num))
  )
  
  descriptive_num <- descriptive_vars %>% 
    filter(!is.na(num_parsed)) %>% 
    pull(name)
  
  descriptive_wide <- descriptive_vars %>% 
    select(name, num) %>% 
    pivot_wider(names_from = name, values_from = num) %>% 
    mutate(across(any_of(descriptive_num), parse_number))
  
  # Get facilities
  facilities <- url_text %>% 
    html_nodes(".u-mb16") %>% 
    html_text() %>% 
    str_squish() %>% 
    str_replace_all(" ",", ")
  
  # Get description text
  description_text <- url_text %>% 
    html_nodes(".u-mb32") %>% 
    html_text() %>% 
    str_squish() %>% 
    paste(., collapse = " ")
  
  # Collect all data in a data frame
  data_out <- tibble(
    new_building = new_building,
    location = loc,
    address = address,
    price = price,
    title = title,
    facilities = facilities,
    description_text = description_text,
    ad_url = ad_url,
    id = ad_id) %>% 
    bind_cols(descriptive_wide) %>% 
    janitor::clean_names()
  
  if (verbose) {
    print(glue::glue("{data_out %>% pull(location)}, \\
               {data_out %>% pull(bruksareal)}m2, \\
               {format(data_out %>% pull(price), big.mark=\",\")} kr"))
  }
  
  return(data_out)
  
}

#-- Get individual ad ids ------------------------

ids <- c()

for (page in seq(n_pages)) {
  
  print(glue::glue("Scraping ads for page {page}"))
  
  new_url <- str_replace(list_url, pattern = "page=1", replacement = glue::glue("page={page}"))
  
  ad_id <- read_html(new_url) %>% 
    html_nodes(".ads__unit") %>% 
    as.character() %>% 
    str_extract_all("\\d{9}") %>% 
    map(., 1) %>% 
    unlist() %>% 
    as.integer()
  
  ids <- c(ids, ad_id)
  ids <- unique(ids)
  
  print(glue::glue("Number of ads scraped: {length(ids)}"))
  
}

#-- Scrape ads for already built houses ------------------------

data <- tibble()

for (i in ids) {
  
  idx <- which(ids == i)
  print(glue::glue("Scraping data for id {i} ({round(idx * 100 / length(ids),2)}%)"))
  try(
    ad_data <- scrape_ad(ad_url = glue::glue("https://www.finn.no/realestate/homes/ad.html?finnkode={i}"),
                         verbose = FALSE, new_building = FALSE)
  )
  
  data <- bind_rows(data, ad_data) %>% 
    select(everything(), new_building)
  
}

#map_dfr(ids, ~ scrape_ad(ad_url = glue::glue("https://www.finn.no/realestate/homes/ad.html?finnkode={.x}"),
#        verbose = TRUE, new_building = FALSE))

#-- Write to file ------------------------

write_csv(data, here::here("files", glue::glue("housing_listings_{Sys.Date()}.csv")))






