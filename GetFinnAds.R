### SCRAPE FINN ADS FOR BUILD LISTINGS ########################

#-- Libraries -------------------------

library(tidyverse)
library(rvest)
library(tidygeocoder)

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

#-- Define function to scrape individual ads ------------------------

scrape_ad <- function(ad_url, verbose = TRUE, new_building = FALSE) {
  
  url_text <- read_html(ad_url)
  ad_id <- parse_number(str_extract(ad_url, "\\d+"))
  
  # Get location
  loc <- url_text %>% 
    html_nodes(".u-t3.u-display-block") %>% 
    html_text() %>% 
    ifelse(is_empty(.), NA, .)
  
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
  
  last_name <- which(info_name %in% c("Formuesverdi","Fellesformue",
                                      "Boligselgerforsikring","Tomteareal","Bruttoareal",
                                      "Byggeår","Energimerking",
                                      "Rom","Tomt","Verditakst"))
  last_name <- max(last_name)
  
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
  
  date_changed <- url_text %>% 
    html_table() %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    filter(X1 == "Sist endret") %>% 
    pull(X2) %>% 
    str_extract("[^*]+ 2021") %>% 
    parse_date("%d. %b %Y", locale = locale("nb"))
  
  
  # Collect all data in a data frame
  data_out <- tibble(
    new_building = new_building,
    location = str_to_title(loc),
    address = address,
    price = price,
    title = title,
    facilities = ifelse(!is_empty(facilities),facilities,NA),
    description_text = description_text,
    date_changed = date_changed,
    ad_url = ad_url,
    id = ad_id) %>% 
    bind_cols(descriptive_wide) %>% 
    janitor::clean_names() %>% 
    geocode(address, method = 'osm')
  
  if (verbose) {
    print(str_glue("{data_out %>% pull(location)}, \\
               {data_out %>% pull(bruksareal)}m2, \\
               {format(data_out %>% pull(price), big.mark=\".\", decimal.mark=\",\")} kr"))
  }
  
  return(data_out)
  
}

#-- Get individual ad ids ------------------------

ids <- c()

for (page in seq(n_pages)) {
  
  print(str_glue("Scraping ads for page {page}"))
  
  new_url <- str_replace(list_url, pattern = "page=1", replacement = str_glue("page={page}"))
  
  ad_id <- read_html(new_url) %>% 
    html_nodes(".ads__unit") %>% 
    as.character() %>% 
    str_extract_all("\\d{9}") %>% 
    map(., 1) %>% 
    unlist() %>% 
    as.integer()
  
  ids <- c(ids, ad_id)
  ids <- unique(ids)
  
  print(str_glue("Number of ads scraped: {length(ids)}"))
  
}

#-- Scrape ads for already built houses ------------------------

data <- tibble()

for (i in ids) {
  
  idx <- which(ids == i)
  print(str_glue("Scraping data for id {i} ({round(idx * 100 / length(ids),2)}%)"))
  ad_url <- str_glue("https://www.finn.no/realestate/homes/ad.html?finnkode={i}")
  try(
    ad_data <- scrape_ad(ad_url = ad_url, verbose = FALSE, new_building = FALSE)
  )
  
  data <- bind_rows(data, ad_data) %>% 
    select(everything(), new_building)
  
}

#map_dfr(ids, ~ scrape_ad(ad_url = str_glue("https://www.finn.no/realestate/homes/ad.html?finnkode={.x}"),
#        verbose = TRUE, new_building = FALSE))

#-- Write to file ------------------------

write_csv(data, here::here("files", str_glue("housing_listings_{Sys.Date()}.csv")))






