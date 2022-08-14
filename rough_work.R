
## to get started on scraping
# https://www.scraperapi.com/blog/web-scraping-with-r/

# https://www.dataquest.io/blog/web-scraping-in-r-rvest/
# https://zenscrape.com/web-scraping-r/
# https://www.geeksforgeeks.org/web-scraping-using-r-language/
# https://scrapfly.io/blog/web-scraping-with-r/

# https://towardsdatascience.com/web-scraping-with-r-easier-than-python-c06024f6bf52
# https://towardsdatascience.com/web-scraping-tutorial-in-r-5e71fd107f32

# https://thatdatatho.com/tutorial-web-scraping-rselenium/


suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(plotly)
  library(tictoc)
  library(glue)
  
  options(dplyr.summarise.inform = FALSE)
})

## ****************

# scrape_url <- "https://www.scrapingbee.com/"
# flat_html <- readLines(con = scrape_url)
# 
# 
# scrape_url <- "http://www.google.com"
# flat_html <- readLines(con = scrape_url)
# 
# 
# scrape_url <- "https://www.york.ac.uk/teaching/cws/wws/webpage1.html"
# flat_html <- readLines(con = scrape_url)

## ****************

# library(RCurl)
# 
# ftp_url <- "ftp://cran.r-project.org/pub/R/web/packages/BayesMixSurv/"
# get_files <- RCurl::getURL(ftp_url, dirlistonly = TRUE)
# extracted_filenames <- str_split(get_files, "\r\n") %>% unlist()
# extracted_html_filenames <- str_extract_all(extracted_filenames, ".+(.html)") %>% unlist()
# 
# FTPDownloader <- function(ftp, filename, folder, handle) {
#   dir.create(folder, showWarnings = FALSE)
#   fileurl <- str_c(ftp, filename)
#   if (!file.exists(str_c(folder, "/", filename))) {
#     file_name <- try(RCurl::getURL(fileurl, curl = handle))
#     write(file_name, str_c(folder, "/", filename))
#     Sys.sleep(1)
#   }
# }
# Curlhandle <- RCurl::getCurlHandle(ftp.use.epsv = FALSE)
# walk(extracted_html_filenames, 
#      FTPDownloader, 
#      ftp = ftp_url,
#      folder = "scrapingbee_html", 
#      handle = Curlhandle)

## ****************

# library(XML)
# 
# wiki_url <- "https://en.wikipedia.org/wiki/Leonardo_da_Vinci"
# wiki_read <- readLines(wiki_url, encoding = "UTF-8")
# parsed_wiki <- htmlParse(wiki_read, encoding = "UTF-8")
# wiki_intro_text <- parsed_wiki["//p"]
# wiki_intro_text[[4]]
# getHTMLLinks(wiki_read)
# length(getHTMLLinks(wiki_read))
# 
# wiki_url1 <- "https://en.wikipedia.org/wiki/Help:Table"
# wiki_read1 <- readLines(wiki_url1, encoding = "UTF-8")
# length((readHTMLTable(wiki_read1)))
# names(readHTMLTable(wiki_read1))
# readHTMLTable(wiki_read1)$"The table's caption\n"
# 
# wiki_url2 <- "https://en.wikipedia.org/wiki/United_States_census"
# wiki_read2 <- readLines(wiki_url2, encoding = "UTF-8")
# length((readHTMLTable(wiki_read2)))
# names(readHTMLTable(wiki_read2))
# readHTMLTable(wiki_read2)$"The table's caption\n"

## ****************

# library(rvest)
# html_form_page <- 'http://www.weather.gov.sg/climate-historical-daily' %>% read_html()
# weatherstation_identity <- 
#   html_form_page %>% 
#   html_nodes('button#cityname + ul a') %>% 
#   html_attr('onclick') %>%  
#   sub(".*'(.*)'.*", '\\1', .)
# weatherdf <- 
#   expand.grid(weatherstation_identity, 
#               month = sprintf('%02d', 1:12),
#               year = 2016:2020)
# str(weatherdf)
# urlPages <- paste0('http://www.weather.gov.sg/files/dailydata/DAILYDATA_', 
#                    weatherdf$Var1, '_', weatherdf$year, weatherdf$month, '.csv')
# lapply(urlPages, function(url){download.file(url, basename(url), method = 'curl')})

## ****************

# library(rvest)
# link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure"
# page = read_html(link)
# title = page %>% html_nodes(".lister-item-header a") %>% html_text()
# year  = page %>% html_nodes(".text-muted.unbold") %>% html_text()
# rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
# synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text() %>% str_squish()
# movies = data.frame(title, year, rating, synopsis, stringsAsFactors = FALSE)
# movies = 
#   movies %>% 
#   mutate(year = year %>% str_replace('\\(I\\)', '')) %>% 
#   extract(year, into = 'year', regex = '\\((.*)\\)')
# # movie_url = page %>% html_nodes(".lister-item-header a") %>% html_attr("href")
# movie_url = page %>% html_nodes(".lister-item-header a") %>% html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
# 
# movies <- tibble()
# for (page_result in seq(from = 1, to = 101, by = 50)) {
#   link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&start=", page_result, "&ref_=adv_nxt")
#   page = read_html(link)
#   title = page %>% html_nodes(".lister-item-header a") %>% html_text()
#   year  = page %>% html_nodes(".text-muted.unbold") %>% html_text()
#   rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
#   synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text() %>% str_squish()
#   movies = rbind(movies, data.frame(title, movie_url, year, rating, synopsis, stringsAsFactors = FALSE))
# }

## ****************

library(rvest)
link <- "https://www.heroic.us/optimize"
page <- read_html(link)
categories <- 
  page %>% 
  html_nodes(".formatted-type p strong a") %>% 
  html_attr('href') %>% 
  set_names(html_text(html_nodes(page, ".formatted-type p strong a")))

titles <- tibble()
for (i in 1:length(categories)) {
  current_category <- categories[i]
  print(glue('{current_category}'))
  subpage <- read_html(current_category)
  title <-
    subpage %>% 
    html_nodes("header a h2") %>% 
    html_text() %>% 
    str_squish()
  title_type <-
    subpage %>% 
    html_nodes(".badge") %>% 
    html_text() %>% 
    str_squish() %>% 
    discard(~ .x == 'Locked')
  # subtitle <-
  #   subpage %>% 
  #   html_nodes("header div span") %>% 
  #   html_text() %>% 
  #   str_squish()
  synopsis <- 
    subpage %>% 
    html_nodes(".clamp-sm") %>% 
    html_text() %>% 
    str_squish()
  titles <- rbind(titles, tibble(title, title_type, synopsis) %>% mutate(category = names(current_category)))
  Sys.sleep(5)
}




