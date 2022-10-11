suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(plotly)
  library(tictoc)
  library(rvest)
  library(glue)
  
  options(dplyr.summarise.inform = FALSE)
})

link <- "https://www.heroic.us/optimize"
page <- read_html(link)
missions <- 
  page %>% 
  html_nodes(".formatted-type p strong a") %>% 
  html_attr('href') %>% 
  set_names(html_text(html_nodes(page, ".formatted-type p strong a")))

titles <- tibble()
for (i in 1:length(missions)) {
  current_mission <- missions[i]
  print(glue('{current_mission}'))
  subpage <- read_html(current_mission)
  title <-
    subpage %>% 
    html_nodes("header a h2") %>% 
    html_text() %>% 
    str_squish()
  subtitle <- 
    subpage %>% 
    html_nodes(".content header") %>% 
    # html_nodes("header") %>% 
    # keep(~ .x %>% str_detect('class="title"')) %>% 
    # discard(~ .x %>% str_detect('role="banner"')) %>% 
    map_chr(~ .x %>% html_nodes("a+ div") %>% html_text())
  author <- 
    subpage %>% 
    html_nodes(".feed-item") %>% 
    keep(~ .x %>% str_detect('h2')) %>% 
    map_chr(~ .x %>% html_nodes("div span a") %>% html_text() %>% str_squish() %>% paste(collapse = ' & '))
  title_type <-
    subpage %>% 
    html_nodes(".badge") %>% 
    html_text() %>% 
    str_squish() %>% 
    discard(~ .x == 'Locked')
  synopsis <- 
    subpage %>% 
    html_nodes(".feed-item") %>% 
    keep(~ .x %>% str_detect('h2')) %>% 
    map_chr(~ .x %>% html_nodes(".clamp-sm") %>% html_text() %>% str_squish() %>% paste(collapse = ', '))
  titles <- 
    rbind(titles, 
          tibble(title, 
                 subtitle, 
                 author, 
                 synopsis, 
                 title_type) %>% mutate(mission = names(current_mission)))
  Sys.sleep(5)
}

titles %>% write_csv('titles.csv')

titles %>% 
  group_by(title, title_type) %>% 
  summarise(num_missions = n_distinct(mission), 
            missions = mission %>% sort() %>% paste(collapse = ', ')) %>% 
  select(title, title_type, missions, num_missions) %>% 
  View('titles')
