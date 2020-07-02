library(tidyverse)
library(magrittr)
library(lubridate)
library(rvest)
library(pdftools)
library(tabulizer)

#function to download pdf and extract text
extract_text <- function(pdf_file) {
  #wait one sec
  Sys.sleep(1)
  #get text from pdf url
  tabulizer::extract_text(pdf_file) %>%
    str_squish() %>%
    #remove linebreaks
    str_remove_all(pattern = "(?<=\\b[:alpha:]{2,50})\\-[:blank:]") %>%
    #remove everything from QUELLEN
    str_remove_all(pattern = "\\bQUELLEN\\b.+") %>%
    #remove date and episode no
    str_remove_all(pattern = "Stand[:blank:][:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}[:blank:]NDR.DE/CORONAUPDATE[:blank:][:digit:]{1,2}\\/[:digit:]{1,2}") %>%
    #remove everyting including first episode no
    str_remove(pattern = ".+FOLGE[:blank:][:digit:]{1,3}[:blank:]") %>%
    #remove headlines
    gsub(pattern = "\\s*\\p{Lu}{2,}(?:\\s+\\p{Lu}{2,})+\\b[\\p{P}\\p{S}]*", replacement = "", perl=TRUE) %>%
    str_squish() %>% 
    str_remove((pattern = " ndr\\.de\\/coronaupdate$"))
}

#function to split interview into speakers
split_interview <- function(.interview_text, .autor_interview) {
  pattern_string <- str_glue("(?<![:alnum:]\\s){.autor_interview}|((?<![:alnum:]\\s)Christian Drosten(?![:punct:]))|((?<![:alnum:]\\s)Korinna Henning(?![:punct:]))|((?<![:alnum:]\\s)Corinna Hennig(?![:punct:]))|((?<![:alnum:]\\s)Korinna Henning(?![:punct:]))|((?<![:alnum:]\\s)Dirk Brockmann(?![:punct:]))")
  speaker <- str_extract_all(string = .interview_text, pattern = pattern_string) %>% 
    unlist() %>% 
    discard(~.x == "")
  transcript <- str_split(string = .interview_text, pattern = pattern_string) %>% 
    unlist() %>% 
    discard(~.x == "")
  tibble(speaker, transcript) %>% 
    mutate_all(.funs = str_squish) %>% 
    mutate_at(.vars = vars(speaker), .funs = ~ case_when(.x == "Korinna Henning" ~ "Korinna Hennig",
                                                         .x == "Corinna Hennig" ~ "Korinna Hennig",
                                                         TRUE ~ .x))
}


#read html of podcast homepage
corona_update <- read_html("https://www.ndr.de/nachrichten/info/Coronavirus-Update-Die-Podcast-Folgen-als-Skript,podcastcoronavirus102.html")

#extract title, date, pdf-link, episode number, text and interviewer
episodes_df <- corona_update %>%
  html_nodes(css = ".voll") %>%
  extract2(1) %>% 
  html_nodes(css = ".teaser") %>% 
  map_df(~{
    title <- .x %>% html_nodes('h2') %>% html_text() %>% str_trim()
    date <- .x %>% html_nodes('.date') %>% html_text()
    link <- .x %>% html_nodes('h2') %>% html_nodes('a') %>% html_attr("href")
    tibble(title, date, link)
  }) %>% 
  mutate(link = paste0("https://www.ndr.de", link),
         episode_no = str_extract(string = title, pattern = "(?<=\\()[:digit:]+(?=\\))"),
         title = trimws(str_remove(string = title, pattern = "\\(.+\\)[:blank:]")),
         date = dmy_hm(date)) %>% 
  mutate(text = map_chr(.x = link, .f = extract_text)) %>% 
  mutate(interviewer = str_squish(str_extract(string = text, pattern = "[:alpha:]+ [:alpha:]+ ")),
         interviewer = case_when(interviewer == "Korinna Henning" ~ "Korinna Hennig",
                                   interviewer == "Corinna Hennig" ~ "Korinna Hennig",
                                   TRUE ~ interviewer))

#splitting up episode into speakers
interviews_df <- episodes_df %>%
  mutate(result = map2(.x = text, .y = interviewer, .f = split_interview)) %>% 
  unnest(result) %>% 
  select(-text)

#export transcript tibble
saveRDS(interviews_df, file = "interviews_raw.rds")
  
  
