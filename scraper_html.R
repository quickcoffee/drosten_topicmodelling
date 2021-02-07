library(tidyverse)
library(magrittr)
library(lubridate)
library(rvest)
library(urltools)
library(arrow)

#import rds if available
if (file.exists("corona_update_transcripts.rds")) {
  corona_update_transcripts <- readRDS(file = "corona_update_transcripts.rds")
}


extract_last_change <- function(.episode_html) {
  .episode_html %>%
    html_node(css = ".lastchanged") %>% 
    html_text() %>% 
    str_remove(pattern = "[:alpha:]+[:punct:]") %>% 
    str_remove(pattern = "Uhr") %>% 
    str_squish() %>% 
    lubridate::dmy_hm()
}

#TODO get episode length from iframe player
extract_player_iframe_url <- function(.episode_html){
  .episode_html %>% 
    html_node("iframe") %>% 
    html_attr("src")
}

extract_episode_length <- function(.episode_html) {
  .episode_html %>% 
    html_node(css = ".textcontent h2") %>% 
    html_text() %>% 
    str_extract(pattern = "(?<=\\().{2,20}(?=\\)$)")
}

extract_transcript_nodes <- function(.episode_html) {
  .episode_html %>%
    #get all siblings of node p after the last node a that starts with # for the href attribute
    html_nodes(xpath = '//p[a[starts-with(@href, "#")]][last()]/following-sibling::p')
}

extract_speaker_name <- function(.transcript_nodes) {
  html_node(x = .transcript_nodes, xpath = "strong") %>%
    html_text(trim = TRUE) %>%
    str_squish() %>% 
    str_extract(pattern = "^[:upper:][:alpha:]+.+\\:$") %>%
    str_remove(pattern = ":") %>%
    #manual fix for episode 38
    str_replace(pattern = "Eine Bitte an unsere Hörer", replacement = "Korinna Hennig") %>%
    str_squish() %>%
    na_if(y = "")
}

extract_transcript <- function(.episode_url) {
  #sleep to be polite
  Sys.sleep(runif(1, min = 0.5, max = 2))
  
  #get html for episode_url
  episode_html <- read_html(.episode_url)
  
  #extract all information via functions
  episode_last_change <- extract_last_change(episode_html)
  episode_length <-  extract_episode_length(episode_html)
  
  transcript_nodes <- extract_transcript_nodes(episode_html)
  speaker_names <- extract_speaker_name(transcript_nodes)
  speaker_text <- html_text(transcript_nodes, trim = TRUE)
  
  #put it all together and some clean up on the speaker column
  tibble(speaker = speaker_names,
         text = speaker_text) %>%
    tidyr::fill(speaker, .direction = "down") %>%
    drop_na() %>% 
    mutate(text = str_remove(text, pattern = speaker) %>% 
             str_remove(pattern = "^\\:") %>% 
             str_squish(),
           paragraph_no = row_number()) %>% 
    nest(speaker = speaker, text = text, paragraph_no = paragraph_no) %>% 
    mutate(last_change = episode_last_change,
           duration_episode = episode_length)
}





all_episodes_url <- "https://www.ndr.de/nachrichten/info/Coronavirus-Update-Alle-Folgen,podcastcoronavirus134.html"
#read html of podcast homepage
corona_update_html <- read_html(all_episodes_url)

#get list of episodes including urls to transcript
corona_update_transcripts <- corona_update_html %>%
  html_nodes(css = ".std h2") %>%
  map_df(~{
    title <- .x %>% html_nodes('a') %>% html_text() %>% str_trim()
    link <- .x %>% html_nodes('a') %>% html_attr("href")
    tibble(title, link)
  }) %>%
  mutate(link = paste0("https://", domain(all_episodes_url), link),
         episode_no = str_extract(string = title, pattern = "(?<=\\()[:digit:]+(?=\\))"),
         episode_no = as.integer(episode_no),
         #clean title
         title = trimws(str_remove(string = title, pattern = "\\(.+\\)[:blank:]") %>% 
                          str_remove("Coronavirus-Update\\: "))) %>%
  #get transcript data and unnest the results to get a big data frame
  mutate(result_text = map(.x = link, .f = extract_transcript)) %>% 
  unnest(result_text) %>% 
  unnest(c(paragraph_no, speaker, text))

#sanity check of speaker names
speaker_names <- count(corona_update_transcripts, speaker, sort = TRUE)
speaker_names

#manually clean the speaker names from typos and different variants
corona_update_transcripts <- corona_update_transcripts %>%
  mutate(speaker = case_when(
    speaker %in% c("Hennig",
                   "Hennig",
                   "Henning",
                   "Korinna Hennig") ~ "Korinna Hennig",
    speaker %in% c("Drosten") ~ "Christian Drosten",
    speaker %in% c("Ciesek",
                   "Cisek",
                   "Sandra Cisek") ~ "Sandra Ciesek",
    speaker %in% c("Martini") ~ "Anja Martini",
    speaker %in% c("Rohde") ~ "Gernot Rohde",
    speaker %in% c("Kluge") ~ "Stefan Kluge",
    speaker %in% c("Kriegel") ~ "Martin Kriegel",
    speaker %in% c("Wieler") ~ "Lothar Wieler",
    speaker %in% c("Addo") ~ "Marylyn Addo",
    speaker %in% c("Muntau") ~ "Ania Muntau",
    speaker %in% c("Buyx") ~ "Alena Buyx",
    speaker %in% c("Greiner",
                   "Wolfang Greiner") ~ "Wolfgang Greiner",
    speaker %in% c("Prof. Dr. Hans-Georg Eichler",
                   "Eichler") ~ "Hans-Georg Eichler",
    TRUE ~ speaker
    )
  )


#export transcript tibble
saveRDS(corona_update_transcripts, file = "corona_update_transcripts.rds")
write_parquet(corona_update_transcripts, sink = "corona_update_transcripts.parquet")
write_csv(corona_update_transcripts, file = "corona_update_transcripts.csv")





