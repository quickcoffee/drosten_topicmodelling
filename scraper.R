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
  #get text from pdf url and do first part of cleaning
  text <- tabulizer::extract_text(pdf_file) %>%
    str_squish() %>%
    #remove linebreaks
    str_remove_all(pattern = "(?<=\\b[:alpha:]{2,50})\\-[:blank:]") %>%
    #remove everything from QUELLEN
    str_remove_all(pattern = "\\bQUELLEN\\b.+") %>%
    str_remove_all(pattern = "\\bGLOSSAR Erklärungen zu den Fachausdrücken\\b.+")
  
  
    #extract date from text
  episode_date <- text %>% 
    str_extract(pattern = "(?<=Stand[:blank:])[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}") %>% 
    lubridate::dmy()
  
  #more cleaning..
    text <- text %>%
      #remove date and episode number
      str_remove_all(pattern = "Stand[:blank:][:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}[:blank:]NDR.DE/CORONAUPDATE[:blank:][:digit:]{1,2}\\/[:digit:]{1,2}") %>%
    #remove everything including first episode no
    str_remove(pattern = ".+FOLGE[:blank:][:digit:]{1,3}[:blank:]") %>%
    #remove headlines
    gsub(pattern = "\\s*\\p{Lu}{2,}(?:\\s+\\p{Lu}{2,})+\\b[\\p{P}\\p{S}]*", replacement = "", perl=TRUE) %>%
    str_squish() %>% 
    str_remove(pattern = " ndr\\.de\\/coronaupdate$")
  return(list(episode_date, text))
}

create_guest_pattern <- function(.guest) {
  glue::glue("((?<![:alnum:]\\s){.guest}(?![:punct:]))")
} 


create_pattern_string <- function(.autor_interview,..., prefix = "", sep = "|") {
  guests_pattern <- ... %>% 
    map_chr(create_guest_pattern)
  guests_pattern <- paste0(guests_pattern, prefix = prefix, collapse = sep)
  
  autor_interview_pattern <- str_glue("(?<![:alnum:]\\s){.autor_interview}")
  
  paste(autor_interview_pattern, guests_pattern, sep = sep)
}


#function to split interview into speakers
split_interview <- function(.interview_text, .autor_interview, .guests) {
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

guests <- c("Christian Drosten",
            "Korinna Henning",
            "Corinna Hennig",
            "Dirk Brockmann",
            "Sandra Ciesek",
            "Lothar Wieler",
            "Martin Kriegel",
            "Marylyn Addo",
            "Prof. Dr. Hans-Georg Eichler",
            "Wolfang Greiner",
            "Alena Buyx",
            "")


#read html of podcast homepage
corona_update_html <- read_html("https://www.ndr.de/nachrichten/info/Coronavirus-Update-Die-Podcast-Folgen-als-Skript,podcastcoronavirus102.html")

#extract title, date, pdf-link, episode number, text and interviewer
#episodes_df <-
corona_update_html %>%
  html_nodes(css = ".voll") %>%
  extract2(1) %>% 
  html_nodes(css = ".teaser") %>%
  extract(1) %>% 
  map_df(~{
    title <- .x %>% html_nodes('h2') %>% html_text() %>% str_trim()
    link <- .x %>% html_nodes('h2') %>% html_nodes('a') %>% html_attr("href")
    tibble(title, link)
  }) %>% 
  mutate(link = paste0("https://www.ndr.de", link),
         episode_no = str_extract(string = title, pattern = "(?<=\\()[:digit:]+(?=\\))"),
         #clean title
         title = trimws(str_remove(string = title, pattern = "\\(.+\\)[:blank:]")),
         title = trimws(str_remove(string = title, pattern = "[:blank:]\\-[:blank:]Skript herunterladen$"))) %>% 
  mutate(text = map_chr(.x = link, .f = extract_text),
         guests = map(.x = link, .f = extract_guests)) %>%
  mutate(interviewer = str_squish(str_extract(string = text, pattern = "[:alpha:]+ [:alpha:]+ ")),
         interviewer = case_when(interviewer == "Korinna Henning" ~ "Korinna Hennig",
                                   interviewer == "Corinna Hennig" ~ "Korinna Hennig",
                                   TRUE ~ interviewer))

  


  
  #get rid of titles as they are (sometimes) not used in the transcript
  no_title_names <- if_else(str_detect(string = clean_names, pattern = "Prof|Dr"), true = str_remove_all(string = clean_names, pattern = "Prof\\.|Prof|Dr\\.|Dr"), false = "") %>% 
    str_squish()
  #a bit of base R to remove empty strings
  no_title_names <- no_title_names[nchar(no_title_names) > 0]
  #combine everything
  guest_names <- c(clean_names, no_title_names)
  return(guest_names)
}

extract_guests(test_url)


  

pattern_string <- str_glue("(?<![:alnum:]\\s){.autor_interview}|((?<![:alnum:]\\s)Christian Drosten(?![:punct:]))|((?<![:alnum:]\\s)Korinna Henning(?![:punct:]))|((?<![:alnum:]\\s)Corinna Hennig(?![:punct:]))|((?<![:alnum:]\\s)Korinna Henning(?![:punct:]))|((?<![:alnum:]\\s)Dirk Brockmann(?![:punct:]))")

guests <- c("Christian Drosten", "Dirk Brockmann", "Corinna Hennig")


#splitting up episode into speakers
interviews_df <- episodes_df %>%
  mutate(result = map2(.x = text, .y = interviewer, .f = split_interview)) %>% 
  unnest(result) %>% 
  select(-text)

#export transcript tibble
saveRDS(episodes_df, file = "episodes_df.rds")
saveRDS(interviews_df, file = "interviews_raw.rds")
write_csv(interviews_df, file = "interviews_raw.csv")
  
  
