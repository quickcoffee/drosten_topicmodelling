library(tidyverse)
library(tidytext)
library(stopwords)
library(SnowballC)

stop_german <- tibble(word = c(stopwords::stopwords("de"),
                               lsa::stopwords_de,
                               read_delim(file = "https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt", delim = "/t", col_names = F, trim_ws = T) %>% 
                                 pull())) %>%
  bind_rows(tibble(word = c("dass", "schon", "mal", "deswegen", "eigentlich", "ganz", "ja", "genau", "gar", "jeweils", "zumindest", "klar", "finde", "nämlich", "sowieso", "sagen", "eher", "bisschen", "glaube", "sodass"))) %>% 
  distinct()

interviews_df <- interviews_df %>% 
  group_by(episode_no, speaker) %>%
  mutate(count = TRUE,
         count = cumsum(count),
    id = glue::glue("{episode_no}-{speaker}-{count}")) %>% 
  ungroup() %>% 
  select(-count)

tidy_interviews <-  interviews_df %>% 
  unnest_tokens(output = word, input = transcript, token = c("words")) %>% 
  anti_join(stop_german) %>%
  add_count(word) %>%
  filter(n > 2) %>%
  select(-n)

interviews_tf_idf <- tidy_interviews %>%
  count(id, word) %>%
  bind_tf_idf(id, word, n)


interviews_sparse <- interviews_tf_idf %>%
  count(id, word) %>%
  cast_sparse(id, word, n)

library(stm)
library(furrr)
no_cores <- availableCores() - 1
plan(multicore, workers = no_cores)

many_models <- tibble(K = seq(6, 20, by=1)) %>%
  mutate(topic_model = future_map(K, ~stm(interviews_sparse, K = .,
                                          verbose = F)))

heldout <- make.heldout(interviews_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, interviews_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, interviews_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "Between 10 to 15 seems like a reasonable number of topics")


k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(10, 13, 14)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

#select K = 14 topics
topic_model <- k_result %>% 
  filter(K == 14) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model
labelTopics(topic_model)

td_beta <- tidy(topic_model)
td_beta

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(interviews_sparse))
td_gamma

library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.27),
                     labels = scales::percent_format()) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence in the NDR Info Coronavirus-Update corpus",
       subtitle = "With the top words that contribute to each topic")

library(knitr)
gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))


episodes_topic_tbl <- td_gamma %>%
  left_join(interviews_df, by = c("document" = "id")) %>% 
  mutate(transcript_length = nchar(transcript)) %>% 
  group_by(episode_no, topic) %>% 
  mutate(gamma_weighted = gamma * (transcript_length/sum(transcript_length)))
episodes_topic_tbl <- episodes_topic_tbl %>%
  left_join(episodes_topic_tbl %>%
              top_n(n = 1, wt = gamma) %>%
              select(episode_no, topic, top_gamma_transcript = transcript))

episodes_topic_tbl %>% 
  ungroup() %>% 
  group_by(episode_no, speaker) %>% 
  summarise(length = sum(transcript_length)) %>% 
  ggplot(aes(x=as_factor(as.numeric(episode_no)), y=length, fill = fct_relevel(speaker, "Korinna Hennig",
                                                                              "Anja Martini",
                                                                              "Christian Drosten",
                                                                              "Dirk Brockmann")))+
  geom_col()+
  labs(fill = "Speaker")

p_topics <- episodes_topic_tbl %>% 
  summarise(gamma_sum = sum(gamma_weighted), top_gamma_transcript = max(top_gamma_transcript)) %>% 
  left_join(top_terms, by = "topic") %>%
  left_join(episodes_df, by = c("episode_no")) %>% 
  ggplot(aes(x=as_factor(as.numeric(episode_no)), y=gamma_sum, fill = as.factor(topic), group = topic,
             label=terms))+
  geom_area()+
  facet_wrap(~as.factor(topic))

p_topics

library(plotly)
ggplotly(p_topics)

