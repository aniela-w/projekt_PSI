#' ---
#' title: "Analiza sentymentu w czasie w komediach romantycznych"
#' author: "Aniela Wróblewska, Natalia Woźniak, Adam Ćwierzyński "
#' date:   "27.05.2025 "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable     
#'     highlight: kate 
#'     toc: true     
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show  
#'     number_sections: false  
#' ---

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

#' # Analiza sentymentu w czasie dla dwóch komedii romantycznych z użyciem słowników AFINN, NRC i Bing
# Analiza sentymentu w czasie dla dwóch komedii romantycznych z użyciem słowników AFINN, NRC i Bing ----

#' # 0. Wymagane pakiety 
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

#' # 1. Wczytywanie danych tekstowych z plików
# Wczytywanie danych tekstowych z plików ----

sciezka1 <- "/Users/anielkawroblewska/Documents/projekt psi/how_to_lose_a_guy_in_10_days_script.txt"
sciezka2 <- "/Users/anielkawroblewska/Documents/projekt psi/the_proposal_scrpit.txt"

text1 <- readLines(sciezka1, encoding = "UTF-8")
text2 <- readLines(sciezka2, encoding = "UTF-8")

#' # 2. Przekształcenie danych do ramki
# Przekształcenie danych do ramki ----

df1 <- tibble(line = 1:length(text1), text = text1)
df2 <- tibble(line = 1:length(text2), text = text2)

#' # 3. Słowniki
# Słowniki ----

afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

#' # 4. AFINN: analiza sentymentu liczbowego
# AFINN: analiza sentymentu liczbowego ----

sent_afinn1 <- df1 %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word") %>%
  group_by(line) %>%
  summarise(sentiment = sum(value, na.rm = TRUE)) %>%
  filter(sentiment != 0) %>%
  mutate(film = "How to Lose a Guy in 10 Days",
         line_norm = line / max(line))

sent_afinn2 <- df2 %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word") %>%
  group_by(line) %>%
  summarise(sentiment = sum(value, na.rm = TRUE)) %>%
  filter(sentiment != 0) %>%
  mutate(film = "The Proposal",
         line_norm = line / max(line))

#' # Statystyki podsumowujące
# Statystyki podsumowujące ----

afinn_stats <- tibble(
  Film = c("How to Lose a Guy in 10 Days", "The Proposal"),
  Sredni_Sentyment = c(mean(sent_afinn1$sentiment), mean(sent_afinn2$sentiment)),
  Mediana = c(median(sent_afinn1$sentiment), median(sent_afinn2$sentiment)),
  Odchylenie = c(sd(sent_afinn1$sentiment), sd(sent_afinn2$sentiment)),
  Liczba_Linii = c(nrow(sent_afinn1), nrow(sent_afinn2))
)
print(afinn_stats)

#' # Oddzielne wykresy AFINN
# Oddzielne wykresy AFINN ----

ggplot(sent_afinn1, aes(x = line, y = sentiment)) +
  geom_line(color = "blue") +
  ggtitle("AFINN: Sentyment w czasie – How to Lose a Guy in 10 Days") +
  xlab("Numer linii (czas trwania filmu)") +
  ylab("Poziom sentymentu") +
  theme_minimal(base_size = 14)

ggplot(sent_afinn2, aes(x = line, y = sentiment)) +
  geom_line(color = "red") +
  ggtitle("AFINN: Sentyment w czasie – The Proposal") +
  xlab("Numer linii (czas trwania filmu)") +
  ylab("Poziom sentymentu") +
  theme_minimal(base_size = 14)

#' # Oba filmy na jednym wykresie
# Oba filmy na jednym wykresie ----

combined_afinn <- bind_rows(sent_afinn1, sent_afinn2)

ggplot(combined_afinn, aes(x = line_norm, y = sentiment, color = film)) +
  geom_line(alpha = 0.7) +
  ggtitle("Porównanie sentymentu AFINN w czasie (znormalizowane)") +
  xlab("Znormalizowana długość filmu") +
  ylab("Sentyment") +
  theme_minimal(base_size = 14)

#' # 5. Analiza za pomocą słownika Bing
# Analiza za pomocą słownika Bing ----

bing_words1 <- df1 %>%
  unnest_tokens(word, text) %>%
  inner_join(bing, by = "word") %>%
  count(line, sentiment)

bing_words2 <- df2 %>%
  unnest_tokens(word, text) %>%
  inner_join(bing, by = "word") %>%
  count(line, sentiment)

#' #  Wykresy pozytywnych i negatywnych słów (Bing)
# Wykresy pozytywnych i negatywnych słów (Bing) ----
# Wizualizacja pokazuje liczbę pozytywnych i negatywnych słów używanych w różnych momentach filmu. ----

bing_words1 %>%
  ggplot(aes(x = line, y = n, color = sentiment)) +
  geom_line() +
  ggtitle("Bing: pozytywne i negatywne słowa – How to Lose a Guy in 10 Days") +
  xlab("Numer linii (czas trwania filmu)") +
  ylab("Liczba słów") +
  theme_minimal(base_size = 14)

bing_words2 %>%
  ggplot(aes(x = line, y = n, color = sentiment)) +
  geom_line() +
  ggtitle("Bing: pozytywne i negatywne słowa – The Proposal") +
  xlab("Numer linii (czas trwania filmu)") +
  ylab("Liczba słów") +
  theme_minimal(base_size = 14)

#' # 6. NRC analiza emocji
# NRC analiza emocji ----

emo1 <- df1 %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  count(line, sentiment)

emo2 <- df2 %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  count(line, sentiment)

#' # NRC: Emocje w czasie (wybrane)
# NRC: Emocje w czasie (wybrane) ----
# Te wykresy pokazują zmienność emocji takich jak radość, smutek, zaufanie i złość w scenariuszach filmów.----

wybrane_emocje <- c("joy", "sadness", "anger", "trust")

emo1 %>%
  filter(sentiment %in% wybrane_emocje) %>%
  ggplot(aes(x = line, y = n, color = sentiment)) +
  geom_line() +
  ggtitle("NRC: Emocje w czasie – How to Lose a Guy in 10 Days") +
  xlab("Numer linii (czas trwania filmu)") +
  ylab("Liczba słów emocjonalnych") +
  theme_minimal(base_size = 14)

emo2 %>%
  filter(sentiment %in% wybrane_emocje) %>%
  ggplot(aes(x = line, y = n, color = sentiment)) +
  geom_line() +
  ggtitle("NRC: Emocje w czasie – The Proposal") +
  xlab("Numer linii (czas trwania filmu)") +
  ylab("Liczba słów emocjonalnych") +
  theme_minimal(base_size = 14)

#' # Chmury słów emocjonalnych (NRC)
#' Tak dodatkowo:)
# Chmury słów emocjonalnych (NRC)----

nrc_words1 <- df1 %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word", relationship = "many-to-many")

nrc_words2 <- df2 %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word", relationship = "many-to-many")

nrc_words1 %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 100, scale = c(4, 0.5), colors = brewer.pal(8, "Dark2")))

title("Chmura słów emocjonalnych – How to Lose a Guy in 10 Days")

nrc_words2 %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 100, scale = c(2.5, 0.5), colors = brewer.pal(8, "Paired")))

title("Chmura słów emocjonalnych – The Proposal")

