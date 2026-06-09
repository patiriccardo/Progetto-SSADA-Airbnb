library(tidyverse)
library(tidytext)
library(tidyr)
library(stringr)
library(textstem)
library(quanteda)
library(ggwordcloud)

tab_variabili <- function(dati){
  tab=matrix(nrow=ncol(dati), ncol=4)
  tipi=sapply(dati, class)
  unici=sapply(dati, function(x) length(unique(x)))
  na=sapply(dati, function(x) sum(is.na(x)))
  
  for (i in 1:ncol(dati)) {
    tab[i,1]=names(dati)[i]
    tab[i,2]=tipi[i]
    tab[i,3]=unici[i]
    tab[i,4]=na[i]
    
  }
  colnames(tab) <- c("Nome variabile", "Class", "Modalità", "Na's")
  return(tab)
}

#PULIZIA DATASET AIRBNB

load("dati/airbnb.Rdata")

var_rimosse<- c("scrape_id","last_scraped","source","name",
                "picture_url","host_id","host_url","host_name","host_thumbnail_url",
                "host_picture_url","host_neighbourhood","host_total_listings_count",
                "calendar_last_scraped","license", "calendar_updated")
airbnb_pulito <- airbnb_isole %>%
  select(-any_of(var_rimosse))

# host_location -----------------------------------------------------------

# Creo una variabile indicatrice per indicare se l'host è un local o meno 
# dalla location dell'host
table(airbnb_pulito$host_location)

airbnb_pulito$host_is_local <- ifelse(airbnb_pulito$host_location == "Venice, Italy" | 
                                        airbnb_pulito$host_location == "Venezia, Italy" |
                                        airbnb_pulito$host_location == "Lido di Venezia, Italy" |
                                        airbnb_pulito$host_location == "Lido, Italy", 1, 0)

table(airbnb_pulito$host_location, airbnb_pulito$host_is_local)
table(airbnb_pulito$host_is_local)

# Dubbio: Chioggia, Dolo ecc.. li consideriamo local, o solo Venezia? 

airbnb_pulito$host_location <- NULL


# neighbourhood -----------------------------------------------------------

table(airbnb_pulito$neighbourhood)
# Sembra inutile, ci sono solo tanti modi diversi per indicare Venezia
# più qualche Burano ecc..
# La toglierei

# Meglio neighbourhood_cleansed
table(airbnb_pulito$neighbourhood_cleansed)

airbnb_pulito$neighbourhood <- NULL


# bathrooms ---------------------------------------------------------------

table(airbnb_pulito$bathrooms)
table(airbnb_pulito$bathrooms_text)

table(airbnb_pulito$bathrooms, airbnb_pulito$bathrooms_text)

# Terrei bathrooms_text, accorpando 0 bagni che sono uguali

airbnb_pulito$bathrooms_clean <- ifelse(airbnb_pulito$bathrooms_text %in% c("0 baths", "0 shared baths"),
                                        "0 baths", airbnb_pulito$bathrooms_text)

airbnb_pulito$bathrooms_text <- NULL

# airbnb_pulito$bathrooms lo tengo perché mi serve dopo per pulire airroi

airbnb_pulito %>% 
  filter(is.na(bathrooms_clean)) %>% 
  select(listing_url)

# Aggiusto a mano
airbnb_pulito$bathrooms_clean[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/1316352")] <- "1 bath"
airbnb_pulito$bathrooms_clean[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/15812750")] <- "1.5 baths"
airbnb_pulito$bathrooms_clean[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/1435553468734748323")] <- "1 bath"


# Bedrooms e beda ---------------------------------------------------------
airbnb_pulito %>% 
  filter(is.na(bedrooms)) %>% 
  select(listing_url)

airbnb_pulito$bedrooms[which(is.na(airbnb_pulito$bedrooms))] <- 1

airbnb_pulito %>% 
  filter(is.na(beds)) %>% 
  select(listing_url)

airbnb_pulito$beds[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/33741782")] <- 2
airbnb_pulito$beds[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/45479345")] <- 1
airbnb_pulito$beds[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/50558263")] <- 1
airbnb_pulito$beds[which(airbnb_pulito$listing_url == "https://www.airbnb.com/rooms/50762583")] <- 3

# has_availability --------------------------------------------------------

table(airbnb_pulito$has_availability) #??

airbnb_pulito$has_availability <- NULL

# description -------------------------------------------------------------

# elabora_testo <- function(dati) {
#   dati %>%
#     unnest_tokens(word, description) %>% # Tokenizzazione: suddivide il testo in token
#     filter(!word %in% stop_words$word) %>%    # Rimuove le stopwords standard
#     filter(str_detect(word, "^[a-z]+$")) %>%  # Filtra solo parole composte da lettere minuscole (esclude numeri e punteggiatura)
#     mutate(lemma = lemmatize_words(word)) %>% # Lemmatizzazione: riduce ogni parola alla sua forma base (es. "running" → "run")
#     count(lemma, sort = TRUE) %>%             # Conta la frequenza di ciascun lemma, ordinando per frequenza decrescente
#     filter(!lemma %in% stop_words$word)       # Rimuove nuovamente le stopwords sui lemmi (in caso di forme diverse rimaste)
# }
# 
# frequenze <- elabora_testo(airbnb_pulito)
# 
# # Boxplot delle frequenze delle parole
# frequenze %>% select(n) %>% boxplot(main = "Boxplot") 
# # la "parola" più usata è br che sarebbe per andare a capo tipo, quindi la rimuovo
# frequenze <- frequenze %>%
#   filter(lemma != "br")
# 
# # Parole più frequenti
# frequenze %>%
#   filter(n > 1000) %>%
#   mutate(word = reorder(lemma, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col(fill = "steelblue") +
#   coord_flip()
# 
# # L'idea è quella di rimuovere quelle più frequenti(poco informative) e tenere quelle "rare"
# # oppure tf-idf(però diventerebbero troppe variabili)
# 
# # wordcloud
# frequenze %>%
#   arrange(desc(n)) %>%
#   slice_head(n = 60) %>%
#   ggplot(aes(label = lemma, size = n, color = n)) +
#   scale_color_gradient(low = "lightblue", high = "darkblue") +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 15) +
#   labs(title = "Wordcloud parole più frequenti") +
#   theme_minimal()
# 
# # Analisi degli n-grammi (con stop words)
# 
# bigrammi <- airbnb_pulito %>%
#   unnest_tokens(ngram, description, token = "ngrams", n = 2) %>%
#   count(ngram, sort = TRUE)
# head(bigrammi, 10)
# 
# trigrammi <- airbnb_pulito %>%
#   unnest_tokens(ngram, description, token = "ngrams", n = 3) %>%
#   count(ngram, sort = TRUE)
# head(trigrammi, 10)
# 
# quadrigrammi <- airbnb_pulito %>%
#   unnest_tokens(ngram, description, token = "ngrams", n = 4) %>%
#   count(ngram, sort = TRUE)
# head(quadrigrammi, 10)
# 
# rm(bigrammi, trigrammi, quadrigrammi)
# gc()
# 
# # Senza stop words
# bigrammi <- airbnb_pulito %>%
#   unnest_tokens(ngram, description, token = "ngrams", n = 2) %>%
#   separate(ngram, into = c("word1", "word2"), sep = " ") %>%   # Separa le due parole
#   filter(!word1 %in% stop_words$word,                          # Rimuove se word1 è stopword
#          !word2 %in% stop_words$word) %>%                      # Rimuove se word2 è stopword
#   filter(word1 != "br",
#          word2 != "br") %>% 
#   filter(str_detect(word1, "^[a-z]+$"),                        # Filtra solo lettere
#          str_detect(word2, "^[a-z]+$")) %>%
#   unite(ngram, word1, word2, sep = " ") %>%                    # Riunisce le due parole
#   count(ngram, sort = TRUE)
# head(bigrammi, 10)
# 
# trigrammi <- airbnb_pulito %>%
#   unnest_tokens(ngram, description, token = "ngrams", n = 3) %>%
#   separate(ngram, into = c("word1", "word2", "word3"), sep = " ") %>%   # Separa le due parole
#   filter(!word1 %in% stop_words$word,                          # Rimuove se word1 è stopword
#          !word2 %in% stop_words$word,
#          !word3 %in% stop_words$word) %>%                      # Rimuove se word2 è stopword
#   filter(word1 != "br",
#          word2 != "br", 
#          word3 != "br") %>% 
#   filter(str_detect(word1, "^[a-z]+$"),                        # Filtra solo lettere
#          str_detect(word2, "^[a-z]+$"),
#          str_detect(word3, "^[a-z]+$")) %>%
#   unite(ngram, word1, word2, word3, sep = " ") %>%                    # Riunisce le due parole
#   count(ngram, sort = TRUE)
# head(trigrammi, 10)
# 
# rm(bigrammi, trigrammi)
# gc()

# Andare avanti(trovare le parole da "flaggare") poi fare lo stesso per amenities

# Altre var ---------------------------------------------------------------

# host_since e non so se ce ne sono altre così,
# le trasfmormiamo in giorni trascorsi (tipo oggi - l'anno scorso = 365)
airbnb_pulito$host_since <- as.numeric(airbnb_pulito$host_since)

# neighborhood_overviw: descrizione del quartiere in cui si trova la casa -> toglierei
airbnb_pulito$neighborhood_overviw <- NULL

# host_abaout? ci cacciamo 2/3 parole chiave? Ma anche no

# host_response_rate e host_acceptance_rate -> discretizzare?
table(airbnb_pulito$host_acceptance_rate, useNA = "always")
table(airbnb_pulito$host_response_rate, useNA = "always")


airbnb_pulito <- airbnb_pulito %>%
  mutate(host_acceptance_numeric = as.numeric(str_remove(host_acceptance_rate, "%")),
    
    # 2. Categorizzazione in classi intelligenti
    host_acceptance_classe = case_when(
      is.na(host_acceptance_numeric) ~ "Mancante",
      host_acceptance_numeric == 100 ~ "100%",
      host_acceptance_numeric >= 90 & host_acceptance_numeric < 100 ~ "90-99%",
      host_acceptance_numeric >= 70 & host_acceptance_numeric < 90  ~ "70-89%",
      host_acceptance_numeric < 70                                  ~ "<70%"
    ),
    
    # 3. Trasformiamo in fattore (factor) ordinato, utile per i modelli predittivi
    host_acceptance_classe = factor(
      host_acceptance_classe, 
      levels = c("<70%", "70-89%", "90-99%", "100%", "Mancante")
    )
  )

# Verifichiamo il risultato della nuova variabile categorica
table(airbnb_pulito$host_acceptance_classe)

airbnb_pulito$host_acceptance_numeric <- airbnb_pulito$host_acceptance_rate <- NULL

airbnb_pulito <- airbnb_pulito %>%
  mutate(host_response_numeric = as.numeric(str_remove(host_response_rate, "%")),
    
    # 2. Categorizzazione in classi basate sulla densità dei dati
    host_response_classe = case_when(
      is.na(host_response_numeric) ~ "Mancante/NA",
      host_response_numeric == 100 ~ "100%",
      host_response_numeric >= 90 & host_response_numeric < 100 ~ "90-99%",
      host_response_numeric >= 70 & host_response_numeric < 90  ~ "70-89%",
      host_response_numeric < 70                                 ~ "<70%"
    ),
    
    # 3. Trasformazione in fattore ordinato
    host_response_classe = factor(
      host_response_classe, 
      levels = c("<70%", "70-89%", "90-99%", "100%", "Mancante/NA")
    )
  )

# Verifica finale della nuova variabile
table(airbnb_pulito$host_response_classe, useNA = "always")

airbnb_pulito$host_response_numeric <- airbnb_pulito$host_response_rate <- NULL

# host_listings_count (senza na, sono 5)
airbnb_pulito <- airbnb_pulito %>%
  mutate(host_listings_numeric = as.numeric(host_listings_count),
    
    # 2. Creazione delle classi basate sul profilo dell'host
    host_type_classe = case_when(
      host_listings_numeric == 1     ~ "1 alloggio (Privato)",
      host_listings_numeric >= 2 & host_listings_numeric <= 4   ~ "2-4 alloggi (Piccolo host)",
      host_listings_numeric >= 5 & host_listings_numeric <= 19  ~ "5-19 alloggi (Medio host)",
      host_listings_numeric >= 20 & host_listings_numeric <= 99 ~ "20-99 alloggi (Professionista)",
      host_listings_numeric >= 100                               ~ "100+ alloggi (Grande Agenzia)"
    ),
    
    # 3. Trasformazione in fattore ordinato
    host_type_classe = factor(
      host_type_classe,
      levels = c("1 alloggio (Privato)", "2-4 alloggi (Piccolo host)", 
                 "5-19 alloggi (Medio host)", "20-99 alloggi (Professionista)", 
                 "100+ alloggi (Grande Agenzia)")
    )
  )

# Verifica la distribuzione delle nuove classi
table(airbnb_pulito$host_type_classe, useNA = "always")

airbnb_pulito$host_listings_count <- airbnb_pulito$host_listings_numeric <- NULL

# host_verification toglierei
table(airbnb_pulito$host_verifications)
airbnb_pulito$host_verifications <- NULL

# host_has_profile_pic sbilanciato, magari da problemi in stima e verifica
airbnb_pulito$host_has_profile_pic <- NULL

# Tenere room_type piuttosto che property?
table(airbnb_pulito$property_type)
table(airbnb_pulito$room_type)
table(airbnb_pulito$room_type, airbnb_pulito$property_type)

airbnb_pulito$property_type <- NULL

# super_host -> discretizzo e aggiungo NA
airbnb_pulito$host_is_superhost <- ifelse(is.na(airbnb_pulito$host_is_superhost), "NA's", airbnb_pulito$host_is_superhost)

# host_is_local idem
airbnb_pulito$host_is_local <- ifelse(is.na(airbnb_pulito$host_is_local), "NA's", airbnb_pulito$host_is_local)

# Rimuovo altre variabili
airbnb_pulito <- airbnb_pulito %>% 
  select(-host_about, -description, -neighborhood_overview, -first_review, -last_review, -minimum_nights, -maximum_nights,
         -minimum_minimum_nights, -minimum_maximum_nights, -maximum_minimum_nights, -maximum_maximum_nights)

tab_variabili(airbnb_pulito)


# MERGE CON AIRROI --------------------------------------------------------
library(arrow)

airroi <- read_parquet("dati/listings.parquet")

# Aggiungiamo il flag 'airroi' al dataset principale
airbnb_pulito$airroi <- airbnb_pulito$id %in% airroi$listing_id

airbnb_pulito %>% 
  filter(is.na(review_scores_rating)) %>% 
  select(airroi) %>%
  table()

## MANCANO DA SISTEMARE LE 376 OSS.

save(airbnb_pulito, file = "dati/airbnb_pulito.Rdata")