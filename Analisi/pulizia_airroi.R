load("dati/airbnb_pulito.Rdata")
#pulizia airroi-->listings.parquet
#QUESTO FILE E' DA GUARDARE CON IL FILE WORD DIZIONARIO_VARIABILI_AIRROI
library(arrow)
airroi <- read_parquet("dati/listings.parquet")

View(airroi) #73 colonnex 300
dim(airroi)

#merge di airroi con airbnb pulito con chiave listing_id di airroi e chiave id del dataset airbnb_pulito
# Tieni solo le righe presenti in entrambi, mantenendo solo le colonne di airroi
airroi_filtrato <- airroi %>% 
  semi_join(airbnb_pulito, by = c("listing_id" = "id"))
dim(airroi_filtrato)


#1 IDENTIFICATIVI ANNUNCIO------
airroi_pulito<-airroi_filtrato
dim(airroi_filtrato) #250 x 73 
airroi_pulito$listing_name<-airroi_pulito$description<-airroi_pulito$cover_photo_url<-airroi_pulito$photo_urls<-NULL

#8. VARIABILI ULTIMI 90 GIORNI -----
#elimino tutte perche non mi interessano solo gli ultimi 90g
colonne_90giorni<-grepl("l90d", colnames(airroi_pulito))
airroi_pulito<- airroi_pulito[,!colonne_90giorni]
View(airroi_pulito)

#2. CARATTERISTICHE STRUTTURALI -----
#listing type e room type
table(airroi_pulito$listing_type)
table(airroi_pulito$room_type, useNA = "ifany") 
#tengo room type che accorpa gia in 3 modalità
airroi_pulito$listing_type<-NULL
airroi_pulito$room_type<-as.factor(airroi_pulito$room_type)

#ospiti tengo numerica
table(airroi_pulito$guests, useNA="ifany")
# ==============================================================================
# RECUPERO VALORI MANCANTI DI GUESTS DA AIRBNB_PULITO
# ==============================================================================
#VAI A CONTROLLARE ID NELL'ALTRO DATASET DI QUEGLI NA E IMPUTA IL NUMERO DI OSPITI SE LA' E' PRESENTE

# Identifichiamo le righe dove 'guests' è NA in airroi_pulito
righe_na <- which(is.na(airroi_pulito$guests))
options(scipen=999) #toglie forma esponenziale
#Occhio che devo usare il dataset airroi_filtrato che contiene ancora gli identificativi
for(i in righe_na) {
  # Prendiamo il listing_id dell'annuncio con il dato mancante
  id_corrente <- airroi_filtrato$listing_id[i]
  
  # Cerchiamo se questo ID esiste in airbnb_pulito e se ha il dato sugli ospiti
  valore_recuperato <- airbnb_pulito$accommodates[airbnb_pulito$id == id_corrente]
  
  # Se troviamo una corrispondenza univoca e non è NA, la inseriamo
  if(length(valore_recuperato) == 1 && !is.na(valore_recuperato)) {
    airroi_pulito$guests[i] <- valore_recuperato
  }
}
# Verifica quanti NA sono rimasti dopo il recupero diretto
cat("NA rimasti dopo il controllo nell'altro dataset:", sum(is.na(airroi_pulito$guests)), "\n")


#beds############# 
table(airroi_pulito$beds)
table(airroi_pulito$beds,airroi_pulito$guests)
#rimuovo?????si dai
airroi_pulito$beds<-NULL

#bedrooms ###########
table(airroi_pulito$bedrooms,useNA="ifany") 
#gestiona NA con airbnb_pulito$bedrooms
righe_na <- which(is.na(airroi_pulito$bedrooms))
for(i in righe_na) {
  # Prendiamo il listing_id dell'annuncio con il dato mancante
  id_corrente <- airroi_filtrato$listing_id[i]
  # Cerchiamo se questo ID esiste in airbnb_pulito
  valore_recuperato <- airbnb_pulito$bedrooms[airbnb_pulito$id == id_corrente]
  # Se troviamo una corrispondenza univoca e non è NA, la inseriamo
  if(length(valore_recuperato) == 1 && !is.na(valore_recuperato)) {
    airroi_pulito$bedrooms[i] <- valore_recuperato
  }
}
# Verifica quanti NA sono rimasti dopo il recupero diretto
cat("NA rimasti dopo il controllo nell'altro dataset:", sum(is.na(airroi_pulito$bedrooms)), "\n")

##  righe_na
##  airroi_filtrato[197,"listing_id"] #1581387 in airbnb pulito si capisce che ha 1 camera anche se era NA
##  imputo bedrooms=1 in airbnb_pulito per l'annuncio con id=1581387
##  airbnb_pulito[airbnb_pulito$id == "1581387", "bedrooms"] <- 1
##  rifaccio il ciclo for

#crea modalita 4 o superiore e accorpo 0 e 1
airroi_pulito$bedrooms <- cut(
  airroi_pulito$bedrooms,
  breaks = c(-Inf, 1, 2, 3, Inf),
  labels = c("1", "2", "3", "4+"),
  right  = TRUE
)

#baths
table(airroi_pulito$baths, useNA="ifany")

#gestione NA con airbnb_pulito$bathrooms
righe_na <- which(is.na(airroi_pulito$baths))
for(i in righe_na) {
  # Prendiamo il listing_id dell'annuncio con il dato mancante
  id_corrente <- airroi_filtrato$listing_id[i]
  valore_recuperato <- airbnb_pulito$bathrooms[airbnb_pulito$id == id_corrente]
  # Se troviamo una corrispondenza univoca e non è NA, la inseriamo
  if(length(valore_recuperato) == 1 && !is.na(valore_recuperato)) {
    airroi_pulito$baths[i] <- valore_recuperato
  }
}
# Verifica quanti NA sono rimasti dopo il recupero diretto
cat("NA rimasti dopo il controllo nell'altro dataset:", sum(is.na(airroi_pulito$baths)), "\n")


#modalità [0,1,2,3+]
airroi_pulito$baths <- cut(
  airroi_pulito$baths,
  breaks = c(-Inf, 0, 1, 2, Inf),
  labels = c("0", "1", "2", "3+"),
  right  = TRUE
)

#photos_count lo tengo numerico
hist(airroi_pulito$photos_count)

#amenities 
#analisi descrittiva come nell'altro dataset ma poi togli
airroi_pulito$amenities<-NULL


#3. POSIZIONE---------
airroi_pulito$exact_location<-NULL

#4. HOST ------------
#tengo solo superhost e professional_management 
airroi_pulito$host_id<-airroi_pulito$host_name<-airroi_pulito$cohost_ids<-
  airroi_pulito$cohost_names<-airroi_pulito$registration<-NULL 
table(airroi_pulito$superhost,useNA="ifany") #non ha NA, metti in fattori
table(airroi_pulito$professional_management,useNA="ifany") #ha solo 4 true, quindi devo toglierla
airroi_pulito$superhost<-as.factor(airroi_pulito$superhost)
airroi_pulito$professional_management<-NULL

#5. POLITICHE E CONDIZIONI ----------
#min_nights tolgo perche c'è gia ttm_avg_min_nights
airroi_pulito$min_nights <- NULL

#cancellation_policy #### :Moderate(canc.gratis fino a 5 giorni prima checkin), Flexible(fino a 1 giorno prima),...
table(airroi_pulito$cancellation_policy,useNA = "ifany")

policy_char <- as.character(airroi_pulito$cancellation_policy)
# Applichiamo il raggruppamento logico
airroi_pulito$cancellation_policy <- factor(
  ifelse(policy_char == "Flexible", "Flessibile",
         ifelse(policy_char == "Moderate", "Moderata", 
                ifelse(policy_char %in% c("Firm", "Limited", "Super Strict 30 Days"), "Rigida", NA))),
  levels = c("Flessibile", "Moderata", "Rigida")
)

#CHECKIN #######
table(airroi_pulito$checkin_time, useNA = "ifany")
#gli NA qui possono essere informativi quindi faccio modalità a parte(potrebbe essere checkin flessibile)

# 1. Convertiamo in vettore di caratteri pulito
checkin_vettore <- trimws(as.character(airroi_pulito$checkin_time))
# 2. Struttura condizionale con controllo degli errori
fasce_checkin <- ifelse(
  is.na(checkin_vettore) | checkin_vettore == "", "Non_Specificato",
  ifelse(grepl("9:00|10:00|11:00|12:00|1:00", checkin_vettore), "Mattina_Mezzogiorno",
         ifelse(grepl("2:00|3:00", checkin_vettore), "Primo_Pomeriggio",
                ifelse(grepl("4:00|5:00|6:00", checkin_vettore), "Tardo_Pomeriggio_Sera",
                       "ERRORE LETTURA")))) 
# 3. Trasformiamo in fattore includendo l'eventuale livello di errore per il controllo
airroi_pulito$checkin_time <- factor(
  fasce_checkin, 
  levels = c("Primo_Pomeriggio", "Mattina_Mezzogiorno", "Tardo_Pomeriggio_Sera", "Non_Specificato", "ERRORE_LETTURA")
)
airroi_pulito$checkin_time <- droplevels(airroi_pulito$checkin_time)#elimina la mod con 0 oss.
table(airroi_pulito$checkin_time, useNA = "ifany")

#CHECKOUT ####
table(airroi_pulito$checkout_time, useNA = "ifany")
#non ha senso includerla in base alle frequenze e alla possibile collinearità con il checkin
airroi_pulito$checkout_time<-NULL

#instant_book ####
table(airroi_pulito$instant_book,useNA="ifany")
airroi_pulito$instant_book<-NULL

#cleaning_fee ####
#costo fisso di pulizia aggiunto al prezzo per soggiorno
table(airroi_pulito$cleaning_fee,useNA = "ifany")
#c'è da capire se sono gia incluse sulla y(guarda airbnb)
airroi_filtrato$listing_id[airroi_filtrato$cleaning_fee==1]
airbnb_pulito$listing_url[airbnb_pulito$id==511064]
airbnb_pulito$price[airbnb_pulito$id==83610]


#io toglierei perche camere con prezzi alti avranno anche pulizie costose quindi ci sarà correlazione non causale
#tuttavia sarebbe interessante capire se alcuni airbnb alzano il prezzo delle pulizie per abbassare il prezzo y 
#e capire se questa strategia possa funzionare anche per il nostro airbnb
#Quindi io la toglierei dal modello di regressione ma farei uno studio su questa variabile per il nostro business
#Perche la spesa di pulizia influenza la durata del soggiorno(se vogliamo puntare a pernottamenti lunghi ad esempio sarebbe da tenere alta)

# Analisi di Marketing: Spesa di pulizia mediana per ogni tipologia di camera
# (Usiamo la mediana così gli outlier >2600€ non sballano il dato)
#se per 2 camere la spesa mediana è 60eur , potrei mettere il mio prezzo pulizie a 0 euro per intercettare i turisti mordi e fuggi
#oppure se per qualche motivo il target sono i pernottamenti lunghi, posso alzare le spese di pulizia e non dovrebbe influenzare troppo la scelta di acquisto

airroi_pulito$cleaning_fee<-NULL

#extraguestfee ##: tassa da pagare per aggiunta di ospiti
table(airroi_pulito$extra_guest_fee,useNA="ifany")
#escludo a causa di poca variabilità, sono quasi tutti 0: per il marketing è da tenere presente che chi applica costi extra è una minoranza
airroi_pulito$extra_guest_fee<-NULL

#singlefee_structure? ma che è bo togliamo 
table(airroi_pulito$single_fee_structure)
airroi_pulito$single_fee_structure<-NULL
airroi_pulito$currency<-NULL

#6. RECENSIONI ----------------------------------
#guest favorite sono i migliori 10% degli alloggi di airbnb 
#gli NA sono dei no
badge_vettore <- ifelse(
  is.na(airroi_pulito$guest_favorite), 
  "No_Badge", 
  "Guest_Favorite"
)
airroi_pulito$guest_favorite <- factor(
  badge_vettore, 
  levels = c("No_Badge", "Guest_Favorite")
)
table(airroi_pulito$guest_favorite)

#REVIEWS ########
#proviamo a tenere le reviews parziali in un dataset a parte
airroi_pulito_recensioniparziali<-airroi_pulito
airroi_pulito_recensioniparziali$rating_overall<-NULL


# Esplorative sulle misure parziali ---------------------------------------
airroi_pulito_recensioniparziali %>%
  # 1. Seleziona le variabili di interesse
  select(starts_with("rating")) %>%
  
  # 2. Trasforma il dataset da "largo" a "lungo"
  pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") %>%
  
  # 3. Crea il grafico
  ggplot(aes(x = valore)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  
  # 4. Questa riga crea un istogramma separato per ogni variabile, mettendoli in griglia
  facet_wrap(~ variabile, scales = "free") + 
  
  theme_minimal() +
  labs(title = "Istogrammi delle variabili di Rating", x = "Punteggio", y = "Frequenza")

#Nel dataset principale tolgo le misure parziali perche ho pochi dati e multicollinearità 
airroi_pulito$rating_accuracy<-airroi_pulito$rating_checkin<-airroi_pulito$rating_cleanliness<-airroi_pulito$rating_communication<-airroi_pulito$rating_location<-airroi_pulito$rating_value<-NULL


#7. KPI ANNUALI -----------------------
#metriche di performance

#VARIABILE RISPOSTA: tariffa media giornaliera negli ultimi 12 mesi
summary(airroi_pulito$ttm_avg_rate)

#teniamo la misura in dollari quindi cancelliamo tutte le variabili native
#VALUTIAMO QUALE DELLE DUE MISURE E' L'EURO
airroi_filtrato$listing_id[airroi_filtrato$ttm_avg_rate_native==34.2]
airbnb_pulito$price[airbnb_pulito$id==344046]

summary(airroi_pulito$ttm_avg_rate) #dollari (media annua)
summary(airroi_pulito$ttm_avg_rate_native) #euro
summary(airbnb_pulito$price_num[airbnb_pulito$airroi == T]) #dollari(settembre 15)
#-->native è euro
#CANCELLO I NATIVE
airroi_pulito$ttm_revenue_native<-airroi_pulito$ttm_avg_rate_native<-airroi_pulito$ttm_revpar_native<-airroi_pulito$ttm_adjusted_revpar_native<-NULL

#Le misure di ricavo e di occupazione non ci servono per stimare il prezzo
summary(airroi_pulito$ttm_revenue)
summary(airroi_pulito$ttm_occupancy) #giorni prenotati/totali
airroi_pulito$ttm_revenue<-airroi_pulito$ttm_occupancy<-airroi_pulito$ttm_adjusted_occupancy<-NULL
airroi_pulito$ttm_reserved_days<-airroi_pulito$ttm_blocked_days<-airroi_pulito$ttm_available_days<-airroi_pulito$ttm_total_days<-NULL
#Durata media del soggiorno nei 12 mesi
airroi_pulito$ttm_avg_length_of_stay<-NULL #non penso la possiamo usare per prevedere il prezzo???
#revpar = y* tasso di occupazione . per ora non ci interessa
airroi_pulito$ttm_revpar<-airroi_pulito$ttm_adjusted_revpar<-NULL

#ttm_avg_min_nights : media delle notti minime richieste nelle prenotazioni
table(airroi_pulito$ttm_avg_min_nights,useNA = "ifany")
#lascio numerica

###############################################################################
#controllo NA
dati.mancanti = function(dati){
  p = dim(dati)[2]
  tab = matrix(NA, p, 3)
  rownames(tab) = colnames(dati)
  
  for (i in 1:p){
    var = dati[, i]
    tab[i, 1] = length(unique(var))
    tab[i, 2] = sum(is.na(var))          # Più efficiente di length(which(...))
    tab[i, 3] = class(var)[1]            # RISOLUZIONE: Prende solo la prima classe
  }
  
  df = data.frame(Class = tab[, 3], 
                  Modalità_uniche = as.numeric(tab[, 1]), 
                  n_NA = as.numeric(tab[, 2]))
  
  # Ritorna solo le variabili con NA > 0
  return(df[df$n_NA > 0, ])
}
dati.mancanti(airroi_pulito) #0

View(airroi_pulito)
str(airroi_pulito)

#dataset con solo score recensioni totale
fit<-lm(ttm_avg_rate~.,data=airroi_pulito)
summary(fit)
#dataset con recensioni parziali
#fit_revpar<-lm(ttm_avg_rate~.,data=airroi_pulito_recensioniparziali)
#summary(fit_revpar)

save(airroi_pulito, file = "dati/airroi_pulito.Rdata")
