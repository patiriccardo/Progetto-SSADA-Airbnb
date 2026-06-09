# ==========================================
# LIBRERIE NECESSARIE
# ==========================================
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial) # Per la mappa di sfondo in stile mappa reale
library(viridis)   # Per la scala colori elegante
library(leaflet)
library(readr)
library(arrow)     # Per leggere i file parquet

# ==========================================
# CARICAMENTO DATI E MERGE AIRROI
# ==========================================

# 1. Eventi
eventi <- read_csv("dati/palinsesto_venezia_georeferenziato.csv")

eventi_mappa <- eventi %>%
  filter(`Luogo Principale` != "Venezia") %>%
  filter(!is.na(Latitudine) & !is.na(Longitudine))

# 2. Airbnb (Main Dataset) e Airroi (Parquet)
airbnb <- read_csv("dati/listings.csv")
airroi <- read_parquet("dati/listings.parquet")

# Aggiungiamo il flag 'airroi' al dataset principale
airbnb$airroi <- airbnb$id %in% airroi$listing_id

# 3. Hotel
hotel_sf <- st_read("dati/hotel.geojson") %>% st_transform(4326)


# ==========================================
# PREPARAZIONE DATI E PULIZIA
# ==========================================

# Definiamo i quartieri lagunari (include Venezia, Lido, Murano, Burano, ecc.)
quartieri_lagunari <- c(
  "San Marco", "San Polo", "Santa Croce", "Dorsoduro", "Cannaregio", "Castello", 
  "Sant'Elena", "Tronchetto", "Giudecca", "Sacca Fisola", "Isola San Giorgio",
  "Lido", "Alberoni", "Malamocco", "Pellestrina", "San Pietro in Volta",
  "Murano", "Burano", "Torcello", "Mazzorbo", "Mazzorbetto", 
  "Sant'Erasmo", "Vignole"
)

# Filtriamo per isole e puliamo i prezzi
airbnb_isole <- airbnb %>%
  filter(neighbourhood_cleansed %in% quartieri_lagunari) %>%
  mutate(
    price_num = as.numeric(gsub("[\\$,]", "", price)),
    affitto_annuale_stimato = price_num * 365 * 0.5 
  ) %>%
  filter(price_num < 600)

cat("Alloggi insulari filtrati:", nrow(airbnb_isole), "\n")


# ==========================================
# MAPPA INTERATTIVA GENERALE
# ==========================================

icona_evento <- awesomeIcons(
  icon = 'star',
  iconColor = 'white',
  library = 'ion',
  markerColor = 'red'
)

mappa_venezia <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # LIVELLO 1: Tutti gli Airbnb
  addCircleMarkers(
    data = airbnb_isole,
    lat = ~latitude,
    lng = ~longitude,
    radius = 3, color = "#2980b9", stroke = FALSE, fillOpacity = 0.4,
    popup = ~paste("<b>", name, "</b><br>Quartiere: ", neighbourhood, "<br>Prezzo: $", price)
  ) %>%
  
  # LIVELLO 2: Eventi
  addCircleMarkers(
    data = eventi_mappa,
    lat = ~Latitudine,
    lng = ~Longitudine,
    radius = 6, color = "darkred", weight = 2, fillColor = "red", fillOpacity = 1,
    popup = ~paste("<b>", Evento, "</b><br><i>", `Spazio Specifico`, "</i>")
  )

print(mappa_venezia)


# ==========================================
# CONVERSIONE IN FORMATO SPAZIALE (SF) E BOUNDING BOXES
# ==========================================

airbnb_sf <- st_as_sf(airbnb_isole, coords = c("longitude", "latitude"), crs = 4326)
eventi_sf <- st_as_sf(eventi_mappa, coords = c("Longitudine", "Latitudine"), crs = 4326)

box_est <- st_bbox(c(xmin = 12.330, ymin = 45.425, xmax = 12.370, ymax = 45.440), crs = st_crs(4326))
box_ovest <- st_bbox(c(xmin = 12.310, ymin = 45.425, xmax = 12.340, ymax = 45.445), crs = st_crs(4326))
box_lido_nord <- st_bbox(c(xmin = 12.350, ymin = 45.400, xmax = 12.390, ymax = 45.425), crs = st_crs(4326))
box_lido_sud <- st_bbox(c(xmin = 12.300, ymin = 45.320, xmax = 12.350, ymax = 45.380), crs = st_crs(4326))

# Ritaglio Dati (Airbnb Totali)
airbnb_est <- st_crop(airbnb_sf, box_est)
airbnb_ovest <- st_crop(airbnb_sf, box_ovest)
airbnb_lido_nord <- st_crop(airbnb_sf, box_lido_nord)
airbnb_lido_sud <- st_crop(airbnb_sf, box_lido_sud)

# Ritaglio Dati (Eventi)
eventi_est <- st_crop(eventi_sf, box_est)
eventi_ovest <- st_crop(eventi_sf, box_ovest)
eventi_lido_nord <- st_crop(eventi_sf, box_lido_nord)
eventi_lido_sud <- st_crop(eventi_sf, box_lido_sud)


# ==========================================
# MAPPE PER ZONE: AIRBNB TOTALI + EVENTI
# ==========================================

# -- EST --
mappa_est <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_est, aes(color = price_num), size = 1.8, alpha = 0.7) +
  geom_sf(data = eventi_est, fill = "lightblue", color = "white", shape = 21, size = 6, stroke = 1.2, alpha = 0.85) +
  scale_color_distiller(palette = "YlOrRd", direction = 1, name = "Prezzo a Notte ($)") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Venezia Est: San Marco, Castello e Sedi Storiche Biennale")

# -- OVEST --
mappa_ovest <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_ovest, aes(color = price_num), size = 1.8, alpha = 0.7) +
  geom_sf(data = eventi_ovest, fill = "lightblue", color = "white", shape = 21, size = 6, stroke = 1.2, alpha = 0.85) +
  scale_color_distiller(palette = "YlOrRd", direction = 1, name = "Prezzo a Notte ($)") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Venezia Ovest: Dorsoduro, San Polo e Santa Croce")

# -- LIDO NORD --
mappa_lido_nord <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_lido_nord, aes(color = price_num), size = 1.8, alpha = 0.7) +
  geom_sf(data = eventi_lido_nord, fill = "lightblue", color = "white", shape = 21, size = 6, stroke = 1.2, alpha = 0.85) +
  scale_color_distiller(palette = "YlOrRd", direction = 1, name = "Prezzo a Notte ($)") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Lido Nord: Area Mostra Internazionale d'Arte Cinematografica")

# -- LIDO SUD --
mappa_lido_sud <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_lido_sud, aes(color = price_num), size = 1.8, alpha = 0.7) +
  geom_sf(data = eventi_lido_sud, fill = "lightblue", color = "white", shape = 21, size = 6, stroke = 1.2, alpha = 0.85) +
  scale_color_distiller(palette = "YlOrRd", direction = 1, name = "Prezzo a Notte ($)") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Lido Sud e Pellestrina: Aree Turistiche Periferiche")

print(mappa_est)
print(mappa_ovest)
print(mappa_lido_nord)
print(mappa_lido_sud)


# ==========================================
# MAPPE HOTEL
# ==========================================

hotel_est <- st_crop(hotel_sf, box_est)

mappa_est_solo_hotel <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = hotel_est, color = "#2c3e50", shape = 15, size = 3) +
  geom_sf(data = eventi_est, fill = "lightblue", color = "black", shape = 21, size = 6, stroke = 1) +
  geom_sf_text(data = hotel_est %>% filter(!is.na(name)), aes(label = name), 
               size = 2, vjust = 1.5, color = "black", check_overlap = TRUE) +
  theme_void() +
  labs(title = "Distribuzione Hotel e Punti di Riferimento: Venezia Est")

print(mappa_est_solo_hotel)


# ==========================================
# MAPPE PER ZONE: SOLO ALLOGGI 'AIRROI'
# ==========================================
# Estraiamo per ogni zona solo gli alloggi in cui la colonna airroi è TRUE

airbnb_est_airroi <- airbnb_est %>% filter(airroi == TRUE)
airbnb_ovest_airroi <- airbnb_ovest %>% filter(airroi == TRUE)
airbnb_lido_nord_airroi <- airbnb_lido_nord %>% filter(airroi == TRUE)
airbnb_lido_sud_airroi <- airbnb_lido_sud %>% filter(airroi == TRUE)

# -- EST (Airroi) --
mappa_est_airroi <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_est_airroi, color = "#d35400", size = 1.8, alpha = 0.8) +
  theme_void() +
  labs(title = "Appartamenti 'airroi' - Venezia Est",
       subtitle = paste("Totale alloggi visualizzati:", nrow(airbnb_est_airroi))) +
  theme(plot.title = element_text(face = "bold", size = 14))

# -- OVEST (Airroi) --
mappa_ovest_airroi <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_ovest_airroi, color = "#d35400", size = 1.8, alpha = 0.8) +
  theme_void() +
  labs(title = "Appartamenti 'airroi' - Venezia Ovest",
       subtitle = paste("Totale alloggi visualizzati:", nrow(airbnb_ovest_airroi))) +
  theme(plot.title = element_text(face = "bold", size = 14))

# -- LIDO NORD (Airroi) --
mappa_lido_nord_airroi <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_lido_nord_airroi, color = "#d35400", size = 1.8, alpha = 0.8) +
  theme_void() +
  labs(title = "Appartamenti 'airroi' - Lido Nord",
       subtitle = paste("Totale alloggi visualizzati:", nrow(airbnb_lido_nord_airroi))) +
  theme(plot.title = element_text(face = "bold", size = 14))

# -- LIDO SUD (Airroi) --
mappa_lido_sud_airroi <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_lido_sud_airroi, color = "#d35400", size = 1.8, alpha = 0.8) +
  theme_void() +
  labs(title = "Appartamenti 'airroi' - Lido Sud e Pellestrina",
       subtitle = paste("Totale alloggi visualizzati:", nrow(airbnb_lido_sud_airroi))) +
  theme(plot.title = element_text(face = "bold", size = 14))

# Visualizziamo le mappe 'airroi'
print(mappa_est_airroi)
print(mappa_ovest_airroi)
print(mappa_lido_nord_airroi)
print(mappa_lido_sud_airroi)


#############
#MAPPA UNICA GIGANTE AIRBNB
# ============================================
# MAPPA UNICA: 4 ZONE IN UN'IMMAGINE, LEGENDA PREZZO CONDIVISA
# ============================================
library(dplyr)
# Richiede `scala_prezzo` già definita (scale_color_distiller con limits = c(0,600), oob = squish)
scala_prezzo <- scale_color_distiller(
  palette = "YlOrRd", direction = 1,
  name    = "Prezzo a notte ($)",
  limits  = c(0, 600),
  oob     = scales::squish      # i valori > 600 vengono "schiacciati" sul colore di punta
)
tema_trasparente <- theme(
  plot.background   = element_rect(fill = "transparent", color = NA),
  panel.background  = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA),
  legend.key        = element_rect(fill = "transparent", color = NA)
)

# ============================================================
# MAPPA 1 — VENEZIA CENTRO (sinistra della slide), SENZA legenda
# ============================================================
centro <- c("San Marco", "Castello", "Dorsoduro", "San Polo", "Santa Croce",
            "Cannaregio", "Giudecca", "Sant'Elena", "Isola San Giorgio")
airbnb_centro <- airbnb_sf %>% filter(neighbourhood_cleansed %in% centro)

m_centro <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_centro, aes(color = price_num), size = 1.5, alpha = 0.75) +
  scala_prezzo +
  theme_void(base_size = 12) +
  theme(legend.position = "none")+ tema_trasparente          # niente legenda qui

print(m_centro)
ggsave("grafici/mappa_centro.png", m_centro, width = 8, height = 8, dpi = 300, bg = "white")

# ============================================================
# MAPPA 2 — LIDO nord + sud UNITI (destra della slide), CON la legenda a destra
# ============================================================
lido <- c("Lido", "Alberoni", "Malamocco", "Pellestrina", "San Pietro in Volta")
airbnb_lido <- airbnb_sf %>% filter(neighbourhood_cleansed %in% lido)


# Codice precedente (m_lido) con modifiche a legenda e scritte
m_lido <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_lido, aes(color = price_num), size = 1.5, alpha = 0.75) +
  scala_prezzo +
  theme_void(base_size = 12) +
  theme(
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm"),   # Lunghezza modificata a 4 cm
    legend.key.width  = unit(0.9, "cm"), 
    legend.title      = element_text(size = 15, face = "bold", color = "white"),
    legend.text       = element_text(size = 13, color = "white")
  ) +
  tema_trasparente
print(m_lido)

ggsave("grafici/mappa_lido.png", m_lido, width = 6, height = 9, dpi = 300, bg = "transparent")


plot(m_centro)
#ggsave("grafici/mappa_centro.png", m_centro, width = 8, height = 8, dpi = 300, bg = "transparent")
ggsave("grafici/mappa_lido.png",   m_lido,   width = 6, height = 9, dpi = 300, bg = "transparent")

##################################################

library(dplyr); library(sf); library(ggplot2)
# Richiede `scala_prezzo` e `tema_trasparente` già definiti

# Solo gli annunci presenti anche su AirROI (flag creato in Mappe2.R)
airbnb_airroi <- airbnb_sf %>% filter(airroi == TRUE)
# se il flag non c'è/ha altro nome, in alternativa:
# airbnb_airroi <- airbnb_sf %>% filter(id %in% airroi_pulito$listing_id)

centro <- c("San Marco","Castello","Dorsoduro","San Polo","Santa Croce",
            "Cannaregio","Giudecca","Sant'Elena","Isola San Giorgio")
lido   <- c("Lido","Alberoni","Malamocco","Pellestrina","San Pietro in Volta")

airbnb_centro <- airbnb_airroi %>% filter(neighbourhood_cleansed %in% centro)
airbnb_lido   <- airbnb_airroi %>% filter(neighbourhood_cleansed %in% lido)

# ---- CENTRO (sinistra), senza legenda ----
m_centro_airroi <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_centro, aes(color = price_num), size = 4, alpha = 0.75) +
  scala_prezzo +
  theme_void(base_size = 12) +
  theme(legend.position = "none") +
  tema_trasparente

# ---- LIDO nord+sud uniti (destra), con legenda ----
# Codice aggiornato per m_lido_airroi
m_lido_airroi <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = airbnb_lido, aes(color = price_num), size = 3, alpha = 0.85) +
  scala_prezzo +
  theme_void(base_size = 12) +
  theme(legend.position = "right",
        legend.key.height = unit(1.5, "cm"), 
        legend.key.width = unit(0.9, "cm"),
        legend.title = element_text(size = 15, face = "bold", color = "white"),
        legend.text  = element_text(size = 13, color = "white")) +
  tema_trasparente

print(m_lido_airroi)


print(m_centro_airroi); print(m_lido_airroi)
ggsave("grafici/airroi_centro.png", m_centro_airroi, width = 8, height = 8, dpi = 300, bg = "transparent")
ggsave("grafici/airroi_lido.png",   m_lido_airroi,   width = 6, height = 9, dpi = 300, bg = "transparent")

################################################################################
# ==========================================
# MAPPE HOTEL — VENEZIA CENTRO + LIDO
# Struttura identica alle mappe Airbnb sopra
# ==========================================

# Colori coerenti con Descrittive_grafici.R
COL_HOTEL <- "#1A2030"   # navy — tutti gli hotel
COL_ILC   <- "#F36F21"   # arancione — Il Canaletto (Cannaregio)
COL_LS    <- "#006241"   # verde — Lido Sands Resort

# Bounding box centro (unione di box_est + box_ovest, leggermente allargata)
box_centro_hotel <- st_bbox(
  c(xmin = 12.305, ymin = 45.418, xmax = 12.378, ymax = 45.455),
  crs = st_crs(4326)
)

# Bounding box lido (unione di box_lido_nord + box_lido_sud)
box_lido_hotel <- st_bbox(
  c(xmin = 12.285, ymin = 45.310, xmax = 12.410, ymax = 45.430),
  crs = st_crs(4326)
)
# Ritaglio hotel per zona — usando i box già definiti in Mappe2.R
hotel_centro_sf   <- st_crop(hotel_sf, box_centro_hotel)   # box nuovo, centro unificato

# Lido: unisco nord + sud con rbind, come si fa per airbnb_lido con il filter sui quartieri
hotel_lido_nord_sf <- st_crop(hotel_sf, box_lido_nord)     # box già esistente in Mappe2.R
hotel_lido_sud_sf  <- st_crop(hotel_sf, box_lido_sud)      # box già esistente in Mappe2.R
hotel_lido_sf      <- rbind(hotel_lido_nord_sf, hotel_lido_sud_sf)   # ← unione

# ---- MAPPA HOTEL CENTRO ----
m_hotel_centro <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = hotel_centro_sf, color = COL_HOTEL, fill = COL_HOTEL,
          shape = 15, size = 2.2, alpha = 0.85) +
  geom_sf_text(data = hotel_centro_sf %>% filter(!is.na(name)),
               aes(label = name),
               size = 1.7, vjust = -0.6, color = COL_HOTEL, check_overlap = TRUE) +
  theme_void(base_size = 12) +
  theme(legend.position = "none") +
  tema_trasparente

print(m_hotel_centro)
ggsave("grafici/hotel_centro.png", m_hotel_centro,
       width = 8, height = 8, dpi = 300, bg = "white")

# ---- MAPPA HOTEL LIDO (nord + sud uniti in verticale) ----
m_hotel_lido <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = hotel_lido_sf, color = COL_HOTEL, fill = COL_HOTEL,
          shape = 15, size = 2.2, alpha = 0.85) +
  geom_sf_text(data = hotel_lido_sf %>% filter(!is.na(name)),
               aes(label = name),
               size = 1.7, vjust = -0.6, color = COL_HOTEL, check_overlap = TRUE) +
  theme_void(base_size = 12) +
  theme(legend.position = "none") +
  tema_trasparente

print(m_hotel_lido)
ggsave("grafici/hotel_lido.png", m_hotel_lido,
       width = 6, height = 9, dpi = 300, bg = "white")