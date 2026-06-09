# Progetto SSADA – Analisi degli affitti brevi Airbnb a Venezia

Progetto realizzato per il corso **Strumenti per l'Analisi dei Dati Aziendali (SSADA)** – Università degli Studi di Padova, A.A. 2025/2026.

L'obiettivo è analizzare il mercato degli affitti brevi Airbnb nella città di Venezia attraverso analisi descrittive, modellazione predittiva del prezzo, analisi di diffusione temporale, regole associative sulle amenities e visualizzazioni geospaziali.

---

## Struttura del progetto

```
Progetto-SSADA-Airbnb/
├── Analisi/
│   ├── Modellazione.Rmd              # Modellazione predittiva (GBM, RF, GAM, MERT, Ensemble)
│   ├── Descrittive.Rmd               # Analisi descrittive degli annunci
│   ├── Diffusione.Rmd                # Modelli di diffusione applicati agli host
│   ├── RegoleAssociative.Rmd         # Market basket analysis delle amenities
│   ├── mappe.Rmd                     # Mappe interattive Leaflet
│   ├── Mappe2.R                      # Mappe geospaziali 
│   ├── Pulizia.R                     # Pulizia e preprocessing del dataset Airbnb principale
│   ├── pulizia_airroi.R              # Preprocessing del dataset AirROI
│   ├── utils.R                       # Funzione MERT (Mixed Effects Regression Trees)
│   ├── scraping_airbnb.py            # Web scraping prezzi Airbnb – Lido di Venezia
│   └── output_recensioni/            # Output analisi testuale delle recensioni
├── Modelli/                          # Cache e modelli addestrati (.rds)
│   ├── modello_gam_finale.rds
│   ├── cache_gbm_base_cv_v1_def.rds
│   ├── cache_gbm_mostra_v7.rds
│   ├── cache_metboost_tuning.rds
│   ├── cache_pesi_ensemble_v6.rds
│   └── ncv_oof_backup.rds
│   # I modelli pesanti (>50 MB) sono esclusi dal repository e vengono
│   # ricalcolati automaticamente al primo run di Modellazione.Rmd
├── dati/
│   ├── listings.csv / listings.parquet   # Inside Airbnb – Venezia (set. 2024)
│   ├── airbnb_pulito.Rdata               # Dataset Airbnb preprocessato (input principale)
│   ├── airroi_pulito.Rdata               # Dataset AirROI preprocessato
│   ├── airroi_pulito_merge.rds           # Merge AirROI × Airbnb
│   ├── Future_calander_rates.parquet     # Tariffe future AirROI
│   ├── Past_calander_rates.parquet       # Tariffe storiche AirROI
│   ├── palinsesto_venezia_georeferenziato.csv  # Eventi con coordinate
│   ├── hotel.geojson / neighbourhoods.geojson  # Dati geografici
│   ├── prezzi_airbnb_lido*.csv           # Prezzi scraping Airbnb (Lido)
│   ├── prezzi_lido_mostra*.csv           # Prezzi scraping Booking (Mostra del Cinema)
│   ├── Strato09_Ambiti_Amministrativi/   # Shapefile confini amministrativi
│   ├── Att_ricettive.xlsx / Attivi.csv   # Capacità ricettiva comunale
│   ├── Capacità comunale 2002-2012.xlsx  # Serie storica capacità ricettiva
│   ├── Capacità comunale 2013-2024.xlsx
│   └── Inside Airbnb Data Dictionary.xlsx
│   # reviews.csv (~270 MB) è escluso dal repository (vedi sezione Dati)
└── Progetto-SSADA-Airbnb.Rproj
```

---

## Dati

### Dataset inclusi nel repository

I dataset principali sono già presenti in `dati/`:

| File | Descrizione |
|------|-------------|
| `listings.csv` | Snapshot Inside Airbnb di Venezia – settembre 2024 (5 811 annunci) |
| `listings.parquet` | Versione Parquet di `listings.csv` |
| `airbnb_pulito.Rdata` | Dataset Airbnb preprocessato – input diretto delle analisi |
| `airroi_pulito.Rdata` | Dataset AirROI preprocessato (250 annunci con dati di revenue) |
| `airroi_pulito_merge.rds` | Merge tra AirROI e Inside Airbnb |

### Dataset da scaricare separatamente

**`dati/reviews.csv`** (~270 MB) supera il limite GitHub e va scaricato manualmente:

1. Accedere a [Inside Airbnb – Get the Data](http://insideairbnb.com/get-the-data/)
2. Selezionare la città **Venice, Veneto, Italy**
3. Scaricare `reviews.csv` dallo snapshot di settembre 2024
4. Salvarlo in `dati/reviews.csv`

> Questo file è necessario solo per l'ultima sezione di `Modellazione.Rmd` (analisi testuale delle recensioni tramite `data.table::fread`).

---

## Pacchetti R richiesti

```r
install.packages(c(
  # Tidyverse
  "tidyverse", "dplyr", "ggplot2", "scales", "lubridate",
  "stringr", "forcats", "readr", "here",
  # Dati
  "arrow", "data.table",
  # Modellazione
  "gbm", "randomForestSRC", "lme4", "lmerTest", "emmeans",
  "mgcv", "rpart", "partykit", "flexclust", "geosphere",
  # Geospaziale
  "sf", "ggspatial", "viridis", "leaflet",
  # Serie storiche
  "forecast", "prophet",
  # Analisi testuale (Pulizia.R)
  "tidytext", "textstem", "quanteda", "ggwordcloud",
  # Regole associative
  "arules", "arulesViz",
  # Output
  "kableExtra", "gridExtra"
))
```

Il pacchetto `DIMORA` (usato in `Diffusione.Rmd`) potrebbe non essere su CRAN:

```r
# Prima prova CRAN
install.packages("DIMORA")

# Se non disponibile, installa da GitHub
remotes::install_github("stantinov/DIMORA")
```

---

## Come riprodurre le analisi

### 1. Aprire il progetto

Aprire `Progetto-SSADA-Airbnb.Rproj` in RStudio: il working directory viene impostato automaticamente alla root del progetto.

### 2. Eseguire le analisi

Aprire i file `.Rmd` dalla cartella `Analisi/` e cliccare **Knit**:

| File | Output | Note |
|------|--------|------|
| `Descrittive.Rmd` | HTML | Statistiche descrittive, distribuzione prezzi, host |
| `Diffusione.Rmd` | PDF | Crescita host nel tempo con modelli Bass/DIMORA/Prophet |
| `RegoleAssociative.Rmd` | HTML | Regole associative tra amenities (arules) |
| `mappe.Rmd` | HTML | Mappe interattive Leaflet degli annunci |
| `Modellazione.Rmd` | PDF | Modellazione predittiva del prezzo notturno |

> **`Modellazione.Rmd`** è computazionalmente intensivo. I modelli vengono **salvati automaticamente** in `Modelli/` al primo run e ricaricati nelle esecuzioni successive, azzerando i tempi di calcolo.

### 3. Script di supporto (dalla root del progetto)

```r
# Passo 1 – ricrea dati/airbnb_pulito.Rdata a partire da dati/airbnb.Rdata
source("Analisi/Pulizia.R")

# Passo 2 – ricrea dati/airroi_pulito.Rdata a partire da dati/listings.parquet
source("Analisi/pulizia_airroi.R")

# Genera le mappe geospaziali avanzate
source("Analisi/Mappe2.R")
```

---

## Web scraping (Python)

Lo script `Analisi/scraping_airbnb.py` raccoglie prezzi da Airbnb (Lido di Venezia) tramite Selenium con interazione manuale.

**Requisiti:**

```bash
pip install selenium pandas webdriver-manager
```

I dati prodotti dallo scraper sono già inclusi in `dati/` (`prezzi_airbnb_lido.csv`), quindi non è necessario rieseguirlo.

---

## Note tecniche

- I percorsi dei file nei `.Rmd` sono **relativi alla directory `Analisi/`** (comportamento standard di RStudio Knit). Gli script `.R` usano percorsi relativi alla **root del progetto** (comportamento standard con `.Rproj`).
- I modelli pesanti (`modello_merf_finale_rfsrc.rds`, `modello_rf_finale_rfsrc.rds`, `modello_metboost_finale_na_def1.rds`) non sono inclusi nel repository per limiti di dimensione. Vengono rigenerati automaticamente alla prima esecuzione di `Modellazione.Rmd`.
- La cache OSM (`Analisi/rosm.cache/`) viene creata automaticamente da `ggspatial`/`rosm` al primo run delle mappe.
