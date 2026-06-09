import time
import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager

print("Avvio del browser in corso...")
options = webdriver.ChromeOptions()
options.add_argument('--disable-blink-features=AutomationControlled')
options.add_argument("window-size=1200,1000")

driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

# L'URL di Airbnb che mi hai fornito
url = "https://www.airbnb.it/s/lido-di-venezia/homes?refinement_paths%5B%5D=%2Fhomes&date_picker_type=calendar&checkin=2026-09-02&checkout=2026-09-12&adults=2&search_type=user_map_move&query=lido%20di%20venezia&place_id=ChIJF3b5UICvfkcRe7qaDLPi_MA&flexible_trip_lengths%5B%5D=one_week&monthly_start_date=2026-07-01&monthly_length=3&monthly_end_date=2026-10-01&search_mode=regular_search&price_filter_input_type=2&price_filter_num_nights=10&channel=EXPLORE&ne_lat=45.43841092992301&ne_lng=12.455322323113705&sw_lat=45.37003820112392&sw_lng=12.34172399669231&zoom=12.901886135001519&zoom_level=12.901886135001519&search_by_map=true"

dati_totali = []
pagina_corrente = 1

try:
    driver.get(url)
    print("Pagina caricata su Airbnb.")

    # Ciclo di estrazione pagina per pagina
    while True:
        print("\n" + "="*50)
        print(f"🛑 PAUSA SCRIPT: SEI SULLA PAGINA {pagina_corrente}")
        print("1. Chiudi eventuali pop-up dei cookies o traduzione.")
        print("2. Scorri la pagina per far caricare tutte le foto/prezzi.")
        print("="*50)
        
        # Input utente per gestire le pagine
        scelta = input("👉 Premi INVIO per estrarre questa pagina, oppure scrivi 'FINE' per salvare il CSV e uscire: ").strip().upper()
        
        if scelta == 'FINE':
            break
            
        print("Estrazione della pagina in corso...")
        time.sleep(1) # Breve pausa di sicurezza
        
        # Troviamo tutti i contenitori che hanno al loro interno un titolo (la vera card dell'annuncio)
        property_cards = driver.find_elements(By.XPATH, "//div[div[@data-testid='listing-card-title']]")
        print(f"Trovati {len(property_cards)} annunci su questa pagina.")

        for card in property_cards:
            intero_testo_card = card.text # Estraiamo tutto il testo per le ricerche booleane
            
            # Tipologia e Luogo (es. Minicase - Cavallino)
            try:
                tipologia_luogo = card.find_element(By.CSS_SELECTOR, "[data-testid='listing-card-title']").text
            except:
                tipologia_luogo = "N/A"
                
            # Nome / Breve descrizione
            try:
                nome_descrizione = card.find_element(By.CSS_SELECTOR, "[data-testid='listing-card-name']").text
            except:
                nome_descrizione = "N/A"
                
            # Dettagli (Camere, letti, bagni, Host privato). Airbnb usa più sottotitoli.
            try:
                # Troviamo tutti i sottotitoli e li uniamo con una barra |
                subtitles = card.find_elements(By.CSS_SELECTOR, "[data-testid='listing-card-subtitle']")
                dettagli = " | ".join([sub.text for sub in subtitles if sub.text != ""])
            except:
                dettagli = "N/A"
                
            # Prezzo Totale
            try:
                prezzo_row = card.find_element(By.CSS_SELECTOR, "[data-testid='price-availability-row']").text
                # Il prezzo spesso appare come "1372 € in totale". Lo estraiamo così com'è.
                prezzo_totale = prezzo_row.replace("\n", " ")
            except:
                prezzo_totale = "N/A"
                
            # Info accessorie (0€ oggi e Cancellazione) verificate dal testo globale della card
            paga_oggi = "Sì" if "Paga 0" in intero_testo_card else "No"
            cancellazione = "Sì" if "Cancellazione gratuita" in intero_testo_card else "No"
            
            # Link URL
            try:
                # Airbnb mette il link in un tag <a> che avvolge o precede la card. 
                # Cerchiamo il link più vicino salendo di un paio di livelli nel DOM
                link_element = card.find_element(By.XPATH, "./ancestor::div[position()<=3]//a[contains(@href, '/rooms/')]")
                url_annuncio = link_element.get_attribute("href")
                # Puliamo l'URL rimuovendo i lunghi parametri di tracciamento dopo il punto interrogativo
                url_annuncio = url_annuncio.split("?")[0]
            except:
                url_annuncio = "N/A"

            # Salviamo l'annuncio
            dati_totali.append({
                "Tipologia_Luogo": tipologia_luogo,
                "Nome_Descrizione": nome_descrizione,
                "Dettagli_Struttura": dettagli,
                "Prezzo_Totale": prezzo_totale,
                "Paga_Zero_Oggi": paga_oggi,
                "Cancellazione_Gratuita": cancellazione,
                "URL_Annuncio": url_annuncio
            })

        print(f"✅ Pagina {pagina_corrente} estratta con successo! ({len(dati_totali)} annunci totali in memoria).")
        print("⏳ Ora clicca su 'Successivo' (o pagina successiva) su Airbnb.")
        pagina_corrente += 1

    # ==========================================
    # SALVATAGGIO FILE CSV ALL'USCITA
    # ==========================================
    if len(dati_totali) > 0:
        df = pd.DataFrame(dati_totali)
        percorso_salvataggio = "../dati/prezzi_airbnb_lido.csv"
        
        df.to_csv(percorso_salvataggio, index=False, encoding='utf-8')
        print("\n" + "*"*50)
        print(f"🎉 ESTRAZIONE COMPLETATA!")
        print(f"Hai salvato {len(dati_totali)} annunci di Airbnb.")
        print(f"File disponibile in: {percorso_salvataggio}")
        print("*"*50)
    else:
        print("Nessun dato estratto, nessun file salvato.")

finally:
    driver.quit()