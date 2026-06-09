import time
import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager

print("Avvio del browser in corso...")
options = webdriver.ChromeOptions()
options.add_argument('--disable-blink-features=AutomationControlled')
options.add_argument("window-size=1200,1000")

driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

url = "https://www.booking.com/searchresults.it.html?ss=Venezia%2C+castello&ssne=Venezia&ssne_untouched=Venezia&highlighted_hotels=176923&label=metagha-link-LUIT-hotel-176923_dev-desktop_los-10_bw-89_dow-Wednesday_defdate-0_room-0_gstadt-2_rateid-public_aud-0_gacid-21404705919_mcid-50_bc-AAKzGw_ppa-1_clrid-0_ad-1_gstkid-0_checkin-20260902_ppt-B_lp-2380_r-8598807694474795632&aid=2419849&lang=it&sb=1&src_elem=sb&src=searchresults&dest_id=625&dest_type=district&ac_position=0&ac_click_type=b&ac_langcode=it&ac_suggestion_list_length=5&search_selected=true&search_pageview_id=448141197dad07b4&ac_meta=GhA0NDgxNDExOTdkYWQwN2I0IAAoATICaXQ6EVZlbmV6aWEsIGNhc3RlbGxv&checkin=2026-09-02&checkout=2026-09-12&group_adults=2&no_rooms=1&group_children=0#map_closed"

try:
    driver.get(url)
    print("Pagina caricata.")

    # 1. Rifiutare i Cookies
    try:
        cookie_btn = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable((By.ID, "onetrust-reject-all-handler"))
        )
        cookie_btn.click()
        print("Cookies rifiutati.")
    except Exception:
        pass

    # 2. Chiudere il pop-up di accesso
    try:
        close_login_btn = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable((By.XPATH, "//button[@aria-label='Ignora le informazioni sull\\'accesso.']"))
        )
        close_login_btn.click()
        print("Pop-up di accesso chiuso.")
    except Exception:
        pass

    # ==========================================
    # 3. LA PAUSA MANUALE (Human-in-the-loop)
    # ==========================================
    print("\n" + "="*50)
    print("🛑 PAUSA SCRIPT: ORA TOCCA A TE!")
    print("Vai sulla finestra del browser di Chrome appena aperta.")
    print("Scorri la pagina normalmente e clicca su 'Carica più risultati' finché non vedi la fine.")
    print("="*50 + "\n")
    
    # Lo script si ferma qui e aspetta che tu prema INVIO sulla tastiera
    input("👉 Quando hai caricato tutti gli hotel, torna qui e PREMI INVIO per avviare l'estrazione...")
    
    print("\nPerfetto! Procedo con l'estrazione di tutti gli annunci visibili...")

    # 4. Estrazione dei dati
    property_cards = driver.find_elements(By.CSS_SELECTOR, "[data-testid='property-card']")
    print(f"Sto estraendo i dati da {len(property_cards)} hotel trovati...")
    
    dati_estratti = []

    for card in property_cards:
        # Nome
        try:
            nome = card.find_element(By.CSS_SELECTOR, "[data-testid='title']").text
        except:
            nome = "N/A"
            
        # Prezzo
        try:
            prezzo = card.find_element(By.CSS_SELECTOR, "[data-testid='price-and-discounted-price']").text
            prezzo = prezzo.replace("€", "").replace("\xa0", "").strip()
        except:
            prezzo = "N/A"
            
        # Distanza
        try:
            distanza = card.find_element(By.CSS_SELECTOR, "[data-testid='distance']").text
        except:
            distanza = "N/A"
            
        # Rating
        try:
            rating = card.find_element(By.XPATH, ".//div[contains(@class, 'f63b14ab7a') or contains(@class, 'a3b8729ab1')]").text
        except:
            rating = "N/A"

        # Tipologia
        try:
            tipologia_camera = card.find_element(By.XPATH, ".//h4[@role='link']").text
        except:
            tipologia_camera = "N/A"

        # Salvataggio nel dizionario
        dati_estratti.append({
            "Nome_Hotel": nome,
            "Tipologia_Sistemazione": tipologia_camera,
            "Prezzo_Totale_10Notti": prezzo,
            "Rating": rating,
            "Distanza_Centro": distanza
        })

    # 5. Esportazione in CSV
    df = pd.DataFrame(dati_estratti)
    percorso_salvataggio = "../dati/prezzi_lido_mostra_castello.csv"
    
    df.to_csv(percorso_salvataggio, index=False, encoding='utf-8')
    print(f"\n✅ Estrazione completata con successo!")
    print(f"File salvato in: {percorso_salvataggio}")

finally:
    # Lascio un piccolo margine di 3 secondi per farti leggere il messaggio finale prima di chiudere
    time.sleep(3)
    driver.quit()