
#' @title Import
#' @author Anthony Tricarico
#' @description 
#' La funzione importa il file excel per l'analisi eseguendo automaticamente tutti i passaggi per preparare i dati alla corretta gestione nei passaggi successivi
#' @details
#' è importante fornire il nome del file da importare in maniera corretta esattamente come appare nella cartella dove è stato salvato
#' @return
#' la funzione produce come output un oggetto data.frame di R da assegnare alla variabile import
#' @examples 
#' import <- import()
#' @export
import <- function() {

  nome_file_import <- readline("inserire nome del file caricato: ")
  movimenti <- read_excel(nome_file_import)
  names(movimenti) <- c("data_operazione", "data_valuta", "descrizione", "importo", "divisa")
  movimenti$accordato <- as.numeric(readline("inserire accordato alla data dell'analisi: "))
  saldo_precedente <-  as.numeric(readline("inserire il saldo al giorno precedente rispetto alla prima data (più lontana) presente nella tabella: "))
  movimenti <- rbind(movimenti, c(NA, NA, NA, saldo_precedente, NA, NA))
  return(movimenti)
}

#' @title clean
#' @author Anthony Tricarico
#' @description 
#' La funzione pulisce il file excel precedentemente importato. La funzione va eseguita dopo aver eseguito import().
#' @return
#' la funzione produce come output un oggetto data.frame di R da assegnare alla variabile moviment_cleaned
#' @examples
#' movimenti_cleaned <- clean()
#' @export
clean <- function() {
  
  movimenti_cleaned <-  movimenti %>%
    select(-divisa) %>%
    mutate(data_operazione = dmy(data_operazione),
           data_valuta = dmy(data_valuta),
           descrizione = factor(descrizione),
           saldo = round(rev(cumsum(rev(importo))),2),
           utilizzo = ifelse(saldo < 0, abs(saldo/accordato), 0))
  
  return(movimenti_cleaned)
}

#' @title pagato_reintegri
#' @author Anthony Tricarico
#' @description 
#' La funzione analizza e contrasta l'ammontare delle fatture pagate con l'ammontare dei bonifici di reintegro nel periodo specificato
#' @return
#' La funzione produce come output un commento sulla gestione delle fatture pagate e dei reintegri effettuati evidenziando una corretta o un'errata gestione
#' @examples 
#' pagato_reintegri()
#' @export
pagato_reintegri <- function() {
  
  inizio <- dmy(readline("selezionare la data di inizio nel formato gg-mm-aaaa: "))
  fine <- dmy(readline("selezionare la data di fine nel formato gg-mm-aaaa: "))
  intervallo <- inizio %--% fine
  
  summary <- movimenti_cleaned %>%
    filter(data_valuta %within% intervallo) %>%
    group_by(descrizione) %>%
    summarize(somma = sum(importo))
  
  fatture_pagate <- summary %>%
    filter(descrizione %in% c("Pagamenti diversi")) %>%
    select(somma)
  
  reintregri_effettuati <- summary %>%
    filter(descrizione %in% c("Bonifico a Vs Favore")) %>%
    select(somma)
  
  differenza <- fatture_pagate + reintregri_effettuati
  
  if (differenza < 0){
    print("gestione corretta")
  } else {
    print(paste("i bonifici sono superiori di", abs(differenza), 
                "rispetto all'importo necessario per coprire le fatture pagate"))
  }
  
  print(paste("Nel periodo dal", inizio,"al", fine, "sono stati pagati", 
              abs(fatture_pagate),
              "€ di fatture su nuovo e sono stati effettuati bonifici di reintegro per",
              reintregri_effettuati, "€"))
}

#' @title utilizzo_daily
#' @author Anthony Tricarico
#' @description 
#' La funzione genera il saldo giornaliero dell'utilizzo della linea Revolving andando a stimare un utilizzo giornaliero progressivo della linea.
#' La funzione concede, inoltre, la possibilità di generare un grafico dell'utilizzo della linea su base giornaliera nell'intervallo di tempo specificato.
#' @return
#' La funzione produce come output primario un file Excel che permette di vedere il saldo giornaliero del dealer in oggetto.
#' @examples
#' utilizzo_daily()
#' @export
utilizzo_daily <- function() {
  
  utilizzo_giornaliero <- movimenti_cleaned %>%
    filter(!is.na(data_operazione)) %>%
    mutate(time_diff = (data_operazione) - (lag(data_operazione))) %>%
    filter(time_diff <= -1 | is.na(time_diff))
  
  write.xlsx(utilizzo_giornaliero, "utilizzo_giornaliero.xlsx")
  
  print(paste("file Excel generato e disponibile in ", getwd()))
  
  grafico_domanda <- readline("vuoi produrre il grafico dell'utilizzo? (SI/NO): ")
  
  if (toupper(grafico_domanda) == "SI") {
    
    inizio <- dmy(readline("selezionare la data di inizio nel formato gg-mm-aaaa: "))
    fine <- dmy(readline("selezionare la data di fine nel formato gg-mm-aaaa: "))
    intervallo <- inizio %--% fine
    
    grafico <- ggplot(subset(utilizzo_giornaliero, utilizzo_giornaliero$data_valuta %within% intervallo), 
                      aes(x = data_operazione, y = saldo)) +
      geom_line() +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Utilizzo Revolving") +
      xlab("Data") +
      ylab("Saldo") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),
            axis.text.y = element_text(angle = 0)) +
      scale_y_continuous(labels = function(x) {
        paste0("€", formatC(x, format="f", digits=0, big.mark=".", decimal.mark = ","))
      })
    
    return(grafico)
    
  }
  
}

#' @title valutazione_reintegri_saldo_attivo
#' @author Anthony Tricarico
#' @description 
#' La funzione produce come output un file Excel che permette di isolare i bonifici di reintegro che sono stati effettuati con saldo attivo
#' @return
#' La funzione produce come output primario un file Excel che permette di vedere quali bonifici di reintegro sono stati effettuati in presenza di saldo attivo
#' @examples 
#' valutazione_reintegri_saldo_attivo()
#' @export
valutazione_reintegri_saldo_attivo <- function() {
  
  #valutare se reintegro è stato effettuato quando il giorno precedente c'era saldo attivo
  
  #calcola il saldo a livello dell'ultima transazione giornaliera
  utilizzo_giornaliero <- movimenti_cleaned %>%
    filter(!is.na(data_operazione)) %>%
    mutate(time_diff = (data_operazione) - (lag(data_operazione))) %>%
    filter(time_diff <= -1 | is.na(time_diff))
  
  #calcola i reintegri che sono stati eseguiti in un certo periodo
  reintegri <- movimenti_cleaned %>%
    mutate(data_operazione_precedente = ifelse(wday(data_operazione) %in% 3:6, data_operazione - 1, data_operazione - 3),
           data_operazione_precedente = as.Date(data_operazione_precedente, origin = "1970-01-01")) %>%
    left_join(utilizzo_giornaliero, by = c("data_operazione_precedente" = "data_operazione")) %>%
    filter(descrizione.x == "Bonifico a Vs Favore" & saldo.y > 0)
  
  write.xlsx(reintegri, "reintegri_saldo_attivo.xlsx")
  
  return(reintegri)
}
