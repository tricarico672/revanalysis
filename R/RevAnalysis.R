
#' @title import
#' @author Anthony Tricarico
#' @description 
#' La funzione importa il file excel per l'analisi eseguendo automaticamente tutti i passaggi per preparare i dati alla corretta gestione nei passaggi successivi. Il file Excel che viene caricato deve provenire da TFSBank e deve essere anonimizzato rimuovendo la colonna "Descrizione" dal file dopo averlo aperto con Excel. Inoltre, il file non deve contenere righe bianche all'inizio, ma la prima riga deve essere occupata dai nomi dei vari campi (da non modificare!).
#' @details
#' è importante fornire il nome del file da importare in maniera corretta esattamente come appare nella cartella dove è stato salvato
#' @return
#' la funzione produce come output un oggetto data.frame di R da assegnare alla variabile import
#' @examples 
#' movimenti <- import()
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
#' La funzione pulisce il file excel precedentemente importato permettendo anche di gestire due "accordati" diversi in base alla data di operatività della linea. La funzione va eseguita dopo aver eseguito import().
#' @return
#' la funzione produce come output un oggetto data.frame di R da assegnare alla variabile movimenti_cleaned
#' @examples
#' movimenti_cleaned <- clean()
#' @export
clean <- function() {
  
   domanda_accordato <- readline("Il dealer ha avuto un incremento nel periodo? (SI/NO): ")
  
  if(toupper(domanda_accordato) == "SI") {
    inizio <- dmy(readline("inserire la data di operatività dell'attuale accordato (gg-mm-aaaa): "))
    
    movimenti_cleaned <-  movimenti %>%
      select(-divisa) %>%
      mutate(data_operazione = dmy(data_operazione),
             data_valuta = dmy(data_valuta),
             descrizione = factor(descrizione),
             saldo = round(rev(cumsum(rev(importo))),2),
             accordato = ifelse(data_operazione >= inizio, 
                                accordato,
                                as.numeric(readline(paste("inserire l'accordato precedente al", inizio, ":")))),
             utilizzo = ifelse(saldo < 0, abs(saldo/accordato), 0))
  } else {
    movimenti_cleaned <-  movimenti %>%
      select(-divisa) %>%
      mutate(data_operazione = dmy(data_operazione),
             data_valuta = dmy(data_valuta),
             descrizione = factor(descrizione),
             saldo = round(rev(cumsum(rev(importo))),2),
             utilizzo = ifelse(saldo < 0, abs(saldo/accordato), 0))
  }
  
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


#' @title classificazione_reintegri
#' @author Anthony Tricarico
#' @description 
#' La funzione produce come output un file Excel che riepiloga e categorizza i vari bonifici di reintegro effettuati dal dealer nell'arco di tempo specificato dal file importato originariamente
#' @return
#' La funzione produce come output primario un file Excel che categorizza i vari bonifici di reintegro effettuati dal dealer nell'arco di tempo specificato dal file importato originariamente
#' @examples 
#' classificazione_reintegri()
#' @export
classificazione_reintegri <- function() {
  reintegri_giornalieri <- movimenti_cleaned %>%
    group_by(data_operazione, descrizione) %>%
    summarize(somma = sum(importo)) %>%
    filter(descrizione == "Bonifico a Vs Favore")
  
  utilizzo_giornaliero <- movimenti_cleaned %>%
    filter(!is.na(data_operazione)) %>%
    mutate(time_diff = (data_operazione) - (lag(data_operazione))) %>%
    filter(time_diff <= -1 | is.na(time_diff)) %>%
    select(data_operazione, saldo, utilizzo, accordato)
  
  joined_df <- reintegri_giornalieri %>%
    right_join(utilizzo_giornaliero, by = "data_operazione") %>%
    arrange(desc(data_operazione)) %>%
    mutate(saldo_giorno_precedente = coalesce(lead(saldo), 0),
           categoria = if_else(lead(saldo, 0, default = 0) > 0, "bonifici effettuati in presenza di saldo attivo", "corretto")) 
  
  joined_df <- reintegri_giornalieri %>%
    right_join(utilizzo_giornaliero, by = "data_operazione") %>%
    arrange(desc(data_operazione))
  
  saldo_giorno_precedente <- lead(joined_df$saldo, default = 0)
  accordato_giorno_precedente <- lead(joined_df$accordato, default = 0)
  
  joined_df$saldo_precedente <- saldo_giorno_precedente
  joined_df$accordato_precedente <- accordato_giorno_precedente
  
  fatture_pagate <- movimenti_cleaned %>%
    filter(descrizione == "Pagamenti diversi") %>%
    group_by(data_operazione) %>%
    summarize(fatture_pagate = sum(importo)) %>%
    arrange(desc(data_operazione))
  
  anticipi_TMI <- movimenti_cleaned %>%
    filter(descrizione == "Giroconto Anticipazioni TMI") %>%
    group_by(data_operazione) %>%
    summarize(anticipazioni_TMI = sum(importo)) %>%
    arrange(desc(data_operazione))
  
  addebito_estinzioni <- movimenti_cleaned %>%
    filter(descrizione == "Addebito Estinz. Finanziamento") %>%
    group_by(data_operazione) %>%
    summarize(addebito_estinzioni = sum(importo)) %>%
    arrange(desc(data_operazione))
  
  liquidazioni_finanziamento <- movimenti_cleaned %>%
    filter(descrizione == "Accredito per liq.ne finan.to") %>%
    group_by(data_operazione) %>%
    summarize(liquidazioni_finanziamento = sum(importo)) %>%
    arrange(desc(data_operazione))
  
  domanda_attivo <- readline("vuoi caricare anche i dati relativi ad entrate e uscite (SI/NO): ")
  
  if (toupper(domanda_attivo) == "SI"){
    joined_df <- joined_df %>%
      mutate(categoria = ifelse(saldo_precedente > 0, "bonifici effettuati in presenza di saldo attivo", 
                                ifelse(abs(saldo_precedente) < (0.75 * accordato_precedente), "bonifici non necessari (utilizzo < 75% dell'accordato)", 
                                       ifelse(somma > accordato, "bonifici che superano il totale accordato", "corretto")))) %>%
      filter(descrizione == "Bonifico a Vs Favore") %>%
      select(-accordato_precedente) %>%
      full_join(fatture_pagate, by = "data_operazione") %>%
      full_join(anticipi_TMI, by = "data_operazione") %>%
      full_join(addebito_estinzioni, by = "data_operazione") %>%
      full_join(liquidazioni_finanziamento, by = "data_operazione") %>%
      arrange(desc(data_operazione))
  } else {
    joined_df <- joined_df %>%
      mutate(categoria = ifelse(saldo_precedente > 0, "bonifici effettuati in presenza di saldo attivo", 
                                ifelse(abs(saldo_precedente) < (0.75 * accordato_precedente), "bonifici non necessari (utilizzo < 75% dell'accordato)", 
                                       ifelse(somma > accordato, "bonifici che superano il totale accordato", "corretto")))) %>%
      filter(descrizione == "Bonifico a Vs Favore") %>%
      select(-accordato_precedente)
  }
  
  nome_file <- "classificazione_bonifici_di_reintegro.xlsx"
  
  write.xlsx(joined_df, nome_file)
  
  print(paste("File Excel generato col nome:", nome_file))
  
  domanda_grafico <- readline("Vuoi generare il grafico del dettaglio classificazione bonifici di reintegro? (SI/NO): ")
  
  grafico_classificazione <- if (toupper(domanda_grafico) == "SI"){
    joined_df2 <- joined_df %>%
      mutate(categoria = ifelse(categoria == "bonifici effettuati in presenza di saldo attivo", "SA", 
                                ifelse(categoria == "bonifici non necessari (utilizzo < 75% dell'accordato)", "B < 75%", 
                                       ifelse(categoria == "bonifici che superano il totale accordato", "BSA", "C")))) %>%
      filter(!is.na(categoria)) %>%
      arrange(categoria) %>%
      mutate(categoria = factor(categoria, 
                                levels = c("B < 75%", "BSA","C", "SA")))
    
      ggplot(joined_df2, aes(x = categoria, fill= categoria)) +
      geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = -.3) +
      scale_fill_discrete(labels = unique(subset(joined_df2$categoria, !is.na(joined_df2$categoria)))) +
      labs(y = "Conteggio Bonifici",
           title = "Classificazione Bonifici",
           caption = "SA: Effettuati in Presenza di Saldo Attivo al giorno precedente\n
           B < 75%: Effettuati con Utilizzo inferiore al 75% del giorno precedente\n
           BSA: Bonifici che superano il Totale Accordato\n
           C: Corretto") +
      theme_clean() +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.title = element_text(hjust = .5),
            legend.position = "top",
            legend.title = element_blank())
  }
  return(grafico_classificazione)
}
