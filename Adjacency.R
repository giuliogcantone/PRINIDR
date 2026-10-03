adj_matrix <- db2 %>%
  filter(!Org |> is.na()) |>
  mutate(n_org = n(),
         .by = Org) |>
  filter(n_org > 4) -> dati0

crea_matrice_contingenza_org <- function(dati) {
  # Passo 1: Identificare tutte le organizzazioni uniche e ordinarle alfabeticamente
  tutte_org <- sort(unique(dati$Org))
  n_org <- length(tutte_org)
  
  # Passo 2: Creare una matrice vuota con nomi ordinati alfabeticamente
  matrice_contingenza <- matrix(0, nrow = n_org, ncol = n_org)
  rownames(matrice_contingenza) <- tutte_org
  colnames(matrice_contingenza) <- tutte_org
  
  # Passo 3: Raggruppare i dati per Code
  dati_raggruppati <- split(dati$Org, dati$Code)
  
  # Passo 4: Per ogni codice, incrementare il contatore per ogni coppia di organizzazioni
  for (org_gruppo in dati_raggruppati) {
    if (length(org_gruppo) >= 2) {
      # Consideriamo solo i codici con almeno 2 organizzazioni
      for (i in 1:(length(org_gruppo)-1)) {
        for (j in (i+1):length(org_gruppo)) {
          org_i <- org_gruppo[i]
          org_j <- org_gruppo[j]
          
          # Incrementa in entrambe le direzioni (matrice simmetrica)
          matrice_contingenza[org_i, org_j] <- matrice_contingenza[org_i, org_j] + 1
          matrice_contingenza[org_j, org_i] <- matrice_contingenza[org_j, org_i] + 1
        }
      }
    }
  }
  
  return(matrice_contingenza)
}

crea_matrice_contingenza_org(dati0) |>
  as.tibble(rownames = "Org") |> View()
  writexl::write_xlsx("org_adjacency.xlsx")
  
googledrive::drive_upload(
  "org_adjacency.xlsx",
  path = as_id("1CuQwQB49loVaRKnootfTVCV8c8ZUwCzF")
)
