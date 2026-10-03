db2 |>
  filter(
    is.na(sub) | sub != "sub"
  ) |>
  summarise(
    Total = sum(Total),
    .by = c(Code,Org_trim,ERC,Issue)
  ) |>
  mutate(ERC = ERC |> str_sub(1,2)) |>
  writexl::write_xlsx("db_importi.xlsx")

readxl::read_xlsx(
  "db_importi.xlsx"
) -> db_prin

googledrive::drive_upload(
  "db_importi.xlsx",
  path = as_id("1CuQwQB49loVaRKnootfTVCV8c8ZUwCzF"),
  overwrite = TRUE
)

db_prin |>
  mutate(
    Issue2 = if_else(str_detect(Issue, "PNRR"),
                     "PNRR", "PRIN")
  ) |>
  summarise(
    n = n(),
    .by = c(Org_trim,Issue2)
  ) |>
  pivot_wider(names_from = Issue2,
              values_from = n, values_fill = 0) |>
  mutate(
    n = PRIN + PNRR
  ) |>
  filter(
    n > 29
  ) |>
  left_join(
    db_prin |>
      summarise(
        Median = median(Total) |> floor(),
        SD = sd(Total),
        .by = Org_trim
      )) |>
  left_join(
    db_prin |>
      summarise(
        Total = sum(Total),
        .by = c(Org_trim,ERC)
      )) |>
  mutate(
    f = Total/sum(Total),
    .by = c(Org_trim)
  ) |>
  select(-Total) |>
  pivot_wider(names_from = ERC,
                  values_from = f, values_fill = 0) |>
  janitor::adorn_rounding(2) |>
  arrange(-Median) |>
#  write_xlsx("ranking_uni.xlsx")
  View()



  mutate(
    n_issue = n(),
    .by = c(Org_trim, Issue)
  ) |>
  mutate(
    Total_ERC = sum(Total),
    .by = c(Org_trim,ERC)
  ) |>
  summarise(
    n_total = first(n_total),
    n_issue = first(n_issue),
    Avg = first(Avg),
    Total = first(Total),
    Total_ERC = first(Total_ERC),
    .by = c(Org_trim,ERC,Issue)
  ) |> View()
  arrange(-n) |>
  writexl::write_xlsx("db_importi_uni.xlsx")

googledrive::drive_upload(
  "db_importi_uni.xlsx",
  path = as_id("1CuQwQB49loVaRKnootfTVCV8c8ZUwCzF"),
  overwrite = TRUE
)


readxl::read_xlsx(
  "db_importi_uni.xlsx"
) -> db_summa

db_summa$n |> cor(db_summa$Avg, method = "kendall")

?cor()

### Correlazioni specializzazione

readxl::read_xlsx("ranking_uni.xlsx") -> ranking_uni

ranking_uni$Median |>
  cor(ranking_uni$LS, method = "kendall")

ranking_uni$Median |>
  cor(ranking_uni$PE, method = "kendall")

ranking_uni$Median |>
  cor(ranking_uni$SH, method = "kendall")

ranking_uni$Median |>
  cor(ranking_uni$n, method = "kendall")

ranking_uni$n |>
  cor(ranking_uni$SH, method = "kendall")

### Linea Sud

dati |>
  filter(
    Org_type == "Università"
  ) |>
  mutate(
    Org_trim = str_trim(str_replace_all(Org_trim, "[[:space:]]", " ")),
    Org_trim = case_match(
    Org_trim,
    "Napoli Federico II" ~ "Federico II",
    "BARI ALDO MORO" ~ "Bari",
    "Politecnico di BARI" ~ "OTHER",
    "PALERMO" ~ "Palermo",
    "CATANIA" ~ "Catania",
    "MESSINA" ~ "Messina",
    "SALERNO" ~ "Salerno",
    "Università della CALABRIA" ~ "ALL of Calabria",
    "\"G. d'Annunzio\" CHIETI-PESCARA" ~ "ALL of Abruzzo",
    "Campania \"Luigi Vanvitelli\"" ~ "Vanvitelli",
    "CAGLIARI" ~ "ALL of Sardinia",
    "Stazione Zoologica \"Anton Dohrn\" di Napoli" ~ "OTHER",
    "\"Magna Graecia\" di CATANZARO" ~ "ALL of Calabria",
    "TERAMO" ~ "ALL of Abruzzo",
    "Università del SALENTO" ~ "Salento",
    "NAPOLI \"L'Orientale\"" ~ "OTHER",
    "SASSARI" ~ "ALL of Sardinia",
    "NAPOLI \"Parthenope\"" ~ "OTHER",
    "UKE - Università Kore di ENNA" ~ "OTHER",
    "FOGGIA" ~ "OTHER",
    "dell'AQUILA" ~ "ALL of Abruzzo",
    "SANNIO di BENEVENTO" ~ "OTHER",
    "MOLISE" ~ "OTHER",
    "BASILICATA" ~ "OTHER",
    "\"Mediterranea\" di REGGIO CALABRIA" ~ "ALL of Calabria",
    .default = "NO"
    )) |>
  summarise(
    Total = sum(Total),
    .by = c(Org_trim,Issue)
  ) |>
  mutate(
    Perc = Total/sum(Total),
    .by = c(Issue)
  ) |>
  select(
    Org_trim,Perc,Issue
  ) |>
  pivot_wider(names_from = Issue,
              values_from = Perc) |>
  janitor::adorn_rounding(3) |>
  mutate(across(where(is.numeric), as.character)) |>
  mutate(across(everything(), ~str_replace_all(., "0\\.", "."))) |>
  View()
740