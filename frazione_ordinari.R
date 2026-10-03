readxl::read_xlsx("universitari/2022.xlsx") |>
  filter(Ateneo == "Sapienza") |>
  filter(
    Fascia |> str_detect("Associato|Ordinario|Ricercatore")
  ) |>
  mutate(SC = str_sub(`S.C.`, 1, -2),
         Dept = `Struttura di afferenza` |> tolower(),
         Ordinario = ifelse(Fascia == "Ordinario",1,0)
         )|>
  summarise(
    Perc_ord = sum(Ordinario) / n(),
    .by = Dept
  ) |>
  View()
