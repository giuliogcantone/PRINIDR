pacman::p_load(
  tidyverse,googledrive,openalexR
)

db2 |>
  filter(
    is.na(sub) | sub != "sub"
  ) |>
  mutate(
    cognome = str_extract_all(Person, "(\\b[A-Z][A-Z]+\\b|\\b[A-Z][A-Z']+'?[A-Z]+\\b)"),
    cognome = sapply(cognome, paste, collapse = " "),
    nome = str_trim(str_replace(Person, cognome, "")),
    Person2 = str_c(
      str_to_title(nome), 
      str_to_title(cognome), 
      sep = " "
    ) |>
      str_remove_all(",") |>
      str_replace_all("'([a-z])", function(x) {
      paste0("'", str_to_upper(str_sub(x, 2, 2)))
    })
  ) |>
  select(Person, Person2) |> 
  pull(Person2) |>
  unique() -> people

oa_fetch(
  entity = "authors",
  display_name = people
) -> people_records


people
