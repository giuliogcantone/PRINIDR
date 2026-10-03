pacman::p_load(
  tidyverse,googledrive
)

drive_download(as_id("10DtsX0m1QS1gb9os6cPB2KblqJQtCMh1"),
               path = "dati.xlsx", overwrite = TRUE)

readxl::read_xlsx("dati.xlsx") -> db

db |>
  select(-1) |>
  mutate(across(where(is.character), ~ na_if(.x, "na"))) |>
  set_names(
    c("Issue",
      "ERC",
      "Code",
      "Title",
      "Person",
      "sub",
      "Gender",
      "Role",
      "PI_type",
      "Org",
      "Org_type",
      "ETER_ID",
      "Org2",
      "Person2",
      "Acad_type",
      "Org3",
      "ETER_ID2",
      "Faculty",
      "SSD",
      "SC",
      "Faculty2",
      "Cofin",
      "MUR_quota",
      "MUR_premium",
      "Total",
      "notes",
      "check"
    )
  ) |>
  mutate(
    Org_trim = Org |> str_remove("Università degli Studi di "),
    Org_trim = Org_trim |> str_remove("Università degli Studi del "),
    Org_trim = Org_trim |> str_remove("Università degli Studi della "),
    Org_trim = Org_trim |> str_remove("Università degli Studi"),
    Org_trim = Org_trim |> str_remove("Università degli Studi della ")
  ) |>
  filter(
    is.na(sub) | sub != "sub"
  ) -> dati


drive_download(as_id("1-kgWrZyuahMGx0iBnaviwDhf8EeMU_3d"),
               overwrite = TRUE)

#Stat

dati |>
  filter(Org_type == "Università") |>
  summarise(
    n = n())

db_prin |>
  summarise(.by = c(Code,ERC),
            Total = sum(Total)) |>
  summarise(
    n = n(),
    Total = sum(Total),
    .by = ERC)
  
dati$Org |> unique() |> length()
