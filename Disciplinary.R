pacman::p_load(
  tidyverse,googledrive,writexl,readxl
)

readxl::read_xlsx(
  "ERC_Codes.xlsx"
) |> left_join(
  db2 |>
    filter(
      is.na(sub) | sub != "sub"
    ) |>
    select(ERC,Total)
) |>
  summarise(
    .by = Macro_ERC,
    Total = sum(Total)
  ) |>
  mutate(
    f = Total/sum(Total)
  )

readxl::read_xlsx(
  "ERC_Codes.xlsx"
) |> left_join(
  db2 |>
    select(ERC,Total)
) |>
  summarise(
    .by = ERC_ext,
    Macro_ERC = first(Macro_ERC),
    Total = sum(Total)
  ) |>
  mutate(
    f = Total/sum(Total),
    .by = Macro_ERC
  ) |>
  select(-Total) |>
  arrange(Macro_ERC,ERC_ext) |>
  janitor::adorn_rounding(2) |> View()

readxl::read_xlsx(
  "ERC_Codes.xlsx"
) |> left_join(
  db2 |>
    select(ERC,Total,Issue)
) |>
  summarise(
    .by = c("ERC_ext","Issue"),
    Macro_ERC = first(Macro_ERC),
    Total = sum(Total)
  ) |>
  mutate(
    f = Total/sum(Total),
    .by = c(Issue,Macro_ERC)
  ) |>
  select(-Total) |>
  janitor::adorn_rounding(3) |>
  pivot_wider(names_from =Issue,
              values_from = f) |>
  arrange(Macro_ERC,ERC_ext) |>
  mutate(across(where(is.numeric), ~ replace_na(., 0))) |>
  writexl::write_xlsx("ERC_fundings.xlsx")

googledrive::drive_upload(
  "ERC_fundings.xlsx",
  path = as_id("1CuQwQB49loVaRKnootfTVCV8c8ZUwCzF"),
  overwrite = TRUE
)

###

readxl::read_xlsx("universitari/2017.xlsx") |>
  filter(
    Fascia |> str_detect("Associato|Ordinario")
  ) |>
  mutate(SC = str_sub(`S.C.`, 1, -2)) |>
  summarise(exp_2017 = n(),
            .by = SC) |>
  arrange(SC) |>
  left_join(
    readxl::read_xlsx("universitari/2020.xlsx") |>
      filter(
        Fascia |> str_detect("Associato|Ordinario")
      ) |>
      mutate(SC = str_sub(`S.C.`, 1, -2)) |>
      summarise(exp_2020 = n(),
                .by = SC) 
  ) |>
  left_join(
    readxl::read_xlsx("universitari/2022.xlsx") |>
      filter(
        Fascia |> str_detect("Associato|Ordinario")
      ) |>
      mutate(SC = str_sub(`S.C.`, 1, -2)) |>
      summarise(exp_2022 = n(),
                .by = SC) 
  ) |>
  mutate(Area = str_remove(SC, "/.*"),
         .before = 1) |>
  group_by(Area) |>  
  mutate(across(where(is.numeric), ~ . / sum(., na.rm = TRUE))) |>  
  ungroup() |>
  left_join(readxl::read_xlsx("ssd.xlsx") |>
              mutate(Area = str_remove(SC, "/.*")) |>
              left_join(
                db2 |>
                  filter(
                    is.na(sub) | sub != "sub"
                  ) |>
                  select(SC,Total,Issue) |>
                  mutate(
                    SC = SC |> str_sub(1,-2) |>
                      na_if("")
                  )
              ) |>
              summarise(
                .by = c(SC,name,Issue,Area),
                Total = sum(Total)
              ) |>
              mutate(
                f = Total/sum(Total),
                .by = c(Issue,Area)
              ) |>
              select(-Total) |>
              janitor::adorn_rounding(3) |>
              pivot_wider(names_from =Issue,
                          values_from = f) |>
              mutate(across(where(is.numeric), ~ replace_na(., 0)))
            ) |>
  janitor::adorn_rounding(3) |>
  transmute(
    Area,name,
    exp_2017,`PRIN 2017`,
    exp_2020,`PRIN 2020`,
    exp_2022,`PRIN 2022`,
    `PRIN 2022 PNRR`,
  ) |>
  write_xlsx("SMC_fundings.xlsx")

googledrive::drive_upload(
  "SMC_fundings.xlsx",
  path = as_id("1CuQwQB49loVaRKnootfTVCV8c8ZUwCzF"),
  overwrite = TRUE
)

###

readxl::read_xlsx("SMC_fundings.xlsx") |>
  select(Area,name,exp_2022) |>
  left_join(readxl::read_xlsx("ssd.xlsx") |>
              mutate(Area = str_remove(SC, "/.*")) |>
              left_join(
                db2 |>
                  filter(
                    is.na(sub) | sub != "sub"
                  ) |>
                  select(SC,Total,Issue) |>
                  mutate(
                    SC = SC |> str_sub(1,-2) |>
                      na_if(""),
                    Issue2 = if_else(str_detect(Issue, "PNRR"),
                                            "PNRR", "PRIN")
                  )
              ) |>
              summarise(
                .by = c(SC,name,Issue2,Area),
                Total = sum(Total,na.rm = T)
              ) |>
              mutate(
                f = Total/sum(Total),
                .by = c(Issue2,Area)
              ) |>
              select(-Total) |>
              janitor::adorn_rounding(3) |>
              pivot_wider(names_from =Issue2,
                          values_from = f) |>
              mutate(across(where(is.numeric), ~ replace_na(., 0)))
  ) |>
  janitor::adorn_rounding(3) |>
  transmute(
    Area,name,
    exp_2022,`PRIN`,`PNRR`,
  ) |>
  write_xlsx("SMC_fundings_2.xlsx")

###

readxl::read_xlsx("universitari/2017.xlsx") |>
  filter(Fascia |> str_detect("Associato|Ordinario")) |>
  mutate(Area = str_remove(`S.C.`, "/.*"),
         .before = 1) |>
  summarise(exp_2017 = n(),
            .by = Area) |>
  arrange(Area) |>
  left_join(
    readxl::read_xlsx("universitari/2020.xlsx") |>
      filter(Fascia |> str_detect("Associato|Ordinario")) |>
      mutate(Area = str_remove(`S.C.`, "/.*"),
             .before = 1) |>
      summarise(exp_2020 = n(),
                .by = Area)
  ) |>
  left_join(
    readxl::read_xlsx("universitari/2022.xlsx") |>
      filter(Fascia |> str_detect("Associato|Ordinario")) |>
      mutate(Area = str_remove(`S.C.`, "/.*"),
             .before = 1) |>
      summarise(exp_2022 = n(),
                .by = Area)
  ) |>
  mutate(across(where(is.numeric), ~ . / sum(.))) |>
  left_join(db2 |>
              filter(is.na(sub) | sub != "sub") |>
                  select(SC,Total,Issue) |>
                  mutate(Area = str_remove(SC, "/.*") |> na_if("")) |>
              summarise(
                .by = c(Issue,Area),
                Total = sum(Total)
              ) |>
              mutate(
                f = Total/sum(Total),
                .by = c(Issue)
              ) |>
              select(-Total) |>
              pivot_wider(names_from =Issue,
                          values_from = f) |>
              mutate(across(where(is.numeric), ~ replace_na(., 0)))
            ) |>
  transmute(
    Area,
    exp_2017,`PRIN 2017`,
    exp_2020,`PRIN 2020`,
    exp_2022,`PRIN 2022`,
    `PRIN 2022 PNRR`,
  ) |>
  janitor::adorn_rounding(3) |>
  write_xlsx("Area_fundings.xlsx")

readxl::read_xlsx("Area_fundings.xlsx") |>
  select(Area,exp_2022) |>
    left_join(db2 |>
                filter(is.na(sub) | sub != "sub") |>
                select(SC,Total,Issue) |>
                mutate(Area = str_remove(SC, "/.*") |> na_if(""),
                         Issue2 = if_else(str_detect(Issue, "PNRR"),
                                          "PNRR", "PRIN")
                       ) |>
                summarise(
                  .by = c(Issue2,Area),
                  Total = sum(Total)
                ) |>
                mutate(
                  f = Total/sum(Total),
                  .by = c(Issue2)
                ) |>
                select(-Total) |>
                pivot_wider(names_from =Issue2,
                            values_from = f) |>
                mutate(across(where(is.numeric), ~ replace_na(., 0)))
              ) |>
      transmute(Area,exp_2022,PRIN,PNRR) |>
      janitor::adorn_rounding(3) |>
  write_xlsx("Area_fundings_2.xlsx")

###

readxl::read_xlsx("universitari/2017.xlsx") |>
  filter(
    Fascia |> str_detect("Associato|Ordinario")
  ) |>
  summarise(
    Tenured = n(),
    .by = Ateneo
  ) |>
  arrange(-Tenured) |>
  View()
