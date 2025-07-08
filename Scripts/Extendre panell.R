
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")


panell_pers_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_pers_2032_infprim.dta")


# Expandir panell pers ----

panell_pers <- panell_pers_raw %>% 
  # slice(1:10000) %>% 
  # allargar el panell 10 cursos
  group_by(dni) %>%
  mutate(expand = case_when(
    any == 2023 ~ 10,
    T ~ 1)) %>%
  ungroup() %>%
  uncount(expand, .id = "expanded") %>% 
  mutate(expanded = expanded - 1) %>% 
  ## Ajustar anys i curs
  mutate(any = any + expanded) %>%
  mutate(curs = paste0(any, "-", any + 1)) %>%
  mutate(anys_treballats = anys_treballats + expanded) %>% 
  mutate(t = t + expanded) %>% 
  ## Ajustar edat (respecte al 30 de juny de cada any)
  mutate(edat = lubridate::interval(data_naixement, as.Date(paste0(any, "-06-30"))) / years(1)) %>% 
  relocate(edat, .after = data_naixement) %>% 
  # Unir amb màxima complexitat
  mutate(codi_centre = as.numeric(codi_centre)) %>% 
  # # modificar sortides
  # mutate(sortida = case_when(
  #   is.na(sortida) & any != 2023 ~ 0,
  #   T ~ sortida)) %>%
  # triennis
  mutate(triennis = case_when(
    expanded > 0 & !is.na(triennis) ~ ceiling(anys_treballats / 3),
    T ~ triennis)) %>% 
  # sexennis
  mutate(sexennis = case_when(
    is.na(sexennis) ~ 0,
    T ~ sexennis)) %>% 
  mutate(sexennis = case_when(
    expanded > 0 & !is.na(sexennis) ~ as.numeric(str_sub(anys_treballats / 6, 1,1)),
    T ~ sexennis)) %>%   
  select(dni, curs, any, t, edat, sortida, sex, mes_naixement, any_naix, jornada, area_educativa, 
         codi_centre, codi_municipi, anys_treballats, triennis, sexennis, expanded, vincle_label,
         sexennis, any_inici_treball, pla_a_a, codi_st, categoria, cmc)


# Exportar dades ----

write_dta(
  panell_pers,
  "Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_pers_2032_infprim_expanded.dta"
)

  