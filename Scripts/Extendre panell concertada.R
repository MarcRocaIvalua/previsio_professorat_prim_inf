
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")


panell_concertada_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/primaria concertada/panell_concertada_2032.dta")

panell_concertada_32 <- panell_concertada_raw %>% 
  clean_names %>% 
  remove_labels %>% 
  select(dni, curs, t, sortida, temps_treballat, hores_classe_infantil, hores_classe_primaria,
         contains("hores_tipus_"), edat, male, any_nomenament, any_naixement, contains("mes_naixement_"),
         contains("carrec"), contains("nivell_carrec_"), contains("subtipus_carrec_")) %>% 
  # canviar missings per 0s
  mutate(across(c(sortida, contains("hores")), ~ case_when(
    is.na(.) ~ 0,
    T ~ .))) %>% 
  # extendre observacions
  mutate(expanded = case_when(
    curs == 2024 ~ 9,
    T ~ 1)) %>% 
  uncount(expanded, .id = "expanded") %>% 
  mutate(expanded = expanded - 1) %>% 
  # extendre variables de temps
  mutate(across(c(curs, t, edat, temps_treballat), ~ . + expanded)) %>% 
  mutate(across(c(nivell_carrec, tipus_carrec), ~ case_when(. == "" ~ "0", T ~ .)))



# Exportar dades

write_dta(
  panell_concertada_32,
  "Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/primaria concertada/panell_concertada_2032_expanded.dta"
)  
