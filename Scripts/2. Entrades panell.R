
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")

# Dades del panell_pers ----

panell_pers_ent <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_pers.dta")


# Importar dades de sortides ----

cursos <- c("24-25")
# "25-26", "26-27", "27-28",
# "28-29", "29-30", "30-31", "31-32")


importar_sortides <- function(curs){
  
  # curs <- "24-25"
  
  read_dta(paste0("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/sortides/infantil i primària/sortides", curs, ".dta")) %>% 
    select(especialitat = area_educativa, sortides_mean) %>% 
    mutate(curs = curs)
  
}

# Unir sortides per curs

sortides <- map_dfr(cursos, importar_sortides) %>%  
  bind_rows(
    read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/sortides/infantil i primària/sortides23-24.dta") %>%
      mutate(sortides_mean = p_sortida1 * n) %>% 
      select(especialitat = area_educativa, sortides_mean) %>%
      mutate(curs = "23-24")) %>%
  filter(especialitat %in% especialitats_primaria)

especialitats_sortides <- sortides %>% 
  distinct(especialitat) %>% 
  pull

# Entrades al panell desde la borsa ----

entrades_panell <- panell_pers_ent %>% 
  select(dni, area_educativa, curs, any, edat, t, anys_treballats, sexe, pla_a_a,
         especialitat = area_educativa, contains("mes_naixement"),
         # any_naix,
         # share_imm_mig = share_imm, renda_bruta_llar_norm, cmc,
         contains("jornada")) %>% 
  # seleccionar observacions quan entren
  group_by(dni) %>%
  mutate(any_lag = lag(any)) %>% 
  mutate(entrada = case_when(
    any != (any_lag - 1) ~ 0,
    T ~ 1
  )) %>% 
  ungroup() %>% 
  filter(entrada == 1) %>% 
  filter(any != 2015) %>% 
  # seleccionar especialitats d'interès
  filter(especialitat %in% c(especialitats_sortides))

# Distribució de variables segons especialitat ----

## Sexe per especialitat ----

distribucio_sexe <- entrades_panell %>%
  count(especialitat, sexe) %>%
  group_by(especialitat) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() 

distribucio_sexe %>% 
  ggplot(aes(x = sexe,
             y = n)) +
  geom_col(aes(fill = especialitat)) +
  facet_wrap(~ especialitat,
             scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Entrades")  

## Edat per especialitat i sexe ----

entrades_panell %>% 
  ggplot(aes(x = edat,
             fill = sexe)) +
  geom_histogram(bins = 10,
                 position = "dodge") +
  facet_wrap(~ especialitat,
             scales = "free_y") +
  theme_minimal() +
  labs(title = "Entrades") 

## Anys treballats per especialitat
entrades_panell %>% 
  ggplot(aes(x = anys_treballats,
             fill = sexe)) +
  geom_histogram(bins = 10,
                 position = "dodge") +
  facet_wrap(~ especialitat,
             scales = "free_y") +
  theme_minimal() +
  labs(title = "Nous") 


## Plaça per especialitat i sexe ----

entrades_panell %>%
  group_by(pla_a_a, especialitat, sexe) %>%
  count %>%
  ungroup() %>%
  ggplot(aes(x = especialitat,
             y = n,
             fill = sexe)) +
  geom_col() +
  facet_wrap(~ pla_a_a,
             scales = "free") +
  theme_minimal() +
  # theme(legend.position = "none") +
  labs(title = "Nous")






# Generar nous individus ----

set.seed(123)  # Reproductibilitat amb el slice_sample


entrades <- sortides %>%
  mutate(sortides_mean = round(sortides_mean)) %>% 
  # generar observació per persona
  uncount(sortides_mean) %>%
  # curs entrada = curs sortida + 1
  mutate(curs = paste0(
    as.numeric(str_sub(curs,1,2)) + 1,
    "-",
    as.numeric(str_sub(curs,4,5)) + 1)) %>%
  # imputar un id
  mutate(id = row_number()) %>% 
  # Sexe
  left_join(distribucio_sexe, by = "especialitat") %>%
  # Escollir la opció amb més opcions
  group_by(id) %>%  # cada fila és un individu
  slice_sample(n = 1, weight_by = prop) %>%
  ungroup() %>%
  select(-n, -prop) %>% 
  
  # Edat
  left_join(
    entrades_panell %>%
      select(sexe, especialitat, edat),
    by = c("sexe", "especialitat")
  ) %>% 
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  
  # Anys treballats
  left_join(
    entrades_panell %>%
      select(especialitat, edat, anys_treballats),
    by = c("especialitat", "edat")
  ) %>% 
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  
  # Jornada
  
  left_join(
    entrades_panell %>%
      select(sexe, edat, jornada),
    by = c("sexe", "edat")
  ) %>% 
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  
  # Plaça
  left_join(
    entrades_panell %>%
      select(sexe, especialitat, pla_a_a),
    by = c("sexe", "especialitat")
  ) %>% 
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>% 
  
  # Any naixement (segons edat)
  mutate(any_naix = as.numeric(paste0("20", str_sub(curs, 4, 5)))) %>% 
  mutate(any_naix = round(any_naix - edat)) %>% 
  
  # Mes de naixement (random)
  mutate(mes_naixement = sample(1:12, size = n(), replace = TRUE)) %>% 
  relocate(id, curs) %>% 
  mutate(edat = round(edat)) %>% 
  mutate(any = as.numeric(paste0("20", str_sub(curs,1,2)))) %>% 
  relocate(any, .after = curs) %>% 
  
  # Allargar fins el 2032
  mutate(expand = 2032 - any + 1) %>% 
  uncount(expand, .id = "expanded") %>%
  mutate(expanded = expanded - 1) %>% 
  
  # Ajustar variables a l'extensió
  mutate(
    any = any + expanded,
    anys_treballats = anys_treballats + expanded,
    edat = edat + expanded) %>%
  mutate(curs_sortida = paste0(as.numeric(str_sub(curs,1,2)) - 1, "-", as.numeric(str_sub(curs,4,5)) - 1)) %>% 
  relocate(curs_sortida, .after = curs) %>% 
  mutate(curs = paste0(as.character(any - 2000), "-", as.character(any - 1999))) %>% 
  mutate(t = any - 2014)



entrades %>% distinct(id, curs_sortida) %>% count(curs_sortida)

write_dta(
  entrades %>%
    filter(curs_sortida %in% c("24-25")),
  "Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/entrades_creades_inf_prim/entrades_panell_25_26.dta"
)



# Gràfics per comparar distribucions ----
  

## Sexe per especialitat ----
entrades %>%
  group_by(especialitat, sexe) %>% 
  count %>% 
  ungroup() %>% 
  ggplot(aes(x = sexe,
             y = n)) +
  geom_col(aes(fill = especialitat)) +
  facet_wrap(~ especialitat,
             scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Nous")  

## Edat per especilitat i sexe ----

entrades %>% 
  ggplot(aes(x = edat,
           fill = sexe)) +
  geom_histogram(bins = 10,
                 position = "dodge") +
  facet_wrap(~ especialitat,
             scales = "free_y") +
  theme_minimal() +
  labs(title = "Nous") 

## Anys treballats per sexe

entrades %>% 
  ggplot(aes(x = anys_treballats,
             fill = sexe)) +
  geom_histogram(bins = 10,
                 position = "dodge") +
  facet_wrap(~ especialitat,
             scales = "free_y") +
  theme_minimal() +
  labs(title = "Nous") 



## Placa per especialitat i sexe ----

entrades %>%
  group_by(pla_a_a, especialitat, sexe) %>%
  count %>%
  ungroup() %>%
  ggplot(aes(x = especialitat,
             y = n,
             fill = sexe)) +
  geom_col() +
  facet_wrap(~ pla_a_a,
             scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Nous")


## Jornada per sexe i edat ----

entrades %>%
  ggplot(aes(x = edat,
             fill = sexe)) +
  geom_histogram(position = "dodge") +
  facet_wrap(~ jornada,
             scales = "free") +
  theme_minimal() +
  # theme(legend.position = "none") +
  labs(title = "Nous")




