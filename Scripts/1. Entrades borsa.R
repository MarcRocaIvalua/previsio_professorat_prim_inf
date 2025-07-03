
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")

# Importar bases de dades ----

borsa_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_borsa.dta")

panell_pers <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_pers.dta") %>% 
  clean_names


# dades de la borsa

borsa <- borsa_raw %>% 
  clean_names %>% 
  remove_labels() %>% 
  select(-c(altres_st, barem, servei_territorial))
    

# Entrades per curs i colectiu ----

entrades <- borsa %>% 
  group_by(id) %>% 
  arrange(id, curs_academic) %>% 
  mutate(entrada = case_when(
    lag(curs_academic) != curs_academic - 1 ~ 1,
    curs_academic == min(curs_academic) ~ 1)) %>% 
  ungroup %>% 
  filter(entrada == 1) %>% 
  select(-entrada) %>% 
  # Determinar el colectiu pel que demanen assigfanció
  mutate(colectiu = paste(colectiu_primaria, colectiu_secundaria)) %>% 
  mutate(colectiu = str_replace_all(colectiu, "NA", "-")) %>% 
  mutate(colectiu = str_replace_all(colectiu, " ", "")) %>% 
  # Obtenir aquells que només demanen formació per FP
  mutate(fp = case_when(
    str_detect(esp_demanades, "^[0-9,]+$") == T ~ 1,
    T ~ 0)) %>% 
  # Classificació segons colectiu
  mutate(colectiu = case_when(
    colectiu %in% c("-0", "-1", "-2") ~ "2. Secundària",
    colectiu %in% c("1-", "2-") ~ "1. Primària",
    T ~ "3. Primària i secundària")) %>%
  mutate(colectiu = case_when(
    fp == 1 ~ "Només FP",
    T ~ colectiu)) %>% 
  select(id, dni_nie_anonimitzat, curs_academic, esp_demanades, colectiu) %>% 
  mutate(esp_demanades = esp_demanades %>%
           str_remove_all("[0-9]") %>%                      # elimina nombres (FP)
           str_replace_all(",\\s*,+", ",") %>%              # elimina comes duplicades
           str_replace_all("^\\s*,+|,+\\s*$", "") %>%       # elimina comes al principi o final
           str_squish()                                     # neteja espais extra
  ) %>% 
  # assegurar-se que no hi ha combinacions repetides
  mutate(esp_demanades = esp_demanades %>%
           str_split(",") %>%            # separem per comes
           map(~ sort(.x)) %>%           # ordenem alfabèticament
           map_chr(~ paste(.x, collapse = ",")) # tornem a unir
  ) 

entrades %>% 
  count(curs_academic)
  


# Assignats ----


## Identificar especialitat dels assignats ----

assignats_espcialitat <- panell_pers %>%
  mutate(any = as.numeric(str_sub(curs, 1, 4))) %>% 
  distinct(dni, area_educativa, any) %>% 
  # Observació d'entrada
  group_by(dni) %>% 
  filter(any == min(any)) %>% 
  ungroup %>% 
  rename(dni_nie_anonimitzat = dni) %>% 
  select(-any) %>% 
  distinct
  

## Veure l'especialitat assignada dels que estaven a la borsa i ara al panell ----

assignats_match <- entrades %>% 
  left_join(assignats_espcialitat) %>% 
  rename(especialitat = area_educativa) %>% 
  # Quedar-se només amn els individus que tenen assignació
  filter(!is.na(especialitat)) %>% 
  filter(str_detect(esp_demanades, ",")) %>% 
  # eliminar especialitats de FP
  mutate(especialitat = especialitat %>%
           str_remove_all("[0-9]") %>%                      # elimina nombres (FP)
           str_replace_all(",\\s*,+", ",") %>%              # elimina comes duplicades
           str_replace_all("^\\s*,+|,+\\s*$", "") %>%       # elimina comes al principi o final
           str_squish()                                     # neteja espais extra
  ) %>% 
  # assegurar-se que no hi ha combinacions repetides
  mutate(especialitat = especialitat %>%
           str_split(",") %>%            # separem per comes
           map(~ sort(.x)) %>%           # ordenem alfabèticament
           map_chr(~ paste(.x, collapse = ",")) # tornem a unir
  ) %>% 
  # Eliminar especialitats buides
  filter(especialitat != "") %>% 
  filter(esp_demanades != "") %>%
  arrange(esp_demanades) %>% 
  # casos per especialitat demanada, curs acadèmic i especialitat assignada
  group_by(esp_demanades, especialitat) %>% 
  count %>% 
  ungroup() %>% 
  # proporció d'especialitat assignada segons especialitat demanada
  group_by(esp_demanades) %>% 
  mutate(n_esp_demanades = sum(n)) %>% 
  ungroup %>% 
  mutate(share_esp = n / n_esp_demanades) %>% 
  arrange(desc(n_esp_demanades), desc(n))


# Excloure els que surten de la borsa i no entren al panell ----

## Dnis dins del panell

panell_ids <- panell_pers %>% 
  distinct(dni) %>% 
  pull



sortides_borsa <- borsa %>% 
  # Determinar el colectiu pel que demanen assigfanció
  mutate(colectiu = paste(colectiu_primaria, colectiu_secundaria)) %>% 
  mutate(colectiu = str_replace_all(colectiu, "NA", "-")) %>% 
  mutate(colectiu = str_replace_all(colectiu, " ", "")) %>% 
  # Obtenir aquells que només demanen formació per FP
  mutate(fp = case_when(
    str_detect(esp_demanades, "^[0-9,]+$") == T ~ 1,
    T ~ 0)) %>% 
  # Classificació segons colectiu
  mutate(colectiu = case_when(
    colectiu %in% c("-0", "-1", "-2") ~ "2. Secundària",
    colectiu %in% c("1-", "2-") ~ "1. Primària",
    T ~ "3. Primària i secundària")) %>%
  mutate(colectiu = case_when(
    fp == 1 ~ "Només FP",
    T ~ colectiu)) %>% 
  # obtenir aquells que NO apareixen al panell
  rename(dni = dni_nie_anonimitzat,
         curs = curs_academic) %>% 
  filter(dni %nin% c(panell_ids)) %>% 
  # obtenir la darrera observació
  group_by(dni) %>% 
  filter(curs == max(curs)) %>% 
  ungroup %>% 
  # excloure aquells que al 2024 encara hi son
  filter(curs != 2024) %>% 
  # eliminar especialitats de FP
  mutate(esp_demanades = esp_demanades %>%
           str_remove_all("[0-9]") %>%                      # elimina nombres (FP)
           str_replace_all(",\\s*,+", ",") %>%              # elimina comes duplicades
           str_replace_all("^\\s*,+|,+\\s*$", "") %>%       # elimina comes al principi o final
           str_squish()                                     # neteja espais extra
  ) %>% 
  # assegurar-se que no hi ha combinacions repetides
  mutate(esp_demanades = esp_demanades %>%
           str_split(",") %>%            # separem per comes
           map(~ sort(.x)) %>%           # ordenem alfabèticament
           map_chr(~ paste(.x, collapse = ",")) # tornem a unir
  ) %>% 
  # Eliminar especialitats buides
  filter(esp_demanades != "") %>% 
  filter(esp_demanades != "") %>% 
  mutate(esp = case_when(
    str_detect(esp_demanades, ",") == T ~ "Mult",
    T ~ esp_demanades)) %>% 
  group_by(curs, esp_demanades, colectiu) %>% 
  count %>%
  ungroup() %>% 
  arrange(esp_demanades, curs) %>% 
  # establir curs que ja no hi son, no el curs de l'última observació
  mutate(curs_academic = curs + 1) %>% 
  rename(sortides_borsa = n) %>% 
  select(-curs) %>% 
  # Només una observació per curs i especialitat
  group_by(curs_academic, esp_demanades) %>% 
  summarise(sortides_borsa = sum(sortides_borsa, na.rm = T)) %>% 
  ungroup %>% 
  full_join(assignats_match %>%
              select(esp_demanades, especialitat, share_esp)  
  ) %>% 
  mutate(share_esp = case_when(
    is.na(share_esp) ~ 1,
    T ~ share_esp
  )) %>% 
  mutate(n_assignat = round(sortides_borsa * share_esp)) %>%   
  mutate(especialitat = case_when(
    is.na(especialitat) & str_detect(esp_demanades, ",") == F ~ esp_demanades,
    T ~ especialitat)) %>% 
  drop_na(especialitat) %>% 
  group_by(curs_academic, especialitat) %>% 
  summarise(sortides_borsa = sum(n_assignat, na.rm = T)) %>% 
  ungroup()


# Aspecte de les sortides de la borsa

sortides_borsa %>% 
  mutate(prim_sec = case_when(
    especialitat %in% especialitats_primaria ~ "Infantil i primària",
    T ~ "Secundària"
  )) %>% 
  group_by(curs_academic, prim_sec) %>% 
  mutate(sortides_borsa = sum(sortides_borsa, na.rm = T)) %>% 
  ungroup %>% 
  ggplot(aes(x = curs_academic,
             y = sortides_borsa)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  facet_wrap(~prim_sec) +
  labs(x = "", y = "", 
       title = "Sortides de la borsa que no entren al panell_pers")

sortides_borsa %>% 
  filter(especialitat %in% especialitats_primaria) %>% 
  ggplot(aes(x = curs_academic)) +
  geom_line(aes(y = sortides_borsa,
                color = especialitat),
            size = 1)
  


# Especialitat demanada única ----


minim <- entrades %>% 
  # eliminar primer ant (no sabem si entren o no)
  filter(curs_academic != 2018) %>%
  # quedar-se amb aquells que demanen només una especialitat
  filter(str_detect(esp_demanades, ",") == F) %>% 
  group_by(esp_demanades, colectiu, curs_academic) %>% 
  count() %>% 
  ungroup() %>% 
  filter(esp_demanades != "") %>% 
  arrange(esp_demanades, curs_academic) %>% 
  filter(colectiu != "Només FP") %>% 
  rename(min = n) %>% 
  select(curs_academic, colectiu, esp_demanades, min) %>% 
  # eliminar colectius (primaria i secundària)
  group_by(curs_academic, esp_demanades) %>% 
  summarise(min = sum(min, na.rm = T)) %>% 
  ungroup() %>% 
  rename(especialitat = esp_demanades) %>% 
  # unir sortides borsa per tenir les entrades netes
  left_join(sortides_borsa %>% 
              select(curs_academic, especialitat, sortides_borsa)) %>% 
  rename(min_pre = min) %>% 
  mutate(sortides_borsa = case_when(
    is.na(sortides_borsa) ~ 0,
    T ~ sortides_borsa)) %>% 
  mutate(min = min_pre - sortides_borsa) %>% 
  mutate(canvi = round((min / min_pre) - 1, 3)) %>% 
  filter(especialitat %in% especialitats_primaria)
  


# Match per especialitats múltiples ----


# Profes a la borsa amb especialitat demanada

esp_mult <- entrades %>% 
  # eliminar primer ant (no sabem si entren o no)
  filter(curs_academic != 2018) %>% 
  filter(esp_demanades != "") %>% 
  filter(!str_detect(colectiu, "FP")) %>% 
  filter(str_detect(esp_demanades, ",")) %>% 
  group_by(curs_academic, esp_demanades) %>% 
  count %>% 
  ungroup %>%
  rename(profes_curs_esp = n) %>% 
  # unir amb el match dels assignats per obtenir els shares d'assignació
  full_join(assignats_match %>%
              select(esp_demanades, especialitat, share_esp)  
            ) %>% 
  #  tirar aquelles combinacions que no poden fer merge
  drop_na(curs_academic) %>% 
  drop_na(share_esp) %>%    #  residual
  mutate(n_assignat = round(profes_curs_esp * share_esp)) %>% 
  # comparar nombre total amb nombre real
  group_by(especialitat, curs_academic) %>% 
  summarise(n_assignat = sum(n_assignat)) %>% 
  ungroup %>% 
  filter(n_assignat > 0) %>% 
  filter(especialitat %in% especialitats_primaria)
  
  # OJU MISMATCH D'ALGUNA UNITAT
  
  


entrades_netes <- minim %>% 
  select(curs_academic, especialitat, n = min_pre, sortides_borsa) %>% 
  bind_rows(esp_mult %>% 
              rename(n = n_assignat)) %>%  
  group_by(curs_academic, especialitat) %>% 
  summarise(
    entrades_borsa = sum(n),
    sortides_borsa = sum(sortides_borsa, na.rm = T)) %>% 
  ungroup()
  

# Exportar dades per 

# write_parquet(
#   entrades_netes,
#   "Dades/entrades_netes_inf_prim_19_24"
# )




# Visualització de dades ----


## Escenaris entrades per especialitat

  
entrades_netes %>%
  # filter(especialitat %in% c("PRI", "EF", "GE")) %>%
  ggplot(aes(x = curs_academic)) +
  geom_line(aes(y = entrades_borsa, color = especialitat),
            size = 1) +
  geom_line(aes(y = sortides_borsa, color = especialitat),
            size = 1,
            linetype = "dashed") +
  facet_wrap(~especialitat, scales  = "free_y",
             ncol = 5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "")







## Entrades netes per colectiu ----



entrades_netes %>%
  group_by(curs_academic) %>% 
  summarise(
    entrades_borsa = sum(entrades_borsa, na.rm = T),
    sortides_borsa = sum(sortides_borsa)) %>% 
  ungroup %>% 
  mutate(sortides_borsa = sortides_borsa * (-1)) %>%
  mutate(entrades_netes = entrades_borsa + sortides_borsa) %>%
  mutate(curs_academic = paste0(as.character(curs_academic), "-", as.character(curs_academic + 1))) %>%
  ggplot(aes(x = curs_academic)) +
  geom_col(aes(y = entrades_borsa,
               fill = "Entrades")) +
  geom_col(aes(y = sortides_borsa,
               fill = "Sortides")) +
  geom_line(aes(y = entrades_netes,
                group = 1,
                color = "Entrades netes"),
            size = 1.1) +
  geom_point(aes(y = entrades_netes,
                color = "Entrades netes"),
            size = 2.5,
            shape = 15) +
  scale_fill_manual(values = c("Entrades" = paleta_ivalua[5],
                                "Sortides" = paleta_ivalua[10])) +
  scale_color_manual(values = c("Entrades netes" = paleta_ivalua[1])) +
  theme_ivalua(axis_text_y = T) +
  labs(x = "", y = "", fill = "", color = "") +
  geom_hline(yintercept = 0)

ggsave("Outputs/inf_prim_entrades_borsa_netes.png", width = 6, height = 4)




