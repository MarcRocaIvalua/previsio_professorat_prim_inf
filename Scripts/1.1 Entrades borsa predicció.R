
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")

# Edat mitja entrades ----

borsa_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_borsa.dta")

borsa_raw %>% 
  filter(str_detect(ESP_DEMANADES, ",") == T)

edat_entrades <- borsa_raw %>% 
  clean_names %>% 
  remove_labels() %>% 
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
  # Reformatejar data de naixement
  mutate(data_naixement = as.Date(
    paste0(str_sub(data_naixement, 7, 10),    # any
           "-",
           str_sub(data_naixement, 4, 5),     # mes
           "-",
           str_sub(data_naixement, 1, 2)      # dia
           ))) %>% 
  mutate(edat = round(lubridate::interval(as.Date(data_naixement), as.Date(paste0(curs_academic, "-06-30"))) / years(1))) %>% 
  select(id, dni_nie_anonimitzat, curs_academic, edat, colectiu) %>% 
  # Obtenir primera observació, entrada
  group_by(id) %>% 
  filter(curs_academic == min(curs_academic)) %>% 
  ungroup() %>% 
  group_by(curs_academic) %>% 
  summarise(
    mitja = mean(edat),
    mediana = median(edat)
  ) %>% 
  ungroup()
    
  


# Importar dades demogràfiques ----

## aquells que tenen 18 anys del 2013 al 2021 (29 anys del 2024 al 2032)
## https://www.idescat.cat/pub/?id=ep&n=9123&hist=taules%2Fv2%2Fep%2F9123%2F20149%2Fcat%2Fdata%5Ec%3D3%2Fr%3D0%2Ft%3D0d%2C2%2C4%2C6%2C8%2C10%2C12%2C14%2C16%2C18%3B2%2C18%3B-3c%2Fe%3D0

factor <- 2

idescat <- read_excel("Dades/Població_18anys_13_21.xlsx",
                      sheet = "Dades",
                      range =  "A2:D11") %>% 
  rename(curs_academic = 1) %>% 
  mutate(curs_academic = as.numeric(str_sub(curs_academic, 4, 8))) %>% 
  select(curs_academic, pop = Total) %>% 
  mutate(curs_academic = curs_academic + 11) %>% 
  arrange(curs_academic) %>% 
  mutate(pop_yoy = (pop / lag(pop)) - 1) %>% 
  mutate(
    pop_yoy_alt = case_when(
      pop_yoy < 0 ~ pop_yoy / factor,
      pop_yoy > 0 ~ pop_yoy * factor),
    pop_yoy_baix = case_when(
      pop_yoy < 0 ~ pop_yoy * factor,
      pop_yoy > 0 ~ -pop_yoy * factor)
  )
  


# Importar entrades netes ----

entrades <- read_parquet("Dades/entrades_netes_inf_prim_19_24") %>% 
  arrange(especialitat, curs_academic) %>% 
  # observacions per als següents anys
  mutate(expand = case_when(
    curs_academic == 2024 ~ 9,
    T ~ 1)) %>% 
  uncount(expand, .id = "expanded") %>% 
  mutate(expanded = expanded - 1) %>% 
  mutate(curs_academic = curs_academic + expanded) %>% 
  left_join(idescat) %>%
  group_by(especialitat) %>% 
  mutate(across(contains("pop_yoy"), ~ case_when(
    curs_academic <= 2024 ~ 0,
    T ~ .))) %>%
  mutate(entrades_netes = entrades_borsa - sortides_borsa) %>% 
  select(-c(entrades_borsa, sortides_borsa)) %>% 
  # 3 escenaris d'evolució
  ## Mig
  mutate(across(c(entrades_netes), ~ case_when(
    curs_academic > 2024 ~ . * (1 + cumsum(pop_yoy)),
    T ~ .),
    .names = "{.col}_mig")) %>% 
  ## Alt
  mutate(across(c(entrades_netes), ~ case_when(
    curs_academic > 2024 ~ . * (1 + cumsum(pop_yoy_alt)),
    T ~ .),
    .names = "{.col}_alt")) %>% 
  ## Baix
  mutate(across(c(entrades_netes), ~ case_when(
    curs_academic > 2024 ~ . * (1 + cumsum(pop_yoy_baix)),
    T ~ .),
    .names = "{.col}_baix")) %>% 
  ungroup() %>% 
  select(-contains("pop"), -expanded) %>% 
  mutate(across(contains("entrades"), ~ round(.))) %>% 
  filter(especialitat %in% c(especialitats_primaria)) 


# Exportar dades ----



# write_dta(
#   entrades,
#   "Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/entrades_borsa_inf_prim_32.dta"
# )



# Visualització de dades ----


entrades_plot <- entrades %>% 
  filter(especialitat %in% c(especialitats_primaria)) %>% 
  select(-entrades_netes) %>% 
  pivot_longer(
    contains("entrades_netes"),
    names_to = "escenari",
    values_to = "value"
  ) %>% 
  mutate(escenari = str_replace(escenari, "^entrades_netes_", "")) %>% 
  mutate(value_pre = case_when(
    curs_academic <= 2024 ~ value)) %>% 
  mutate(value_post = case_when(
    curs_academic >= 2024 ~ value)) %>% 
  group_by(curs_academic, escenari) %>% 
  summarise(
    value = sum(value),
    value_pre = sum(value_pre),
    value_post = sum(value_post),
  ) %>% 
  ungroup() %>%
  mutate(escenari = paste0(toupper(substr(escenari, 1, 1)),
         tolower(substr(escenari, 2, nchar(escenari))))) %>%   
  mutate(curs_academic = paste0(as.character(curs_academic), "-", as.character(curs_academic + 1))) 

entrades_plot %>% 
  ggplot(aes(x = curs_academic,
             y = value_post)) +
  geom_line(aes(y = value_pre),
            group = 1,
            color = paleta_ivalua[1],
            size = 1.2) +
  geom_line(aes(color = escenari,
                y = value_post),
            group = 1,
            size = 1.2) +
  scale_color_manual(values = c("Alt" = paleta_ivalua[3],
                                "Mig" = paleta_ivalua[5],
                                "Baix" = paleta_ivalua[10])) +
  theme_ivalua(axis_text_y = T) +
  scale_y_continuous(limits = c(1000,6000),
                     breaks = seq(0,6000,1000)) +
  labs(color = "Escenari") +
  geom_text(
    data = entrades_plot %>% filter(curs_academic == max(curs_academic)),
    aes(label = value_post,
        color = escenari),
    vjust = .5, hjust = -.1,
    show.legend = FALSE) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Outputs/inf_prim_entrades_borsa_predicció.png", width = 6, height = 4)




entrades_plot_esp <- entrades %>% 
  filter(especialitat %in% c(especialitats_primaria)) %>% 
  select(-entrades_netes) %>% 
  pivot_longer(
    contains("entrades_netes"),
    names_to = "escenari",
    values_to = "value"
  ) %>% 
  mutate(escenari = str_replace(escenari, "^entrades_netes_", "")) %>% 
  mutate(value_pre = case_when(
    curs_academic <= 2024 ~ value)) %>% 
  mutate(value_post = case_when(
    curs_academic >= 2024 ~ value)) %>% 
  group_by(curs_academic, escenari, especialitat) %>% 
  summarise(
    value = sum(value),
    value_pre = sum(value_pre),
    value_post = sum(value_post),
  ) %>% 
  ungroup() %>%
  mutate(escenari = paste0(toupper(substr(escenari, 1, 1)),
                           tolower(substr(escenari, 2, nchar(escenari))))) %>%   
  mutate(curs_academic = paste0(as.character(curs_academic), "-", as.character(curs_academic + 1))) 

entrades_plot_esp %>% 
  ggplot(aes(x = curs_academic,
             y = value_post)) +
  geom_line(aes(y = value_pre),
            group = 1,
            color = paleta_ivalua[1],
            size = 1.2) +
  geom_line(aes(color = escenari,
                y = value_post),
            group = 1,
            size = 1.2) +
  scale_color_manual(values = c("Alt" = paleta_ivalua[3],
                                "Mig" = paleta_ivalua[5],
                                "Baix" = paleta_ivalua[10])) +
  theme_ivalua(axis_text_y = T) +
  # scale_y_discrete(breaks = seq("2019-2020", 2022)) +
  labs(color = "Escenari") +
  facet_wrap(~especialitat, scales = "free_y")
  geom_text(
    data = entrades_plot %>% filter(curs_academic == max(curs_academic)),
    aes(label = value_post,
        color = escenari),
    vjust = .5, hjust = -.1,
    show.legend = FALSE) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  



