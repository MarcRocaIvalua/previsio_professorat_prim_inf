
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")


# Bases de dades

panell_pers_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/panell_pers.dta") %>% 
  clean_names %>% 
  remove_labels



# Calibració ----


calibracio_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Dades gràfics/gràfic_calibració (restringir n50)_infprim.dta")


calibracio <- calibracio_raw %>% 
  filter(n>50) %>% 
  ggplot(aes(x = p_sortida1,
             y = sortida)) +
  geom_abline(slope = 1, intercept = 0, color = paleta_ivalua[10], linetype = "dashed",
              size = 1,
              aes(color = )) +
  geom_point(shape = 16,
             size = 3,
             alpha = .7,
             color = paleta_ivalua[1]) +
  theme_ivalua(axis_text_y = T,
               axis_titles = T) +
  labs(x = "Proporció real de sortida",
       y = "Predicció mitjana de sortida") +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  # top = 10 punts
    axis.title.y = element_text(margin = margin(r = 10))   # right = 10 punts
  ) +
  scale_y_continuous(
    breaks = seq(0,.1,.02),
    limits = c(.02, .1),
    labels = scales::label_percent()) +
  scale_x_continuous(
    breaks = seq(0,.1,.02),
    limits = c(.02, .1),
    labels = scales::label_percent())
calibracio

ggsave("Outputs/inf_prim_caibració.png", width = 6, height = 4)





# Baixes per dia ----


baixes_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Dades gràfics/baixes15_16_infprim.dta") %>% 
  bind_rows(
    read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Dades gràfics/baixes23_24_infprim.dta")
  )

tot_profes <- panell_pers_raw %>% 
  select(dni, curs, especialitat = area_educativa) %>% 
  filter(especialitat %in% especialitats_primaria) %>% 
  filter(curs %in% c("2015-2016", "2023-2024")) %>%
  group_by(curs) %>% 
  count %>% 
  ungroup() %>% 
  rename(any = curs)


baixes <- baixes_raw %>% 
  mutate(any = year(data_numerica)) %>% 
  mutate(data_numerica = str_replace(as.character(data_numerica), "2023","2015")) %>%
  mutate(data_numerica = str_replace(as.character(data_numerica), "2024","2016")) %>%
  mutate(data_numerica = as.Date(data_numerica)) %>% 
  mutate(any = case_when(
    any > 2020 ~ "2023-2024",
    any < 2020 ~ "2015-2016"
  )) %>% 
  # select(-data_numerica) %>% 
  left_join(tot_profes) %>% 
  mutate(share_professors = (n_professors / n)) %>% 
  ggplot(aes(x = data_numerica)) +
  geom_line(aes(y = share_professors,
                color = as.factor(any)),
            size = 1) +
  theme_ivalua(axis_text_y = T) +
  labs(color = "Curs") +
  scale_color_manual(values = c(
    "2023-2024" = paleta_ivalua[1],
    "2015-2016" = paleta_ivalua[10]
  )) +
  scale_y_continuous(labels = scales::label_percent()) 
baixes



ggsave("Outputs/inf_prim_baixes_diàries.png", width = 6, height = 4)




