
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





# Durada baixa per plaça ----

durada_baixa_plaça <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Dades gràfics/durada_baixa_plaça_infprim.dta")


durada_baixa_plaça_plot <- durada_baixa_plaça %>% 
  clean_names %>% 
  mutate(any = str_sub(curs, 1, 4)) %>% 
  filter(any %in% c(2015, 2023)) %>%
  filter(n > 100) %>% 
  mutate(any = paste0(as.character(any), "-", as.character(as.numeric(any) + 1))) %>% 
  select(-n, -curs) %>% 
  pivot_wider(
    names_from = any,
    values_from = durada_it
  ) %>% 
  drop_na %>% 
  mutate(pla_a_a = case_when(
    pla_a_a == "CS" ~ "Comissió de serveis",
    pla_a_a == "DD" ~ "Destinació definitiva",
    pla_a_a == "IN" ~ "Interinatge",
    pla_a_a == "PP" ~ "Destinació provisional",
    pla_a_a == "SU" ~ "Substitució",
  )) %>% 
  mutate(pla_a_a = factor(pla_a_a, levels = c("Comissió de serveis", "Destinació provisional",
                                              "Destinació definitiva", "Substitució","Interinatge"))) 


plot_durada_baixa <- durada_baixa_plaça_plot %>% 
  ggplot(aes(y = pla_a_a)) +
  geom_segment(aes(x = `2015-2016`, xend = `2023-2024`, y = pla_a_a, yend = pla_a_a),
               size = .8,
               color = paleta_ivalua[2]) +
  geom_point(aes(x = `2015-2016`,
                 color = "2015-2016"),
             shape = 19,
             size = 3) +
  geom_point(aes(x = `2023-2024`,
                 color = "2023-2024"),
             shape = 19,
             size = 3) +
  scale_color_manual(
    values = c("2015-2016" = paleta_ivalua[10],
               "2023-2024" = paleta_ivalua[1])) +
  theme_ivalua(axis_text_y = T,
               axis_text_x = F) +
  # valors 2015-2016
  ## sense pp
  geom_text(
    data = durada_baixa_plaça_plot %>% filter(pla_a_a %nin% c("Destinació provisional")),
    aes(label = round(`2015-2016`,1),
        x = `2015-2016`),
    color = paleta_ivalua[10],
    vjust = .5 , hjust = 1.5,
    show.legend = FALSE) +
  ## pp
  geom_text(
    data = durada_baixa_plaça_plot %>% filter(pla_a_a %in% c("Destinació provisional")),
    aes(label = round(`2015-2016`,1),
        x = `2015-2016`),
    color = paleta_ivalua[10],
    vjust = .5 , hjust = -.5,
    show.legend = FALSE) +
  # valors 2023-2024
  ## sense pp
  geom_text(
    data = durada_baixa_plaça_plot %>% filter(pla_a_a %nin% c("Destinació provisional")),
    aes(label = round(`2023-2024`,1),
        x = `2023-2024`),
    color = paleta_ivalua[1],
    vjust = .5 , hjust = -.7,
    show.legend = FALSE) +
  ## pp
  geom_text(
    data = durada_baixa_plaça_plot %>% filter(pla_a_a %in% c("Destinació provisional")),
    aes(label = round(`2023-2024`,1),
        x = `2023-2024`),
    color = paleta_ivalua[1],
    vjust = .5 , hjust = 1.4,
    show.legend = FALSE) +
  labs(color = "") +
  ggtitle("Dies de baixa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = expansion(mult = c(0.3, 0.4)))
plot_durada_baixa


n_baixa_plaça_plot <- durada_baixa_plaça %>% 
  clean_names %>% 
  mutate(any = str_sub(curs, 1, 4)) %>% 
  mutate(any = paste0(as.character(any), "-", as.character(as.numeric(any) + 1))) %>% 
  filter(any %in% c("2015-2016", "2023-2024")) %>%
  filter(n > 100) %>%
  mutate(n = n/1000) %>% 
  select(-durada_it, -curs) %>% 
  pivot_wider(
    names_from = any,
    values_from = n
  ) %>% 
  drop_na %>% 
  mutate(pla_a_a = case_when(
    pla_a_a == "CS" ~ "Comissió de serveis",
    pla_a_a == "DD" ~ "Destinació definitiva",
    pla_a_a == "IN" ~ "Interinatge",
    pla_a_a == "PP" ~ "Destinació provisional",
    pla_a_a == "SU" ~ "Substitució",
  )) %>% 
  mutate(pla_a_a = factor(pla_a_a, levels = c("Comissió de serveis", "Destinació provisional",
                                              "Destinació definitiva", "Substitució","Interinatge"))) 

plot_n_baixa <- n_baixa_plaça_plot %>% 
  ggplot(aes(y = pla_a_a)) +
  geom_segment(aes(x = `2015-2016`, xend = `2023-2024`, y = pla_a_a, yend = pla_a_a),
               size = .8,
               color = paleta_ivalua[2]) +
  geom_point(aes(x = `2015-2016`,
                 color = "2015-2016"),
             shape = 19,
             size = 3) +
  geom_point(aes(x = `2023-2024`,
                 color = "2023-2024"),
             shape = 19,
             size = 3) +
  scale_color_manual(
    values = c("2015-2016" = paleta_ivalua[10],
               "2023-2024" = paleta_ivalua[1])) +
  theme_ivalua(axis_text_y = T,
               axis_text_x = F) +
  # valors 2015-2016
  ## sense dd
  geom_text(
    data = n_baixa_plaça_plot %>% filter(pla_a_a %nin% c("Destinació definitiva")),
    aes(label = round(`2015-2016`,1),
        x = `2015-2016`),
    color = paleta_ivalua[10],
    vjust = .5 , hjust = 1.5,
    show.legend = FALSE) +
  ## dd
  geom_text(
    data = n_baixa_plaça_plot %>% filter(pla_a_a %in% c("Destinació definitiva")),
    aes(label = round(`2015-2016`,1),
        x = `2015-2016`),
    color = paleta_ivalua[10],
    vjust = .5 , hjust = -.2,
    show.legend = FALSE) +
  # valors 2023-2024
  ## sense dd
  geom_text(
    data = n_baixa_plaça_plot %>% filter(pla_a_a %nin% c("Destinació definitiva")),
    aes(label = round(`2023-2024`,1),
        x = `2023-2024`),
    color = paleta_ivalua[1],
    vjust = .5 , hjust = -.3,
    show.legend = FALSE) +
  ## dd
  geom_text(
    data = n_baixa_plaça_plot %>% filter(pla_a_a %in% c("Destinació definitiva")),
    aes(label = round(`2023-2024`,1),
        x = `2023-2024`),
    color = paleta_ivalua[1],
    vjust = .5 , hjust = 1.5,
    show.legend = FALSE) +
  labs(color = "") +
  ggtitle("Milers de docents de baixa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.3))) 
plot_n_baixa


ggarrange(
  plot_durada_baixa,
  plot_n_baixa,
  common.legend = T,
  legend = "bottom"
)

ggsave("Outputs/inf_prim_dies_durada_baixa.png", width = 9, height = 4)


# Baixes durada per edat ----

baixes_durada_edat <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Dades gràfics/durada_mitjana_edat_infprim.dta") %>% 
  drop_na %>% 
  mutate(edat = paste0(as.character(edat), "-", as.character(edat + 5))) %>% 
  filter(edat != "65-70")


baixes_durada_edat %>% 
  ggplot(aes(x = edat,
             y = mitjana_durada)) +
  geom_line(size = 1,
            group = 1,
            color = paleta_ivalua[1]) +
  geom_point(size = 2,
             shape = 15,
             color = paleta_ivalua[1]) +
  theme_ivalua() +
  # Nombres al final
  geom_text(
    aes(label = paste0(round(mitjana_durada, 1))),
    color = paleta_ivalua[1],
    vjust = 2, hjust = .2,
    show.legend = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))


ggsave("Outputs/inf_prim_baixes_edat_durada.png", width = 7, height = 3.5)

# Baixes per edat ----

baixes_edat <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Dades gràfics/prop_baixa_edat_infprim.dta") %>% 
  mutate(edat = paste0(as.character(edat), "-", as.character(edat + 5)))


baixes_edat %>% 
  ggplot(aes(x = edat,
             y = prop_baixa)) +
  geom_line(size = 1,
            group = 1,
            color = paleta_ivalua[10]) +
  geom_point(size = 2,
             shape = 15,
             color = paleta_ivalua[10]) +
  theme_ivalua() +
  # Nombres al final
  geom_text(
    data = baixes_edat %>% filter(edat %nin% c("25-30", "30-35", "60-65", "65-70")),
    aes(label = paste0(round(prop_baixa*100, 1), " %")),
    color = paleta_ivalua[10],
    vjust = 2.5, hjust = .5,
    show.legend = FALSE) + 
  geom_text(
    data = baixes_edat %>% filter(edat %in% c("25-30", "30-35", "60-65","65-70")),
    aes(label = paste0(round(prop_baixa*100, 1), " %")),
    color = paleta_ivalua[10],
    vjust = -1.2, hjust = .7,
    show.legend = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

ggsave("Outputs/secundària_baixes_edat.png", width = 7, height = 3.5)





