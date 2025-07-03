
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")


alumnes_inf <- read.csv2("Dades/alumnes_infantil_1r_cicle.csv") %>% 
  bind_rows(read.csv2("Dades/alumnes_infantil_2n_cicle.csv"))

alumnes_inf_ <- alumnes_inf %>%
  clean_names %>% 
  filter(estudis == "EDUCACIÓ INFANTIL") %>% 
  select(curs, naturalesa, nivell, matricules = alumnes_matriculats) %>% 
  mutate(matricules = as.numeric(matricules)) %>% 
  group_by(curs, nivell, naturalesa) %>% 
  summarise(
    matricules = sum(matricules, na.rm = T)
  ) %>% 
  ungroup 

alumnes_inf_ %>% 
  mutate(nivell = as.factor(nivell)) %>% 
  ggplot(aes(x = matricules)) +
  geom_col(aes(y = reorder(nivell, matricules),
               fill = naturalesa),
           position = "dodge") +
  theme_ivalua(axis_text_y = T)
