
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")


alumnes_inf <- read.csv2("Dades/Alumnes idescat/alumnes_infantil_1r_cicle_24_25.csv") %>% 
  bind_rows(
    read.csv2("Dades/Alumnes idescat/alumnes_infantil_2n_cicle_24_25.csv"),
    read.csv2("Dades/Alumnes idescat/alumnes_infantil_1r_cicle_23_24.csv"),
    read.csv2("Dades/Alumnes idescat/alumnes_infantil_2n_cicle_23_24.csv"),
    # read.csv2("Dades/Alumnes idescat/alumnes_infantil_1r_cicle_22_23.csv"),
    # read.csv2("Dades/Alumnes idescat/alumnes_infantil_2n_cicle_22_23.csv"),
    read_excel("Dades/Alumnes idescat/alumnes_infantil_1r_cicle_21_22.xlsx") %>% 
      select(-Districte)
    # read.csv2("Dades/Alumnes idescat/alumnes_infantil_2n_cicle_21_22.csv")
    )

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
  theme_ivalua(axis_text_y = T) +
  facet_wrap(~curs)



# Exportar dades Àlex ----


dades_export <- alumnes_inf_ %>% 
  group_by(curs, nivell) %>% 
  summarise(matricules = sum(matricules, na.rm = T)) %>% 
  ungroup


write_dta(
  dades_export,
  "Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/alumnes/primaria/Alumnes_infantil_total.dta"
)


