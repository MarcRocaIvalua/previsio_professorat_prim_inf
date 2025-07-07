
# Importar llibreries

source("Scripts/0. Libs.R")
source("Scripts/0. Funcions.R")


# Tenim publics i privats? ----

# Dades alumnes

alumnes_raw <- read_dta("Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/alumnes/primaria/alumnes_panell.dta")

# Dades obertes de naturalesa dels centres (públics privats)

dades_obertes <- read_excel("D:/previsio_professorat_prim_inf/Dades/Centres_educatius.xlsx",
                                  sheet = "TOTCAT_CENTRES_EDUCATIUS") %>% 
  select(codi_centre = Codi_centre,
         nom_naturalesa = Nom_naturalesa) %>% 
  mutate(codi_centre = as.numeric(codi_centre))

## Centres dels alumnes

alumnes_naturalesa_centre <- alumnes_raw %>% 
  left_join(dades_obertes) 

## Proporció de missings

alumnes_naturalesa_centre %>% 
  count(nom_naturalesa) %>% 
  mutate(share = n/sum(n)*100)


# Exportar dades ----

write_dta(
  alumnes_naturalesa_centre,
  "Z:/19137_Unitat_Segura_Dades/2025_PROFESSORAT/Stata/alumnes/primaria/alumnes_panell.dta"
)
