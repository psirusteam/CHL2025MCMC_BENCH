library(tidyverse)
library(survey)
library(srvyr)

options(survey.lonely.psu = "adjust")

#------------------------------------------
# 1. Base 
#------------------------------------------
encuesta <- readRDS("Recursos/Día1/Recurso/data/encuesta2017CHL.Rds") %>% 
  transmute(
    dam = haven::as_factor(dam_ee ,levels = "values"),
    dam2 = haven::as_factor(comuna ,levels = "values"),
    dam  = stringr::str_pad(dam,  width = 2, pad = "0"),
    dam2 = stringr::str_pad(dam2, width = 5, pad = "0"),
    wkx = `_fep`, 
    upm = `_upm`,
    estrato = `_estrato`,
    ingreso = ingcorte
  )

#------------------------------------------
# 2. Diseño muestral
#------------------------------------------
diseno <- encuesta %>%
  as_survey_design(
    ids     = upm,
    weights = wkx,
    strata  = estrato,
    nest    = TRUE
  )

summary(diseno)
hist(encuesta$ingreso)
hist(log(encuesta$ingreso))
#------------------------------------------
# 3. Número de UPM, observaciones y DEFF por dominio
#------------------------------------------

# Número de UPM por dominio
tabla_upm <- encuesta %>%
  group_by(dam2) %>%
  summarise(
    n_upm = n_distinct(upm),
    n_obs = n(),
    n_obs_posw = sum(wkx > 0)
  )

# Estimación directa del ingreso y DEFF
tabla_ingreso <- diseno %>%
  group_by(dam2) %>%
  summarise(
    ingreso_mean  = survey_mean(ingreso, vartype = c("se", "ci"), na.rm = TRUE,
                                deff = TRUE),
  
  ) %>%
  rename(
    ingreso     = ingreso_mean,
    ingreso_se  = ingreso_mean_se,
    ingreso_lci = ingreso_mean_low,
    ingreso_uci = ingreso_mean_upp
  )

summary(tabla_ingreso$ingreso)
hist(tabla_ingreso$ingreso)
hist(log(tabla_ingreso$ingreso))
#------------------------------------------
# 4. Consolidar tabla final
#------------------------------------------
resultado_final <- tabla_upm %>%
  left_join(tabla_ingreso, by = "dam2")

resultado_final %>% 
saveRDS("Recursos/Día1/Recurso/data/ingreso/estimacion_dir.rds")
