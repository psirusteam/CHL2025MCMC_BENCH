#===============================================================
# Librerías
#===============================================================
library(tidyverse)
library(survey)
library(srvyr)

# Funciones propias
source("../Recurso/data/Modelo_unidad/funciones/plot_validacion.R")
source("../Recurso/data/Modelo_unidad/funciones/calc_theta.R")

options(survey.lonely.psu = "adjust")

#===============================================================
# Cargar resultados del modelo
#===============================================================
estimate_bench <- readRDS(
  "../Recurso/data/Modelo_unidad/11.theta_modelo_ingreso_con_dam.rds"
) %>% 
  transmute(dam, bench = estimate)

modelo <- readRDS(
  "../Recurso/data/Modelo_unidad/10.theta_modelo_ingreso_sin_dam.rds"
) %>% 
  transmute(dam, modelo = estimate)

#===============================================================
# Cargar encuesta y diseño
#===============================================================
encuesta <- readRDS("../Recurso/data/Modelo_unidad/encuesta_mrp.rds")

diseno <- encuesta %>% 
  as_survey_design(
    ids     = upm,
    weights = fep,
    strata  = estrato,
    nest    = TRUE
  )

#===============================================================
# Estimación directa por DAM
#===============================================================
estimate_dir <- diseno %>% 
  group_by(dam) %>% 
  summarise(
    n_sample = unweighted(n()),
    directo  = survey_mean(ingreso, vartype = "ci")
  )

#===============================================================
# Unir estimaciones: directo + modelo + calibrado
#===============================================================
tabla_dam <- estimate_dir %>% 
  inner_join(estimate_bench, by = "dam") %>% 
  inner_join(modelo, by = "dam")

#===============================================================
# Gráfico de validación
#===============================================================
p1 <- plot_validacion(
  tabla    = tabla_dam,
  aggregado = "dam",
  titulo    = "Estimación por DAM calibrada"
)

ggsave(
  filename = "img/02_calib_nac_dam.png",
  plot     = p1,
  width    = 12, 
  height   = 16
)

#===============================================================
# (Opcional) Post-stratificación por iteraciones
#===============================================================
poststrat_df_iter_nacional <- readRDS(
  "../Recurso/data/Modelo_unidad/poststrat_df_iter_ingreso_dam.rds"
)

# calc_theta(result_list = poststrat_df_iter_nacional, levels = "dam", var_n = "n2")$estimates
# calc_theta(result_list = poststrat_df_iter_nacional, levels = "dam", var_n = "n")$estimates

#===============================================================
# Segunda versión del gráfico (si deseas conservarlo)
#===============================================================
p2 <- plot_validacion(
  tabla     = tabla_dam,
  aggregado = "dam",
  titulo    = "Estimación por DAM"
)

ggsave(
  filename = "img/01_calib_nac_dam.png",
  plot     = p2,
  width    = 12, 
  height   = 16
)

# Mostrar en pantalla
plot_validacion(
  tabla     = tabla_dam,
  aggregado = "dam",
  titulo    = "Estimación por DAM"
)
