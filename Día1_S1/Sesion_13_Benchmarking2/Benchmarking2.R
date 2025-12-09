################################################################################
## Función auxiliar para formatear tablas HTML con estética estándar
################################################################################
tba <- function(dat, cap = NA){
  kable(dat,
        format = "html", digits = 4,
        caption = cap) %>% 
    kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
    kable_classic(full_width = FALSE, html_font = "Arial Narrow")
}

################################################################################
## Carga de librerías utilizadas en todo el flujo de FH + Benchmarking
################################################################################
library(rstan)        # ajuste de modelos Bayesianos Stan
library(knitr)        # reporte
library(kableExtra)   # formato de tablas
library(tidyverse)    # manipulación de datos
library(magrittr)     # operador %<>%
library(bayesplot)    # diagnósticos MCMC
library(posterior)    # manejo de draws
library(patchwork)    # combinar gráficos
library(sampling)     # calib() para benchmarking

################################################################################
## 1. LECTURA DE TOTALES CENSALES POR DOMINIO (DAM2)
##    Esta información será usada para el benchmarking (total populacional)
################################################################################
total_persona <- readRDS(
  "Día1_S1/Recurso/data/Modelo_area/MCMC/total_personas_dam2.rds"
) %>% 
  group_by(dam)

head(total_persona, 10) %>% tba()

################################################################################
## 2. PREPARACIÓN DE LA ENCUESTA 2017 CHILE PARA ESTIMACIÓN DIRECTA
##    Se armonizan variables: dam, dam2, pesos, UPM, estrato, ingreso
################################################################################
set_data <- readRDS("Día1_S1/Recurso/data/encuesta2017CHL.Rds") %>% 
  transmute(
    dam   = haven::as_factor(dam_ee,   levels = "values"),
    dam2  = haven::as_factor(comuna,   levels = "values"),
    dam   = str_pad(dam,  width = 2, pad = "0"),   # asegura formato 2 dígitos
    dam2  = str_pad(dam2, width = 5, pad = "0"),   # asegura formato 5 dígitos
    wkx   = `_fep`,         # factor de expansión
    upm   = `_upm`,         # unidad primaria
    estrato = `_estrato`,   # estrato de diseño
    ingreso = ingcorte      # variable de ingreso
  ) %>% 
  ungroup()

################################################################################
## 3. CREACIÓN DEL DISEÑO MUESTRAL COMPLEJO
################################################################################
options(survey.lonely.psu = "adjust")
library(survey)
library(srvyr)

diseno <- as_survey_design(
  ids     = upm,
  weights = wkx,
  strata  = estrato,
  nest    = TRUE,
  .data   = set_data
)

################################################################################
## 4. ESTIMACIONES DIRECTAS POR DOMINIO (DAM)
##    Se calculan:
##     - den_dir: total de personas por DAM
##     - num_dir: total de ingreso
##     - theta_dir: ingreso medio directo con IC
################################################################################
directoDam <- diseno %>% 
  group_by(dam) %>% 
  summarise(
    den_dir   = survey_total(vartype = NULL),      
    num_dir   = survey_total(ingreso, vartype = NULL),
    theta_dir = survey_mean(ingreso, vartype = c("ci"))
  )

head(directoDam, 5) %>% tba()

################################################################################
## 5. CARGA DE ESTIMACIONES FH (modelo normal en log) PREVIAMENTE OBTENIDAS
################################################################################
estimacionesPre <- readRDS(
  "Día1_S1/Recurso/data/Modelo_area/ingreso/estimacionesPre_normal_log.rds"
)

# Se seleccionan variables relevantes y se unen totales poblacionales por DAM2
estimacionesPre <- estimacionesPre %>% 
  select(dam, dam2, estimacion_normal) %>% 
  inner_join(total_persona, by = c("dam2", "dam"))

head(estimacionesPre, 5) %>% tba()

################################################################################
## 6. CONSTRUCCIÓN DEL VECTOR DE TOTALES DIRECTOS (DEN Y NUM)
##    Este será el objetivo del benchmarking: respetar totales directos
################################################################################
totales <- c(directoDam$den_dir, directoDam$num_dir)

names(totales) <- c(
  paste0("Den_DAM_", directoDam$dam),
  paste0("Num_DAM_", directoDam$dam)
)

################################################################################
## 7. MATRIZ INDICADORA POR DAM (one-hot encoding)
################################################################################
X_dam <- model.matrix(~ dam - 1, data = estimacionesPre)

################################################################################
## 8. MATRIZ DE CALIBRACIÓN (Xs)
##    Contiene:
##     - Indicador por DAM (Den)
##     - Indicador * estimación FH (Num)
################################################################################
Xs <- cbind(
  Den_DAM = X_dam,
  Num_DAM = X_dam * estimacionesPre$estimacion_normal
)

colnames(Xs) <- names(totales)

# Vista preliminar para algunas DAM
Xs %>% 
  data.frame() %>% 
  select(matches("01|02|03")) %>% 
  head(10) %>% 
  tba()

################################################################################
## 9. LECTURA DEL MODELO FH COMPLETO EN LOG (ajuste posterior)
################################################################################
model_FH_normal <- readRDS(
  "Día1_S1/Recurso/data/Modelo_area/ingreso/model_FH_normal_loging.rds"
)

################################################################################
## 10. EXTRACCIÓN DE DRAWS MCMC: thetaFH (áreas muestrales) y y_pred (no muestrales)
##     Se transforman exp() para volver al dominio original
################################################################################
extra <- rstan::extract(model_FH_normal, pars = c("thetaFH", "y_pred"))

thetaFH_df <- as_draws_matrix(exp(extra$thetaFH)) %>% as.data.frame()
y_pred_draws <- as_draws_matrix(exp(extra$y_pred)) %>% as.data.frame()

temp_draws <- bind_cols(thetaFH_df, y_pred_draws)
colnames(temp_draws) <- estimacionesPre$dam2

# Se pasa a formato largo → ancho por iteración
temp_draws <- temp_draws %>%
  mutate(iter = paste0("iter_", row_number())) %>% 
  pivot_longer(cols = all_of(estimacionesPre$dam2),
               names_to = "dam2",
               values_to = "value") %>% 
  pivot_wider(names_from = iter, values_from = value)

################################################################################
## 11. BENCHMARKING ITERATIVO SOBRE TODAS LAS ITERACIONES MCMC
##     Para cada iteración:
##        (a) Construye matriz Xs con predicción de esa iteración
##        (b) Aplica calib(linear) usando totales directos
##        (c) Obtiene ingreso benchmarkeado: gk * estimacion_normal
################################################################################

n_iter = 2000                                         # cantidad de iteraciones a usar
lista_res <- vector("list", n_iter)                  # almacena resultados por iteración
iter_cols <- paste0("iter_", seq_len(n_iter))        # nombres de columnas iterativas

for (i in seq_len(n_iter)) {
  
  iter_name <- iter_cols[i]                          # columna con draws de esta iteración
  
  # Xs para esta iteración: indicador por DAM + ingreso FH_iter
  Xs <- cbind(
    Den_DAM = X_dam,
    Num_DAM = X_dam * temp_draws[[iter_name]]
  )
  
  colnames(Xs) <- names(totales)
  
  # Calibración lineal para cumplir totales directos
  gk <- calib(
    Xs    = Xs,
    total = totales,
    d     = estimacionesPre$total_pp,   # totales poblacionales censales
    method = "linear"
  )
  
  # ingreso benchmarkeado para esta iteración
  lista_res[[i]] <- data.frame(
    iter    = i,
    dam2    = estimacionesPre$dam2,
    y_bench = gk * estimacionesPre$estimacion_normal
  )
}

################################################################################
## 12. CONSOLIDACIÓN DE TODAS LAS ITERACIONES MCMC BENCHMARKEADAS
################################################################################
df_all <- bind_rows(lista_res)

# Resumen por dominio:
#   - media posterior
#   - sd posterior
#   - IC 95%
#   - coeficiente de variación
resumen <- df_all %>%
  group_by(dam2) %>%
  summarise(
    mean = mean(y_bench, na.rm = TRUE),
    sd   = sd(y_bench, na.rm = TRUE),
    lci  = quantile(y_bench, 0.025, na.rm = TRUE),
    uci  = quantile(y_bench, 0.975, na.rm = TRUE),
    cve  = (sd / mean) * 100,
    .groups = "drop"
  )

head(resumen, 10) %>% inner_join(estimacionesPre) %>% arrange(mean) %>% tba()

################################################################################
## 13. BENCHMARKING ALTERNATIVO LOGIT (FUNCIÓN EXTERNA)
################################################################################
source("Benchmarking_area.R")

res_logit <- Benchmarking_area(
  temp_draws      = temp_draws,
  X_dam           = X_dam,
  totales         = totales,
  estimacionesPre = estimacionesPre,
  n_iter          = 2000,
  calib_method    = "linear",
  parallel        = FALSE,
  n_cores         = 4
)

head(res_logit$resumen, 10) %>% inner_join(estimacionesPre) %>% arrange(mean) %>% tba()

################################################################################
## 14. AGRUPACIÓN POR DAM Y COMPARACIÓN: FH, FH-Bench, Directo
################################################################################
tab_resul <- inner_join(res_logit$resumen, estimacionesPre, by = "dam2") %>%
  mutate(
    dam = str_sub(dam2, 1, 2),
    wi  = mean / estimacion_normal
  ) %>%
  group_by(dam) %>%
  summarise(
    estimacion_normal_bench = weighted.mean(estimacion_normal, total_pp * wi),
    estimacion_normal       = weighted.mean(estimacion_normal, total_pp)
  ) %>%
  inner_join(directoDam, by = "dam") %>% 
  data.frame()

tab_resul %>% head(10) %>%
  select(dam, estimacion_normal, estimacion_normal_bench, theta_dir) %>% tba()

################################################################################
## 15. GRÁFICO FINAL DE COMPARACIÓN ENTRE:
##         - Modelo FH normal
##         - Modelo FH benchmarkeado
##         - Estimación directa + intervalos de confianza
################################################################################
df_long <- tab_resul %>% 
  pivot_longer(
    cols = c(
      estimacion_normal, 
      estimacion_normal_bench, 
      theta_dir
    ),
    names_to = "tipo",
    values_to = "estimacion"
  )

ggplot(df_long, aes(x = dam, y = estimacion, color = tipo, group = tipo)) +
  geom_line(aes(y = theta_dir_low), linetype = 2) +
  geom_line(aes(y = theta_dir_upp), linetype = 2) +
  geom_point(aes(color = tipo, shape = tipo), size = 2) +
  scale_color_manual(
    values = c(
      "estimacion_normal" = "blue",
      "estimacion_normal_bench" = "red",
      "theta_dir" = "black"
    ),
    labels = c(
      "estimacion_normal" = "Modelo (Normal)",
      "estimacion_normal_bench" = "Modelo Benchmarked",
      "theta_dir" = "Estimación Directa"
    )
  ) +
  labs(
    title = "Comparación de Estimaciones por DAM",
    x = "DAM",
    y = "Estimación",
    color = "Tipo de Estimación",
    shape = "Tipo de Estimación"
  ) +
  scale_shape_manual(
    values = c(
      "estimacion_normal" = 16,
      "estimacion_normal_bench" = 17,
      "theta_dir" = 20
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
