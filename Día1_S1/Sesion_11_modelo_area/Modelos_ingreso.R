################################################################################
## Función auxiliar para presentar tablas en formato HTML con estilo uniforme
################################################################################
tba <- function(dat, cap = NA){
  kable(dat,                       # Renderiza data.frame como tabla
        format = "html",
        digits = 4,                # Número de decimales
        caption = cap) %>% 
    kable_styling(bootstrap_options = "striped", 
                  full_width = FALSE) %>%       # Ajusta el ancho
    kable_classic(full_width = FALSE, 
                  html_font = "Arial Narrow")    # Estética clásica
}

################################################################################
## Carga de librerías necesarias para el flujo completo
################################################################################
library(rstan)        # Interfaz para modelos Stan
library(knitr)        # Salida de tablas y gráficos
library(kableExtra)   # Formateo de tablas
library(tidyverse)    # Manipulación de datos
library(magrittr)     # Operador %<>%
library(bayesplot)    # Diagnósticos MCMC
library(posterior)    # Manejo de draws MCMC
library(patchwork)    # Composición de gráficos

################################################################################
## Lectura y preparación de la base del modelo Fay-Herriot
################################################################################
base_FH <- readRDS("Día1_S1/Recurso/data/Modelo_area/ingreso/base_FH.rds") %>% 
  transmute(
    dam2,                    # Identificador del dominio a 4 dígitos
    nd = n_obs,              # Tamaño de muestra directa
    ingreso,                 # Estimación directa
    vardir = ingreso_se^2,   # Varianza directa
    hat_var                  # Varianza modelada (preestimada)
  )

# Vista rápida de los datos iniciales
head(base_FH) %>% tba()

################################################################################
## Lectura de predictores auxiliares a nivel dominio y estandarización
## El escalamiento permite una mejor estabilidad del muestreador MCMC
################################################################################
statelevel_predictors_df <- readRDS(
  "Día1_S1/Recurso/data/Modelo_area/ingreso/statelevel_predictors_df_dam2.rds"
) %>% 
  mutate_at(
    .vars = c("luces_nocturnas", "cubrimiento_cultivo", "cubrimiento_urbano",
              "modificacion_humana", "accesibilidad_hospitales",
              "accesibilidad_hosp_caminado"),
    function(x) as.numeric(scale(x))  # Estandarización
  )

################################################################################
## Unión de bases: variables directas + predictores auxiliares
################################################################################
base_FH <- full_join(base_FH, statelevel_predictors_df, by = "dam2")

tba(base_FH[1:5,1:8])   # Vista preliminar

################################################################################
## Separación entre:
##  data_dir = dominios con estimación directa
##  data_syn = dominios sin información (áreas sintéticas)
################################################################################
data_dir <- base_FH %>% filter(!is.na(ingreso))
data_syn <- base_FH %>% anti_join(data_dir %>% select(dam2))

tba(data_syn[1:10,1:8] %>% head())

################################################################################
## Definición de fórmula del modelo de unidad que alimentará a FH
## Este modelo captura estructura socio-demográfica de apoyo para el modelo FH
################################################################################
formula_mod <- formula(
  ~ dam + sexo2 + anoest2 + anoest3 + anoest4 +
    edad2 + edad3 + edad4 + edad5 +
    etnia1 + etnia2 + tasa_desocupacion
)

library(stringr)

# Extrae dam a 2 dígitos como variable categórica auxiliar
data_dir %<>% mutate(dam = str_sub(dam2, 1, 2))
data_syn %<>% mutate(dam = str_sub(dam2, 1, 2))

################################################################################
## Construcción de matrices de diseño X (muestral) y Xs (no muestral)
## Notar que deben tener idénticas columnas para Stan
################################################################################
Xdat <- model.matrix(formula_mod, data = data_dir)
Xs   <- model.matrix(formula_mod, data = data_syn)

# Ajuste para asegurar que Xdat y Xs tengan mismas variables
temp <- setdiff(colnames(Xdat), colnames(Xs))
temp <- matrix(0, nrow = nrow(Xs), ncol = length(temp),
               dimnames = list(1:nrow(Xs), temp))

Xs <- cbind(Xs, temp)[, colnames(Xdat)]  # Reordena columnas para que coincidan

################################################################################
## Construcción de la lista de datos para Stan
################################################################################
sample_data <- list(
  N1 = nrow(Xdat),                  # Número de dominios con información directa
  N2 = nrow(Xs),                    # Dominios sin información
  p  = ncol(Xdat),                  # Número de covariables
  X  = as.matrix(Xdat),
  Xs = as.matrix(Xs),
  y  = as.numeric(data_dir$ingreso),
  sigma_e = sqrt(data_dir$hat_var)  # Desviación estándar directa
)

################################################################################
## Compilación y ajuste del modelo Fay-Herriot Normal en Stan
################################################################################
fit_FH_normal <- "Día1_S1/Recurso/data/Modelo_area/ingreso/modelosStan/17FH_normal.stan"

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

model_FH_normal <- stan(
  file = fit_FH_normal,
  data = sample_data,
  warmup = 9500,                # Periodo de calentamiento
  iter = 10000,                 # Iteraciones totales
  cores = 4,
  verbose = FALSE
)

# Guardar ajuste
saveRDS(model_FH_normal,
        "Día1_S1/Recurso/data/Modelo_area/ingreso/model_FH_normal.rds")

# Recargar (si es necesario)
model_FH_normal <- readRDS(
  "Día1_S1/Recurso/data/Modelo_area/ingreso/model_FH_normal.rds"
)

################################################################################
## Diagnóstico del ajuste mediante PPC
################################################################################
y_pred_B <- as.array(model_FH_normal, pars = "theta") %>%
  as_draws_matrix()

rowsrandom <- sample(nrow(y_pred_B), 500)  # Muestra para visualizar densidad
y_pred2 <- y_pred_B[rowsrandom, ]

p1 <- ppc_dens_overlay(
  y = as.numeric(data_dir$ingreso),
  y_pred2
) + theme_light()

ggsave("img/FH1.png", plot = p1, width = 12, height = 16)
knitr::include_graphics("img/FH1.png")

################################################################################
## Diagnóstico para sigma²_u (heterogeneidad entre áreas)
################################################################################
posterior_sigma2_u <- as.array(model_FH_normal, pars = "sigma2_u")

p1 <- (mcmc_areas(posterior_sigma2_u) + theme_light()) /
  (mcmc_trace(posterior_sigma2_u) + theme_light())

ggsave("img/FH2.png", plot = p1, width = 12, height = 16)
knitr::include_graphics("img/FH2.png")

################################################################################
## Construcción de estimaciones modelo FH para áreas con información directa
################################################################################
theta <- summary(model_FH_normal, pars = "theta")$summary %>% data.frame()

data_dir %<>% mutate(
  thetadir = ingreso,                 # Estimación directa
  theta_pred = theta$mean,            # Predicción del modelo
  theta_pred_EE = theta$sd,           # Error estándar modelo
  Cv_theta_pred = theta_pred_EE / theta_pred
)

# Comparación gráfico: directa vs modelo
p22 <- ggplot(data_dir, aes(x = thetadir, y = theta_pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  theme_bw(10)

ggsave("img/FH3.png", plot = p22, scale = 2)
knitr::include_graphics("img/FH3.png")

################################################################################
## Estimaciones para áreas sin información (predicción pura)
################################################################################
theta_syn_pred <- summary(model_FH_normal, pars = "y_pred")$summary %>% 
  data.frame()

data_syn <- data_syn %>%
  mutate(
    theta_pred = theta_syn_pred$mean,
    theta_pred_EE = theta_syn_pred$sd,
    Cv_theta_pred = theta_pred_EE / theta_pred
  )

# Guardar resultados
saveRDS(data_syn, "Día1_S1/Recurso/data/Modelo_area/ingreso/data_syn.rds")
saveRDS(data_dir, "Día1_S1/Recurso/data/Modelo_area/ingreso/data_dir.rds")

################################################################################
## Tabla resumen de áreas sintéticas
################################################################################
tba(
  data_syn %>% slice(1:10) %>% 
    select(dam2:hat_var, theta_pred:Cv_theta_pred)
)

################################################################################
## Consolidación final de estimaciones FH
################################################################################
estimacionesPre <- bind_rows(data_dir, data_syn) %>% 
  select(
    dam2,
    estimacion_normal = theta_pred,
    ee_normal = theta_pred_EE,
    cv_normal = Cv_theta_pred
  ) %>% 
  mutate(dam = substr(dam2, 1, 2))

saveRDS(estimacionesPre,
        file = "Día1_S1/Recurso/data/Modelo_area/ingreso/estimacionesPre_normal.rds")
