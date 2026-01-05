knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE
)

tba <- function(dat, cap = NA){
  kable(dat,
      format = "html", digits =  4,
      caption = cap) %>% 
     kable_styling(bootstrap_options = "striped", full_width = F)%>%
         kable_classic(full_width = F, html_font = "Arial Narrow")
}
library(rstan)
library(knitr)
library(kableExtra)
library(tidyverse)
library(magrittr)
library(bayesplot)
library(posterior)
library(patchwork)
library(survey)
library(srvyr)
library(haven)

conteo_pp_dam <- readRDS("../Recurso/Modelo_area/censo_dam2.rds") %>%
  filter(edad > c(2:4))  %>% 
  group_by(dam , dam2) %>% 
  summarise(N_mpio = sum(n), .groups = "drop") %>% 
  group_by(dam) %>% 
  mutate(N_dam = sum(N_mpio))

head(conteo_pp_dam) %>% tba()

set_data <- readRDS('../Recurso/encuesta2017CHL.Rds')

length_upm <- max(nchar(set_data[["_upm"]]))
length_estrato <- max(nchar(set_data[["_estrato"]]))
id_dominio <- "dam2"

set_data <- set_data %>%
  transmute(
    dam = as_factor(dam_ee, levels = "values"),
    dam = str_pad(string = dam, width = 2, pad = "0"),
    dam2 = as_factor(comuna, levels = "values"),
    dam2 = str_pad(string = dam2, width = 5, pad = "0"),
    nombre_dam = as_factor(dam_ee, levels = "labels"),
    nombre_dam2 = as_factor(comuna, levels = "labels"),
    upm = str_pad(`_upm`, width = length_upm, pad = "0"),
    estrato = str_pad(`_estrato`, width = length_estrato, pad = "0"),
    fep = `_fep`,
    empleo = condact3
  )

options(survey.lonely.psu = 'adjust')

diseno <- set_data %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = fep,
    nest = TRUE
  )

indicador_agregado <-
  diseno %>% 
  group_by_at("dam") %>% 
  filter(empleo %in% c(1:3)) %>% 
  summarise(
    Den = survey_total(vartype = NULL),
    tot_Desocupado = survey_total(empleo  %in% c(2), vartype = NULL),
    tot_Ocupado = survey_total(empleo  %in% c(1), vartype = NULL),
    tot_Inactivo = survey_total(empleo  %in% c(3), vartype = NULL)
    )

tba(indicador_agregado)

temp <- gather(indicador_agregado, key = "agregado", value = "estimacion", -dam) %>%
  mutate(nombre = paste0("dam_", dam, "_", agregado))

Razon_empleo <- setNames(temp$estimacion, temp$nombre)

theta_obs_ordenado <- readRDS("../Recurso/Modelo_area/Predi_obs_theta.rds")
theta_pred_ordenado <- readRDS("../Recurso/Modelo_area/Predi_no_obs_theta.rds")
id_dam <- bind_rows(theta_obs_ordenado, theta_pred_ordenado) %>% pull(dam2)
fit <- readRDS("../Recurso/Modelo_area/fit_multinomial_no_cor.Rds")
P <- 3
D <- nrow(theta_obs_ordenado)

# Extraemos TODAS las muestras MCMC de theta
set.seed(1234)
theta_draws <- rstan::extract(fit, pars = "theta")$theta  
theta_pred_draws <- rstan::extract(fit, pars = "theta_pred")$theta_pred  

# Dimensión esperada: draws × (D*P)
S <- nrow(theta_draws)



source("../Recurso/Modelo_area/funciones/benchmark_mcmc_multinomial.r")

res_bench <- benchmark_mcmc_multinomial(
  fit = fit,
  theta_obs_ordenado = theta_obs_ordenado,
  theta_pred_ordenado = theta_pred_ordenado,
  conteo_pp_dam = conteo_pp_dam,
  Razon_empleo = Razon_empleo,
  method = "linear"
)



theta_ocupado <- resumir_bench_multinomial(
  res_bench   = res_bench,
  group_vars  =  "dam",
  vars        = "Ocupado_pred",
  n = "N_mpio", 
  gk = "gk_Ocupado",
  ci_level    = 0.95
)

theta_desocupado <- resumir_bench_multinomial(
  res_bench   = res_bench,
  group_vars  =  "dam",
  vars        = "Desocupado_pred",
  n = "N_mpio", 
  gk = "gk_desocupado",
  
  ci_level    = 0.95
)

theta_Inactivo <- resumir_bench_multinomial(
  res_bench   = res_bench,
  group_vars  =  "dam",
  vars        = "Inactivo_pred",
  n = "N_mpio", 
  gk = "gk_sim",
  ci_level    = 0.95
)

theta_resumen_bench <- bind_rows(theta_Inactivo, 
                                theta_desocupado,
                                theta_ocupado) %>% 
separate(col = categoria, into = c("categoria", "tipo")) %>% 
  transmute(
 dam,  categoria,  Pred = estimate
) 

theta_resumen <- theta_resumen_bench %>% 
  pivot_wider(
    names_from  = categoria,
    values_from = Pred
  )


theta_resumen %>% head(10) %>% tba()


indicador_agregado <-
  diseno %>% 
    filter(empleo %in% c(1:3)) %>% 
  group_by(dam, empleo) %>% 
  summarise(
    P_dir = survey_mean(vartype = "ci")
    ) %>% 
   mutate(categoria = haven::as_factor(empleo)) %>% 
    transmute(
    dam,
    categoria,
    estimate = P_dir,
    lwr = P_dir_low,
    upr = P_dir_upp
  )


df_plot <- inner_join(theta_resumen_bench, indicador_agregado)

df_plot %>% head(10) %>% tba()

df_long_plot <- df_plot %>%   # usa el nombre real de tu tabla
  pivot_longer(
    cols = c(Pred , estimate),   # estimate = Directo
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(tipo,
                  "estimate" = "Directo",
                  "Bench"    = "Pred")
  )

ggplot(df_long_plot, 
       aes(x = dam, y = valor, color = tipo)) +

  # Intervalos de confianza SOLO para Directo
  geom_errorbar(
    data = df_long_plot %>% filter(tipo == "Directo"),
    aes(ymin = lwr, ymax = upr),
    width = 0.12,
    linewidth = 0.3
  ) +

  # Puntos de cada estimación
   geom_jitter(size = 1.1, width = 0.08, height = 0) + 

  facet_grid(categoria ~ ., scales = "free_y") +

  scale_color_manual(values = c(
    "Directo" = "black",
    "Pred"    = "#1f78b4",
    "Bench"   = "#33a02c"
  )) +

  labs(
    title = "Comparación de Directo, Predicho y Bench por DAM",
    x = "",
    y = "Tasa"
  ) +

  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


data_TO <- inner_join(
  res_bench$resultados_iter[[1]] %>%
    group_by(dam) %>%
    summarise(Bench = weighted.mean(Ocupado_pred, N_mpio * gk_ocupado))
  ,
  diseno %>% group_by(dam) %>%
    filter(empleo %in% c(1:3)) %>%
    summarise(directo = survey_mean(empleo %in% c(1), vartype = "ci")),
  by = "dam"
) %>% mutate(tipo = "TO")

data_TP <-
  inner_join(
    res_bench$resultados_iter[[1]] %>%
      group_by(dam) %>%
      summarise(Bench = weighted.mean(TP_Bench, N_mpio))
    ,
    diseno %>% group_by(dam) %>%
      filter(empleo %in% c(1:3)) %>%
      summarise(directo = survey_mean(empleo %in% c(1, 2), vartype = "ci")),
    by = "dam"
  ) %>% mutate(tipo = "TP")


data_TD <-
  inner_join(
res_bench$resultados_iter[[1]] %>% 
  group_by(dam) %>% 
  summarise(Bench = weighted.mean(TD_Bench, N_mpio))
,
diseno %>% group_by(dam) %>% 
  filter(empleo %in% c(1:3)) %>% 
  summarise(TD = survey_ratio(empleo %in% c(2),  empleo %in% c(1,2), vartype = "ci")),
by = "dam"
  ) %>% mutate(tipo = "TD")





theta_TP <- resumir_bench_multinomial(
  res_bench   = res_bench,
  group_vars  =  "dam2",
  vars        = "TP_Bench",
  n = "N_mpio", 
  gk = "gk",
  ci_level    = 0.95
)

theta_TD <- resumir_bench_multinomial(
  res_bench   = res_bench,
  group_vars  =  "dam2",
  vars        = "TD_Bench",
  n = "N_mpio", 
  gk = "gk",
  
  ci_level    = 0.95
)

theta_TO <- resumir_bench_multinomial(
  res_bench   = res_bench,
  group_vars  =  "dam2",
  vars        = "TO_Bench",
  n = "N_mpio", 
  gk = "gk",
  ci_level    = 0.95
)

theta_resumen_bench <- bind_rows(theta_TP, theta_TD, theta_TO) %>%
  separate(col = categoria, into = c("categoria", "tipo")) %>%
  transmute(dam2, categoria, Pred = estimate) 

theta_resumen_bench %>% 
  pivot_wider(
    names_from  = categoria,
    values_from = Pred
  )


Tasa_resumen %>% head(10) %>% tba()


# 1. VARIABLES INDICADORAS
dis0 <- diseno %>% 
  mutate(
    O = ifelse(empleo == 1, 1, 0),
    D = ifelse(empleo == 2, 1, 0),
    I = ifelse(empleo == 3, 1, 0),
    PEA = ifelse(empleo %in% c(1,2), 1, 0)
  )

# 2. TASAS POR DAM
tasas_dam <- dis0 %>% 
  group_by(dam) %>% 
  summarise(
    # Proporciones simples
    TO  = survey_mean(O, vartype = "ci"),                       # Ocupación
    TD  = survey_ratio(numerator = D, denominator = PEA, 
                       vartype = "ci"),                         # Desocupación
    TP  = survey_ratio(numerator = O + D, denominator = O + D + I,
                       vartype = "ci")                          # Participación
  )

tasas_long <- tasas_dam %>%
  tidyr::pivot_longer(
    cols = c(TO, TD, TP),
    names_to = "tasa",
    values_to = "estimate"
  ) %>%
  mutate(
    lwr = case_when(
      tasa == "TO" ~ TO_low,
      tasa == "TD" ~ TD_low,
      tasa == "TP" ~ TP_low
    ),
    upr = case_when(
      tasa == "TO" ~ TO_upp,
      tasa == "TD" ~ TD_upp,
      tasa == "TP" ~ TP_upp
    )
  ) %>%
  transmute(dam, categoria   =  tasa,estimate, lwr, upr) 


df_plot <- inner_join(theta_resumen_bench, tasas_long)

df_plot %>% head(10) %>% tba()

df_long_plot <- df_plot %>%   # usa el nombre real de tu tabla
  pivot_longer(
    cols = c(Pred,  estimate),   # estimate = Directo
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(tipo,
                  "estimate" = "Directo",
                  "Pred"     = "Pred",
                  "Bench"    = "Pred")
  )

ggplot(df_long_plot, 
       aes(x = dam, y = valor, color = tipo)) +

  # Intervalos de confianza SOLO para Directo
  geom_errorbar(
    data = df_long_plot %>% filter(tipo == "Directo"),
    aes(ymin = lwr, ymax = upr),
    width = 0.12,
    linewidth = 0.3
  ) +

  # Puntos de cada estimación
   geom_jitter(size = 1.1, width = 0.08, height = 0) + 

  facet_grid(categoria ~ ., scales = "free_y") +

  scale_color_manual(values = c(
    "Directo" = "black",
    "Pred"    = "#1f78b4",
    "Bench"   = "#33a02c"
  )) +

  labs(
    title = "Comparación de Directo, Predicho y Bench por DAM",
    x = "",
    y = "Tasa"
  ) +

  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


