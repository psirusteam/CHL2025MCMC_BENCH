library(tidyverse)
library(survey)
library(srvyr)
library(lmtest)
library(ggfortify)
library(scales)


#------------------------------------------
# 1. Base 
#------------------------------------------
resultado_dir <- 
  readRDS("Día1_S1/Recurso/data/Modelo_area/ingreso/estimacion_dir.rds")


base_fgv <- resultado_dir %>%
  filter(n_upm > 4, ingreso_se > 0, !is.na(ingreso_se), 
         ingreso_mean_deff  > 1) %>%
  transmute(dam2 , n_obs, ingreso, vardir = ingreso_se^2,
            log_var =  log(ingreso_se^2), 
            deff = ingreso_mean_deff )



base_fgv %>%
  ggplot(aes(x = n_obs, y = log_var)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  scale_y_log10() +
  labs(
    title = "Relación entre tamaño muestral y varianza directa",
    x = "Número de observaciones (n_obs)",
    y = "Log varianza del ingreso (escala log)"
  ) +
  theme_minimal()


base_fgv %>%
  ggplot(aes(x = log(ingreso), y = log_var)) +
  geom_point(alpha = 0.7, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_log10() +
  labs(
    title = "Relación entre ingreso medio y varianza directa",
    x = "Ingreso medio directo",
    y = "Varianza del ingreso (log)"
  ) +
  theme_minimal()

base_fgv %>% 
  ggplot(aes(x = log(n_obs), y = log_var)) +
  geom_point(
      alpha = 0.7
  ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c() +
  labs(
    title = "Relación conjunta: n_obs, ingreso y varianza",
    x = "Número de observaciones",
    y = "Ingreso medio"
  ) +
  theme_minimal()


#----------------------------------------------------
# 2. Ajuste de la FGV (modelo log-lineal)
#----------------------------------------------------
# Modelo log-lineal de la FGV

# log(Var_dir) = a0 + a1*log(n_obs) + a2*log(ingreso)
fgv_fit <- lm(
  log_var ~ log(ingreso) + n_obs,
  data = base_fgv
)

summary(fgv_fit)


## Determinar el valor de la constante delta. 

delta.hat = sum(base_fgv$vardir) / sum(exp(fitted.values(fgv_fit)))
delta.hat

baseFGV <- 
  base_fgv %>% ungroup() %>% mutate(hat_var = delta.hat * exp(fitted.values(fgv_fit)))

diag_model <- autoplot(fgv_fit)
diag_model

dwtest(fgv_fit)
bptest(fgv_fit)
shapiro.test(residuals(fgv_fit))



#Plot2
g1 <- ggplot(baseFGV, 
             aes(x = vardir, y = hat_var)) + 
  geom_point() +
  geom_smooth(method = "loess")
g1

#------------Unir las estimaciones con la data original-------------
base_sae <- left_join(resultado_dir,
                      baseFGV %>% select(all_of("dam2"), hat_var), 
                      by = "dam2") 


base_FH <- base_sae %>%
  mutate(
    deff = ifelse(is.nan(ingreso_mean_deff  ), 1, ingreso_mean_deff  ),
    deff_FGV = ifelse(ingreso_se^2  == 0 ,
                      1,
                      hat_var / (ingreso_se^2  / deff) #Fórmula del nuevo DEFF
    ),
    # Criterio MDS para regularizar el DeffFGV
    deff_FGV = ifelse(deff_FGV <= 0.8 , NA_real_, deff_FGV), #Deff estimado
    n_eff_FGV = n_obs  / deff_FGV, #Número efectivo de personas encuestadas
    #hat_var = ifelse(deff_FGV <= 1 , NA_real_, hat_var), #Si no se estimó varianza para ese municipio, también excluir la estimación directa de este municipio, esto es relevante para el modelo FH 
    ingreso  = ifelse(is.na(hat_var), NA_real_, ingreso ) ,
    ingreso_se  = ifelse(is.na(hat_var), NA_real_, ingreso_se ) 
  )

base_FH %>% 
  saveRDS("Recursos/Día1/Recurso/data/Modelo_area/ingreso/base_FH.rds")
