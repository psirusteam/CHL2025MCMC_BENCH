###############################################################
# Benchmarking para modelos de área
# Autores: Andrés Gutiérrez y Stalyn Guerrero
# 
# Nota: Este código es completamente reproducible y puede ser 
#       utilizado o adaptado siempre que se cite a los autores.
#
# Objetivo:
# Demostrar el procedimiento básico de benchmarking para ajustar
# predicciones de un modelo de área con el fin de cumplir una 
# restricción nacional (total directo observado).
#
# Concepto clave:
# Se ajustan las estimaciones modelo mediante calibración 
# (lineal o logit), garantizando que la suma ponderada coincida 
# con el total observado.    
###############################################################

library(TeachingSampling)
library(sampling)
library(tidyverse)

# Limpieza del entorno
rm(list = ls())

###############################################################
# 1. Datos de entrada
###############################################################


# Vector con tasas predichas por el modelo para cada municipio (entre 0 y 1)
# Estas tasas podrían provenir de cualquier modelo: logístico, beta-binomial,
# Fay-Herriot, MCMC, etc.
y_pred <- c(0.35, 0.42, 0.28, 0.70)

# Tamaño poblacional de cada municipio
# Este corresponde al denominador para el cálculo del número de personas.
N_mpio <- c(500, 600, 400, 200)

# Verificación del total predicho sin ajustar (solo referencia)
# Suma de pobres predicha por el modelo sin benchmarking
sum(y_pred * N_mpio)

# Totales directos nacionales --------------------------------------------------
# Pobreza_dir contiene:
#   [1] = total poblacional nacional
#   [2] = total de personas pobres según estimación directa
#
# En este ejemplo:
#   - Población total nacional = 1700
#   - Pobres observados = 700
Pobreza_dir <- c(1700, 700)

###############################################################
# 2. Calibración (Benchmarking)
###############################################################
# La calibración busca encontrar factores g_k tales que:
#
#   sum_d N_d * (g_k * y_pred_d) = Ingreso_directo_total
#
# preservando al mismo tiempo la estructura relativa de y_pred.
#
# La matriz Xs incluye un intercepto y la predicción del modelo.
###############################################################

# Xs requiere dos columnas:
#   1. Columna de unos para ajustar el total poblacional
#   2. Columna de la tasa predicha para ajustar el total de pobreza
#
# Matriz:
# Xs = [1, y_pred]

Xs <- cbind(1, y_pred)

gk <- calib(
  Xs = Xs,         # Restricciones
  total = Pobreza_dir,  # Totales directos que deben cumplirse
  d = N_mpio,      # Pesos iniciales (tamaños poblacionales)
  method = "logit" # Método recomendado para tasas
)

gk
# gk son los factores de calibración que ajustan las predicciones.

###############################################################
# 3. Cálculo de la tasa ajustada (benchmarking)
###############################################################

# Tasa calibrada por municipio
# y_bench = gk * y_pred
# Ajusta las predicciones cumpliendo las restricciones nacionales.
y_bench <- gk * y_pred
y_bench


###############################################################
# 4. Total nacional después del benchmarking
###############################################################

tot_pobreza_bench <- y_bench * N_mpio
tot_pobreza_bench


###############################################################
# 5. Validación del benchmark
###############################################################
# Se verifica que el total ajustado coincide con el total directo.

sum(tot_pobreza_bench)
round(sum(tot_pobreza_bench)) == Pobreza_dir[2]  # TRUE = validación correcta

round(sum(N_mpio)) == Pobreza_dir[1]  # TRUE = correcto


###############################################################
# Si el valor es TRUE, la calibración fue exitosa.
###############################################################

df_plot <- tibble(dominio = paste0("d", 1:4),
                  modelo  = y_pred,
                  bench   = y_bench)
df_plot

df_plot <-  df_plot %>%  pivot_longer(
  cols = c(modelo, bench),
  names_to = "tipo",
  values_to = "valor"
)

ggplot(df_plot, aes(x = dominio, y = valor, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(title = "Comparación entre modelo área y valores con benchmarking", 
       y = "Ingreso promedio", x = "Dominio") +
  scale_fill_manual(values = c("steelblue", "orange")) +
  theme_minimal()
