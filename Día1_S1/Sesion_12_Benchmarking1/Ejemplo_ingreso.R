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

# Ingreso promedio predicho por municipio (positivo)
# Representa el resultado previo de un modelo FH u otro modelo de área.
y_pred <- c(3500, 4200, 2800, 5500)

# Tamaño poblacional por municipio
# Este vector contiene la población o tamaños de dominio.
N_mpio <- c(500, 600, 400, 200)

# Ingreso total predicho por el modelo
sum(y_pred * N_mpio)

# Total directo observado a nivel nacional.
# Aquí se supone que el ingreso medio directo es 4000
# y que la población total es la suma de N_mpio = 1700.
Ingreso_dir_total <- c(
  sum(N_mpio),                 # Total de población
  4000 * sum(N_mpio)           # Total de ingreso DIRECTO observado
)

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

Xs <- cbind(1, y_pred)

gk <- calib(
  Xs    = Xs,
  total = Ingreso_dir_total,
  d     = N_mpio,
  method = "linear"   # método de calibración (lineal)
)

gk
# gk son los factores de calibración que ajustan las predicciones.

###############################################################
# 3. Estimación calibrada por municipio
###############################################################

y_bench <- gk * y_pred
y_bench
# Estos valores corresponden al ingreso ajustado (benchmarking).

###############################################################
# 4. Total nacional después del benchmarking
###############################################################

tot_ingreso_bench <- y_bench * N_mpio
tot_ingreso_bench

###############################################################
# 5. Validación del benchmark
###############################################################
# Se verifica que el total ajustado coincide con el total directo.

sum(tot_ingreso_bench)
round(sum(tot_ingreso_bench)) == round(Ingreso_dir_total[2])
###############################################################

# Si el valor es TRUE, la calibración fue exitosa.
###############################################################

df_plot <- tibble(
  dominio = paste0("d", 1:4),
  modelo  = y_pred,
  bench   = y_bench
) %>%
  pivot_longer(cols = c(modelo, bench),
               names_to = "tipo",
               values_to = "valor")

ggplot(df_plot, aes(x = dominio, y = valor, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(title = "Comparación entre modelo FH y valores con benchmarking",
       y = "Ingreso promedio",
       x = "Dominio") +
  scale_fill_manual(values = c("steelblue", "orange")) +
  theme_minimal()
