################################################################################
## OBJETIVO
## --------
## Mostrar cómo realizar benchmarking para un vector de probabilidades
## multinomiales (Ocupado–Inactivo–Desocupado) utilizando:
##   - calibración logit con calib()
##   - ajuste marginal columna por columna
##   - reconstrucción de la tercera categoría mediante la restricción de suma 1
##
## El procedimiento asegura que las probabilidades calibradas multiplicadas por 
## los tamaños poblacionales N_mpio reproduzcan EXACTAMENTE los totales directos 
## (dir).
################################################################################

library(TeachingSampling)
library(sampling)
rm(list = ls())


################################################################################
## 1. MATRIZ DE PREDICCIONES DEL MODELO (y_pred)
## ----------------------------------------------
## Cada fila corresponde a un municipio (DAM2)
## Cada columna es una categoría de la multinomial:
##   - Ocupado
##   - Inactivo
##   - Desocupado
##
## Cada fila suma 1.
################################################################################

y_pred <- matrix(
  c(0.55, 0.35, 0.10,     # Municipio 1
    0.50, 0.38, 0.12,     # Municipio 2
    0.45, 0.30, 0.25,     # Municipio 3
    0.60, 0.30, 0.10),    # Municipio 4
  ncol = 3,
  byrow = TRUE
)
colnames(y_pred) <- c("Ocupado", "Inactivo", "Desocupado")


################################################################################
## 2. TAMAÑOS POBLACIONALES POR MUNICIPIO (N_mpio)
################################################################################

N_mpio <- c(500, 600, 400, 200)


################################################################################
## 3. TOTALES DIRECTOS (dir)
## -------------------------
## Representan el total de personas Ocupadas, Inactivas y Desocupadas
## provenientes de la encuesta directa (estimación directa).
################################################################################

dir <- c("Ocupado"    = 850,
         "Inactivo"   = 680,
         "Desocupado" = 170)


################################################################################
## 4. CALIBRACIÓN DE LA COLUMNA 1: OCUPADOS
## -----------------------------------------
## Se calibra la primera columna de y_pred para que:
##      sum(gk1 * y_pred[,1] * N_mpio) == dir["Ocupado"]
##
## Esto obliga a que el total de ocupados ajustados sea EXACTAMENTE el total
## directo.
################################################################################

gk1 <- calib(
  Xs    = y_pred[, 1],   # columna Ocupado
  total = dir[1],        # total directo de Ocupados
  d     = N_mpio,        # tamaños poblacionales
  method = "logit"       # garantiza gk > 0 y restricciones válidas
)

gk1

# Tasa calibrada de Ocupado por municipio
(y_bench1 <- gk1 * y_pred[, 1])

# Total calibrado de Ocupados por municipio
(tot_ocupado_bench <- N_mpio * y_bench1)

# Verificar que el total es EXACTO al directo
round(sum(tot_ocupado_bench)) == dir[1]


################################################################################
## 5. CALIBRACIÓN DE LA COLUMNA 2: INACTIVOS
## ------------------------------------------
## Igual que antes, pero ajustando la segunda columna.
################################################################################

gk2 <- calib(
  Xs    = y_pred[, 2],   # columna Inactivo
  total = dir[2],        # total directo de Inactivos
  d     = N_mpio,
  method = "logit"
)

gk2
(y_bench2 <- gk2 * y_pred[, 2])
(tot_Inactivo_bench <- N_mpio * y_bench2)

round(sum(tot_Inactivo_bench)) == dir[2]


################################################################################
## 6. RECONSTRUCCIÓN DE LA COLUMNA 3: DESOCUPADOS
## -----------------------------------------------
## Como las probabilidades deben sumar 1 en cada fila:
##
##    p3_calibrado = 1 - (p1_calibrado + p2_calibrado)
##
## Esto garantiza consistencia interna sin calibrar directamente la tercera 
## columna.
################################################################################

y_bench3 <- 1 - (y_bench1 + y_bench2)

# Totales de Desocupados calibrados
tot_Desocupado_bench <- N_mpio - (tot_ocupado_bench + tot_Inactivo_bench)

# Validación del total directo
round(sum(tot_Desocupado_bench)) == dir[3]


################################################################################
## 7. CONSOLIDACIÓN DE LA MATRIZ BENCHMARKING (y_bench)
## -----------------------------------------------------
## Matriz final ajustada de probabilidades que cumplen:
##   - suma fila = 1
##   - totales marginales EXACTOS por categoría
################################################################################

y_bench <- cbind(y_bench1, y_bench2, y_bench3)
y_bench

# Comparación con predicciones originales
y_pred


################################################################################
## 8. CONSOLIDACIÓN DE LOS TOTALES CALIBRADOS
################################################################################

tot_bench <- cbind(
  tot_ocupado_bench,
  tot_Inactivo_bench,
  tot_Desocupado_bench
)

tot_bench

# Validación final: totales marginales
colSums(tot_bench)
