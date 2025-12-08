################################################################################
# BENCHMARKING DE POBREZA – EJEMPLO BÁSICO
#
# Objetivo:
#   Ajustar (benchmark) las tasas de pobreza predichas por un modelo
#   para que cumplan con:
#     (1) Total nacional de pobreza (una restricción)
#     (2) Totales por dominio (DAM) — múltiples restricciones simultáneas
#
# Método:
#   Calibración logit (Deville & Särndal) usando calib() del paquete sampling.
#
################################################################################

library(dplyr)
library(sampling)
library(TeachingSampling)
rm(list = ls())

################################################################################
# 1. Construcción de los 16 post-estratos (4 DAM × 2 Sexo × 2 Área)
################################################################################

dam  <- rep(c("A","B","C","D"), each = 4)
sexo <- rep(c("H","H","M","M"), times = 4)
area <- rep(c("U","R"), each = 2, times = 4)

post <- data.frame(
  k = 1:16,
  dam, sexo, area
)

################################################################################
# 2. Tamaño poblacional y pobreza predicha por modelo
################################################################################

# Tamaño poblacional de cada post-estrato
post$n_k <- c(623,466,428,717,
              770,598,569,765,
              486,606,780,384,
              576,661,737,793)

# Tasa de pobreza predicha por el modelo (entre 0 y 1)
post$p_pred <- c(0.264,0.383,0.430,0.214,
                 0.346,0.188,0.230,0.266,
                 0.154,0.265,0.411,0.252,
                 0.295,0.330,0.298,0.206)

################################################################################
# 3. Benchmarking nacional de pobreza (1 restricción)
################################################################################

# Supongamos que la encuesta oficial reporta:
#   Pobreza nacional = 30%
# Convertimos esa tasa directa en total de pobres:
Pobreza_dir <- c(
  den = sum(post$n_k),
  num = 0.30 * sum(post$n_k))

# Calibración logit con una sola restricción:

Xs <- cbind(den = 1, num = post$p_pred)

gk <- calib(
  Xs = Xs,     # variable a calibrar (tasa de pobreza predicha)
  d  = post$n_k,        # tamaños poblacionales
  total = Pobreza_dir,  # total de pobres directo nacional
  method = "logit"
)

# Tasa ajustada por post-estrato:
post$p_bench <- gk * post$p_pred

# Pobres ajustados:
post$tot_pobreza_bench <- post$p_bench * post$n_k

# VALIDACIÓN:
data.frame(
  tot_pobreza_bench  = sum(post$tot_pobreza_bench),  
  Pobreza_dir = Pobreza_dir[2], 
  diff = Pobreza_dir[2] - sum(post$tot_pobreza_bench))

# Pobreza calibrada por DAM (post-nacional)
pobreza_dam <- post %>%
  group_by(dam) %>%
  summarise(
    pobreza_bench_dam = sum(tot_pobreza_bench) / sum(n_k)
  )
pobreza_dam 

################################################################################
# 4. Benchmarking por DAM (múltiples restricciones)
################################################################################
# Ahora queremos que cada DAM cumpla con su tasa de pobreza directa.

# Supongamos estimaciones directas por DAM:
pobreza_dir_dam <- data.frame(
  dam = c("A","B","C","D"),
  pobreza_dir = c(0.33, 0.28, 0.25, 0.30)   # tasas directas DAM
)

# Convertimos tasas directas a totales de pobres por DAM:
pobreza_dir_dam <- pobreza_dir_dam %>%
  left_join(
    post %>% group_by(dam) %>% summarise(N_dam = sum(n_k)),
    by = "dam"
  ) %>%
  mutate(
    tot_pobreza_dir = pobreza_dir * N_dam
  )

pobreza_dir_dam

## Construcción del vector de totales a calibrar:
##   - 4 totales (pobres por DAM)
##   - 4 totales (población N_dam por DAM)
totales_dam <- c(
  pobreza_dir_dam$tot_pobreza_dir,   # numerador por DAM
  pobreza_dir_dam$N_dam              # denominador por DAM
)

################################################################################
# 5. Matriz de diseño para calib(): dummies por DAM
################################################################################

# Matriz de dummies: 16×4
X_dam_den <- model.matrix(~ dam - 1, data = post)
# Para el numerador multiplicamos por p_pred
X_dam_num <- X_dam_den * post$p_pred

# Renombrar columnas para evitar conflictos:
colnames(X_dam_den) <- paste0("den_", colnames(X_dam_den))
colnames(X_dam_num) <- paste0("num_", colnames(X_dam_num))

# Matriz final Xs:
Xs_dam <- cbind(X_dam_num, X_dam_den)

# Alinear nombres del vector de totales:
names(totales_dam) <- colnames(Xs_dam)

################################################################################
# 6. Calibración logit por DAM
################################################################################

gk <- calib(
  Xs    = Xs_dam,       # matriz 16×8 con numerador y denominador por DAM
  d     = post$n_k,     # tamaños por post-estrato
  total = totales_dam,  # vector de totales (numerador+denominador)
  method = "logit"
)

################################################################################
# 7. Pobres calibrados y validación por DAM
################################################################################

# Tasa ajustada:
post$p_bench <- gk * post$p_pred

# Número ajustado de pobres:
post$tot_pobreza_bench <- post$p_bench * post$n_k

# Validación por dominio:
validacion <- post %>%
  group_by(dam) %>%
  summarise(
    tot_pobreza_dir   = pobreza_dir_dam$tot_pobreza_dir[pobreza_dir_dam$dam == dam[1]],
    tot_pobreza_bench = sum(tot_pobreza_bench),
    diff              = tot_pobreza_bench - tot_pobreza_dir
  )

validacion    # diferencias ≈ 0

################################################################################
# 8. Tasas de pobreza ajustadas por DAM
################################################################################

pobreza_dam <- post %>%
  group_by(dam) %>%
  summarise(
    pobreza_bench_dam = sum(tot_pobreza_bench) / sum(n_k)
  )

pobreza_dam
