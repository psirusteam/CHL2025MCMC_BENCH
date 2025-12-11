i = 1

iter_name <- iter_cols[i]

# Matriz Xs = [Den_DAM, Num_DAM]
# Den_DAM: indicador por dominio
# Num_DAM: indicador por dominio * predicción del modelo en esta iteración
Xs <- cbind(
  Den_DAM = 1,
  Num_DAM = temp_draws[[iter_name]]
)
# Asignación ordenada de nombres según 'totales'
colnames(Xs) <- names(totales)
# Benchmarking mediante calibración lineal


t1 = sum(set_data$wkx)
t2 = sum(set_data$wkx*set_data$pobreza)

totales = c(t1, t2)

gk <- calib(
  Xs    = Xs,
  total = totales,
  d     = estimacionesPre$total_pp,
  method = "logit"
)

sum(gk * estimacionesPre$total_pp)
t1

sum(gk * estimacionesPre$total_pp * temp_draws[[iter_name]])
t2

dftemp = data.frame(
dam = estimacionesPre$dam,   
dam2 = estimacionesPre$dam2, 
n = estimacionesPre$total_pp,
y = temp_draws[[iter_name]],
n2 = gk * estimacionesPre$total_pp,
ybench = gk * temp_draws[[iter_name]]
)

dftemp %>% 
  summarise(media = weighted.mean(y, n2))

t2/t1

dftemp %>% 
  group_by(dam) %>% 
  summarise(media = weighted.mean(y, n2))

directoDam

##### Ahora calib por DAM

i = 2

iter_name <- iter_cols[i]

# Matriz Xs = [Den_DAM, Num_DAM]
# Den_DAM: indicador por dominio
# Num_DAM: indicador por dominio * predicción del modelo en esta iteración
Xs <- cbind(
  Den_DAM = X_dam,
  Num_DAM = X_dam * temp_draws[[iter_name]]
)

totales = c(directoDam$den_dir, directoDam$num_dir) 
# Asignación ordenada de nombres según 'totales'
colnames(Xs) <- names(totales)
# Benchmarking mediante calibración lineal

gk <- calib(
  Xs    = Xs,
  total = totales,
  d     = estimacionesPre$total_pp,
  method = "linear"
)

sum(gk * estimacionesPre$total_pp)
t1

sum(gk * estimacionesPre$total_pp * temp_draws[[iter_name]])
t2

dftemp = data.frame(
  dam = estimacionesPre$dam,   
  dam2 = estimacionesPre$dam2, 
  n = estimacionesPre$total_pp,
  y = temp_draws[[iter_name]],
  n2 = gk * estimacionesPre$total_pp,
  ybench = gk * temp_draws[[iter_name]]
)

dftemp %>% 
  summarise(media = weighted.mean(y, n2))

t2/t1

dftemp %>% 
  group_by(dam) %>% 
  summarise(media = weighted.mean(y, n2))

directoDam



