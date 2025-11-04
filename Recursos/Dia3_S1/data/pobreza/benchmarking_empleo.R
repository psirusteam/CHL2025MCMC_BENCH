library(dplyr)
library(purrr)
library(posterior) 

benchmarking_empleo <- function(poststrat_df, encuesta_sta, names_cov, metodo){
conti <- NA

while(!conti %in% c(1:2)) {
Xk <- poststrat_df %>% 
  select(n, TO,TD,TP,all_of(names_cov)) %>% 
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)

estimaciones <-
  map(names_cov ,~ Xk %>% group_by_at(all_of(.x)) %>%
        summarise(
          Nhat = sum(n),
          medias_TO = weighted.mean(TO, n),
          medias_TP = weighted.mean(TP, n),
          Total_TO = sum(TO *n),
          Total_TP = sum(TP *n),
          Total_TD = sum(TD *n),
          medias_TD = Total_TD/Total_TP,
        ))

Xdummy <- Xk %>% select((-matches("TO|TP|TD")),-n) %>% 
  mutate_at(vars(matches("_\\d")) ,
            list(Total_TP = function(x) x,
                 Total_TO = function(x) x,
                 Total_TD = function(x) x,
                 TP = function(x) x*poststrat_df$TP,
                 TO = function(x) x*poststrat_df$TO,
                 TD = function(x) x*poststrat_df$TD)) %>% 
  select((matches("*_(TO|TP|TD)$"))) 


### total
aux_total_ind <- function(encuesta = encuesta_sta, var_by = "dam"){
  diseno_temp <- as_survey_design(.data = encuesta, weights = fep)
  
  diseno_temp %>% group_by_at(var_by) %>%
    summarise(
      Nhat = survey_total(),
      Nhat_TP = survey_total(empleo %in% c("Ocupado", "Desocupado")),
      TO = survey_mean(empleo == "Ocupado"),
      TP = survey_ratio(empleo %in% c("Ocupado", "Desocupado"), 1),
      TD = survey_ratio(
        empleo %in% c("Desocupado"),
        empleo %in% c("Ocupado", "Desocupado")
      )
    ) %>%
    transmute(
      by_temp = get(var_by),
      Total_TO = Nhat,
      Total_TP = Nhat,
      Total_TD = Nhat_TP,
      TO = TO*Nhat,
      TD = TD*Nhat_TP,
      TP = TP*Nhat
      )  %>% 
    reshape2::melt(id = "by_temp") %>%
    transmute(paso = paste0(var_by,"_", by_temp,"_",variable),value) %>% 
    reshape2::dcast(.~paso)
}


Total <-
  map(names_cov,
      ~ encuesta_sta %>% aux_total_ind(var_by = .x) %>% select_if(is.numeric)) %>% 
  unlist()

temp_len <- length(intersect(names(Total) , names(Xdummy)))

if(temp_len != length(Total) ||
   temp_len != ncol(Xdummy)) {
  cat("Revisar covariables \n")
} else{
  Total <- Total[colnames(Xdummy)]
}


metodo_calib = list(TD = metodo,
              TP = metodo,
              TO = metodo)
gk_calib <- list()

## Tasa de desocupación 
for (ii in c("TD","TP","TO")){

cat("---------------", ii , "----------------\n")
  
names_Tasa <- grep(x = colnames(Xdummy), pattern = ii ,value = TRUE)
conti <- NA
while(!conti %in% c(1:2)) {
gk_Tasa <- tryCatch(calib(
  Xs = Xdummy[,names_Tasa],
  d = poststrat_df$n,
  total = Total[names_Tasa],
  method = metodo_calib[[ii]],
  max_iter = 1000
), 
error = function(e) {NULL}
)

if(is.null(gk_Tasa)){
  cat("Editar el parámetro  metodo (linear o logit) \n")
  metodo_calib[[ii]] <- edit(metodo_calib[[ii]])
  conti <- NA
}else{

check_Tasa <- checkcalibration(Xs = Xdummy[,names_Tasa], 
                 d = poststrat_df$n,
                 total = Total[names_Tasa],
                 g = gk_Tasa)$result

if(!check_Tasa){cat("La calibración NO hizo convergencia \n")}
if(min(gk_Tasa) <= 0){cat("Los gks tienen valores negativos \n")}

cat("
----------------------------------------------------
    Tabla de comparación de los gks \n")

print(
  data.frame(
    Poblacion = colSums(Xdummy[, names_Tasa] * poststrat_df$n * gk_Tasa),
    Muestra = Total[names_Tasa]
  ) %>% mutate(diff = round(abs(Poblacion - Muestra), 3)) %>%
    arrange(diff)
  
)

cat("Resumen de los gks \n")
print(summary(gk_Tasa))

cat("
--------------------------------------------------------------------------------
")

cat("\n\nConteno de gks menor o igual a cero = ", sum(gk_Tasa<=0.00000001), "\n\n")
cat("Porcentaje de gks menor o igual a cero = ", sum(gk_Tasa<= 0.00000001)/length(gk_Tasa) *100, "\n\n")
cat("Conteno de postestrato =",  length(gk_Tasa), "\n\n")
cat("Conteo de personas con gks menor o igual a cero =",
    sum(poststrat_df$n[gk_Tasa<=0.00000001]), "\n\n")
cat("Porcentaje de personas con gks menor o igual a cero =",
    sum(poststrat_df$n[gk_Tasa<=0.00000001])/sum(poststrat_df$n)*100, "\n\n")
cat("
--------------------------------------------------------------------------------
")

while(!conti %in% c(1:2)) {
  cat("¿Desea continuar? \n 1. Sí \n 2. No\n")
  conti <- as.numeric(readline())
}

if (conti == 2) {
  cat("Editar el parámetro  metodo (linear o logit) \n")
  metodo_calib[[ii]] <- edit(metodo_calib[[ii]])
  conti <- NA
}else{
  break
}
}

}
gk_calib[[ii]] <- gk_Tasa
}

generate_histogram <- function(data, variable, title) {
  ggplot(data = data.frame(x = data[[variable]]), aes(x = x)) +
    geom_histogram(bins = 100) +
    labs(y = "", x = "gks", title = title)
}

plot1 <- generate_histogram(gk_calib, "TO", "Tasa de Ocupación")
plot2 <- generate_histogram(gk_calib, "TD", "Tasa de desocupación")
plot3 <- generate_histogram(gk_calib, "TP", "Tasa de participación")

print(plot1|plot2|plot3)

conti <- NA

while(!conti %in% c(1:2)) {
  cat("¿Desea continuar? \n 1. Sí \n 2. No\n")
  conti <- as.numeric(readline())
}

if (conti == 2) {
  cat("Editar el parámetro  names_cov \n")
  names_cov <- edit(names_cov)
  conti <- NA
}else{
  break
}

}


return(list(gk_bench =  gk_calib,
            var_bench = names_cov, 
            metodo_bench = metodo_calib,
            Total = Total,
            plot_bench = plot1|plot2|plot3))

}



benchmarking_empleo_iter <- function(poststrat_df, Total_Xk,
                                     names_cov, metodo_calib, ...){
  
  # Construir base con dummies
  Xk <- poststrat_df %>% 
    select(n, TO, TD, TP, all_of(names_cov)) %>% 
    fastDummies::dummy_cols(select_columns = names_cov,
                            remove_selected_columns = FALSE)
  
  # Estimaciones por covariable
  estimaciones <- map(names_cov ,~ 
                        Xk %>% group_by_at(all_of(.x)) %>%
                        summarise(
                          Nhat = sum(n),
                          medias_TO = weighted.mean(TO, n),
                          medias_TP = weighted.mean(TP, n),
                          Total_TO = sum(TO * n),
                          Total_TP = sum(TP * n),
                          Total_TD = sum(TD * n),
                          medias_TD = Total_TD / Total_TP,
                          .groups = "drop"
                        )
  )
  
  # Construcción de Xdummy
  Xdummy <- Xk %>% 
    select((-matches("TO|TP|TD")),-n) %>% 
    mutate_at(vars(matches("_\\d")) ,
              list(Total_TP = function(x) x,
                   Total_TO = function(x) x,
                   Total_TD = function(x) x,
                   TP = function(x) x * poststrat_df$TP,
                   TO = function(x) x * poststrat_df$TO,
                   TD = function(x) x * poststrat_df$TD)) %>% 
    select((matches("*_(TO|TP|TD)$"))) 
  
  # Función auxiliar: totales de la encuesta
  
  temp_len <- length(intersect(names(Total_Xk), names(Xdummy)))
  
  if(temp_len != length(Total_Xk) || temp_len != ncol(Xdummy)) {
    stop("Revisar covariables: inconsistencia entre totales y Xdummy")
  } else{
    Total_Xk <- Total_Xk[colnames(Xdummy)]
  }
  
  # Métodos de calibración
  
  gk_calib <- list()
  
  # Calibración por tasa
  for (ii in c("TD","TP","TO")){
    names_Tasa <- grep(x = colnames(Xdummy), pattern = ii ,value = TRUE)
    
    gk_Tasa <- calib(
      Xs = Xdummy[,names_Tasa],
      d = poststrat_df$n,
      total = Total_Xk[names_Tasa],
      method = metodo_calib[[ii]],
      max_iter = 1000,...
    )
    
    # Validación (solo warnings, no interacción)
    check_Tasa <- checkcalibration(
      Xs = Xdummy[,names_Tasa], 
      d = poststrat_df$n,
      total = Total_Xk[names_Tasa],
      g = gk_Tasa
    )$result
    
    if(!check_Tasa) warning(paste("La calibración de", ii, "no convergió"))
    if(min(gk_Tasa) <= 0) warning(paste("Los gks de", ii, "tienen valores negativos"))
    
    gk_calib[[paste0("gk_", ii)]] <- gk_Tasa
  }
  
  # Resultado final
  poststrat_df <- cbind(poststrat_df, as.data.frame(gk_calib))
  return( poststrat_df)
}




# --- Automatización del benchmarking en todas las iteraciones ---
benchmarking_empleo_all <- function(multi_fit, list_bench, pars = "tasa_pred", n_iter = NULL) {
  
  # 1. Extraer draws de la cadena MCMC
  param_chain <- as.array(multi_fit$model_bayes, pars = pars, ) %>%
    as_draws_matrix()
  
  # total de iteraciones disponibles
  total_iter <- nrow(param_chain)
  
  # definir cuántas iteraciones usar
  if (is.null(n_iter) || n_iter > total_iter) {
    n_iter <- total_iter
  }
  
  # 2. Data base de predicciones censales (sin TD, TO, TP)
  censo_pred_temp <- multi_fit$censo_pred %>%
    select(-any_of(c("TD", "TO", "TP")))
  
  # 3. Procesar todas las iteraciones
  res_all <- map(1:n_iter, function(i) {
    # reconstruir tasas para la iteración i
    dat_tasa <- matrix(
      param_chain[i, ],
      nrow = nrow(censo_pred_temp),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(1:nrow(censo_pred_temp), c("TD", "TO", "TP"))
    ) %>% as.data.frame()
    
    # unir con la base del censo
    censo_iter <- bind_cols(censo_pred_temp, dat_tasa)
    
    # ejecutar benchmarking para la iteración i
    benchmarking_empleo_iter(
      poststrat_df  = censo_iter,
      Total_Xk      = list_bench$Total,
      names_cov     = list_bench$var_bench,
      metodo_calib  = list_bench$metodo_bench
    ) %>% 
      mutate(iter = i) # para identificar cada iteración
  })
  
  
  return(iter = res_all)
}
