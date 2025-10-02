#################################################
## Validar el modelo 
#################################################

validar_modelo_bayes <- function(model_bayes, encuesta_sta){ 
byAgrega <-
  grep(
    pattern =  "^(n|fep|lp|li|ingreso|empleo|upm|estrato)",
    x = names(encuesta_sta),
    invert = TRUE,
    value = TRUE
  )

encuesta_df_agg <- encuesta_sta %>%
  group_by_at(byAgrega) %>%
  summarise(across(matches("empleo_"), sum),
            .groups = "drop")


#--- Exporting Bayesian Multilevel Model Results ---#
parametros <- summary(model_bayes)$summary %>% data.frame()

parametros_chain <-  grep(pattern = "theta\\[|beta|z_u|u\\[", 
                         x = rownames(parametros),
                         value = TRUE)
parametros_paso <- parametros[rownames(parametros)%in%parametros_chain,]

tbla_rhat <- mcmc_rhat_data(parametros_paso$Rhat) %>% 
  group_by(description) %>% 
  tally() %>% mutate(Porcen = n/sum(n)*100)


## Validacion de los theta 
y_pred_B <- as.array(model_bayes, pars = "theta") %>%
  as_draws_matrix()

rowsrandom <- sample(nrow(y_pred_B), 100)

theta_1<-  grep(pattern = "1]",x = colnames(y_pred_B),value = TRUE)
theta_2<-  grep(pattern = "2]",x = colnames(y_pred_B),value = TRUE)
theta_3<-  grep(pattern = "3]",x = colnames(y_pred_B),value = TRUE)
y_pred1 <- y_pred_B[rowsrandom,theta_1 ]
y_pred2 <- y_pred_B[rowsrandom,theta_2 ]
y_pred3 <- y_pred_B[rowsrandom,theta_3 ]

n_obs <- encuesta_df_agg %>% select(matches("empleo_")) %>% 
  apply(MARGIN = 1,FUN = sum)


# Forma 1
p_desocupado  <-
  ppc_dens_overlay(y = as.numeric(encuesta_df_agg$empleo_Desocupado / n_obs),
                   y_pred1) + labs(title = "Desocupado")
p_ocupado <-
  ppc_dens_overlay(y = as.numeric(encuesta_df_agg$empleo_Ocupado / n_obs), y_pred2) +
  labs(title = "Ocupado")

p_inactivo <-
  ppc_dens_overlay(y = as.numeric(encuesta_df_agg$empleo_Inactivo / n_obs), y_pred3) +
  labs(title = "Inactivo")

temp_ppc <-  p_ocupado/p_desocupado/p_inactivo

X11()
print(temp_ppc)

conti <- NA
while (!conti %in% c(1:2)) {
  cat("¿ Quiere editar el modelo?
      1. Si
      2. No \n")
  
  conti <- toupper(readline())
}

if(conti == "1"){
  return(list(modelo = TRUE))
}

omega12 <- tryCatch(
    {
       summary(model_bayes, pars = "Omega[1,2]")$summary
      },
    error = function(cond) {
      message("Error: ", cond$message) 
      NULL 
    }
  )

if(!is.null(omega12)){
plot_omega <- Plot_dens_draws(model_bayes, pars = "Omega[1,2]")

X11()
print(plot_omega)
}else{
 plot_omega <- ggplot() 
}
## Efecto fijos 
efecto_fijo <-  grep(pattern = "beta", 
                     x = rownames(parametros),
                     value = TRUE)
X11()
print(
p_fijo <- traceplot(model_bayes, pars = efecto_fijo)
)

conti <- NA
while (!conti %in% c(1:2)) {
  cat("¿ Quiere revisar los resultados de las cadenas para los efectos fijos?
      1. Si
      2. No \n")
  
  conti <- toupper(readline())
}

if(conti == "1"){
  conti <- NA
while(!conti %in% c(1:2)) {  
    cat("¿ Cuál efecto fijo ?\n 0. Cancelar\n ")
    cat(paste0(c(1:length(efecto_fijo)), ". ",
               c(efecto_fijo), "\n"))
    
    conti <- as.numeric(readline())
    
    if (conti %in% 1:length(efecto_fijo)) {
      print(Plot_dens_draws(model_bayes, pars = efecto_fijo[conti]))
      conti <- NA
    } else if (conti == 0) {
      break
    }
    
  } 
}

### Efectos aleatroios 

efecto_aleatorio <-  grep(pattern = "z_u|u\\[", 
                          x = rownames(parametros),
                          value = TRUE)

X11()
print(
 p_alea <- traceplot(model_bayes, pars = efecto_aleatorio)
)


conti <- NA
while (!conti %in% c(1:2)) {
  cat("¿ Quiere revisar los resultados de las cadenas para los efectos aleatorios?
      1. Si
      2. No \n")
  
  conti <- toupper(readline())
}

if(conti == "1"){
  conti <- NA
  while(!conti %in% c(1:2)) {  
    cat("¿ Cuál efecto fijo ?\n 0. Cancelar\n ")
    cat(paste0(c(1:length(efecto_aleatorio)), ". ",
               c(efecto_aleatorio), "\n"))
    
    conti <- as.numeric(readline())
    
    if (conti %in% 1:length(efecto_aleatorio)) {
      print(Plot_dens_draws(model_bayes, pars = efecto_aleatorio[conti]))
      conti <- NA
    } else if (conti == 0) {
      break
    }
    
  } 
}

list(tbla_rhat = tbla_rhat, 
     plot_ppc = temp_ppc,
     omega12 = omega12,
     plot_omega = plot_omega, 
     plot_E_fijo = p_fijo,
     plot_E_Aleatorio = p_alea,
     modelo = FALSE,
     parametros_chain = parametros_chain)
}


