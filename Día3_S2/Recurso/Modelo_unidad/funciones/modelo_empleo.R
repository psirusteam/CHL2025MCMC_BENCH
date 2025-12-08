############################################################################## 
################### Modelo para mercado del trabajo  #########################
##                                                                          ##
##                                                                          ##
## Autor:  Stalyn Guerrero and Andrés Gutierrez                             ##
## Versión: 05 - 01 - 2023                                                  ##
##                                                                          ##
##############################################################################

modelo_empleo <-
  function(encuesta_sta,
           censo_sta,
           predictors,
           X_fijo,
           Z_aleatorio,
           path_model = NULL,
           fit_STAN,...) {
    
    byAgrega <-
      grep(
        pattern =  "^(n|fep|lp|li|ingreso|empleo|upm|estrato)",
        x = names(encuesta_sta),
        invert = TRUE,
        value = TRUE
      )
    
    cat(byAgrega,"\n")

    encuesta_df_agg <- encuesta_sta %>%
      group_by_at(byAgrega) %>%
      summarise(across(matches("empleo_"), sum),
                .groups = "drop")

    n_comb <- nrow(encuesta_df_agg)
    
    if (n_comb > 10000) {
      cat(paste0(rep("#",50),collapse = ""),"\n")
      cat(paste0(rep("#",20),collapse = ""), " Alerta ",
          paste0(rep("#",20),collapse = ""),"\n")
      cat("Tienes que editar el modelo hasta tener menos de 10000 combinaciones")
      cat(paste0(rep("#",50),collapse = ""),"\n\n")
      
      while (n_comb > 10000) {
        conti_edit_byAgrega <- NA
        byAgrega <- edit(byAgrega)
        byAgrega <- c("dam",byAgrega)
        encuesta_agg_temp <- encuesta_sta %>%
          group_by_at(byAgrega) %>%
          summarise(across(matches("empleo_"), sum),
                    .groups = "drop")
        
        n_comb <- nrow(encuesta_agg_temp)
        if (n_comb <= 10000) {
          while (!conti_edit_byAgrega %in% 1:2) {
            cat("¿Quieres editar el parámetro? \n 1. Sí \n 2. No\n")
            conti_edit_byAgrega <- as.numeric(readline())
          }
          if (conti_edit_byAgrega == 1) {
            n_comb = 10001 
          }
        }
      }
      encuesta_df_agg <- encuesta_agg_temp
      rm(encuesta_agg_temp)
    }
    
    encuesta_df_agg <-
      inner_join(encuesta_df_agg, predictors,
                 by = "dam")
    
    #############################################
    ## Creando la variable multinomial (censo)
    #############################################
    if(nrow(predictors)>2){
    censo_df <- inner_join(censo_sta,
                           predictors, by = "dam") %>%
      ungroup()
    }else{
      censo_df <- censo_sta
    }
    ##################################################
    ## Parámetros del modelo
    ##################################################
    if(!is.null(X_fijo)){
    formula_x <- as.formula(paste0("~ -1+", X_fijo))
    }else{
     formula_x <- as.formula(paste0("~ 1"))      
    }
    
    formula_z <- as.formula(paste0("~ -1+", Z_aleatorio))
    
    X_obs <-
      model.matrix(formula_x, data = encuesta_df_agg)
    
    if(ncol(X_obs) == 1)X_obs <- cbind(X_obs, uno = 0)
    
    Y_obs <- encuesta_df_agg %>% select(empleo_Desocupado,
                                        empleo_Ocupado,
                                        empleo_Inactivo) %>%
      as.matrix(.)
    
    Z_obs <- model.matrix(formula_z, data = encuesta_df_agg) %>%
      as.matrix(.)
    
    
    X_pred <-  model.matrix(formula_x, data = censo_df)
    
    if(ncol(X_pred) == 1)X_pred <- cbind(X_pred, uno = 0)
    
    Z_pred <- model.matrix(formula_z, data = censo_df) %>%
      as.matrix(.)
    
    ##################################################
    ## Valida parámetros
    ##################################################
    
    if(length(setdiff(colnames(Z_obs) ,colnames(Z_pred))) > 2){
      agregarZp  <- setdiff(colnames(Z_obs) ,colnames(Z_pred))
      temp <- matrix(0, nrow = nrow(Z_pred),
                     ncol = length(agregarZp),
                     dimnames = list(1:nrow(Z_pred), agregarZp))
      
      Z_pred <- cbind(Z_pred, temp)  
    }
    if(length(setdiff(colnames(Z_pred) ,colnames(Z_obs)))>0){
      agregarZ  <- setdiff(colnames(Z_pred) ,colnames(Z_obs))
      temp <- matrix(0,nrow = nrow(Z_obs),
                     ncol = length(agregarZ),
                     dimnames = list(1:nrow(Z_obs), agregarZ))
      
      Z_obs <- cbind(Z_obs, temp)
      
    }
    
    ##################################################
    ## Definir el sample_data
    ##################################################
    
    xnames <-  intersect(colnames(X_pred) ,colnames(X_obs))
    Znames <-  intersect(colnames(Z_pred) ,colnames(Z_obs))
    
    sample_data <- list(D = nrow(X_obs),           # Número de dominios. 
                        P = ncol(Y_obs),           # Número de estados.
                        K = ncol(X_obs[,xnames]),  # Número de efecto fijo.
                        D1 = nrow(X_pred),         # Número de dominios a predecir. 
                        Kz = ncol(Z_pred[,Znames]),         # Número de efectos aleatorios.
                        Z = Z_obs[,Znames],       # Matriz de efectos aleatorios.
                        Zp = Z_pred[,Znames],      # Matriz de efectos aleatorios.
                        y = Y_obs,                 # Conteos por categorías. 
                        X = X_obs[,xnames],        # Matriz de efecto fijo 
                        Xp = X_pred[,xnames]       # Matriz de efecto fijo
    )
  
    sample_data <- lapply(sample_data, function(x) {
      if (is.matrix(x)) {
        return(unname(x))
      } else {
        return(x)
      }
    })
    
    
    cat("La base tiene: \n Dominios = ", nrow(X_obs), 
        "\n Efecto fijos = ", ncol(X_obs[,xnames]), 
        "\n Efecto Aleatorios ", ncol(Z_pred[,Znames]) )
# STAN fit ----------------------------------------------------------------
   # rstan::rstan_options(auto_write = TRUE) # speed up running time 
   #  options(mc.cores = parallel::detectCores())
    model_bayes <- stan(
      file = fit_STAN,
      data = sample_data,
      verbose = TRUE,
      open_progress = TRUE,
      chains = 2,
      ...
    )
    
    if(all(file.exists(path_model))) {
      outputh_version <-
        gsub("fit_empleo.rds", "Versiones", path_model)
      if (!dir.exists(outputh_version)) {
        dir.create(outputh_version)
      }
      
      temp_fec <- substr(file.mtime(path_model), 1, 10)
      
      outputh_paso <-
        paste0(outputh_version, "/fit_empleo_", temp_fec , ".rds")
      
      file.copy(from = path_model, to = outputh_paso)
    }
    
    
if(!is.null(path_model)){ 
  result <- tryCatch(
    {
      saveRDS(model_bayes, file = path_model) 
      "Guardado exitoso." 
      },
    error = function(cond) {
      message("Error: ", cond$message) 
      NULL 
    }
  )
  cat(result, "\n")
  }    
## Organizando prediccion en el censo 
    
    theta_fit <- summary(model_bayes, pars = "tasa_pred")$summary %>% 
      data.frame()
    
    paso <- matrix(theta_fit$mean, 
                   nrow = nrow(censo_df),
                   ncol = 3, byrow = TRUE,
                   dimnames = list(1:nrow(censo_df),c("TD","TO","TP")) 
    ) %>% 
      data.frame()
    

    censo_pred <- cbind(censo_df[,names(censo_sta)],paso)
    
    list( model_bayes = model_bayes , 
          censo_pred = censo_pred)
  }



