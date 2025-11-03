############################################################################## 
##################### Validación de la encuesta ##############################
##                                                                          ##
## Función creada para realizar la validación de la encuesta estandarizada. ##
## Realiza la validación sobre las columnas: dam, upm, estrato, area, edad, ##
## anoest, etnia, escolaridad, discapacidad.                                ##
##                                                                          ##
## Autor:  Stalyn Guerrero                                                  ##
## Versión: 14 - 12 - 2022                                                  ##
##                                                                          ##
##############################################################################


validar_encuesta <- function(encuesta, edad_min = 7, pais = "ARG"){
  
  ## encuesta: estandarizada y disponible en el repositorio de BADEHOG
  
  ## edad_min: Edad mínima para ingresar en el sistema escolar, cambia según 
  ##           el sistema escolar del país. 

   
  var_col <- c("dam_ee", "ingcorte","lp","li", "_fep", "sexo", 
               "anoest", "edad", "area_ee", "_upm", "_estrato",
               "discapacidad", "etnia_ee", "condact3") 
  var_col <- intersect(names(encuesta), var_col)
  
  # ------------------ funciones varias ----------------------------
  
  comentario <- function(value){
    len_msn <- nchar(value)
    if((50 - len_msn) %% 2 == 0){
      len_msn <- (50 - len_msn) / 2 
    }else{
      len_msn <- (49 - len_msn) / 2  
    }
      
    paste(paste0(rep("-",len_msn), collapse = ""), value,
          paste0(rep("-",len_msn), collapse = ""),"\n")
    
  }
  
  aux_write <- function(x){
    paste0(c(paste0(colnames(x),collapse = "\t") ,
             apply(x,1,paste0, collapse = "\t")),
           collapse = "\n")
  }
  resultado <- list()
  resultado[["Indice"]] <- paste0("Validamos las siguientes: \n ",
                                paste0(var_col,collapse = "\n")) 
  
# ------------------ Validando dam_ee ----------------------------
  if(pais == "NIC") {
    encuesta_temp <- encuesta %>% transmute(
      dam = str_pad(as_factor(dam_ee, "value"),
                    width = 2,
                    pad = "0"),
      nombre = as_factor(dam_ee),
      dam = NA,
      nombre = NA
    )
    paso <- distinct(encuesta_temp, dam, nombre) %>% data.frame()
    resultado[["dam_ee"]] <-
      paste0(
        comentario("dam_ee No esta disponible para NIC 2014"),
        "\n",
        aux_write(paso),
        "\n",
        comentario("dam_ee No esta disponible para NIC 2014")
      )
    
  } else if(pais == "HND"){
    
    encuesta_temp <- encuesta %>% transmute(
      dam = NA,
      nombre = NA
    )
    paso <- distinct(encuesta_temp, dam, nombre) %>% data.frame()
    resultado[["dam_ee"]] <-
      paste0(
        comentario("dam_ee No esta disponible para HND 2023"),
        "\n",
        aux_write(paso),
        "\n",
        comentario("dam_ee No esta disponible para HND 2023")
      )
    
  } else if("dam_ee" %in% var_col) {
    encuesta_temp <- encuesta %>%
      transmute(dam = str_pad(as_factor(dam_ee, "value"),
                              width = 2,
                              pad = "0"),
                nombre = as_factor(dam_ee))
    
    paso <- distinct(encuesta_temp, dam, nombre) %>% data.frame()
    
    if (nrow(paso) > 3) {
      resultado[["dam_ee"]] <- paste0(comentario("dam_ee ok"),
                                      "\n",
                                      aux_write(paso),
                                      "\n",
                                      comentario("dam_ee ok"))
    } else{
      resultado[["dam_ee"]] <- list(comentario("dam_ee"),
                                    comentario("Revisar") ,
                                    comentario("dam_ee"))
    }
    
   
  }else{
    stop("No cuenta con dam_ee")
  }
# ------------------ Validando _upm ----------------------------
  
  if ("_upm" %in% var_col) {
    encuesta_temp$upm <- encuesta[["_upm"]] 
    paso <- distinct(encuesta_temp, upm) %>% data.frame()
    
    if (nrow(paso) > 10) {
      len_upm <- max(nchar(encuesta_temp$upm))
      encuesta_temp %<>%  mutate(upm = str_pad(string = upm,width = len_upm, pad = "0"))
      resultado[["_upm"]] <- comentario("upm ok")
    } else{
      encuesta_temp$upm <- NULL
      resultado[["_upm"]] <- comentario("No cuenta con _upm")
      warning("No cuenta con _upm")
    }
  }

  # ------------------ Validando _estrato ----------------------------
  
  if ("_estrato" %in% var_col) {
    encuesta_temp$estrato <- encuesta[["_estrato"]] 
    paso <- distinct(encuesta_temp, estrato) %>% data.frame()
    
    if (nrow(paso) > 10) {
      len_estrato <- max(nchar(encuesta_temp$estrato))
      
      encuesta_temp %<>%  mutate(estrato = str_pad(string = estrato,
                                                 width = len_estrato,
                                                 pad = "0"))
      
      resultado[["_estrato"]] <- comentario("estrato ok")
    } else{
      encuesta_temp$estrato <- NULL
      resultado[["_estrato"]] <- comentario("No cuenta con _estrato")
      warning("No cuenta con _estrato")
    }
  }
  
  # ------------------ Validando area_ee ----------------------------
  
  if ("area_ee" %in% var_col) {
    encuesta_temp$area <- encuesta[["area_ee"]] 
    
    paso <- distinct(encuesta_temp, area) %>%
      transmute(value = as_factor(area, "value"),
                label = as_factor(area))
    
    if (nrow(paso) >= 1) {
      resultado[["area_ee"]] <- paste0(comentario("area_ee ok"), "\n",
                                       aux_write(paso), "\n",
                                       comentario("area_ee ok"))
      encuesta_temp %<>% mutate(
        area = case_when(area == 1 ~ "1", TRUE ~ "0"))  
      
    } else{
       stop("No cuenta con area_ee")
    }
   
    
  }
  
  # ------------------ Validando lp y li ----------------------------
  
  if ("lp" %in% var_col & "li" %in% var_col) {
    encuesta_temp$lp <- encuesta[["lp"]] 
    encuesta_temp$li <- encuesta[["li"]] 
    
    paso <- distinct(encuesta_temp, area,li,lp) %>% data.frame()
    
    if (nrow(paso) <= 2) {
     
      resultado[["lp y li"]] <- paste0(comentario("lp y li ok"),
                                       "\n",
                                       aux_write(paso),
                                       "\n",
                                       comentario("lp y li ok"))
    } else{
      encuesta_temp <- encuesta_temp %>% group_by(area) %>% 
        mutate(lp = mean(lp),li = mean(li)) %>% ungroup()
      
      resultado[["lp y li"]] <- 
        comentario("Tiene más de nos niveles, se promedio por  area")
      warning("Revisar lp y li")
    }
  }
  
  # ------------------ Validando ingcorte ----------------------------
  
  if ("ingcorte" %in% var_col) {
    encuesta_temp$ingreso <- encuesta[["ingcorte"]] 
      
    if (!anyNA(encuesta_temp$ingreso)|!any(encuesta_temp$ingreso<0)) {
      
      if(any(encuesta_temp$ingreso <= 0)){
        encuesta_temp %<>% mutate(ingreso= ingreso + 1 )
      } 
      
      resultado[["ingcorte"]] <- comentario("ingcorte ok")
    } else{
      resultado[["ingcorte"]] <- comentario("Revisar ingcorte")
      warning("Revisar ingcorte")
    }
  }
  
  # ------------------ Validando sexo ----------------------------
  
  if ("sexo" %in% var_col) {
    encuesta_temp$sexo <- encuesta[["sexo"]] 
    
    paso <- distinct(encuesta_temp, sexo) %>%
      transmute(value = as_factor(sexo, "value"),
                label = as_factor(sexo))
    
    
    if (nrow(paso) == 2) {
      resultado[["sexo"]] <- paste0(comentario("sexo ok"),
                                    "\n",
                                    aux_write(paso),
                                    "\n",
                                    comentario("sexo ok"))
      encuesta_temp$sexo <- as.character(encuesta_temp[["sexo"]]) 
    } else{
      resultado[["sexo"]] <- comentario("No tiene dos niveles, revisar")
      warning("Revisar sexo")
    }
  }
  
  # ------------------ Validando etnia ----------------------------
  
  if ("etnia_ee" %in% var_col) {
    encuesta_temp$etnia <- encuesta[["etnia_ee"]] 
    
    etnia_ref <- data.frame(
      etnia_value = c("-1", "0", "1", "2", "99"),
      label_ref = c(
        NA,
        "No indígena ni afrodescendiente",
        "Indígena",
        "Afrodescendiente",
        "Ignorado, NS/NR"
      )
      
    ) 
    
    paso <- distinct(encuesta_temp, etnia) %>%
      transmute(etnia_value = as_factor(etnia, "value"),
                label = as_factor(etnia))
    
    paso <- inner_join(paso, etnia_ref, by = "etnia_value") %>% 
      na.omit()
    
    if (nrow(paso) >= 2 & all(paso$label == paso$label_ref)) {        
        
      resultado[["etnia"]] <- comentario("etnia ok")
      
      encuesta_temp %<>% mutate(
        etnia = case_when(etnia == 1 ~ "1", # Indigena
                          etnia == 2 ~ "2",   # Afro
                          TRUE ~ "3"         # Otra
                          ))
                          
    } else{
      encuesta_temp$etnia = NULL
      
      resultado[["etnia"]] <- paste0(comentario("Revisar etnia"),
                                     "\n",
                                     aux_write(paso),
                                     "\n",
                                     comentario("Revisar etnia"))
      encuesta_temp[["etnia"]] = NULL
      warning("Revisar etnia")
    }
  }
 
  # ------------------ Validando anoest ----------------------------
  if ("anoest" %in% var_col & pais == "BRA") {
    encuesta_temp$anoest <- encuesta[["vd3004"]]
    encuesta_temp %<>% mutate(
    anoest = case_when(
      is.na(anoest) ~ "98",  # No aplica 
      anoest %in% c(1,2) ~ "1", # Sem instrução e fundamental incompleto
      anoest %in% c(3,4) ~ "2", # Fundamental completo e médio incompleto
      anoest %in% c(5,6) ~ "3", # Médio completo e superior incompleto
      anoest %in% c(7) ~ "4")) # Superior completo
   
     paso <- encuesta_temp %>% group_by(anoest) %>% 
      summarise(conteo = n()) %>% 
      mutate(porcentaje = conteo/sum(conteo)*100)
     
     resultado[["anoest"]] <- paste0(comentario("anoest ok"),
                                     "\n",
                                     aux_write(paso),
                                     "\n",
                                     comentario("anoest ok"))
    
    
  } else if ("anoest" %in% var_col) {
    encuesta_temp$anoest <- encuesta[["anoest"]]
    encuesta_temp$edad <- encuesta[["edad"]]
    paso <- distinct(encuesta_temp, anoest) %>% data.frame()
    
    if (nrow(paso) >= 4 && is.numeric(encuesta_temp$edad) && 
        is.numeric(paso$anoest)) {
      
      encuesta_temp %<>% mutate(
        anoest = case_when(
          edad < edad_min | anoest == -1   ~ "98"  , #No aplica
          anoest == 99 | (edad >= edad_min & is.na(anoest)) ~ "99", #NS/NR
          anoest == 0  ~ "1", # Sin educacion
          anoest %in% c(1:6) ~ "2",       # 1 - 6
          anoest %in% c(7:12) ~ "3",      # 7 - 12
          anoest > 12 ~ "4",      # mas de 12
          TRUE ~ "Error"  ))
      
      paso <- encuesta_temp %>% group_by(anoest) %>% 
        summarise(conteo = n()) %>% 
        mutate(porcentaje = conteo/sum(conteo)*100)
      
      if(any(paso$anoest == "Error")){
        resultado[["anoest"]] <- paste0(
          comentario("**revisar anoest**"),
          "\n",
          aux_write(paso),
          "\n",
          comentario("**revisar anoest**")
        )
        warning("Revisar anoest") 
      }else{
        resultado[["anoest"]] <- paste0(comentario("anoest ok"),
                                    "\n",
                                    aux_write(paso),
                                    "\n",
                                    comentario("anoest ok"))
      }
      } else{
      stop("Revisar anoest")
      }
  }else{
    stop("No tiene anoest") 
    }
    # ------------------ Validando edad ----------------------------
    
    if ("edad" %in% var_col) {
      encuesta_temp$edad <- encuesta[["edad"]]
      paso <- distinct(encuesta_temp, edad) %>% data.frame()
      
      if (nrow(paso) >= 4 && is.numeric(encuesta_temp$edad)) {
        
        encuesta_temp %<>% mutate( edad = case_when(
          edad < 15 ~ "1",
          edad < 30 ~ "2",
          edad < 45 ~ "3",
          edad < 65 ~ "4",
          TRUE ~ "5"),)
        
        paso <- encuesta_temp %>% group_by(edad) %>% 
          summarise(conteo = n()) %>% 
          mutate(porcentaje = conteo/sum(conteo)*100)
                  resultado[["edad"]] <- paste0(comentario("edad ok"),
                                          "\n",
                                          aux_write(paso),
                                          "\n",
                                          comentario("edad ok"))
        
      } else{
        stop("Revisar edad")
      }
     }else{
      stop("No tiene edad") 
    }
  
  # ------------------ Validando edad ----------------------------
  
  if ("_fep" %in% var_col) {
    encuesta_temp$fep <- encuesta[["_fep"]]
    
   if(is.numeric(encuesta_temp$fep) && !anyNA(encuesta_temp$fep)){
     resultado[["_fep"]] <- comentario("_fep ok") 
   }else{
     stop("Fevisar _fep")  
   }
    
  }else{
    stop("No tiene _fep")   
  }
    
  #----------- Validando Condición de actividad (3 categorías)
  
  if ("condact3" %in% var_col) {
    encuesta_temp$empleo <- encuesta[["condact3"]]
    
    condact_ref <- data.frame(
      empleo_value = c("-1", "1", "2", "3", "99"),
      label_ref = c(
        "NA",
        "Ocupado",
        "Desocupado",
        "Inactivo",
        "Ignorado, NS/NR"
      )
    ) 

    paso <- distinct(encuesta_temp, empleo) %>%
      transmute(empleo_value = as_factor(empleo, "value"),
                label = as_factor(empleo), 
                label = as.character(label))
    
    paso <- inner_join(paso, condact_ref, by = "empleo_value") %>% 
      na.omit()
    
    if (nrow(paso) >= 2 && all(paso$label == paso$label_ref)) {        
      resultado[["empleo"]] <- comentario("empleo ok")
      encuesta_temp <- encuesta_temp %>%
        filter(empleo %in% c(1, 2, 3)) %>% 
        mutate(empleo = as.character(as_factor(empleo)))
    } else {
      stop("No tiene condact3")   
    }
  } else {
    stop("No tiene condact3")   
  }
  
  resultado <- c(list(encuesta_mrp = encuesta_temp,
                      reporte = resultado))
  return(resultado)  
}



