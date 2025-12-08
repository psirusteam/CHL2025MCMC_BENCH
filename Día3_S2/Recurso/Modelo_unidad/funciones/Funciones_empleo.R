Indicadores_encuesta <- function(setdata = encuesta_mrp, var_by = NULL){

  if (is.null(var_by)) {
    setdata <- setdata %>% mutate(Nacional = "Nacional")
    var_by = "Nacional"
  }
    diseno_temp <- as_survey_design(.data = setdata, weights = fep)

temp <-   diseno_temp %>% group_by(paso =  get(var_by)) %>%
    summarise(
      n_sample = unweighted(n()),
      TO = survey_mean(empleo == "Ocupado",vartype = "ci"),
      TP = survey_ratio(empleo %in% c("Ocupado", "Desocupado"), 
                        1, vartype = "ci"),
      TD = survey_ratio(
        empleo %in% c("Desocupado"),
        empleo %in% c("Ocupado", "Desocupado"),
        vartype = "ci")
    ) %>% 
    reshape2::melt(id = c("paso","n_sample")) 

  temp %<>% mutate(variable2 = str_replace(string = variable, 
                                   pattern = "(TO|TD|TP)_low",
                                   replacement = "Li"),
            variable2 = str_replace(string = variable2, 
                                   pattern = "(TO|TD|TP)_upp",
                                   replacement = "Ls"),
            variable2 = str_replace(string = variable2, 
                                    pattern = "(TO|TD|TP)",
                                    replacement = "value"),
            variable = str_replace(string = variable, 
                                    pattern = "(_upp|_low)",
                                    replacement = "")
            )
  temp <- reshape2::dcast(data = temp,
                formula = paso + n_sample +variable ~ variable2,
                value.var = "value") 
  temp[[var_by]] <- temp[["paso"]]
  temp[["paso"]] <- NULL
  temp %>% mutate(Metodo = "Directo")
}

Indicadores_censo <- function(setdata, var_by = NULL) {
   if (is.null(var_by)) {
    setdata <- setdata %>% mutate(Nacional = "Nacional")
    by = "Nacional"
   }

  Ind_mod <-
    setdata %>%  group_by_at(var_by) %>% 
    summarise(TO = weighted.mean(x = TO, w = n),
              TD = weighted.mean(x = TD, w = n),
              TP = weighted.mean(x = TP, w = n),
              Metodo = "Mod", .groups = "drop")%>% 
    reshape2::melt(id = c(var_by,"Metodo" ))
  
  Ind_bench <-
    setdata %>%  group_by_at(var_by) %>% 
    summarise(TO = weighted.mean(x = TO, w = n*gk_TO),
              TD = weighted.mean(x = TD, w = n*gk_TD),
              # TP = weighted.mean(x = TP, w = n*gk_TP),
                Metodo = "Bench", .groups = "drop") %>% 
    mutate(TP = TO/(1-TD)) %>% 
    reshape2::melt(id = c(var_by,"Metodo" ))
  Ind <- bind_rows(Ind_mod,Ind_bench)
  return(Ind)
}

## Plot de comparación empleando los pesos de muestreo
plot_compare_Ind <- function(sample, poststrat, by1){

  
  if (all(by1 %in% names(sample))) {
    dat_encuesta <- Indicadores_encuesta(setdata = sample, by1)
    lims_IC <- dat_encuesta %>% select(c(x = by1,variable, Ls, Li, Metodo))
  } else{
    dat_encuesta <- poststrat %>% distinct_at(vars(by1))
    dat_encuesta <- expand.grid(by1 = dat_encuesta[[by1]],
                                variable = c("TO", "TD", "TP"))
    
    dat_encuesta[[by1]] <- dat_encuesta[["by1"]]
    dat_encuesta[["by1"]] <- NULL
    dat_encuesta %<>% mutate(
       n_sample = 1,
       Ls = 0, 
       Li = 0,
       Metodo = "Directo"
      )
    lims_IC <- data.frame()
  }

  dat_censo <- bind_rows(list(
    Encuesta = dat_encuesta %>% select(-n_sample,-Ls,-Li),
    Censo = Indicadores_censo(setdata = poststrat, by1)
  ),
  .id = "Fuente")
  
  
  
  dat_plot <-
    full_join(
      dat_encuesta %>% select(all_of(by1), variable, n_sample),
      dat_censo,
      by = c(by1, "variable")
    )
  
  dat_plot[["x"]] <-   dat_plot[[by1]]
  
  plot1 <- ggplot(data = dat_plot) +
    geom_jitter(aes(
      x = fct_reorder2(x, x, n_sample, .na_rm = FALSE),
      y = value,
      color = Metodo
    ), size = 2.5,width = 0.3) +
    scale_color_manual(
      breaks = c("Directo", "Mod", "Bench"),
      values = c("red", "blue", "green")
    ) +
    theme_bw(20) + labs(x = by1, y = "", color = "") +
    facet_grid(variable ~ ., scales = "free_y") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(
        angle = 90,
        size = 8,
        vjust = 0.3
      ),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15)
    )
  
  if(nrow(lims_IC)>0){
    plot1 <- plot1 + 
    geom_errorbar(data = lims_IC,
      aes(ymin = Li , ymax = Ls,  
          color = Metodo, x = x),
      width = 0.2)  
  }
  return(list(tabla = dat_plot,  Plot = plot1))
}
