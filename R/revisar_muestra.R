#' Title
#'
#' @param dise
#'
#' @return
#' @export
#'
#' @examples
llaves <- function(diseño){
  diseño$niveles %>% ggplot(aes(y = nivel, x = 0, label = glue::glue("{variable} - {llave}"), fill = tipo)) +
    geom_label(hjust = "inward") +
    geom_text(aes(x = 1, label = stringr::str_wrap(descripcion,width = 30)),
              hjust = 0) +
    scale_x_continuous(limits = c(0,2)) +
    labs(x = NULL) +
    theme(rect = element_blank(), legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

plan <- function(diseño){
  diseño$niveles %>% filter(nivel == 0) %>% select(llave, unidades) %>%
    mutate(Entrevistas = diseño$n,
           llave = "Planeado") %>%
    rename(Manzanas = unidades) %>%
    add_row(llave = "Recálculo",
            Manzanas = NA,
            Entrevistas = diseño$niveles %>% filter(nivel == 0) %>% pull(unidades) * diseño$n_0) %>%
    add_row(llave = "Resultado",
            Manzanas = diseño$muestra %>% purrr::pluck(diseño$ultimo_nivel) %>% nrow,
            Entrevistas = sum(diseño$cuotas$n)) %>%
    tidyr::pivot_longer(-llave, names_to = "total") %>%
    na.omit() %>%
    ggplot() + geom_line(aes(x = llave, y = factor(value), group = total)) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~total, scales = "free") +
    theme(rect = element_blank())
}
#' Title
#'
#' @param prop_vars
#' @param var_extra
#'
#' @return
#' @export
#'
#' @examples
revision <- function(self, prop_vars = c("POCUPADA"), var_extra = NULL){
  bd <- self$muestra %>% purrr::pluck(self$ultimo_nivel) %>% tidyr::unnest(data)
  bd <- bd %>% mutate(across(prop_vars,~.x/!!rlang::sym(self$variable_poblacional),.names = "{.col}_prop"))
  mm <- self$poblacion$marco_muestral %>%
    mutate(across(prop_vars,~.x/!!rlang::sym(self$variable_poblacional),.names = "{.col}_prop"))

  clusters <- bd %>% select(contains("cluster")) %>% names()
  strata <- bd %>% select(contains("strata")) %>% names()
  fpc <- bd %>% select(contains("fpc")) %>% names()

  diseño <- survey::svydesign(data = bd,
                              ids = survey::make.formula(clusters),
                              strata = survey::make.formula(strata),
                              fpc = survey::make.formula(fpc), pps = "brewer")
  options(survey.lonely.psu="remove")

  tb <- c(self$variable_poblacional,var_extra) %>% purrr::map_df(~{
    puntual <- survey::svytotal(survey::make.formula(.x), design = diseño, na.rm = T)
    original <- mm %>% summarise(original = sum(!!rlang::sym(.x),na.rm = T))
    intervalo <- confint(puntual) %>% as_tibble
    tb <- tibble(original,
                 puntual = puntual %>% as.numeric,
                 intervalo,
                 variable = .x
    ) %>% mutate(color = if_else(between(original,`2.5 %`,`97.5 %`),"blue","orange"))
  }) %>% mutate(variable = reorder(variable,original))


  a <- tb %>% ggplot() +
    geom_segment(aes(x = puntual, xend = original, y =variable, yend = variable )) +
    geom_point(aes(x = original, y = variable, color = color), size = 5) +
    scale_color_identity() +
    scale_x_continuous(labels = scales::comma) +
    geom_rect(aes(xmin = `2.5 %`,
                  xmax = `97.5 %`,
                  ymin = as.numeric(variable)-.5,
                  ymax = as.numeric(variable)+.5
                  # xmin = `2.5 %`, xmax = `97.5 %`
    ), alpha = .5, fill = "gray70") +
    labs(x = NULL, y = NULL, title = "Totales") +
    theme(rect = element_blank())

  if(!is.null(prop_vars)){
    tb_prop <- prop_vars %>% paste0("_prop") %>% purrr::map_df(~{
      puntual <- survey::svymean(survey::make.formula(.x), design = diseño, na.rm = T)
      original <- mm %>% summarise(original = mean(!!rlang::sym(.x),na.rm = T))
      intervalo <- confint(puntual) %>% as_tibble
      tb <- tibble(original,
                   puntual = puntual %>% as.numeric,
                   intervalo,
                   variable = .x
      ) %>% mutate(color = if_else(between(original,`2.5 %`,`97.5 %`),"blue","orange"))
    }) %>% mutate(variable = reorder(variable,original))


    b <- tb_prop %>% ggplot() +
      geom_segment(aes(x = puntual, xend = original, y =variable, yend = variable )) +
      geom_point(aes(x = original, y = variable, color = color), size = 5) +
      scale_color_identity() +
      scale_x_continuous(labels = scales::percent) +
      geom_rect(aes(xmin = `2.5 %`,
                    xmax = `97.5 %`,
                    ymin = as.numeric(variable)-.5,
                    ymax = as.numeric(variable)+.5
                    # xmin = `2.5 %`, xmax = `97.5 %`
      ), alpha = .5, fill = "gray70") +
      labs(x = NULL, y = NULL, title = "Proporciones") +
      theme(rect = element_blank())
  }



  return(if(!is.null(prop_vars)) cowplot::plot_grid(a,b) else a)

}

#' Title
#'
#' @param self
#' @param prop_vars
#' @param var_extra
#'
#' @return
#' @export
#'
#' @examples
revision_ine <- function(self, prop_vars = NULL, var_extra = NULL){
  bd <- self$muestra %>% purrr::pluck("SECCION") %>%
    mutate(data = map(data, ~.x %>%
                        select(SECCION, contains("strata"), contains("cluster"),
                               contains("fpc")) %>% select(-contains("_0")) %>% distinct(.keep_all = T))) %>%
    tidyr::unnest(data)

  mm <- self$poblacion$informacion_electoral #%>%
  # mutate(across(prop_vars,~.x/!!rlang::sym(self$variable_poblacional),.names = "{.col}_prop"))

  bd <- bd %>% left_join(mm, by = c("SECCION" = "seccion"))
  # bd <- bd %>% mutate(across(prop_vars,~.x/!!rlang::sym(self$variable_poblacional),.names = "{.col}_prop"))

  clusters <- bd %>% select(contains("cluster")) %>% names()
  strata <- bd %>% select(contains("strata")) %>% names()
  fpc <- bd %>% select(contains("fpc")) %>% names()

  diseño <- survey::svydesign(data = bd,
                              ids = survey::make.formula(clusters),
                              strata = survey::make.formula(strata),
                              fpc = survey::make.formula(fpc), pps = "brewer")
  options(survey.lonely.psu="remove")

  tb <- c(self$variable_poblacional,var_extra) %>% purrr::map_df(~{
    puntual <- survey::svytotal(survey::make.formula(.x), design = diseño, na.rm = T, deff = T)
    original <- mm %>% summarise(original = sum(!!rlang::sym(.x),na.rm = T))
    intervalo <- confint(puntual) %>% as_tibble
    tb <- tibble(original,
                 puntual = puntual %>% as.numeric,
                 intervalo,
                 variable = .x,
                 deff = !!puntual %>% as_tibble() %>% pull("deff")
    ) %>% mutate(color = if_else(between(original,`2.5 %`,`97.5 %`),"blue","orange"))
  }) %>% mutate(variable = reorder(variable,original)) %>% mutate(longitud = `97.5 %` - `2.5 %`)


  a <- tb %>% ggplot() +
    geom_segment(aes(x = puntual, xend = original, y =variable, yend = variable )) +
    geom_point(aes(x = original, y = variable, color = color), size = 5) +
    geom_text(data = tb %>% top_n(5,wt = longitud),
              aes(x = `2.5 %`, y = as.numeric(variable)+.5, label = scales::comma(longitud)),
              hjust = 1, vjust = 0) +
    geom_segment(data = tb %>% top_n(5,wt = longitud),
                 aes(x = `2.5 %`,
                     xend = `97.5 %`,
                     y = as.numeric(variable)+.5,
                     yend = as.numeric(variable)+.5)) +
    scale_color_identity() +
    scale_x_continuous(labels = scales::comma) +
    geom_rect(aes(xmin = `2.5 %`,
                  xmax = `97.5 %`,
                  ymin = as.numeric(variable)-.5,
                  ymax = as.numeric(variable)+.5
                  # xmin = `2.5 %`, xmax = `97.5 %`
    ), alpha = .5, fill = "gray70") +
    labs(x = NULL, y = NULL, title = "Totales") +
    theme(rect = element_blank()) + scale_y_discrete(expand = expansion(add = c(0,1)))

  if(!is.null(prop_vars)){
    tb_prop <- prop_vars %>% purrr::map_df(~{
      # puntual <- survey::svymean(survey::make.formula(.x), design = diseño, na.rm = T)
      puntual <- survey::svyratio(numerator =survey::make.formula(.x),
                                  denominator = survey::make.formula(self$variable_poblacional),
                                  design = diseño, na.rm = T, deff = T)

      original <- mm %>%
        summarise(original = sum(!!rlang::sym(.x), na.rm = T)/sum(!!rlang::sym(self$variable_poblacional),
                                                                  na.rm = T))

      intervalo <- confint(puntual) %>% as_tibble
      tb <- tibble(original,
                   puntual = puntual$ratio %>% as.numeric,
                   intervalo,
                   variable = .x %>% paste0("_prop")
      ) %>% mutate(color = if_else(between(original,`2.5 %`,`97.5 %`),"blue","orange"))
    }) %>% mutate(variable = reorder(variable,original)) %>% mutate(longitud = `97.5 %` - `2.5 %`)

    b <- tb_prop %>% ggplot() +
      geom_segment(aes(x = puntual, xend = original, y =variable, yend = variable )) +
      geom_point(aes(x = original, y = variable, color = color), size = 5) +
      geom_text(data = tb_prop %>% top_n(5,wt = longitud),
                aes(x = `2.5 %`, y = as.numeric(variable)+.5, label = scales::percent(longitud)),
                hjust = 1, vjust = 0) +
      geom_segment(data = tb_prop %>% top_n(5,wt = longitud),
                   aes(x = `2.5 %`,
                       xend = `97.5 %`,
                       y = as.numeric(variable)+.5,
                       yend = as.numeric(variable)+.5)) +
      scale_color_identity() +
      scale_x_continuous(labels = scales::percent_format()) +
      geom_rect(aes(xmin = `2.5 %`,
                    xmax = `97.5 %`,
                    ymin = as.numeric(variable)-.5,
                    ymax = as.numeric(variable)+.5
                    # xmin = `2.5 %`, xmax = `97.5 %`
      ), alpha = .5, fill = "gray70") +
      labs(x = NULL, y = NULL, title = "Proporciones") +
      theme(rect = element_blank()) + scale_y_discrete(expand = expansion(add = c(0,1)))
  }

  res <- if(!is.null(prop_vars)) cowplot::plot_grid(a,b) else a

  return(list(res, tb))

}
