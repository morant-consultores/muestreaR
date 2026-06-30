#' Diseño muestral polietápico (marco censal INEGI)
#'
#' Clase R6 que representa un diseño de muestra polietápico sobre el marco
#' censal del INEGI. Acumula los niveles (estratos y conglomerados), calcula el
#' plan de muestra y los factores de corrección poblacional, extrae la muestra y
#' calcula las cuotas de edad y sexo.
#'
#' @param semilla Valor numérico opcional. Si se proporciona, el diseño es
#'   reproducible: cada etapa estocástica fija una sub-semilla derivada de
#'   `semilla`. Si es `NULL` (por defecto), el comportamiento es el histórico.
#'
#' @export
#' @import tibble dplyr
Diseño <- R6::R6Class("Diseño",
                      public =list(
                        poblacion=NULL,
                        ultimo_nivel=-1,
                        n=NULL,
                        n_0=NULL,
                        n_i=list(),
                        variable_poblacional=NULL,
                        niveles= tibble::tibble(nivel=NULL,
                                       tipo=NULL,
                                       descripcion=NULL,
                                       llave=NULL,
                                       aprobado=NULL,
                                       unidades=NULL,
                                       plan_muestra=NULL),
                        muestra = NULL,
                        cuotas = NULL,
                        n_sustitucion = 0,
                        dir.exportar = NULL,
                        semilla = NULL,
                        initialize = function(poblacion,
                                              n,
                                              n_0,
                                              variable_poblacional,
                                              unidad_muestreo,
                                              id_unidad_muestreo,
                                              llave_muestreo,
                                              semilla = NULL){
                          self$poblacion=poblacion
                          self$n=n
                          private$unidad_muestreo=unidad_muestreo
                          self$n_0=n_0
                          self$variable_poblacional=variable_poblacional
                          self$semilla=semilla
                          self$niveles=self$agregar_nivel(variable=id_unidad_muestreo,
                                                          tipo="cluster",
                                                          descripcion = unidad_muestreo,
                                                          llave = llave_muestreo)
                          self$n_i <- self$plan_muestra(nivel = self$ultimo_nivel)
                        },
                        print = function(){
                          mensaje <- cat(
                            glue::glue(
                              "Diseño que representa a la población de {self$poblacion$nombre} con una muestra de tamaño {scales::comma(self$n)} cuya unidad de muestreo es {private$unidad_muestreo}.

                               Se realizarán {self$n_0} entrevistas por unidad mínima.

                               Para efectos del diseño muestral se utilizará {self$variable_poblacional} del censo 2020 del INEGI para cuantificar el tamaño de la población."))
                          return(mensaje)
                        },
                        agregar_nivel=function(variable,
                                               tipo,
                                               descripcion,
                                               llave){
                          if(!tipo %in% c("strata", "cluster")) stop("Tipo debe ser igual a cluster o strata")
                          # Al último nivel le agregamos uno
                          self$ultimo_nivel <- self$ultimo_nivel+1
                          # Modificar el marco muestral
                          self$poblacion$marco_muestral <- self$poblacion$marco_muestral %>%
                            {if(self$ultimo_nivel!=0) agrupar_nivel(., nivel=self$ultimo_nivel)
                              else .
                            } %>%
                            group_by(!!sym(variable), .add = TRUE) %>%
                            mutate("{tipo}_{self$ultimo_nivel}":= cur_group_id()) %>%
                            ungroup()
                          # Modificar sel niveles
                          self$niveles <- self$niveles %>%
                            add_row(.data = tibble(nivel=self$ultimo_nivel,
                                                   variable=variable,
                                                   tipo=tipo,
                                                   descripcion=descripcion,
                                                   llave=llave,
                                                   unidades=NA_integer_,
                                                   aprobado=F,
                                                   plan_muestra=F))

                          return(self$niveles)
                        },
                        eliminar_nivel=function(nivel){
                          aux <- nivel
                          self$niveles <- self$niveles %>%
                            filter(nivel<aux)
                          self$ultimo_nivel <- nivel-1
                          self$poblacion$marco_muestral <- self$poblacion$marco_muestral %>%
                            select(-matches(glue::glue("(cluster|strata)_[{nivel}-9]")))
                          self$n_i <- self$n_i[-grep(glue::glue("(cluster|strata)_[{nivel}-9]"),
                                                     names(self$n_i))]
                        },
                        plan_muestra =function(nivel, criterio, unidades_nivel){
                          nivel_l <- nivel
                          if(nivel_l==0){
                            # Se asigna el nivel 0
                            res <- self$poblacion$marco_muestral %>%
                              group_by(cluster_0) %>%
                              summarise(m_0=self$n_0,
                                        n_0=self$n_0)
                            # Se le agrega el total de unidades al nivel
                            self$niveles <- self$niveles %>%
                              mutate(unidades=if_else(nivel==0,
                                                      ceiling(self$n/self$n_0),
                                                      as.numeric(unidades)))
                            # Se etiqueta en la lista
                            res <- list(cluster_0=res)
                          }
                          else{
                            # Cuando es el último nivel
                            if(nivel_l==self$ultimo_nivel){
                              res <- asignar_m(self,
                                               unidades_nivel = ) %>%
                                left_join(asignar_n(self))
                              res <- purrr::set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
                            }
                            # Si no es nivel=0 ni es nivel=ultimo_nivel
                            else{
                              # Si no es el último nivel
                              # Primero se asigna m
                              # Después se asigna n
                              res <- asignar_m(diseño = self,
                                               criterio = criterio,
                                               unidades_nivel = unidades_nivel) %>%
                                left_join(asignar_n(self))
                              # Se etiqueta
                              res <- purrr::set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
                            }
                          }
                          self$niveles <- self$niveles %>%
                            mutate(plan_muestra=(nivel<=nivel_l))
                          self$n_i <- c(self$n_i, res)
                          return(res)
                        },
                        fpc = function(nivel){
                          self$poblacion$marco_muestral <- calcular_fpc(self, nivel = nivel)
                        },
                        extraer_muestra = function(nivel){
                          # Reproducibilidad: sub-semilla por nivel (ver campo `semilla`).
                          if(!is.null(self$semilla)) set.seed(self$semilla + nivel)
                          m <- muestrear(self, nivel = nivel)
                          aux <- m %>% purrr::pluck(length(m))
                          if(nivel == self$ultimo_nivel){
                            #actualizar fpc_0
                            if(self$niveles %>% filter(nivel == 0) %>% pull(unidades) != nrow(aux)){
                              ajuste <- ((self$niveles %>% filter(nivel == 0) %>% pull(unidades)) - nrow(aux))*self$n_0

                              nuevo <- self$n_i$cluster_0 %>% semi_join(aux) %>% sample_n(size = abs(ajuste), replace = T) %>%
                                mutate(sumar = sign(ajuste)) %>% group_by(cluster_0) %>%
                                summarise(m_0 = unique(m_0), n_0 = unique(n_0),
                                          sumar = sum(sumar)) %>% mutate(n_0 = n_0 + sumar) %>% select(-sumar)
                              self$n_i$cluster_0 <- self$n_i$cluster_0 %>% anti_join(nuevo, by = "cluster_0") %>% bind_rows(nuevo) %>%
                                arrange(cluster_0)
                            }
                          }
                          self$muestra <- m
                        },
                        calcular_cuotas = function(){
                          if(!is.null(self$semilla)) set.seed(self$semilla + 1000)
                          self$cuotas <- cuotas(self)
                        },
                        revisar_muestra = function(prop_vars, var_extra){
                          a <- llaves(self)

                          b <- plan(self)

                          c <- revision(self = self,prop_vars = prop_vars, var_extra = var_extra)

                          return(list(a,b,c))

                        },
                        exportar = function(shp, zoom = 16, carpeta = "Insumos", mapas = TRUE){
                          self$dir.exportar <- carpeta
                          if(!file.exists(carpeta)) dir.create(carpeta)
                          if(mapas){
                            if(!file.exists(glue::glue("{carpeta}/Mapas"))) dir.create(glue::glue("{carpeta}/Mapas"))
                            shp$crear_mapas(diseño = self, zoom = zoom, dir = glue::glue("{carpeta}/Mapas"))
                          }
                          if(!is.null(self$cuotas)) self$cuotas %>% readr::write_excel_csv(glue::glue("{carpeta}/cuotas.csv"))
                          readr::write_rds(self, glue::glue("{carpeta}/diseño.rda"))
                          shp %>% readr::write_rds(glue::glue("{carpeta}/shp.rda"))
                        },
                        sustituir_muestra = function(shp, id, zoom = 16){
                          if(!is.null(self$semilla)) set.seed(self$semilla + 2000 + self$n_sustitucion)
                          self <- sustituir_muestra(self, shp, id, zoom, dir = glue::glue("{self$dir.exportar}/Mapas"))
                          file.rename(glue::glue("{self$dir.exportar}/Mapas/{id}.png"),
                                      glue::glue("{self$dir.exportar}/Mapas/{id} eliminada.png"))
                          self$n_sustitucion <- self$n_sustitucion + 1
                          readr::write_rds(self, glue::glue("{self$dir.exportar}/diseño{self$n_sustitucion}.rda"))
                          self$cuotas %>% readr::write_excel_csv(glue::glue("{self$dir.exportar}/cuotas{self$n_sustitucion}.csv"))
                        }
                      ),
                      private = list(
                        unidad_muestreo=NULL
                      )
)

#' Población y marco muestral (marco censal INEGI)
#'
#' Clase R6 que construye y almacena el marco muestral a partir de las bases del
#' censo y la cartografía, y permite clasificar las unidades en regiones.
#'
#' @export
Poblacion <- R6::R6Class("Poblacion",
                         public = list(
                           nombre = NULL,
                           marco_muestral=NULL,
                           initialize=function(nombre,
                                               base_manzana,
                                               base_localidad,
                                               shp_localidad_no_amanzanada,
                                               shp_localidad_amanzanada){
                             self$nombre = nombre
                             self$marco_muestral = muestreaR::crear_mm(mza = base_manzana,
                                                                       loc = base_localidad,
                                                                       lpr_shp = shp_localidad_no_amanzanada,
                                                                       loc_shp = shp_localidad_amanzanada)

                           },
                           calcular_poblacion=function(na.rm=T){
                             res= sum(self$marco_muestral[,self$variable_poblacion],na.rm = na.rm)
                             return(res)
                           },
                           regiones = function(id, regiones){
                             self$marco_muestral<- regiones(self$marco_muestral,
                                                                 id = id,
                                                                 regiones = regiones)
                           }
                         )
)

#' Cartografía del diseño (marco censal INEGI)
#'
#' Clase R6 que almacena las cartografías del diseño y provee métodos para
#' graficar mapas de la población y de la muestra, y exportar los mapas de campo.
#'
#' @export
Cartografia <- R6::R6Class("Cartografia",
                           public = list(
                             shp = NULL,
                             initialize = function(
                               mun_shp = NULL,
                               loc_shp = NULL,
                               agebR_shp = NULL,
                               agebU_shp = NULL,
                               lpr_shp = NULL,
                               mza_shp = NULL
                             ){
                               self$shp <- crear_shp(mun_shp, loc_shp, agebR_shp,
                                                     agebU_shp, lpr_shp, mza_shp)
                             },
                             graficar_mapa = function(lflt = NULL, bd, nivel){
                               graficar_mapa_muestra(lflt = lflt,
                                                     muestra = if(!is.data.frame(bd)) bd %>% purrr::pluck(nivel) %>% tidyr::unnest(data) else bd,
                                                     shp = self$shp,
                                                     nivel = nivel)
                             },
                             crear_mapas = function(diseño, zoom, dir){
                               google_maps(diseño, shp = self$shp, zoom = zoom, dir = dir)
                             }
                           ))

