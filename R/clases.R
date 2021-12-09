Diseño <- R6::R6Class("Diseño",
                      public =list(
                        poblacion=NULL,
                        ultimo_nivel=-1,
                        n=NULL,
                        n_0=NULL,
                        n_i=list(),
                        variable_poblacional=NULL,
                        niveles=tibble(nivel=NULL,
                                       tipo=NULL,
                                       descripcion=NULL,
                                       llave=NULL,
                                       aprobado=NULL,
                                       unidades=NULL,
                                       plan_muestra=NULL),
                        muestra = NULL,
                        cuotas = NULL,
                        initialize = function(poblacion,
                                              n,
                                              n_0,
                                              variable_poblacional,
                                              unidad_muestreo,
                                              id_unidad_muestreo,
                                              llave_muestreo){
                          self$poblacion=poblacion
                          self$n=n
                          private$unidad_muestreo=unidad_muestreo
                          self$n_0=n_0
                          self$variable_poblacional=variable_poblacional
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
                            group_by(!!sym(variable), add=T) %>%
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
                              res <- set_names(list(res), glue::glue("{self$niveles %>%
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
                              res <- set_names(list(res), glue::glue("{self$niveles %>%
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
                          m <- muestrear(self, nivel = nivel)
                          aux <- m %>% purrr::pluck(length(m))
                          if(nivel == self$ultimo_nivel){
                            #actualizar fpc_0
                            if(self$niveles %>% filter(nivel == 0) %>% pull(unidades) != nrow(aux)){
                              ajuste <- ((self$niveles %>% filter(nivel == 0) %>% pull(unidades)) - nrow(aux))*self$n_0

                              nuevo <- self$n_i$cluster_0 %>% semi_join(aux) %>% sample_n(size = ajuste, replace = T) %>%
                                mutate(sumar = 1) %>% group_by(cluster_0) %>%
                                summarise(m_0 = unique(m_0), n_0 = unique(n_0),
                                          sumar = sum(sumar)) %>% mutate(n_0 = n_0 + sumar) %>% select(-sumar)
                              self$n_i$cluster_0 <- self$n_i$cluster_0 %>% anti_join(nuevo, by = "cluster_0") %>% bind_rows(nuevo) %>%
                                arrange(cluster_0)
                            }
                          }
                          self$muestra <- m
                        },
                        calcular_cuotas = function(){
                          self$cuotas <- cuotas(self)
                        }
                      ),
                      private = list(
                        unidad_muestreo=NULL
                      )
)

Poblacion <- R6::R6Class("Poblacion",
                         public = list(
                           nombre = NULL,
                           marco_muestral=NULL,
                           variable_poblacion=NULL,
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
                           determinar_variable_poblacional =function(variable){
                             self$variable_poblacion = variable
                             return(self$variable_poblacion)

                           },
                           calcular_poblacion=function(na.rm=T){
                             res= sum(self$marco_muestral[,self$variable_poblacion],na.rm = na.rm)
                             return(res)
                           }
                         )
)

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
                                                     muestra = if(!is.data.frame(bd)) bd %>% purrr::pluck(nivel) %>% unnest(data) else bd,
                                                     shp = self$shp,
                                                     nivel = nivel)
                             },
                             crear_mapas = function(diseño, zoom){
                               google_maps(diseño_qro, shp = self$shp, zoom = zoom)
                             }
                           ))

