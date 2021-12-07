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
                                       plan_muestra=NULL),
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
                            # Listo
                            res <- self$poblacion$marco_muestral %>%
                              group_by(cluster_0) %>%
                              summarise(n_0=self$n_0) %>%
                              mutate(m_0=ceiling(self$n/n_0))
                            res <- list(cluster_0=res)
                          }
                          else{
                            if(nivel_l==self$ultimo_nivel){
                              # res <- self$poblacion$marco_muestral %>%
                              #   left_join(self$n_i[["cluster_0"]], by="cluster_0") %>%
                              #   agrupar_nivel(nivel) %>%
                              #   summarise(m_0=unique(m_0))
                              res <- asignar_m(self,
                                               criterio = "uniforme",
                                               unidades_nivel = unique(self$n_i[["cluster_0"]]$m_0)) %>%
                              left_join(asignar_n(self))
                              res <- set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
                            }
                            # Si no es nivel=0 ni es nivel=ultimo_nivel
                            else{
                              # Repartir m_i a través de alguno de los criterios. Se elige
                              # smi=sum(m_i).
                              res <- asignar_m(diseño = self,
                                               criterio = criterio,
                                               unidades_nivel = unidades_nivel) %>%
                                left_join(asignar_n(self))
                              res <- set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
                            }
                          }
                          self$niveles <- self$niveles %>%
                            mutate(plan_muestra=(nivel<=nivel_l))
                          self$n_i <- c(self$n_i, res)
                          return(res)
                        }
                        # extraer_muestra(nivel,
                        #                 ultimo_nivel=F,
                        #                 plan_muestreo){
                        #   self$poblacion$marco_muestral %>%
                        #     select(nivel_principal, nivel_secundario)
                        # }
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


