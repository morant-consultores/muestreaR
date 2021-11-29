Diseño <- R6::R6Class("Diseño",
                      public =list(
                        poblacion=NULL,
                        ultimo_nivel=-1,
                        N=NULL,
                        n=NULL,
                        tamaño_muestras=list(),
                        niveles=tibble(nivel=NULL,
                                       tipo=NULL,
                                       descripcion=NULL,
                                       llave=NULL,
                                       aprobado=NULL),
                        initialize = function(poblacion,
                                              N,
                                              unidad_muestreo,
                                              id_unidad_muestreo,
                                              llave_muestreo){
                          self$poblacion=poblacion
                          private$unidad_muestreo=unidad_muestreo
                          self$niveles=self$agregar_nivel(variable=id_unidad_muestreo,
                                                          tipo="cluster",
                                                          descripcion = unidad_muestreo,
                                                          llave = llave_muestreo)
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
                                                   aprobado=F))

                          return(self$niveles)
                        },
                        plan_muestreo =function(nivel=self$ultimo_nivel,
                                                num,
                                                criterio,
                                                variable_estudio,
                                                ultimo_nivel=F){
                          self$n <- criterio_N(base = self$poblacion$marco_muestral,
                                               nivel=nivel,
                                               variable_estudio = enquo(variable_estudio),
                                               num = num,
                                               criterio = criterio,
                                               ultimo_nivel = ultimo_nivel
                          )
                          return(self$n)
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
                                               shp_ageb_rural,
                                               shp_localidad_amanzanada){
                             self$nombre = nombre
                             self$marco_muestral = muestreaR::crear_mm(mza = base_manzana,
                                                                       loc = base_localidad,
                                                                       ageb_shp = shp_ageb_rural,
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

