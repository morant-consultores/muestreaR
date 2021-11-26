Diseño <- R6::R6Class("Diseño",
                      public =list(
                        poblacion=NULL,
                        ultimo_nivel=0,
                        N=NULL,
                        niveles=NULL,
                        initialize = function(poblacion,
                                              N,
                                              unidad_muestreo,
                                              id_unidad_muestreo,
                                              llave_muestreo){
                          self$poblacion=poblacion
                          private$unidad_muestreo=unidad_muestreo
                          self$niveles=agregar_nivel(variable=id_unidad_muestreo,
                                                   tipo="cluster",
                                                   descripcion = unidad_muestreo,
                                                   llave = llave_muestreo,
                                                   unidad_muestreo)
                        },
                        agregar_nivel=function(variable,
                                               tipo,
                                               descripcion,
                                               llave){
                          if(!tipo %in% c("strata", "cluster")) stop("Tipo debe ser igual a cluster o strata")
                          # Modificar el marco muestral
                          self$poblacion$marco_muestral <- self$poblacion$marco_muestral %>%
                            group_by({{variable}}, .add = T) %>%
                            mutate(!!glue::glue("{tipo}_{self$ultimo_nivel}"):= cur_group_id())
                          # Modificar sel niveles
                          self$niveles <- self$niveles %>%
                            add_row(nivel=self$nivel, tipo=tipo, descripcion=descripcion, llave=llave, aprobado=F)
                          # Al último nivel le agregamos uno
                          self$ultimo_nivel <- self$ultimo_nivel+1

                          return(invisible(self))
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

