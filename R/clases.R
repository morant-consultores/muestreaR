Diseño <- R6::R6Class("Diseño",
                      public =list(
                        poblacion=NULL,
                        niveles=0,
                        N=NULL,
                        initialize = function(poblacion, N){
                          self$poblacion=poblacion
                        },
                        agregar_nivel=function(variable, descripcion=NULL){
                          self$niveles <- self$niveles+1
                          self$poblacion$marco_muestral <- self$poblacion$marco_muestral %>%
                            rename("id_{niveles}":={{variable}})
                          return(invisible(self))
                        }
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

