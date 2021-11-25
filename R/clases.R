Diseño <- R6::R6Class("Diseño",
                      public =list(
                        poblacion=NULL,
                        initialize = function(poblacion){

                        })
)

Poblacion <- R6::R6Class("Poblacion",
                         public = list(
                           nombre = NULL,
                           marco_muestral=NULL,
                           initialize=function(nombre,
                                               base_manzana,
                                               base_localidad,
                                               shp_ageb_rural,
                                               shp_localidad_amanzanada){
                             self$marco_muestral <- muestreaR::crear_mm(mza = base_manzana,
                                                                        loc = base_localidad,
                                                                        ageb_shp = shp_ageb_rural,
                                                                        loc_shp = shp_localidad_amanzanada)

                           }
                         )
)

