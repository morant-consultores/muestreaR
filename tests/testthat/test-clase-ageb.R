# Flujo de la CLASE para el marco censal AGEB: la población vive dentro del
# diseño, el diseño se exporta (diseño.rda + shp.rda + cuotas.csv + Mapas/)
# y es el insumo de AppAuditoria y encuestar (tipo_encuesta = "inegi").
# Fixture: censo_clase_prueba() en helper-fixture.R.

# ---- crear_mm_ageb / PoblacionAGEB ----

test_that("crear_mm_ageb produce el marco por manzana compatible con la clase", {
  marco <- crear_mm_ageb(censo_clase_prueba())
  expect_equal(nrow(marco), 2 * 3 * 6)   # solo filas de manzana
  expect_true(all(c("id", "ENTIDAD", "MUN", "NOM_MUN", "LOC", "NOM_LOC",
                    "AGEB", "MZA", "AMBITO", "ARLU", "AULR",
                    "POBTOT", "P_18YMAS", "P_18A24_F", "P_18A24_M",
                    "P_18YMAS_F", "P_18YMAS_M", "P_60YMAS_F", "P_60YMAS_M")
                  %in% names(marco)))
  # llaves anidadas al estilo crear_mm: MUN 5, LOC 9, AGEB 13, MZA 16
  expect_true(all(nchar(marco$MUN) == 5))
  expect_true(all(nchar(marco$LOC) == 9))
  expect_true(all(nchar(marco$AGEB) == 13))
  expect_true(all(nchar(marco$MZA) == 16))
  expect_true(all(substr(marco$MZA, 1, 13) == marco$AGEB))
  # las llaves de cruce con la cartografía censal (crear_shp)
  expect_true(all(marco$AULR == paste0(marco$AGEB, "-AGEB-Urbana")))
  expect_true(all(marco$ARLU == paste0(marco$LOC, "-LOC-Urbana")))
  expect_equal(unique(marco$AMBITO), "Urbana")
  # bloque censal numérico parseado
  expect_true(is.numeric(marco$P_18YMAS))
  expect_true(is.numeric(marco$POBTOT))
})

test_that("crear_mm_ageb parsea el enmascaramiento INEGI y valida llaves", {
  censo <- censo_clase_prueba()
  censo$P_18YMAS[censo$MZA == "001"][1] <- "*"
  marco <- crear_mm_ageb(censo)
  expect_true(anyNA(marco$P_18YMAS))
  expect_error(crear_mm_ageb(dplyr::bind_rows(censo, censo[censo$MZA == "002", ][1, ])),
               "duplicad")
})

test_that("PoblacionAGEB construye la población con el marco adentro", {
  pob <- PoblacionAGEB$new("EdoMex urbano", censo_clase_prueba())
  expect_s3_class(pob, "Poblacion")
  expect_equal(pob$nombre, "EdoMex urbano")
  expect_equal(nrow(pob$marco_muestral), 36)
  expect_equal(pob$calcular_poblacion(),
               sum(pob$marco_muestral$P_18YMAS, na.rm = TRUE))
})
