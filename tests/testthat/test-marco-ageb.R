# Marcos censales INEGI: construir_marco_ageb (una fila por AGEB urbana) y
# construir_marco_manzanas (una fila por manzana), desde el dataset
# "ageb_mza_urbana" del Censo 2020. El fixture censo_prueba() imita el CSV
# real: todo character, filas de totales mezcladas con manzanas y
# enmascaramiento INEGI ("*").

censo_prueba <- function() {
  filas_total <- tibble::tibble(
    ENTIDAD = "15", NOM_ENT = "México",
    MUN = c("121", "121", "58", "58"),
    NOM_MUN = c("Cuautitlán Izcalli", "Cuautitlán Izcalli",
                "Nezahualcóyotl", "Nezahualcóyotl"),
    LOC = c("1", "1", "1", "1"),
    NOM_LOC = "Total AGEB urbana",
    AGEB = c("0010", "025A", "0033", "0048"),
    MZA = "0",
    POBTOT = c("4000", "2500", "3000", "1800"),
    P_18YMAS = c("2800", "1700", "2100", "1200"),
    VIVPAR_HAB = c("1100", "700", "800", "500")
  )
  filas_mza <- tidyr::expand_grid(
    dplyr::distinct(filas_total, ENTIDAD, NOM_ENT, MUN, NOM_MUN, LOC, AGEB),
    MZA = c("1", "2", "3")
  ) |>
    dplyr::mutate(
      NOM_LOC = "Ciudad ejemplo",
      POBTOT = as.character(100 + as.integer(MZA) * 50),
      P_18YMAS = as.character(70 + as.integer(MZA) * 30),
      VIVPAR_HAB = "40"
    )
  # una manzana enmascarada por confidencialidad INEGI
  filas_mza$POBTOT[1] <- "*"
  filas_mza$P_18YMAS[1] <- "*"
  # y las filas de totales de entidad/municipio/localidad que trae el CSV
  otros_totales <- filas_total[1, ] |>
    dplyr::mutate(NOM_LOC = "Total de la entidad", AGEB = "0000")
  dplyr::bind_rows(filas_total, filas_mza, otros_totales)
}

test_that("construir_marco_ageb: una fila por AGEB, llaves de 13 y tamaños numéricos", {
  marco <- construir_marco_ageb(censo_prueba())
  expect_equal(nrow(marco), 4)
  expect_true(all(nchar(marco$ageb) == 13))
  expect_true(all(c("ageb", "entidad", "municipio_cod", "nombre_municipio",
                    "localidad_cod", "pob18", "pobtot", "viviendas")
                  %in% names(marco)))
  expect_equal(marco$ageb[1], "1512100010010")
  expect_equal(marco$ageb[2], "151210001025A")   # AGEB alfanumérico
  expect_equal(marco$municipio_cod[3], "15058")  # MUN se rellena a 3
  expect_equal(marco$pob18[1], 2800)
  expect_equal(anyDuplicated(marco$ageb), 0)
})

test_that("construir_marco_manzanas: llaves de 16 anidadas en el AGEB y asteriscos a NA", {
  mzas <- construir_marco_manzanas(censo_prueba())
  expect_equal(nrow(mzas), 12)
  expect_true(all(nchar(mzas$manzana) == 16))
  expect_true(all(substr(mzas$manzana, 1, 13) %in%
                    construir_marco_ageb(censo_prueba())$ageb))
  enmascarada <- mzas$manzana == "1512100010010001"
  expect_true(any(enmascarada))
  expect_true(is.na(mzas$pobtot[enmascarada]))
  expect_true(is.na(mzas$pob18[enmascarada]))
  expect_equal(anyDuplicated(mzas$manzana), 0)
})

test_that("los marcos censales validan columnas y llaves duplicadas", {
  expect_error(construir_marco_ageb(censo_prueba()[, 1:4]), "[Ff]alta")
  censo_dup <- dplyr::bind_rows(censo_prueba(), censo_prueba()[1, ])
  expect_error(construir_marco_ageb(censo_dup), "duplicad")
  expect_error(construir_marco_manzanas(
    dplyr::bind_rows(censo_prueba(), censo_prueba()[5, ])), "duplicad")
})

test_that("construir_marco_ageb avisa por AGEBs sin población sorteable", {
  censo <- censo_prueba()
  censo$P_18YMAS[censo$AGEB == "0048" & censo$MZA == "0"] <- "*"
  expect_message(marco <- construir_marco_ageb(censo), "sin poblaci")
  expect_true(is.na(marco$pob18[marco$ageb == "1505800010048"]))
})
