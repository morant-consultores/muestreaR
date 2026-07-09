# Invariante del marco: TODO lo que se puede muestrear tiene información
# completa = censo + polígono de manzana + polígono de AGEB. Un AGEB o una
# manzana sin cartografía NO puede ser seleccionable (no se podría localizar
# ni mapear en campo): se excluye del marco, documentado, ANTES del sorteo
# con reconciliar_marco_cartografia(). Fixtures en helper-fixture.R.

test_that("reconciliar_marco_cartografia excluye y documenta lo que no tiene polígono", {
  censo <- censo_clase_prueba()
  marco <- crear_mm_ageb(censo)
  cart <- cartografia_ageb_prueba(marco)   # cartografía completa

  # quitamos un AGEB entero y una manzana suelta (de otro AGEB) de la carto
  ageb_out <- cart$shp$AGEB$AULR[1]
  mza_out  <- setdiff(cart$shp$MZA$MZA,
                      marco$MZA[marco$AULR == ageb_out])[1]
  cart$shp$AGEB <- cart$shp$AGEB[cart$shp$AGEB$AULR != ageb_out, ]
  cart$shp$MZA  <- cart$shp$MZA[cart$shp$MZA$MZA != mza_out, ]

  rec <- reconciliar_marco_cartografia(marco, cart)

  # INVARIANTE: nada sin polígono sobrevive en el marco
  expect_true(all(rec$MZA %in% cart$shp$MZA$MZA))
  expect_true(all(rec$AULR %in% cart$shp$AGEB$AULR))
  expect_false(mza_out %in% rec$MZA)
  expect_false(any(rec$AULR == ageb_out))
  # el AGEB removido se fue completo (todas sus manzanas)
  expect_equal(sum(marco$AULR == ageb_out) + 1L, nrow(marco) - nrow(rec))

  # documentación de cobertura para la nota metodológica
  doc <- attr(rec, "cobertura")
  expect_true(all(c("MZA", "AGEB", "motivo") %in% names(doc)))
  expect_setequal(unique(doc$motivo),
                  c("AGEB sin polígono", "manzana sin polígono"))
  expect_true(all(doc$motivo[doc$MZA %in% marco$MZA[marco$AULR == ageb_out]]
                  == "AGEB sin polígono"))
})

test_that("con cartografía completa el marco no pierde nada", {
  marco <- crear_mm_ageb(censo_clase_prueba())
  rec <- reconciliar_marco_cartografia(marco, cartografia_ageb_prueba(marco))
  expect_equal(nrow(rec), nrow(marco))
  expect_equal(nrow(attr(rec, "cobertura")), 0)
})

test_that("todo lo muestreable tiene cartografía completa (invariante end-to-end)", {
  pob <- PoblacionAGEB$new("Inv", censo_clase_prueba())
  pob$marco_muestral <- pob$marco_muestral |> dplyr::mutate(region = NOM_MUN)
  cart <- cartografia_ageb_prueba(pob$marco_muestral)

  # un AGEB pierde su polígono => sus manzanas dejan de ser muestreables
  ageb_out <- cart$shp$AGEB$AULR[1]
  cart$shp$AGEB <- cart$shp$AGEB[cart$shp$AGEB$AULR != ageb_out, ]

  pob$marco_muestral <- reconciliar_marco_cartografia(pob$marco_muestral, cart)
  expect_false(any(pob$marco_muestral$AULR == ageb_out))

  diseno <- suppressWarnings(disenar_muestra_ageb(
    pob,
    estratos = tibble::tibble(estrato = c("Nezahualcóyotl", "Toluca"),
                              entrevistas = c(10, 10)),
    n_0 = 5, manzanas_por_ageb = 2, calcular_cuotas = FALSE, semilla = 3
  ))
  bd <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)

  # NADA sorteado carece de polígono: ni la manzana ni su AGEB
  expect_true(all(bd$MZA %in% cart$shp$MZA$MZA))
  expect_true(all(bd$AULR %in% cart$shp$AGEB$AULR))
  expect_false(any(bd$AULR == ageb_out))
})
