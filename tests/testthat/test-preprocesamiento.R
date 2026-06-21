# Tests de las funciones de preprocesamiento (Fase 5).

test_that("leer_cartografia_ine carga solo lo necesario y respeta el distrito", {
  skip_if_not_installed("sf")
  dir <- withr::local_tempdir()
  sq <- function() sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))), crs = 4326))
  for (nm in c("MANZANA", "LOCALIDAD", "MUNICIPIO", "SECCION",
               "DISTRITO_LOCAL", "DISTRITO_FEDERAL", "MANCHA_URBANA")) {
    suppressWarnings(sf::st_write(sq(), file.path(dir, paste0(nm, ".shp")), quiet = TRUE))
  }

  c1 <- leer_cartografia_ine(dir)                       # ninguno
  expect_setequal(names(c1), c("mza", "loc", "mun", "sec"))
  expect_true(all(vapply(c1, inherits, logical(1), "sf")))

  c2 <- leer_cartografia_ine(dir, distrito = "local")
  expect_setequal(names(c2), c("mza", "loc", "mun", "sec", "dl"))

  c3 <- leer_cartografia_ine(dir, distrito = "federal")
  expect_setequal(names(c3), c("mza", "loc", "mun", "sec", "df"))

  # nunca carga la mancha urbana
  expect_false(any(c("murb", "mancha") %in% names(c3)))
})

test_that("corregir_lista_nominal reescala los conteos a la base por sección y sexo", {
  ln <- tibble::tibble(
    SECCION = c(1, 2),
    `LISTA NOMINAL` = c(100, 200), `LISTA HOMBRES` = c(48, 95), `LISTA MUJERES` = c(52, 105),
    `LISTA 18 A 19 HOMBRES` = c(10, 20), `LISTA 18 A 19 MUJERES` = c(11, 21),
    `LISTA 20 A 24 HOMBRES` = c(38, 75), `LISTA 20 A 24 MUJERES` = c(41, 84)
  )
  base_ine <- tibble::tibble(SECCION = c(1, 2),
                             LISTA_HOMBRES = c(50, 90), LISTA_MUJERES = c(55, 110))

  ln2 <- corregir_lista_nominal(ln, base_ine)

  h <- ln2 |> dplyr::select(SECCION, dplyr::contains("HOMBRES")) |>
    tidyr::pivot_longer(-SECCION) |> dplyr::count(SECCION, wt = value, name = "H")
  m <- ln2 |> dplyr::select(SECCION, dplyr::contains("MUJERES")) |>
    tidyr::pivot_longer(-SECCION) |> dplyr::count(SECCION, wt = value, name = "M")

  expect_equal(h$H, c(50, 90))                       # hombres cuadran con la base
  expect_equal(m$M, c(55, 110))                      # mujeres cuadran con la base
  expect_equal(ln2$`LISTA NOMINAL`, c(105, 200))     # total = hombres + mujeres
})

test_that("construir_poblacion_ine arma una PoblacionINE desde la cartografía", {
  fx <- generar_fixture_crear_mm()
  cart <- list(mza = fx$shp_mza, loc = fx$shp_loc, mun = fx$shp_mun)
  secs <- as.character(unique(fx$shp_mza$SECCION))
  electoral <- data.frame(seccion = secs, voto_x = seq_along(secs))

  pob <- suppressWarnings(construir_poblacion_ine(fx$ln, electoral, cart, nombre = "Demo"))

  expect_s3_class(pob, "Poblacion")
  expect_true("lista_nominal" %in% names(pob$marco_muestral))
  expect_length(grep("^LN22_", names(pob$marco_muestral), value = TRUE), 8)
})
