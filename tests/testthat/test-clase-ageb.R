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

# ---- Diseño censal con plan manual (paridad con DiseñoINE) ----

test_that("Diseño censal acepta plan manual por estrato (paridad INE)", {
  pob <- PoblacionAGEB$new("Fixture", censo_clase_prueba())
  pob$marco_muestral <- pob$marco_muestral |> dplyr::mutate(region = NOM_MUN)
  diseno <- Diseño$new(
    poblacion = pob, n = 40, n_0 = 5,
    variable_poblacional = "P_18YMAS",
    unidad_muestreo = "Manzanas",
    id_unidad_muestreo = "id", llave_muestreo = "Man",
    semilla = 11
  )
  diseno$agregar_nivel("region", tipo = "strata",
                       descripcion = "Regiones", llave = "region")
  diseno$agregar_nivel("AGEB", tipo = "cluster",
                       descripcion = "AGEBs", llave = "AGEB")
  diseno$plan_muestra(nivel = 1, criterio = "manual", manual = c(2, 2))
  diseno$n_i$strata_1$n_1 <- c(20, 20)
  suppressWarnings(diseno$plan_muestra(nivel = 2))   # último nivel: criterio fijo

  expect_equal(diseno$n_i$strata_1$m_1, c(2, 2))
  # manzanas por AGEB = (n_1 / m_1) / n_0 = (20/2)/5 = 2
  expect_true(all(diseno$n_i$cluster_2$m_2 == 2))
})

# ---- disenar_muestra_ageb (declarativa de la clase) ----

poblacion_ageb_prueba <- function() {
  pob <- PoblacionAGEB$new("Fixture AGEB", censo_clase_prueba())
  pob$marco_muestral <- pob$marco_muestral |> dplyr::mutate(region = NOM_MUN)
  pob
}

test_that("disenar_muestra_ageb ejecuta el pipeline de la clase de punta a punta", {
  diseno <- suppressWarnings(disenar_muestra_ageb(
    poblacion_ageb_prueba(),
    estratos = tibble::tibble(estrato = c("Nezahualcóyotl", "Toluca"),
                              entrevistas = c(20, 20)),
    n_0 = 5, manzanas_por_ageb = 2,
    tasa_rechazo = 0.5, modo_rechazo = "manzanas",
    semilla = 7
  ))

  # asignación del modelo operativo: 2 AGEBs/estrato, 2->4 manzanas (rechazo 2x)
  asig <- attr(diseno, "asignacion")
  expect_equal(asig$secciones, c(2, 2))
  expect_equal(asig$manzanas_por_seccion, c(4, 4))
  expect_equal(asig$entrevistas_a_levantar, c(40, 40))

  # muestra extraída: 4 AGEBs con 4 manzanas cada uno; n_0 = 5 por manzana
  ult <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  expect_equal(dplyr::n_distinct(ult$AGEB), 4)
  expect_equal(nrow(ult), 16)

  # la población vive dentro de la clase, con los fpc en el marco
  expect_true(all(c("fpc_2", "fpc_0") %in%
                    names(diseno$poblacion$marco_muestral)))

  # cuotas censales (las instrucciones por AGEB de los mapas de campo)
  expect_s3_class(diseno$cuotas, "data.frame")
  expect_true(all(c("rango", "sexo", "n") %in% names(diseno$cuotas)))
  expect_true(all(diseno$cuotas |> dplyr::count(cluster_2, wt = n) |>
                    dplyr::pull(n) == 20))
})

test_that("derivar_plan_ageb reconstruye el plan versionado desde la clase", {
  diseno <- suppressWarnings(disenar_muestra_ageb(
    poblacion_ageb_prueba(),
    estratos = tibble::tibble(estrato = c("Nezahualcóyotl", "Toluca"),
                              entrevistas = c(20, 20)),
    n_0 = 5, manzanas_por_ageb = 2,
    tasa_rechazo = 0.5, modo_rechazo = "manzanas",
    semilla = 7
  ))
  plan <- attr(diseno, "plan_ageb")

  expect_true(all(c("ageb", "estrato", "ln_ageb", "pi_ageb", "n_plan",
                    "contactos") %in% names(plan)))
  expect_equal(attr(plan, "unidad"), "ageb")
  expect_equal(nrow(plan), 4)
  expect_equal(sum(plan$contactos), 80)   # a levantar (20 por AGEB)
  expect_equal(sum(plan$n_plan), 40)      # efectivas objetivo (tasa 0.5)

  # pi exactas del sorteo: el fpc del nivel AGEB sobre el marco completo
  marco <- diseno$poblacion$marco_muestral
  esperado <- marco |> dplyr::distinct(AGEB, fpc_2)
  expect_equal(plan$pi_ageb, esperado$fpc_2[match(plan$ageb, esperado$AGEB)])

  # un solo sorteo: mismo puente a encuestar que el flujo ligero
  capas <- plan_para_capas(plan)
  expect_true(all(c("seccion", "pi_seccion", "ln_seccion", "n_plan")
                  %in% names(capas)))
})
