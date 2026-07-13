# =====================================================================================
# helper-fixture.R
# -------------------------------------------------------------------------------------
# Fixtures sintéticos deterministas para los tests de muestreaR. NO dependen de
# shapefiles ni lista nominal reales del INE: fabrican directamente el marco muestral
# (lo que crear_mm_ine produciría) para poder ejercitar toda la matemática de muestreo.
#
# Es la versión reutilizable del marco que arma inst/ejemplos/disenar_muestra_demo.R.
# =====================================================================================

# Marco muestral sintético: cada fila es una manzana (unidad mínima).
# Anidamiento limpio  region (estrato) > SECCION (cluster) > manzana,
# igual que la topología de las muestras reales de Edomex 2025.
# 2 regiones x n_secciones x n_manzanas. Determinista.
generar_marco_ine <- function(n_secciones = 6, n_manzanas = 4) {
  geo <- tidyr::expand_grid(
    region    = c("Region 1", "Region 2"),
    seccion_n = seq_len(n_secciones),
    manzana_n = seq_len(n_manzanas)
  ) |>
    dplyr::mutate(
      reg_id     = as.integer(factor(region)),
      # 2 municipios por región (mitad de las secciones cada uno)
      MUNICIPIO  = sprintf("MUN_%d_%d", reg_id, ceiling(seccion_n / (n_secciones / 2))),
      NOMBRE_MUN = paste("Municipio", MUNICIPIO),
      # SECCION única dentro de cada región (anidamiento estricto)
      SECCION    = as.character(reg_id * 100 + seccion_n + 1000),
      id         = as.character(dplyr::row_number())
    )

  geo |>
    dplyr::mutate(lista_nominal = 80 + (dplyr::row_number() * 7) %% 120) |>
    dplyr::mutate(
      LN22_18A24_F  = round(lista_nominal * 0.13),
      LN22_18A24_M  = round(lista_nominal * 0.12),
      LN22_25A39_F  = round(lista_nominal * 0.16),
      LN22_25A39_M  = round(lista_nominal * 0.15),
      LN22_40A59_F  = round(lista_nominal * 0.15),
      LN22_40A59_M  = round(lista_nominal * 0.14),
      LN22_60YMAS_F = round(lista_nominal * 0.08),
      LN22_60YMAS_M = round(lista_nominal * 0.07)
    ) |>
    dplyr::select(id, region, MUNICIPIO, NOMBRE_MUN, SECCION, lista_nominal,
                  dplyr::starts_with("LN22_"))
}

# Objeto `poblacion` mínimo compatible con DiseñoINE, sin pasar por crear_mm_ine.
# DiseñoINE solo necesita leer/escribir poblacion$marco_muestral y poblacion$nombre.
generar_poblacion_ine <- function(marco = generar_marco_ine()) {
  poblacion <- new.env()
  poblacion$nombre <- "Fixture sintético"
  poblacion$marco_muestral <- marco
  poblacion$informacion_electoral <- NULL
  poblacion
}

# -------------------------------------------------------------------------------------
# Fixture GEOESPACIAL para ejercitar crear_mm_ine() sin shapefiles reales del INE.
# Fabrica polígonos cuadrados mínimos (manzanas, localidades, municipios) + lista
# nominal, de modo que crear_mm_ine() corra y produzca un marco muestral válido.
#
# Detalles que importan para que crear_mm_ine no falle:
#   - shp_mza necesita STATUS == 1 y geometrías válidas.
#   - las localidades cubren a sus manzanas para que NO haya localidades rurales
#     (shp_lpr vacío) y el flujo siga la rama urbana.
#   - los nombres de columnas de la lista nominal usan rangos que NO contienen los
#     patrones "_18_" ni "_19_" (que crear_mm_ine reescribe para edades de un año).
# -------------------------------------------------------------------------------------
generar_fixture_crear_mm <- function(con_lista = FALSE) {
  # cuadrado de lado s con esquina inferior-izquierda (x, y)
  sq <- function(x, y, s = 0.01) {
    sf::st_polygon(list(matrix(
      c(x, y, x + s, y, x + s, y + s, x, y + s, x, y), ncol = 2, byrow = TRUE)))
  }

  # 2 municipios x 2 secciones x 3 manzanas = 12 manzanas
  grid <- tidyr::expand_grid(
    MUNICIPIO  = c("001", "002"),
    secc_local = 1:2,
    mza_local  = 1:3
  ) |>
    dplyr::mutate(
      ENTIDAD = "15",
      SECCION = as.character(as.integer(factor(paste(MUNICIPIO, secc_local))) + 100),
      MANZANA = sprintf("%s-%s-%d", MUNICIPIO, SECCION, mza_local),
      row     = dplyr::row_number()
    )

  geom_mza <- lapply(grid$row, function(i) sq(i * 0.01, 0)) |> sf::st_sfc(crs = 4326)
  attrs_mza <- grid |> dplyr::select(ENTIDAD, MUNICIPIO, SECCION, MANZANA) |>
    dplyr::mutate(STATUS = 1L)
  # con_lista = TRUE añade la columna LISTA (lista nominal por manzana) que traen
  # los marcos reales del INE y que activa la rama de relleno proporcional al final
  # de crear_mm_ine (ver R/crear_mm.R, `if("LISTA" %in% colnames(mza))`).
  if (con_lista) attrs_mza <- attrs_mza |> dplyr::mutate(LISTA = 50 + grid$row)
  shp_mza <- sf::st_sf(attrs_mza, geometry = geom_mza)

  # una localidad por sección, que cubre sus manzanas (=> sin rurales)
  loc <- grid |>
    dplyr::group_by(ENTIDAD, MUNICIPIO, SECCION) |>
    dplyr::summarise(row_min = min(row), .groups = "drop") |>
    dplyr::mutate(LOCALIDAD = "0001", NOMBRE = paste0("LOC_", SECCION))
  geom_loc <- lapply(loc$row_min, function(i) sq(i * 0.01, 0, s = 0.03)) |>
    sf::st_sfc(crs = 4326)
  shp_loc <- sf::st_sf(
    loc |> dplyr::select(ENTIDAD, MUNICIPIO, SECCION, LOCALIDAD, NOMBRE),
    geometry = geom_loc
  )

  mun <- grid |> dplyr::distinct(ENTIDAD, MUNICIPIO) |>
    dplyr::mutate(NOMBRE = paste0("Municipio ", MUNICIPIO))
  geom_mun <- lapply(seq_len(nrow(mun)), function(i) sq(i * 0.5, 0, s = 0.4)) |>
    sf::st_sfc(crs = 4326)
  shp_mun <- sf::st_sf(mun, geometry = geom_mun)

  # lista nominal por sección (rangos sin "_18_"/"_19_")
  ln <- grid |> dplyr::distinct(SECCION) |>
    dplyr::mutate(
      LISTA_20_24_HOMBRES = 30, LISTA_20_24_MUJERES = 32,
      LISTA_25_39_HOMBRES = 40, LISTA_25_39_MUJERES = 42,
      LISTA_40_59_HOMBRES = 35, LISTA_40_59_MUJERES = 37,
      LISTA_60_Y_MAS_HOMBRES = 18, LISTA_60_Y_MAS_MUJERES = 20
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(`LISTA NOMINAL` = sum(dplyr::c_across(dplyr::starts_with("LISTA_")))) |>
    dplyr::ungroup()

  list(ln = ln, shp_mza = shp_mza, shp_loc = shp_loc, shp_mun = shp_mun)
}

# Diseño INE completo y reproducible (semilla fija) listo para extraer muestra.
# Replica la topología REAL de las muestras de Edomex 2025: 2 niveles
#   nivel 1 = region  (estrato)
#   nivel 2 = SECCION (cluster, último nivel)
# Ejecuta niveles -> plan_muestra -> fpc y deja el diseño listo para extraer_muestra().
generar_diseno_ine <- function(n = 240, n_0 = 5, semilla = 123, unidades_nivel = 8,
                               poblacion = generar_poblacion_ine()) {
  diseno <- DiseñoINE$new(
    poblacion            = poblacion,
    n                    = n,
    n_0                  = n_0,
    variable_poblacional = "lista_nominal",
    unidad_muestreo      = "Manzanas",
    id_unidad_muestreo   = "id",
    llave_muestreo       = "Man",
    semilla              = semilla
  )
  diseno$agregar_nivel("region",  tipo = "strata",  descripcion = "Regiones",             llave = "region")
  diseno$agregar_nivel("SECCION", tipo = "cluster", descripcion = "Secciones electorales", llave = "SECCION")

  diseno$plan_muestra(nivel = 1, criterio = "peso", unidades_nivel = unidades_nivel)
  diseno$plan_muestra(nivel = 2)   # último nivel

  diseno$fpc(nivel = 2)
  diseno$fpc(nivel = 0)

  diseno
}

# -------------------------------------------------------------------------------------
# Marco censal AGEB sintético (estilo construir_marco_ageb): una fila por AGEB
# urbana con llave de 13 caracteres y población 18+ como medida de tamaño.
# Lo comparten test-plan-ageb.R y test-seleccionar-manzanas.R.
marco_ageb_prueba <- function() {
  tibble::tibble(
    ageb = sprintf("15121%04d%04d", rep(1:4, each = 10), 1:10),
    municipio_cod = rep(c("15121", "15121", "15058", "15058"), each = 10),
    region = rep(c("Metropolitana", "Metropolitana", "Resto", "Resto"), each = 10),
    pob18 = rep(c(1500, 2500, 700, 1100), each = 10) +
      rep(seq(0, 900, by = 100), 4),
    estrato = rep(c("Metropolitana", "Metropolitana", "Resto", "Resto"), each = 10)
  )
}

# -------------------------------------------------------------------------------------
# Censo sintético estilo INEGI ageb_mza_urbana PARA EL FLUJO DE LA CLASE
# (PoblacionAGEB/Diseño): 2 municipios x 3 AGEBs x 6 manzanas, todo character
# (como llega del CSV), CON el bloque demográfico que usan cuotas() y el rake
# "inegi" de encuestar (P_18A24_*, P_18YMAS_*, P_60YMAS_*).
censo_clase_prueba <- function(n_manzanas = 6) {
  base <- tidyr::expand_grid(
    ENTIDAD = "15", NOM_ENT = "México",
    tibble::tibble(MUN = c("058", "106"),
                   NOM_MUN = c("Nezahualcóyotl", "Toluca")),
    LOC = "0001",
    AGEB = c("0010", "025A", "0033")
  )
  demograficos <- function(bd, escala) {
    bd |>
      dplyr::mutate(
        pob = escala * (10 + (dplyr::row_number() %% 5) * 4),
        POBTOT = as.character(round(pob * 1.4)),
        P_18YMAS = as.character(pob),
        P_18YMAS_F = as.character(round(pob * 0.52)),
        P_18YMAS_M = as.character(pob - round(pob * 0.52)),
        P_18A24_F = as.character(round(pob * 0.10)),
        P_18A24_M = as.character(round(pob * 0.09)),
        P_60YMAS_F = as.character(round(pob * 0.08)),
        P_60YMAS_M = as.character(round(pob * 0.07)),
        VIVPAR_HAB = as.character(round(pob / 3))
      ) |>
      dplyr::select(-pob)
  }
  totales <- base |>
    dplyr::mutate(NOM_LOC = "Total AGEB urbana", MZA = "000") |>
    demograficos(escala = n_manzanas)
  manzanas <- base |>
    tidyr::expand_grid(MZA = sprintf("%03d", seq_len(n_manzanas))) |>
    dplyr::mutate(NOM_LOC = "Ciudad ejemplo") |>
    demograficos(escala = 1)
  dplyr::bind_rows(totales, manzanas)
}

# Diseño censal AGEB con muestra extraída (flujo de la clase), reproducible.
diseno_ageb_con_muestra <- function() {
  pob <- PoblacionAGEB$new("Fixture", censo_clase_prueba())
  pob$marco_muestral <- pob$marco_muestral |> dplyr::mutate(region = NOM_MUN)
  suppressWarnings(disenar_muestra_ageb(
    pob,
    estratos = tibble::tibble(estrato = c("Nezahualcóyotl", "Toluca"),
                              entrevistas = c(20, 20)),
    n_0 = 5, manzanas_por_ageb = 2,
    tasa_rechazo = 0.5, modo_rechazo = "manzanas",
    calcular_cuotas = FALSE, semilla = 7
  ))
}

# Cartografía censal sintética (Cartografia) cuyos CVEGEO cruzan con el marco
# de crear_mm_ageb: AGEBs urbanas (AULR) y manzanas (MZA) del diseño fixture.
cartografia_ageb_prueba <- function(marco) {
  sq <- function(x0, y0, s = 0.01) {
    sf::st_polygon(list(matrix(c(x0, y0, x0 + s, y0, x0 + s, y0 + s,
                                 x0, y0 + s, x0, y0), ncol = 2, byrow = TRUE)))
  }
  poligonos <- function(claves) {
    sf::st_sfc(lapply(seq_along(claves), function(i) sq(i * 0.02, 0)),
               crs = 4326)
  }
  agebs <- unique(substr(marco$AGEB, 1, 13))
  Cartografia$new(
    mun_shp   = sf::st_sf(CVEGEO = unique(marco$MUN), NOMGEO = "Mun",
                          geometry = poligonos(unique(marco$MUN))),
    loc_shp   = sf::st_sf(CVEGEO = unique(marco$LOC), NOMGEO = "Loc",
                          AMBITO = "Urbana",
                          geometry = poligonos(unique(marco$LOC))),
    agebR_shp = sf::st_sf(CVEGEO = "1509900010001",
                          geometry = poligonos("x")),
    agebU_shp = sf::st_sf(CVEGEO = agebs, geometry = poligonos(agebs)),
    lpr_shp   = sf::st_sf(CVEGEO = "150990002", NOMGEO = "Rancho",
                          geometry = sf::st_sfc(sf::st_point(c(9, 9)),
                                                crs = 4326)),
    mza_shp   = sf::st_sf(CVEGEO = marco$MZA, TIPOMZA = "Típica",
                          geometry = poligonos(marco$MZA))
  )
}
