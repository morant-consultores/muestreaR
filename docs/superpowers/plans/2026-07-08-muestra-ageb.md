# Muestra por AGEB (motor UPM genérico) — Plan de Implementación

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** muestreaR acepta AGEB (marco censal INEGI) como UPM del plan muestral versionado — mismo contrato de pesos por capas que el flujo seccional — más la segunda etapa de manzanas, para el estudio espejo de Enkoll en Estado de México (jul 2026).

**Architecture:** El sorteo seccional se generaliza a un motor de UPM (`planear_muestra_upm`) que emite columnas con sufijo por unidad (`ageb`, `ln_ageb`, `pi_ageb`); `planear_muestra_seccional` queda como envoltura 100 % retrocompatible y `planear_muestra_ageb` es la cara nueva. Dos constructores derivan el marco AGEB/manzana del censo INEGI (dataset "ageb_mza_urbana"), `seleccionar_manzanas` resuelve la Etapa II (PPT dentro de cada UPM) y `plan_para_capas` renombra al contrato seccional que espera `encuestar::construir_diseno_capas`. `planear_siguiente_ola` aprende la unidad del plan.

**Tech Stack:** R (4.6), dplyr/rlang/tibble/readr/sampling, testthat (fixtures sintéticos, sin shapefiles), roxygen2. Repo `morant-consultores/muestreaR`, rama nueva `feat/muestra-ageb` desde `master` (123daee), PR con commits atómicos.

**Contexto ya verificado (no re-derivar):**
- Enkoll (nota metodológica): 1,200 entrevistas; Etapa I 120 AGEBs PPT; Etapa II 2 manzanas/AGEB PPT; Etapa III 5 viviendas/manzana sistemático; sin estratificación explícita; marco = manzanas en AGEBs censo 2020.
- Censo EdoMex `conjunto_de_datos_ageb_urbana_15_cpv2020.csv`: 4,465 filas "Total AGEB urbana", 136,907 filas manzana; claves ENTIDAD/MUN/LOC/AGEB/MZA; tamaños POBTOT, P_18YMAS, VIVPAR_HAB; enmascarado INEGI = `*`.
- Shapefiles marco geoestadístico 2025: A (AGEB urbana, CVEGEO 13 chars), M (manzana, CVEGEO 16), MUN, AR, L, LPR.
- Contrato encuestar (feat/doubly_robust): plan con columnas `seccion, pi_seccion, n_plan` (+`ln_seccion` opcional); `construir_diseno_capas(bd, plan, seccion = "<col snapshot>")`.
- Baseline master: 599 tests PASS, 0 FAIL.
- Primera ola espejo: `tasa_rechazo = 0.5` uniforme (contactos 2×), metodología DR-MNAR se procesa después con el plan versionado.

---

### Task 0: Rama nueva

- [ ] **Step 1: Crear rama desde master actualizado**

```bash
cd /Users/mikiishikawayoshifuji/Documents/encuestas-morant/muestreaR
git checkout master && git pull && git checkout -b feat/muestra-ageb
```

---

### Task 1: `asignar_potencia` con unidad parametrizable

Las columnas `secciones`/`secciones_disponibles` pasan a nombrarse por unidad (para AGEB: `agebs`, `agebs_disponibles`). Default intacto.

**Files:**
- Modify: `R/plan_seccional.R` (asignar_potencia)
- Test: `tests/testthat/test-plan-ageb.R` (nuevo)

- [ ] **Step 1: Test que falla**

```r
# tests/testthat/test-plan-ageb.R
# Muestra por AGEB: motor UPM genérico, marco censal INEGI, manzanas (Etapa II)
# y puente al contrato seccional de encuestar.

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

test_that("asignar_potencia nombra las columnas de conteo por unidad", {
  asig <- asignar_potencia(marco_ageb_prueba(), n_total = 200, m_por_seccion = 10,
                           variable_tamano = "pob18", unidad = "ageb")
  expect_true(all(c("agebs", "agebs_disponibles") %in% names(asig)))
  expect_false(any(c("secciones", "secciones_disponibles") %in% names(asig)))
  expect_equal(asig$entrevistas_plan, asig$agebs * 10)
})
```

- [ ] **Step 2: Correr y ver que falla** — `Rscript -e 'devtools::test(filter="plan-ageb")'` → error `unused argument (unidad)`.

- [ ] **Step 3: Implementación mínima** — en `asignar_potencia`: nuevo parámetro `unidad = "seccion"`; helper interno `plural_unidad <- function(unidad) if (identical(unidad, "seccion")) "secciones" else paste0(unidad, "s")`; renombrar las columnas de salida con `rlang` (`"{pl}_disponibles" := ...`) y el warning de estratos cortos usa el plural. Roxygen: `@param unidad Nombre de la UPM para las columnas de conteo`.

- [ ] **Step 4: Tests verdes** — suite completa (`devtools::test()`), 0 fallas (compat).

- [ ] **Step 5: Commit** — `feat(upm): asignar_potencia nombra sus conteos por unidad (seccion/ageb)`

---

### Task 2: `aplicar_lista_negra` con unidad parametrizable

**Files:**
- Modify: `R/plan_seccional.R` (aplicar_lista_negra)
- Test: `tests/testthat/test-plan-ageb.R`

- [ ] **Step 1: Test que falla**

```r
test_that("aplicar_lista_negra documenta con la llave de la unidad", {
  marco <- marco_ageb_prueba()
  res <- aplicar_lista_negra(marco, secciones = marco$ageb[1:2],
                             municipios = "15058",
                             llave_seccion = "ageb",
                             variable_tamano = "pob18",
                             unidad = "ageb")
  doc <- attr(res, "lista_negra")
  expect_true("ageb" %in% names(doc))
  expect_false("seccion" %in% names(doc))
  expect_setequal(unique(doc$motivo),
                  c("municipio en lista negra", "ageb en lista negra"))
  expect_equal(nrow(res), 18)
})
```

- [ ] **Step 2: Falla** (`unused argument`).
- [ ] **Step 3: Implementación** — parámetros nuevos `unidad = "seccion"` y `variable_tamano = "lista_nominal"` (hoy `lista_nominal` está hardcodeada para documentar tamaño); la columna llave del doc tibble se nombra `unidad`, el motivo dice `"<unidad> en lista negra"`, mensajes/warnings usan la unidad; la columna de tamaño del doc conserva el nombre genérico `tamano` SOLO si `variable_tamano != "lista_nominal"`… **No**: para no romper el contrato, el doc tibble siempre nombra su tercera columna como la `variable_tamano` usada (default `lista_nominal` → compat exacta).
- [ ] **Step 4: Suite completa verde.**
- [ ] **Step 5: Commit** — `feat(upm): aplicar_lista_negra documenta por unidad y tamaño parametrizables`

---

### Task 3: Motor genérico `planear_muestra_upm` + envoltura seccional

**Files:**
- Create: `R/plan_upm.R` (motor)
- Modify: `R/plan_seccional.R` (planear_muestra_seccional delega)
- Test: `tests/testthat/test-plan-ageb.R`

- [ ] **Step 1: Tests que fallan**

```r
test_that("planear_muestra_upm emite el plan con columnas por unidad", {
  plan <- planear_muestra_upm(
    marco_ageb_prueba(), n_total = 200, m_por_upm = 10,
    llave_upm = "ageb", unidad = "ageb",
    variable_tamano = "pob18", dominio = "region",
    tasa_rechazo = 0.5, semilla = 15
  )
  expect_true(all(c("ageb", "region", "estrato", "ln_ageb", "pi_ageb",
                    "n_plan", "contactos") %in% names(plan)))
  expect_true(all(plan$n_plan == 10))
  expect_true(all(plan$contactos == 20))          # 0.5 de rechazo -> 2x
  expect_equal(attr(plan, "unidad"), "ageb")
  # pi sobre TODO el estrato (no sobre las sorteadas)
  asig <- attr(plan, "asignacion")
  marco <- marco_ageb_prueba()
  for (h in unique(plan$estrato)) {
    dispo <- marco[marco$estrato == h, ]
    n_h <- asig$agebs[asig$estrato == h]
    esperadas <- sampling::inclusionprobabilities(dispo$pob18, n_h)
    obs <- plan[plan$estrato == h, ]
    expect_equal(obs$pi_ageb, esperadas[match(obs$ageb, dispo$ageb)])
  }
})

test_that("el motor UPM acepta lista negra con llave genérica 'upms'", {
  marco <- marco_ageb_prueba()
  plan <- planear_muestra_upm(
    marco, 200, 10, llave_upm = "ageb", unidad = "ageb",
    variable_tamano = "pob18", dominio = "region",
    lista_negra = list(upms = marco$ageb[1:3], municipios = NULL),
    semilla = 1
  )
  expect_false(any(plan$ageb %in% marco$ageb[1:3]))
  expect_equal(names(attr(plan, "lista_negra"))[1], "ageb")
})

test_that("planear_muestra_seccional conserva su contrato exacto (delegación)", {
  marco <- tibble::tibble(
    seccion = sprintf("15_%04d", 1:20),
    region = rep(c("A", "B"), each = 10),
    estrato = rep(c("A", "B"), each = 10),
    lista_nominal = 500 + 1:20 * 10
  )
  plan <- planear_muestra_seccional(marco, 80, 8, semilla = 3)
  expect_true(all(c("seccion", "ln_seccion", "pi_seccion", "n_plan",
                    "contactos") %in% names(plan)))
  expect_equal(attr(plan, "unidad"), "seccion")
})
```

- [ ] **Step 2: Falla** (`planear_muestra_upm` no existe).
- [ ] **Step 3: Implementación** — `R/plan_upm.R`:

```r
planear_muestra_upm <- function(marco, n_total, m_por_upm,
                                potencia = 0.5,
                                dominio = "region",
                                variable_estrato = "estrato",
                                variable_tamano = "lista_nominal",
                                llave_upm = "upm",
                                unidad = "upm",
                                min_secciones = 2,
                                tasa_rechazo = 0,
                                lista_negra = NULL,
                                semilla = NULL) {
  # cuerpo = el actual de planear_muestra_seccional con:
  #  - lista_negra$upms o lista_negra[[plural_unidad(unidad)]] o lista_negra$secciones
  #  - aplicar_lista_negra(..., unidad = unidad, variable_tamano = variable_tamano)
  #  - asignar_potencia(..., unidad = unidad) y lectura asig[[plural_unidad(unidad)]]
  #  - mensajes con la unidad
  #  - transmute("{unidad}" := .data[[llave_upm]], across(cols_dom),
  #              estrato, "ln_{unidad}" := .data[[variable_tamano]],
  #              "pi_{unidad}" := pi_upm, n_plan = m_por_upm,
  #              contactos = ceiling(m_por_upm / (1 - tasa_rechazo)))
  #  - attr(plan, "unidad") <- unidad; parametros incluye unidad y m_por_upm
}

# envoltura retrocompatible:
planear_muestra_seccional <- function(marco, n_total, m_por_seccion = 8, ...) {
  planear_muestra_upm(marco, n_total, m_por_upm = m_por_seccion,
                      llave_upm = llave_seccion, unidad = "seccion", ...)
}
```

- [ ] **Step 4: Suite completa verde** (test-plan-seccional.R intacto = red de seguridad del refactor).
- [ ] **Step 5: Commit** — `refactor(upm): motor genérico de plan muestral; el flujo seccional delega`

---

### Task 4: `planear_muestra_ageb`

**Files:**
- Modify: `R/plan_upm.R`
- Test: `tests/testthat/test-plan-ageb.R`

- [ ] **Step 1: Test que falla**

```r
test_that("planear_muestra_ageb es el espejo del flujo seccional con defaults censales", {
  plan <- planear_muestra_ageb(marco_ageb_prueba(), n_total = 200,
                               m_por_ageb = 10, dominio = "region",
                               tasa_rechazo = 0.5, semilla = 8)
  expect_true(all(c("ageb", "ln_ageb", "pi_ageb", "n_plan", "contactos")
                  %in% names(plan)))
  expect_equal(attr(plan, "unidad"), "ageb")
  expect_equal(sum(plan$n_plan), sum(attr(plan, "asignacion")$entrevistas_plan))
})
```

- [ ] **Step 2: Falla.** 
- [ ] **Step 3: Implementación** — envoltura con defaults censales: `llave_ageb = "ageb"`, `variable_tamano = "pob18"`, `m_por_ageb = 10` (2 manzanas × 5 viviendas, modelo operativo), roxygen explicando el espejo (Etapa I de Enkoll) y que la lista_negra acepta `list(agebs =, municipios =)`.
- [ ] **Step 4: Verde.** 
- [ ] **Step 5: Commit** — `feat(ageb): planear_muestra_ageb — plan versionado con AGEB como UPM`

---

### Task 5: `plan_para_capas` (puente al contrato de encuestar)

**Files:**
- Create: `R/plan_para_capas.R`
- Test: `tests/testthat/test-plan-ageb.R`

- [ ] **Step 1: Test que falla**

```r
test_that("plan_para_capas renombra al contrato seccional de encuestar", {
  plan <- planear_muestra_ageb(marco_ageb_prueba(), 200, 10,
                               dominio = "region", semilla = 2)
  capas <- plan_para_capas(plan)
  expect_true(all(c("seccion", "pi_seccion", "ln_seccion", "n_plan",
                    "contactos") %in% names(capas)))
  expect_equal(capas$seccion, plan$ageb)
  expect_equal(attr(capas, "unidad_original"), "ageb")
  # idempotente sobre planes seccionales
  plan_sec <- tibble::tibble(seccion = "x", pi_seccion = 0.1, n_plan = 8)
  attr(plan_sec, "unidad") <- "seccion"
  expect_identical(plan_para_capas(plan_sec), plan_sec)
})
```

- [ ] **Step 2: Falla.** 
- [ ] **Step 3: Implementación** — lee `attr(plan, "unidad")` (o parámetro explícito); si es `"seccion"` regresa tal cual; si no, renombra `{u}→seccion`, `pi_{u}→pi_seccion`, `ln_{u}→ln_seccion` (si existe), conserva atributos y agrega `unidad_original`. Roxygen: es el puente a `encuestar::construir_diseno_capas()` mientras encuestar habla "seccional".
- [ ] **Step 4: Verde.** 
- [ ] **Step 5: Commit** — `feat(capas): plan_para_capas, puente del plan AGEB al contrato seccional de encuestar`

---

### Task 6: Marcos censales — `construir_marco_ageb` y `construir_marco_manzanas`

**Files:**
- Create: `R/marco_ageb.R`
- Test: `tests/testthat/test-marco-ageb.R` (nuevo, con censo sintético estilo INEGI)

- [ ] **Step 1: Tests que fallan**

```r
# tests/testthat/test-marco-ageb.R
censo_prueba <- function() {
  # mini censo estilo INEGI (todo character, como llega del CSV):
  # 2 municipios, 2 AGEBs c/u, 3 manzanas por AGEB; enmascarado "*" incluido
  filas_total <- tibble::tibble(
    ENTIDAD = "15", NOM_ENT = "México",
    MUN = c("121", "121", "058", "058"),
    NOM_MUN = c("Cuautitlán Izcalli", "Cuautitlán Izcalli",
                "Nezahualcóyotl", "Nezahualcóyotl"),
    LOC = c("0001", "0001", "0001", "0001"),
    NOM_LOC = "Total AGEB urbana",
    AGEB = c("0010", "025A", "0033", "0048"),
    MZA = "000",
    POBTOT = c("4000", "2500", "3000", "1800"),
    P_18YMAS = c("2800", "1700", "2100", "1200"),
    VIVPAR_HAB = c("1100", "700", "800", "500")
  )
  filas_mza <- tidyr::expand_grid(
    dplyr::select(filas_total, ENTIDAD, NOM_ENT, MUN, NOM_MUN, LOC, AGEB),
    MZA = c("001", "002", "003")
  ) |>
    dplyr::mutate(
      NOM_LOC = "Ciudad ejemplo",
      POBTOT = as.character(100 + as.integer(MZA) * 50),
      P_18YMAS = as.character(70 + as.integer(MZA) * 30),
      VIVPAR_HAB = "40"
    )
  # una manzana enmascarada por INEGI
  filas_mza$POBTOT[1] <- "*"; filas_mza$P_18YMAS[1] <- "*"
  dplyr::bind_rows(filas_total, filas_mza)
}

test_that("construir_marco_ageb: una fila por AGEB, claves de 13 y tamaños numéricos", {
  marco <- construir_marco_ageb(censo_prueba())
  expect_equal(nrow(marco), 4)
  expect_true(all(nchar(marco$ageb) == 13))
  expect_true(all(c("ageb", "municipio_cod", "nombre_municipio",
                    "localidad_cod", "pob18", "pobtot", "viviendas") %in% names(marco)))
  expect_equal(marco$ageb[1], "1512100010010")
  expect_equal(marco$pob18[1], 2800)
  expect_false(anyDuplicated(marco$ageb) > 0)
})

test_that("construir_marco_manzanas: llaves anidadas en el AGEB y asteriscos a NA", {
  mzas <- construir_marco_manzanas(censo_prueba())
  expect_equal(nrow(mzas), 12)
  expect_true(all(nchar(mzas$manzana) == 16))
  expect_true(all(substr(mzas$manzana, 1, 13) %in%
                    construir_marco_ageb(censo_prueba())$ageb))
  expect_true(is.na(mzas$pobtot[mzas$manzana == "1512100010010001"]))
})

test_that("los marcos censales validan columnas y llaves duplicadas", {
  expect_error(construir_marco_ageb(censo_prueba()[, 1:4]), "falta")
  censo_dup <- dplyr::bind_rows(censo_prueba(), censo_prueba()[1, ])
  expect_error(construir_marco_ageb(censo_dup), "duplicad")
})
```

- [ ] **Step 2: Fallan.** 
- [ ] **Step 3: Implementación** — `R/marco_ageb.R`:

```r
# helper interno: código AGEB alfanumérico a ancho 4 ("10" -> "0010", "010A" queda)
formato_ageb <- function(x) gsub(" ", "0", formatC(toupper(trimws(as.character(x))), width = 4))

parsear_censo <- function(x) readr::parse_double(as.character(x), na = c("", "NA", "*", "N/D"))

construir_marco_ageb <- function(censo) {
  requeridas <- c("ENTIDAD", "MUN", "NOM_MUN", "LOC", "NOM_LOC", "AGEB",
                  "POBTOT", "P_18YMAS", "VIVPAR_HAB")
  # stop("... faltan columnas ...") si setdiff no vacío
  agebs <- censo |> dplyr::filter(.data$NOM_LOC == "Total AGEB urbana") |>
    dplyr::transmute(
      ageb = paste0(formato(ENTIDAD, 2), formato(MUN, 3), formato(LOC, 4), formato_ageb(AGEB)),
      entidad = formato(ENTIDAD, 2),
      municipio_cod = paste0(entidad, formato(MUN, 3)),
      nombre_municipio = NOM_MUN,
      localidad_cod = paste0(municipio_cod, formato(LOC, 4)),
      pob18 = parsear_censo(P_18YMAS),
      pobtot = parsear_censo(POBTOT),
      viviendas = parsear_censo(VIVPAR_HAB)
    )
  # stop si duplicados en ageb; message con AGEBs de pob18 NA/0 (enmascarados: tamaño 0, nunca sorteables)
  agebs
}

construir_marco_manzanas <- function(censo) {
  # filtra filas manzana: !grepl("^Total", NOM_LOC)
  # manzana = paste0(<llave ageb>, formato(MZA, 3)); mismas columnas de tamaño
  # stop si llaves duplicadas
}
```

- [ ] **Step 4: Verde** (+ suite completa). 
- [ ] **Step 5: Commit** — `feat(ageb): marcos censales INEGI por AGEB y manzana (dataset ageb_mza_urbana)`

---

### Task 7: `seleccionar_manzanas` — Etapa II PPT dentro de cada UPM

**Files:**
- Create: `R/seleccionar_manzanas.R`
- Test: `tests/testthat/test-seleccionar-manzanas.R` (nuevo)

- [ ] **Step 1: Tests que fallan**

```r
# tests/testthat/test-seleccionar-manzanas.R
plan_manzanas_fixture <- function() {
  marco <- marco_ageb_prueba()   # helper de test-plan-ageb.R -> moverlo a helper-fixture.R
  plan <- planear_muestra_ageb(marco, 200, 10, dominio = "region",
                               tasa_rechazo = 0.5, semilla = 5)
  mzas <- tidyr::expand_grid(ageb = marco$ageb, mza = sprintf("%03d", 1:5)) |>
    dplyr::mutate(manzana = paste0(ageb, mza),
                  pobtot = 50 + (dplyr::row_number() %% 7) * 40,
                  pob18 = round(pobtot * 0.7))
  list(plan = plan, mzas = mzas)
}

test_that("seleccionar_manzanas: k por UPM, pi sobre todas las manzanas y reparto exacto", {
  fx <- plan_manzanas_fixture()
  sel <- seleccionar_manzanas(fx$plan, fx$mzas, manzanas_por_upm = 2, semilla = 11)
  expect_equal(nrow(sel), 2 * nrow(fx$plan))
  expect_true(all(c("ageb", "manzana", "pi_manzana", "n_plan", "contactos") %in% names(sel)))
  # reparto: cada AGEB conserva sus totales del plan
  tot <- sel |> dplyr::group_by(ageb) |>
    dplyr::summarise(n = sum(n_plan), c = sum(contactos))
  expect_true(all(tot$n == 10)); expect_true(all(tot$c == 20))
  # pi calculadas sobre TODAS las manzanas del AGEB
  a1 <- fx$plan$ageb[1]
  esperadas <- sampling::inclusionprobabilities(
    fx$mzas$pobtot[fx$mzas$ageb == a1], 2)
  obs <- sel[sel$ageb == a1, ]
  expect_equal(obs$pi_manzana,
               esperadas[match(obs$manzana, fx$mzas$manzana[fx$mzas$ageb == a1])])
  # reproducible
  sel2 <- seleccionar_manzanas(fx$plan, fx$mzas, manzanas_por_upm = 2, semilla = 11)
  expect_equal(sel$manzana, sel2$manzana)
})

test_that("UPM con menos manzanas que las pedidas: entran todas con pi 1 y aviso", {
  fx <- plan_manzanas_fixture()
  pocas <- fx$mzas |> dplyr::group_by(ageb) |> dplyr::slice(1) |> dplyr::ungroup()
  expect_warning(sel <- seleccionar_manzanas(fx$plan, pocas, 2, semilla = 1),
                 "menos manzanas")
  expect_true(all(sel$pi_manzana == 1))
  expect_true(all(sel$n_plan == 10))   # todo el plan cae en la única manzana
})

test_that("AGEB con todas las manzanas enmascaradas: sorteo equiprobable con aviso", {
  fx <- plan_manzanas_fixture()
  fx$mzas$pobtot[fx$mzas$ageb == fx$plan$ageb[1]] <- NA
  expect_message(sel <- seleccionar_manzanas(fx$plan, fx$mzas, 2, semilla = 3),
                 "equiprobable")
  expect_equal(sum(sel$ageb == fx$plan$ageb[1]), 2)
})

test_that("UPM del plan sin manzanas en el marco detiene con las llaves", {
  fx <- plan_manzanas_fixture()
  sin <- fx$mzas |> dplyr::filter(ageb != fx$plan$ageb[1])
  expect_error(seleccionar_manzanas(fx$plan, sin, 2), fx$plan$ageb[1])
})
```

- [ ] **Step 2: Fallan** (mover `marco_ageb_prueba()` a `helper-fixture.R` para compartirlo). 
- [ ] **Step 3: Implementación** — `R/seleccionar_manzanas.R` según el diseño: unidad desde `attr(plan, "unidad")`; para cada fila del plan subset de manzanas, `k = min(manzanas_por_upm, disponibles)`, `pi_manzana = sampling::inclusionprobabilities(tamaño, k)` sobre TODAS las del UPM, sorteo con `seleccionar_pps()`, reparto `repartir_cociente()` de `n_plan` y `contactos`; fallback equiprobable si todo el UPM está enmascarado; warning agregado por UPMs cortos; attrs `unidad` + `parametros`. 
- [ ] **Step 4: Verde.** 
- [ ] **Step 5: Commit** — `feat(manzanas): seleccionar_manzanas, Etapa II PPT dentro de cada UPM del plan`

---

### Task 8: `planear_siguiente_ola` para cualquier unidad

**Files:**
- Modify: `R/planear_ola.R`
- Test: `tests/testthat/test-plan-ageb.R`

- [ ] **Step 1: Test que falla**

```r
test_that("planear_siguiente_ola opera sobre planes AGEB (registro por ageb)", {
  marco <- marco_ageb_prueba()
  plan1 <- planear_muestra_ageb(marco, 200, 10, dominio = "region",
                                tasa_rechazo = 0.5, semilla = 4)
  registro <- plan1 |>
    dplyr::transmute(ageb, contactos = 20,
                     efectivas = rep(c(10, 5), length.out = dplyr::n()))
  plan2 <- planear_siguiente_ola(marco, plan1, registro,
                                 metodo = "resortear", dominio = "region",
                                 variable_tamano = "pob18", semilla = 9)
  expect_true(all(c("ageb", "pi_ageb", "n_plan", "contactos") %in% names(plan2)))
  expect_equal(attr(plan2, "unidad"), "ageb")
  expect_s3_class(attr(plan2, "tasas"), "tbl_df")
  # panel conserva agebs y pi
  plan2p <- planear_siguiente_ola(marco, plan1, registro, metodo = "panel")
  expect_equal(plan2p$ageb, plan1$ageb)
  expect_equal(plan2p$pi_ageb, plan1$pi_ageb)
})
```

- [ ] **Step 2: Falla.** 
- [ ] **Step 3: Implementación** — en `planear_siguiente_ola`: resolver `unidad` desde `attr(plan_anterior, "unidad")` (param opcional `unidad = NULL` la sobreescribe; default final "seccion"). Normalizar internamente: plan y registro renombran `{u}→.upm`, `pi_{u}→.pi_upm`; todo el cuerpo opera sobre los nombres canónicos; `metodo = "resortear"` llama a `planear_muestra_upm(..., llave_upm = <unidad>, unidad = <unidad>, ...)` (pasa `variable_tamano` y demás vía `...`); a la salida renombra de regreso y re-etiqueta `attr(plan, "unidad")`. Mensajes de validación citan la unidad. `m_por_seccion` conserva su nombre (compat).
- [ ] **Step 4: Suite completa verde** (test-siguiente-ola.R intacto). 
- [ ] **Step 5: Commit** — `feat(olas): planear_siguiente_ola aprende la unidad del plan (seccion o ageb)`

---

### Task 9: Documentación

**Files:**
- Create: `vignettes/muestra-por-ageb.Rmd`
- Modify: `README.md`
- Regenerar: `man/`, `NAMESPACE` (`devtools::document()`)

- [ ] **Step 1: Viñeta** — flujo compacto ejecutable con censo sintético: censo → `construir_marco_ageb`/`construir_marco_manzanas` → estrato → `planear_muestra_ageb` (tasa_rechazo 0.5) → `seleccionar_manzanas` → `plan_para_capas` → nota de ola 2. Referencia cruzada al flujo seccional y al espejo Enkoll (Etapas I–III).
- [ ] **Step 2: README** — sección/tabla de funciones actualizada con las nuevas.
- [ ] **Step 3: `devtools::document()` + suite completa + commit** — `docs: viñeta 'Muestra por AGEB' + README y man actualizados`

---

### Task 10: Verificación de paquete + code review + PR

- [ ] **Step 1: `devtools::check()`** — 0 errores (warnings non-ASCII conocidos se toleran; CI usa error-on error).
- [ ] **Step 2: Code review** — skill `code-review` sobre el diff de la rama; hallazgos reales se corrigen en commit `fix(review): ...` con sus tests.
- [ ] **Step 3: Push + PR** — `git push -u origin feat/muestra-ageb` y `gh pr create` hacia `master` con resumen del diseño (motor UPM, AGEB, manzanas, puente de capas, olas).

---

### Task 11: Estudio espejo EdoMex — script de muestra completa

**Files:**
- Create: `enc_edomex_estatal_jul_2026/R/generar_muestra_ola1.R`
- Create: `enc_edomex_estatal_jul_2026/README.md`

- [ ] **Step 1: Script ejecutable** (corre desde la raíz del estudio, `Rscript R/generar_muestra_ola1.R`):
  1. Lee el censo INEGI (`data-raw/ageb_mza_urbana_15_cpv2020/...csv`, todo como character).
  2. `construir_marco_ageb` + `construir_marco_manzanas` (muestreaR rama feat/muestra-ageb vía `devtools::load_all("../muestreaR")`).
  3. Espejo Enkoll: estrato único estatal (sin estratificación, como su nota), `region` informativa por tamaño de municipio (metropolitano ≥500k / urbano medio / resto — para el γ regional del DR-MNAR, NO para el sorteo).
  4. `planear_muestra_ageb(n_total = 1200, m_por_ageb = 10, dominio = NULL, potencia = 1, tasa_rechazo = 0.5, semilla = 2026)` → 120 AGEBs, contactos 20 por AGEB.
  5. `seleccionar_manzanas(plan, marco_mzas, manzanas_por_upm = 2, variable_tamano = "pobtot", semilla = 2026)` → 240 manzanas, 5 efectivas + 10 contactos c/u.
  6. Exporta `salidas/`: `plan_ola1_ageb.rds` (VERSIONADO antes de campo), `muestra_manzanas_ola1.csv` (listado de campo con municipio/localidad/AGEB/manzana/n_plan/contactos), `asignacion_ola1.csv`, `resumen_ola1.txt` (cobertura del marco urbano, AGEBs enmascarados, totales).
  7. Comentarios estilo informe Chihuahua (🔵/🟢/🔴): plan versionado, sobremuestra 2× uniforme (primera ola SIN información previa; la tasa real por AGEB se aprende con el registro de contactos), instrumento aleatorizado DR-MNAR en el cuestionario, `plan_para_capas()` como puente a `encuestar::construir_diseno_capas`, y el bloque de ola 2 (`planear_siguiente_ola`) comentado y listo.
- [ ] **Step 2: README del estudio** — tabla espejo (Enkoll ↔ implementación muestreaR), cómo correr, qué NO hace el espejo (sustitución de Enkoll ↔ nuestra sobremuestra + lista negra), estado de ramas, DR-MNAR ola 1.
- [ ] **Step 3: Correr el script completo** con los datos reales y validar: 120 AGEBs, 240 manzanas, sum(n_plan) = 1200, sum(contactos) = 2400, archivos en salidas/.

---

## Self-Review (checklist)

1. **Cobertura**: aceptar AGEB (Tasks 1–8) ✓; script muestra completa (Task 11) ✓; espejo informe Chihuahua aplicado (Task 11 §7, comentarios + plan versionado + 0.5 + DR-MNAR) ✓; TDD/commits atómicos/code review (estructura de cada task + Task 10) ✓; PR nuevo desde master (Tasks 0 y 10) ✓.
2. **Placeholders**: los cuerpos marcados como "el actual de planear_muestra_seccional con…" son refactor de código existente leído en sesión — el ejecutor copia el cuerpo real; no hay TBD.
3. **Consistencia de tipos**: plan AGEB = `ageb, ln_ageb, pi_ageb, n_plan, contactos` en Tasks 3–8 y 11 ✓; `marco_ageb_prueba()` compartido vía helper-fixture (Task 7 Step 2) ✓; `variable_tamano = "pob18"` uniforme ✓.

---

# ADDENDUM (feedback del usuario): la clase exportable es el insumo del equipo

El flujo del equipo es la CLASE (población adentro, `exportar()` → `diseño.rda`
+ `shp.rda` + `cuotas.csv` + `Mapas/` de Google por conglomerado) que consumen
AppAuditoria y encuestar (`Muestra$new` lee `$muestra`, `$variable_poblacional`,
`$poblacion$marco_muestral`; rake `tipo_encuesta = "inegi"` YA existe con
P_18A24/P_18YMAS/P_60YMAS). El flujo censal base (`Poblacion`/`Diseño`/
`Cartografia`, `cuotas()`, `google_maps()`) ya opera marcos censo+AGEB; faltan
piezas de paridad y la declarativa.

### Task 12: `crear_mm_ageb()` + clase `PoblacionAGEB`
Marco censal POR MANZANA compatible con la clase, desde el dataset
ageb_mza_urbana (sin base de localidad ni shp: universo urbano): columnas de
crear_mm (`ENTIDAD`, `MUN` 5, `LOC` 9, `AGEB` 13, `MZA` 16, `ARLU`, `AULR`,
`NOM_MUN`, `NOM_LOC`, `AMBITO="Urbana"`, `id`) + TODO el bloque numérico censal
parseado (`POBTOT:última`, asteriscos→NA; cuotas y rake necesitan
P_18A24_F/M, P_18YMAS_F/M, P_60YMAS_F/M). `PoblacionAGEB` (classname
"Poblacion"): `new(nombre, censo)` + método `regiones()`. TDD con censo_prueba().

### Task 13: `Diseño$plan_muestra` con criterio "manual" (paridad INE)
La base censal no acepta `manual`; DiseñoINE sí. Mismo diff mínimo (param
`manual` + rama `criterio != "manual"`), retrocompatible. TDD: diseño censal
sintético con AGEBs fijados por estrato.

### Task 14: `disenar_muestra_ageb()` declarativa (espejo de disenar_muestra_ine)
`(poblacion, estratos, variable_cluster = "AGEB",
variable_poblacional = "P_18YMAS", n_0 = 5, manzanas_por_ageb = 2,
tasa_rechazo, modo_rechazo, semilla, calcular_cuotas = TRUE)` →
niveles estrato+AGEB, plan manual (AGEBs por estrato de
`calcular_asignacion`), fpc(2)/fpc(0), extraer 1 y 2, `cuotas()` censal.
Devuelve el objeto `Diseño` con attr "asignacion" y attr "plan_ageb"
(Task 15). El modelo operativo del rechazo es el del equipo: modo
"manzanas" infla manzanas por AGEB (2→4 con 0.5), n_0 = 5 fijo.

### Task 15: `derivar_plan_ageb(diseno)` — puente al plan versionado
Reconstruye el contrato DR-MNAR DESDE la clase (un solo sorteo, cero
inconsistencia): `ageb`, `ln_ageb` (= total del AGEB), `pi_ageb`
(= fpc del nivel AGEB, exacto al sorteo PPS), `n_plan` (efectivas
objetivo por AGEB), `contactos` (a levantar = n_0 × manzanas), attr
unidad="ageb" → listo para plan_para_capas()/planear_siguiente_ola().

### Task 16: `leer_cartografia_inegi(carpeta, patron)`
Lee los 6 shapefiles del marco geoestadístico ({patron}_{MUN,L,AR,A,LPR,M})
→ lista para `Cartografia$new()` (CRS 4326). Test con shapefiles sintéticos
escritos a tempdir (fixtures sf ya existen en helper).

### Task 17: docs (viñeta bloque clase, README) + check + PR update

### Task 18: reescribir el script EdoMex al flujo de la clase
PoblacionAGEB → disenar_muestra_ageb (120 AGEBs × 4 mza × 5, rechazo 0.5
modo manzanas) → Cartografia (shapefiles 2025) → `exportar()` a Insumos/
(diseño.rda, shp.rda, cuotas.csv, Mapas/ si hay llave de Google) +
plan versionado derivado a salidas/. Correr end-to-end y verificar.
