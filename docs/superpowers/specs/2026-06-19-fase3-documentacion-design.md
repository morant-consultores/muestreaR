# Fase 3 — Documentación de `muestreaR`

**Fecha:** 2026-06-19
**Repo:** `morant-consultores/muestreaR`
**Branch:** `mejoras/fase3-documentacion` (sobre `mejoras/fase2-limpieza`)
**Autor del diseño:** auditoría asistida (Claude) + Emilio Morones

---

## 1. Contexto

Tras la Fase 2, `R CMD check` ya no tiene ERRORS, pero quedan WARNINGS/NOTES de **documentación**: funciones exportadas sin `@description`/`@param`/`@return`, y `Rd` sin descripción. La Fase 3 documenta el paquete en español, añade una vignette y un README, arregla el script de ejemplo desactualizado, y silencia el ruido NSE.

Es de **bajo riesgo**: no toca lógica ni estructura. Cada cambio se valida con `devtools::test()` (verde) y `devtools::check()`.

---

## 2. Alcance (todo acordado)

### 2.1 Roxygen completo (en español) para las 28 funciones/clases exportadas
Para cada objeto exportado: `@title` + `@description`, `@param` de cada argumento con texto real, `@return`, y `@examples` donde aporten (usando datos sintéticos o `\dontrun{}` para los que requieren shapefiles/IO). Cubre:
- Clases R6: `Diseño`/`DiseñoINE`, `Poblacion`/`PoblacionINE`, `Cartografia`/`CartografiaINE` (descripción de clase + métodos públicos principales + parámetros de `$new`).
- Funciones: `agregar_nivel`, `calcular_fpc`, `regiones`, `muestrear`, `cuotas`/`cuotas_ine`, `crear_mm`/`crear_mm_ine`, `crear_shp`/`crear_shp_ine`, `formato`, `repartir_cociente`, `llaves`, `revision`/`revision_ine`, `sustituir_muestra`/`sustituir_muestra_ine`, `google_maps`/`google_maps_ine`, `graficar_mapa_muestra`/`graficar_mapa_muestra_ine`, `graficar_mapa_poblacion`.

Se regenera con `devtools::document()` y se valida que los WARNINGS de "Undocumented arguments"/"Rd without description" desaparezcan.

### 2.2 Vignette / tutorial
Una vignette (`vignettes/disenar-una-muestra.Rmd`) que recorra el flujo completo de diseño muestral **sobre datos sintéticos** (reusando el enfoque del fixture/demo), para que **compile sin shapefiles reales** durante `R CMD check`. Requiere añadir `knitr`, `rmarkdown` a `Suggests` y `VignetteBuilder: knitr` al DESCRIPTION.

### 2.3 README
`README.md` (desde `README.Rmd`) con: qué es el paquete, instalación, y un ejemplo rápido del flujo (region → SECCION → plan → fpc → extraer → cuotas), basado en el demo.

### 2.4 Arreglar `data-raw/muestra_ine_ejemplo.R`
Actualizar la llamada desactualizada a `PoblacionINE$new` (le falta el parámetro `electoral`) para que refleje la firma actual y el flujo real de producción (2 niveles: region + SECCION). Deja de inducir a error.

### 2.5 Silenciar ruido NSE
Añadir `utils::globalVariables(c(...))` en `R/muestreaR-package.R` con los ~168 nombres de columna usados en NSE de dplyr, para eliminar los NOTE de "no visible binding for global variable". Se genera la lista a partir del propio `R CMD check`.

### Fuera de alcance
- Herencia `*INE` (Fase dedicada).
- Rediseño de API (Fase 4).
- La WARNING de caracteres no-ASCII (la `ñ` de `DiseñoINE`) es inherente al nombre de la API y no se aborda.

---

## 3. Plan de commits atómicos

1. `docs: roxygen completo para funciones de muestreo (diseno.R, crear_mm.R, devtools.R)`
2. `docs: roxygen completo para clases R6 y mapas/revisión`
3. `docs: silenciar NOTE NSE con utils::globalVariables`
4. `docs: vignette 'disenar-una-muestra' sobre datos sintéticos`
5. `docs: README con overview y ejemplo rápido`
6. `docs: actualizar data-raw/muestra_ine_ejemplo.R a la firma actual (electoral, 2 niveles)`

Cada commit deja el paquete cargable y la suite verde.

---

## 4. Criterios de aceptación

- [ ] `devtools::test()` verde (32+ asserts) tras cada commit.
- [ ] `R CMD check`: desaparecen los WARNINGS de "Undocumented arguments" y los NOTE de "Rd without description" y "no visible binding".
- [ ] La vignette compila en el check sin datos reales.
- [ ] README renderiza y el ejemplo corre.
- [ ] El demo y `muestra_ine_ejemplo.R` siguen siendo coherentes con la API actual.
- [ ] No se modifica lógica ni estructura.

---

## 5. Riesgos
- **La vignette debe compilar sin IO ni shapefiles** — se basa en el fixture sintético. Riesgo bajo (ya probado en el demo). Se valida construyéndola con R.
- **`globalVariables`**: lista larga; se deriva del check para no inventar nombres. Riesgo nulo (solo afecta a NOTES).
- R disponible localmente: **todo se valida ejecutando**.
