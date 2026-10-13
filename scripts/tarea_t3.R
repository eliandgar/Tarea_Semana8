
# ====================================================
# TAREA 3 - SEMANA 8
# Regresión económica: CASEN y ENE
# Autor: [ELIZABETH ANDRADES GARRIDO]
# Fecha: 11-10-2026
# ====================================================

# Librerias
library(dplyr)
library(broom)
library(modelsummary)

# PARTE A: CASEN

# A1. Cargar datos y crear experiencia de Mincer
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(
    experiencia = pmax(edad - educ - 6, 0)
  )

# Revisar dimensiones y valores faltantes
dim(casen)
colSums(is.na(casen))


# Primer modelo: educación e ingreso
m1 <- lm(ingreso ~ educ, data = casen)

# Segundo modelo: educación, experiencia y género
m2 <- lm(
  ingreso ~ educ + experiencia + genero,
  data = casen
)

# Revisar los coeficientes
print(coef(m1))
print(coef(m2))


# A2. Tabla de coeficientes del modelo con controles

tabla_coeficientes <- tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
    significativo = p.value < 0.05
  ) |>
  select(
    term, estimate, conf.low, conf.high,
    p.value, significativo
  ) |>
  arrange(p.value)

# Mostrar resultados
print(tabla_coeficientes)


# Primer modelo: ingreso y educacion
m1 <- lm(ingreso ~ educ, data = casen)

# Segundo modelo: agregar controles
m2 <- lm(ingreso ~ educ + experiencia + genero,
         data = casen)

# Revisar coeficientes
coef(m1)
coef(m2)


# A3. Tercer modelo: educacion y genero
# Se incorpora una interaccion para analizar
# si la relacion educacion-ingreso difiere por genero.

m3 <- lm(
  ingreso ~ educ * genero + experiencia,
  data = casen
)

# Guardar los tres modelos en una lista
modelos <- list(
  simple = m1,
  controles = m2,
  interaccion = m3
)

# Comparar los modelos
comparacion_modelos <- lapply(modelos, glance) |>
  bind_rows(.id = "modelo") |>
  select(
    modelo,
    r.squared,
    adj.r.squared,
    AIC,
    nobs
  ) |>
  mutate(
    across(
      c(r.squared, adj.r.squared, AIC),
      \(x) round(x, 3)
    )
  )

print(comparacion_modelos)

# Interpretación A3:
# El R2 aumenta desde 0.458 en el modelo simple hasta
# 0.480 en el modelo con interaccion.
# Sin embargo, el R2 ajustado disminuye de 0.448 a
# 0.438 y el AIC aumenta de 1475 a 1479.
# Por lo tanto, la interacción no muestra una mejora
# clara respecto al modelo simple.
# Añadir mas variables no garantiza un mejor modelo.


# A4. Comparar el coeficiente de educación
# entre los tres modelos

comparacion_educacion <- bind_rows(
  simple = tidy(m1),
  controles = tidy(m2),
  interaccion = tidy(m3),
  .id = "modelo"
) |>
  filter(term == "educ") |>
  select(modelo, term, estimate, p.value) |>
  mutate(
    estimate = round(estimate, 2),
    p.value = round(p.value, 5)
  )

print(comparacion_educacion)

# Interpretación A4:
# El coeficiente de educación cambia de $42.080
# a $42.246 al incorporar experiencia y genero.
# Esta diferencia es pequeña, lo que indica que
# los controles modifican poco la asociación estimada.
#
# En el modelo con interacción, el coeficiente
# de $46.646 corresponde a las mujeres.
# Para los hombres, la pendiente se obtiene
# sumando el coeficiente de interacción.
# Estás asociaciones no demuestran causalidad.


# A5. Diagnostico del modelo con controles

# Obtener datos de diagnostico
diagnostico <- augment(m2)

# Identificar las tres mayores diferencias
# entre ingreso observado e ingreso estimado

peores_predicciones <- diagnostico |>
  mutate(error_absoluto = abs(.resid)) |>
  arrange(desc(error_absoluto)) |>
  select(
    ingreso, educ, experiencia,
    genero, .fitted, .resid, error_absoluto
  ) |>
  head(3)

print(peores_predicciones)


# A5. Diagnostico de influencia y apalancamiento

diagnostico_influencia <- augment(m2) |>
  summarise(
    cook_maximo = max(.cooksd, na.rm = TRUE),
    apalancamiento_maximo = max(.hat, na.rm = TRUE)
  )

print(diagnostico_influencia)


# A5. Análisis de sensibilidad
# Identificar la observacion con mayor Cook

diagnostico_cook <- augment(m2) |>
  arrange(desc(.cooksd))

# Mostrar el registro más influyente
print(head(diagnostico_cook, 1))

# Obtener su número de fila original
fila_influyente <- as.integer(
  diagnostico_cook$.rownames[1]
)

# Volver a estimar el modelo sin esa persona
m2_sin_influyente <- casen |>
  slice(-fila_influyente) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

# Comparar el coeficiente de educacion
comparacion_cook <- data.frame(
  modelo = c("Original", "Sin observacion influyente"),
  coef_educacion = c(
    coef(m2)["educ"],
    coef(m2_sin_influyente)["educ"]
  )
) |>
  mutate(coef_educacion = round(coef_educacion, 2))

print(comparacion_cook)

# Calcular el cambio en pesos
cambio_educacion <- coef(m2_sin_influyente)["educ"] -
  coef(m2)["educ"]

print(round(cambio_educacion, 2))


# Interpretacion A5:
# La observacion 43 presenta la mayor distancia de Cook.
# Al excluirla, el coeficiente de educacion aumenta
# desde $42.245,76 hasta $45.217,92.
# Esto representa una diferencia de $2.972,16.
# Aunque existe cierta sensibilidad a esta observacion,
# la asociacion entre educacion e ingreso sigue siendo
# positiva y de magnitud similar.


# A6. Resultados para la interpretacion economica

resultados_a6 <- tidy(m2, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high, p.value) |>
  mutate(
    across(
      c(estimate, conf.low, conf.high),
      \(x) round(x, 2)
    )
  )

print(resultados_a6)


# A6. Interpretación económica
#
# Educación:
# Según los resultados, cada año adicional de educación
# se asocia con un ingreso mensual aproximadamente
# $42.246 mayor, al comparar personas con la misma
# experiencia y género.
# El intervalo de confianza al 95 % va desde
# $29.487 hasta $55.004.
#
# Género:
# Los hombres presentan un ingreso estimado $55.686
# mayor que las mujeres con la misma educación
# y experiencia. Sin embargo, esta diferencia
# no es estadísticamente significativa.
# Por lo tanto, no podemos afirmar que exista
# una diferencia real en la población.
#
# Experiencia:
# Cada año adicional de experiencia se asocia
# con una disminución estimada de $517 mensuales.
# Sin embargo, este resultado tampoco es
# estadísticamente significativo, por lo que
# no podemos concluir que exista una relación clara.
#
# Limitaciones:
# El análisis considera solamente 55 personas
# con ingresos registrados.
# Por esta razón, los resultados deben interpretarse
# con cuidado y no representan necesariamente
# a todos los trabajadores de Chile.
# Además, las relaciones encontradas no demuestran
# que una variable sea la causa de otra.


# ====================================================
# PARTE B: ANÁLISIS DE LA ENE
# ====================================================

# B1. Pregunta económica
#
# ¿Qué diferencias existen en las horas semanales
# trabajadas entre hombres y mujeres en Chile,
# considerando la edad y el nivel educativo?
#
# El objetivo es conocer si existen diferencias
# en las horas habituales de trabajo según el género,
# y observar si estas diferencias cambian al
# considerar otras características de las personas.

# B2. Cargar la base procesada
ene_a3 <- read.csv("data/processed/ene_a3.csv")

# Revisar dimensiones
dim(ene_a3)

# Revisar valores faltantes
colSums(is.na(ene_a3))

# Revisar las primeras observaciones
head(ene_a3)


# B2. Preparar variables para las regresiones
#
# Se excluyen solamente las observaciones que
# no tienen horas habituales registradas.
# La educación se considera una categoría,
# no una cantidad exacta de años de estudio.

ene_modelo <- ene_a3 |>
  mutate(
    genero = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ "Sin clasificar"
    ),
    educacion = case_when(
      cine11_1d %in% 0:6 ~ as.character(cine11_1d),
      cine11_1d == 9 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    educacion = factor(educacion),
    genero = factor(genero)
  ) |>
  filter(
    !is.na(habituales) &
      !is.na(educacion) &
      !is.na(fact_cal) &
      fact_cal > 0
  )

#   Para comprobar la cantidad de personas disponibles
nrow(ene_modelo)

# Revisar las categorías 
table(ene_modelo$genero)
table(ene_modelo$educacion)


# Adaptar los nombres educativos 
ene_modelo <- ene_modelo |>
  mutate(
    educacion = factor(
      educacion,
      levels = as.character(0:6),
      labels = c(
        "Menos que enseñanza básica",
        "Enseñanza básica",
        "Enseñanza media",
        "Educación técnica superior",
        "Educación superior de grado",
        "Magíster o especialización",
        "Doctorado"
      )
    )
  )

# para revisar la distribución de los niveles educativos
table(ene_modelo$educacion)


# B3. Modelos de regresión con la ENE
#
# Se busca comparar las horas habituales de trabajo
# entre hombres y mujeres, considerando otras
# características de las personas ocupadas.
#
# Ambos modelos utilizan el factor de expansión
# entregado por el INE.

# Modelo 1: diferencia de horas según género
modelo_ene1 <- lm(
  habituales ~ genero,
  data = ene_modelo,
  weights = fact_cal
)

# Modelo 2: género, edad y nivel educativo
modelo_ene2 <- lm(
  habituales ~ genero + edad + educacion,
  data = ene_modelo,
  weights = fact_cal
)

# Revisar los coeficientes
print(coef(modelo_ene1))
print(coef(modelo_ene2))

# B4. Comparación de los modelos de la ENE
#
# Se compara la diferencia estimada de horas
# semanales entre mujeres y hombres.
# El segundo modelo incorpora edad y educación.

comparacion_genero <- bind_rows(
  base = tidy(modelo_ene1, conf.int = TRUE),
  controles = tidy(modelo_ene2, conf.int = TRUE),
  .id = "modelo"
) |>
  filter(term == "generoMujer") |>
  mutate(
    across(
      c(estimate, conf.low, conf.high),
      \(x) round(x, 2)
    ),
    significativo = p.value < 0.05
  ) |>
  select(
    modelo, term, estimate,
    conf.low, conf.high,
    p.value, significativo
  )

print(comparacion_genero)


# B4. Comparación de la capacidad explicativa
# de los dos modelos de la ENE.

comparacion_ene <- bind_rows(
  base = glance(modelo_ene1),
  controles = glance(modelo_ene2),
  .id = "modelo"
) |>
  select(
    modelo,
    r.squared,
    adj.r.squared,
    nobs
  ) |>
  mutate(
    across(
      c(r.squared, adj.r.squared),
      \(x) round(x, 4)
    )
  )

print(comparacion_ene)


# B4. Comparación de la brecha de horas por grupo de edad
#
# Se analiza si la diferencia de horas habituales
# entre hombres y mujeres cambia según la edad.

comparacion_edades <- ene_modelo |>
  group_by(grupo_edad) |>
  group_modify(~ {
    modelo_grupo <- lm(
      habituales ~ genero,
      data = .x,
      weights = fact_cal
    )
    
    tidy(modelo_grupo, conf.int = TRUE) |>
      filter(term == "generoMujer")
  }) |>
  ungroup() |>
  mutate(
    across(
      c(estimate, conf.low, conf.high),
      \(x) round(x, 2)
    ),
    significativo = p.value < 0.05
  ) |>
  select(
    grupo_edad, estimate,
    conf.low, conf.high,
    significativo
  )

print(comparacion_edades)


# B5. Análisis de sensibilidad
#
# Se compara el resultado del modelo con controles
# utilizando y sin utilizar el factor de expansión.
# Esto permite observar si la ponderación cambia
# la diferencia estimada de horas entre géneros.

# Modelo sin ponderación
modelo_sin_peso <- lm(
  habituales ~ genero + edad + educacion,
  data = ene_modelo
)

# Comparar el coeficiente de género
sensibilidad_ene <- bind_rows(
  ponderado = tidy(modelo_ene2),
  sin_ponderar = tidy(modelo_sin_peso),
  .id = "modelo"
) |>
  filter(term == "generoMujer") |>
  select(modelo, estimate) |>
  mutate(estimate = round(estimate, 3))

print(sensibilidad_ene)

# Calcular cuánto cambia la estimación
cambio_ponderacion <- coef(modelo_ene2)["generoMujer"] -
  coef(modelo_sin_peso)["generoMujer"]

print(round(cambio_ponderacion, 3))


# B6. Interpretación económica
#
# Los resultados muestran que las mujeres ocupadas
# registran, en promedio, 4,11 horas semanales menos
# de trabajo habitual que los hombres.
# Al considerar la edad y el nivel educativo,
# la diferencia aumenta a 4,44 horas semanales.
# Esta diferencia es estadísticamente significativa
# y también puede ser importante en la práctica.
# La brecha es mayor entre las personas de 60 años
# o más, donde alcanza las 6,05 horas semanales.
# Al quitar la ponderación del INE, la diferencia
# cambia a 4,75 horas, sin alterar la conclusión general.
#
# Estos resultados muestran una asociación, no una
# relación de causa y efecto. Además, no se consideran
# todas las características laborales que podrían
# explicar las diferencias de horas trabajadas.
# Por último, los errores estándar de los modelos
# ponderados no incorporan el diseño muestral
# completo de la ENE, por lo que los intervalos
# de confianza deben interpretarse con precaución.


# ====================================================
# PARTE C: PRESENTACIÓN DE RESULTADOS
# ====================================================

# C1. Comparar los modelos de la ENE

modelos_ene <- list(
  "Modelo base" = modelo_ene1,
  "Modelo con controles" = modelo_ene2
)

# Crear y guardar tabla comparativa
modelsummary(
  modelos_ene,
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  output = "data/processed/t3_tabla_ene.csv"
)

# C2. Exportar coeficientes de CASEN

tidy(m2, conf.int = TRUE) |>
  write.csv(
    "data/processed/t3_coeficientes_casen.csv",
    row.names = FALSE
  )

# C3. Verificar los archivos creados

file.exists("data/processed/t3_tabla_ene.csv")

file.exists("data/processed/t3_coeficientes_casen.csv")

