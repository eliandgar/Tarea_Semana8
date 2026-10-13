
# ==================================================
# TAREA SEMANA 8
# Preparacion de la base ENE
# Fuente: INE, mayo-julio 2026
# ==================================================

library(dplyr)

# 1. Cargar los datos originales
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")

# 2. Seleccionar variables
variables <- c(
  "region", "sexo", "edad", "activ",
  "habituales", "fact_cal", "ocup_form",
  "categoria_ocupacion", "cine11_1d"
)

ene_trabajo <- ene |>
  select(all_of(variables))

# 3. Revisar valores faltantes
colSums(is.na(ene_trabajo))

# 4. Limpiar codigos especiales
ene_limpia <- ene_trabajo |>
  mutate(
    habituales = na_if(habituales, 888),
    habituales = na_if(habituales, 999)
  )

# 5. Seleccionar personas ocupadas
ene_ocupados <- ene_limpia |>
  filter(activ == 1)

# 6. Revisar categorias
table(ene_ocupados$cine11_1d, useNA = "ifany")
table(ene_ocupados$ocup_form, useNA = "ifany")
table(ene_ocupados$categoria_ocupacion,
      useNA = "ifany")


# 7. Crear variables para el análisis económico

ene_final <- ene_ocupados |>
  mutate(
    mujer = case_when(
      sexo == 1 ~ 0,
      sexo == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    informal = case_when(
      ocup_form == 1 ~ 0,
      ocup_form == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    grupo_edad = case_when(
      edad < 25 ~ "15 a 24 años",
      edad >= 25 & edad < 40 ~ "25 a 39 años",
      edad >= 40 & edad < 60 ~ "40 a 59 años",
      edad >= 60 ~ "60 años o más",
      TRUE ~ "Sin clasificar"
    )
  )

# Verificar categorías
table(ene_final$grupo_edad, useNA = "ifany")

# Revisar valores faltantes
colSums(is.na(ene_final))


# 8. Guardar base procesada

write.csv(
  ene_final,
  "data/processed/ene_a3.csv",
  row.names = FALSE
)

# Verificar que se guardó
file.exists("data/processed/ene_a3.csv")
dim(ene_final)
