# Tarea 8: Regresión económica con CASEN y ENE

**Asignatura:** Fundamentos de Programación para Análisis Económico  
**Autor:** ELIZABETH ANDRADES GARRIDO
**Fecha:** 13 de octubre de 2026

## 1. Descripción del trabajo
En esta tarea se utilizan herramientas de programación en R para analizar información económica y 
laboral de Chile mediante modelos de regresión.
Se trabaja con una base didáctica CASEN y con datos de la Encuesta Nacional de Empleo (ENE) del INE, 
correspondientes al trimestre mayo-julio de 2026.

## 2. Pregunta económica
¿Qué diferencias existen en las horas semanales trabajadas entre hombres y mujeres en Chile,
considerando la edad y el nivel educativo?

## 3. Resultados principales
El modelo base estimó una diferencia de 4,11 horas semanales menos para las mujeres y,
al incorporar edad y educación, la diferencia fue de 4,44 horas.
Se trata de asociaciones y que los resultados no permiten establecer causalidad.

## 4. Archivos del proyecto

- `scripts/preparar_ene.R`: preparación de la base ENE.
- `scripts/tarea_t3.R`: regresiones y análisis económico.
- `data/raw/`: archivos originales.
- `data/processed/ene_a3.csv`: base ENE procesada.
- `data/processed/t3_tabla_ene.csv`: resultados de los modelos ENE.
- `data/processed/t3_coeficientes_casen.csv`: coeficientes del modelo CASEN.

## 5. Cómo ejecutar el proyecto

1. Abrir `Tarea_Semana8.Rproj` en RStudio.
2. Ejecutar `scripts/preparar_ene.R` para generar la base procesada desde el archivo original.
3. Ejecutar `scripts/tarea_t3.R` para realizar las regresiones y generar las tablas.

## 6. Limitaciones

El análisis utiliza datos observacionales, no permite establecer relaciones causales. Además,
los modelos de regresión ponderados no consideran completamente el diseño muestral de la ENE al calcular los errores estándar.

## 7. Declaración de autoría y uso de IA

- **Herramienta utilizada:** Geminis
- **Para qué la usé:**  para resolver porque algún código no corre, y ayuda con la orientación de alguna función
- **Qué hice yo:** estructura del trabajo, pregunta económica y resultados
- **Verificación:** confirmo que entiendo y puedo explicar todo lo que entrego

