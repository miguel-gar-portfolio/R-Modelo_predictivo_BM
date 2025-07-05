# R-Modelo_predictivo_BM

Este proyecto contiene un modelo predictivo desarrollado en **R** para predecir una **variable binaria** utilizando un conjunto de **variables explicativas mixtas**, tanto **continuas** como **categóricas**.

## Objetivo

Evaluar y comparar el rendimiento de dos modelos estadísticos con el fin de seleccionar el mejor ajuste para la variable binaria objetivo:

- **Regresión logística**
- **Bosque aleatorio (Random Forest)**

## Descripción del proyecto

- Los datos utilizados contienen variables de diferentes tipos (numéricas y categóricas).
- Se realiza un preprocesamiento básico, codificación de variables categóricas, y partición en conjuntos de entrenamiento y prueba.
- Se aplican dos enfoques predictivos:
  - **Regresión logística** como modelo base.
  - **Bosque aleatorio** para capturar relaciones no lineales y posibles interacciones.
- Se comparan los modelos utilizando métricas de desempeño como:
  - Matriz de confusión
  - AUC (Área bajo la curva ROC)
  - Accuracy, Sensibilidad, Especificidad
