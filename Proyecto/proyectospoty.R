##### Paso 1: Cargue de Librerias #####

library(tidyverse)
# Libreria para gráficas 2D
library(ggplot2)
# Libreria para la regresión lineal
library(caret)
#Libreria para la insertación de la data
library(csv)
##Modelos para más organizados:
library(MASS)

##### Paso 2: Cargue de Datos #####
# Se carga datos_musica.csv separado por ';'
datos <-read.csv("EstacionMeteorologica.csv", header = TRUE) ##Se encontro que falta la variable datos para llamar la base de datos
# Se muestran las primeras filas de los datos
head(datos)

##### Paso 3: Proprocesamiento #####
# Eliminar NaN
datos <- na.omit(datos)
# Cambiar género a valores numéricos
datos$genero <- as.numeric(datos$genero)
# se muestran los datos
head(datos)

##### Paso 4: Correlaciones #####
##Verificación de las Columnas
colnames
colna()
# Pearson
pearson <- cor(datos, method = 'pearson', use="presion")
print(pearson)
# Spearman
spearman <- cor(datos, method = 'spearman')
print(spearman)
# Kendall
kendall <- cor(datos, method = 'kendall')
print(kendall)

##### Paso 5: Mapa de Calor con ggplot2 #####
# Convertir la matriz de correlación en un dataframe
cor_df <- as.data.frame(pearson)

# Obtener las combinaciones de pares de variables
pairs <- expand.grid(rownames(cor_df), colnames(cor_df))

# Asignar las correlaciones a cada par de variables
pairs$correlation <- pearson[as.matrix(pairs[,1:2])]

# Crear el mapa de calor
heatmap_plot <- ggplot(data = pairs, aes(x = Var1, y, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Mapa de Calor de Correlación de Pearson")

# Mostrar el mapa de calor
print(heatmap_plot)

### Paso 6: Dividir el conjunto de datos en conjunto de entrenamiento y prueba (80% entrenamiento, 20% prueba)
indices_entrenamiento <- createDataPartition(datos$peso, p = 0.8, list = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- datos[-indices_entrenamiento, ]

### Paso 7: Construir el modelo de regresión lineal
modelo <- lm(peso ~ estatura, data = datos_entrenamiento)

### Paso 8: Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = datos_prueba)

### Paso 9: Calcular las métricas
#R2
r2 <- cor(datos_prueba$peso, predicciones)## Se identifico la elevación al cuadrado
print(paste("R-cuadrado (R2):", round(r2, 3) ))
# RMSE
residuos <- resid(modelo)
rmse <- sqrt(mean(residuos^2))
print(paste("RMSE:", round(rmse, 3) ))
# MAE
mae <- mean(abs(residuos))
print(paste(MAE:round(mae, 3) )) ##Se identifico las comillas simple del codigo

### Paso 10: Gráfica de nube de puntos
ggplot +
  # Datos de entrenamiento
  geom_point(data = datos_entrenamiento, aes(x = estatura, y = peso), color = "orange", alpha = 0.6)
  # Recta de regresión para datos de entrenamiento
  geom_smooth(data = datos_entrenamiento, aes(x = estatura, y = peso), method = "lm", color = "red", se = FALSE)
  # Recta de regresión para datos de prueba
  geom_smooth(data = datos_prueba, aes(x = estatura, y = predicciones), method = "lm", color = "blue", se = FALSE) 
  labs(title = "Nube de puntos con Regresión", x = "Estatura", y = "Peso") ##Se identifico un + en el codigo
