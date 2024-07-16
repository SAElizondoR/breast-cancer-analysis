# Biblioteca con el conjunto de datos a utilizar
if (!requireNamespace("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)

# Biblioteca para manipulación de datos
if (!requireNamespace("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Biblioteca para mostrar visualmente las correlaciones
if (!requireNamespace("corrplot")) {
  install.packages("corrplot")
}
library(corrplot)

# Cargar el conjunto de datos de cáncer de mama
data("BreastCancer")

# Asignar el tipo adecuado a las columnas numéricas y
# codificar la clase como ceros y unos
datos <- BreastCancer %>%
  mutate(
    Class = ifelse(Class == "benign", 0, 1),
    across(-Class, as.numeric)
  )

# Mostrar la estructura de los datos
str(datos)

# Mostrar los primeros 10 elementos
head(datos, 10)

# Estadísticas descriptivas
summary(datos)

# Calcular matriz de correlación
correlaciones <- cor(datos, use = "complete.obs")

# Graficar correlaciones en un mapa de calor
corrplot(correlaciones,
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("white", "#B22222"))(200))
