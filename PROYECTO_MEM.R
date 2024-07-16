if (!requireNamespace("mlbench")) {
  install.packages("mlbench")
}
library(mlbench) # Contiene el conjunto de datos a utilizar
library(dplyr) # Biblioteca para manipulación de datos

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
