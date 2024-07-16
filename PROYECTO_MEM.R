# Cargar el paquete mlbench
if (!requireNamespace("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)

# Cargar el conjunto de datos de cáncer de mama
data("BreastCancer")

# Mostrar la estructura de los datos
str(BreastCancer)

# Mostrar los primeros 10 elementos
head(BreastCancer, 10)

# Estadísticas descriptivas
summary(BreastCancer)
