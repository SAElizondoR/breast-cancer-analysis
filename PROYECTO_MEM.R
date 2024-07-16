# Cargar el paquete mlbench
if (!requireNamespace("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)

# Cargar el conjunto de datos de cáncer de mama
data("BreastCancer")

# Estructura de los datos
str(BreastCancer)

# Mostrar los primeros 10 elementos
head(BreastCancer, 10)

# Cantidad de observaciones
nrow(BreastCancer)

# Cantidad de variables
ncol(BreastCancer)
