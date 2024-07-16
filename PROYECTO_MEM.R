# Cargar el paquete mlbench
if (!requireNamespace("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)

# Cargar el conjunto de datos de cáncer de mama
data("BreastCancer")

# Estructura de los datos
str(BreastCancer)
