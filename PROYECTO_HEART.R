# Cargar paquetes necesarios
library(pacman)
packages <- c("tidyverse", "janitor", "readxl", "rstudioapi", "openxlsx",
              "dplyr", "corrplot", "MVN", "factoextra", "ggplot2")
pacman::p_load(char = packages, character.only = TRUE)

# Configurar el directorio de trabajo
setwd(dirname(getActiveDocumentContext()$path))

# Cargar y limpiar datos
datos_orig <- as.data.frame(read_excel("muestra_heart.xlsx")) %>%
  clean_names()

datos <- datos_orig %>%
  select(-death_event, -sex)

## 1. ANÁLISIS EXPLORATORIO DE LOS DATOS ######################################

# Mostrar la estructura de los datos
str(datos)

# Mostrar los primeros 10 elementos
head(datos, 10)

# Estadísticas descriptivas
summary(datos)

# Calcular y graficar matriz de correlación
correlaciones <- cor(datos)
corrplot(correlaciones,
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("white", "#B22222"))(200))

# Vector de medias y matriz de varianza y covarianza
colMeans(datos, na.rm = TRUE) %>%
  matrix(ncol = 1)

cov(datos, use = "complete.obs")

## 2. PRUEBAS DE BONDAD DE AJUSTE #############################################

caracteristicas <- datos %>% select(-serum_creatinine)

# Realizar las pruebas de normalidad multivariada
mvn(caracteristicas, mvnTest = "hz")

## 3. PRUEBAS COMPARATIVAS ####################################################

## Independencia

#H0: Independencia para las variables
#Ha: Las variables no son independientes

p <- ncol(caracteristicas)
n <- nrow(caracteristicas)
R <- cor(caracteristicas)

# Calcular estadístico de prueba
-2 * (1 - ((2 * p + 11) / (6 * n))) * log(det(R)**(50 / 2)) #EP = 8.77

# Calcular valor crítico
qchisq(1 - 0.05, p * (p - 1) / 2) # RR = 12.59

# Rechazo H0 si EP = 8.77 > RR = 12.59

# No rechazo H0, las variables son independientes

# Las variables de edad, ejection_fraction, platelets y serum_sodium
# son independientes 


## Prueba de medias

#H0: Las medias para edad = 60, ejection = 60, platelets =275000,
#    serum_sodium =140
#H0: Las medias son distintas a las planteadas

# Definir medias hipotéticas
mu_hipotesis <- c(60, 60, 275000, 140) %>%
  matrix(ncol = 1)

# Calcular medias observadas
x_barra <- colMeans(datos %>% select(age, ejection_fraction, platelets,
                                     serum_sodium)) %>%
  matrix(ncol = 1)

# Calcular matriz de covarianza y su inversa
covs <- cov(caracteristicas)
inv_cov <- solve(cov(caracteristicas))

# Calcular estadístico T²
t2 <- n * t(x_barra - mu_hipotesis) %*% inv_cov %*% (x_barra - mu_hipotesis)

# Calcular valor p
1 - pf((n - p) * t2 / ((n - 1) * p), p, n - p)

# Rechazo H0 si p-valor = 6.7 x 10^-16 < alfa = 0.05
# Rechazo H0
# Las medias son distintas a las planteadas

## Intervalos de confianza (I. C.)

# Calcular factor auxiliar para los intervalos de confianza
aux <- qf(1 - 0.05, p, n - p) * p * (n - 1) / (n * (n - p))

# Calcular e imprimir intervalos de confianza
ic <- function(media, varianza) {
  sqrt_aux <- sqrt(aux * varianza)
  c(media - sqrt_aux, media + sqrt_aux)
}

ic_values <- sapply(1:p, function(i) ic(x_barra[i], covs[i, i]))
rownames(ic_values) <- c("Límite inferior", "Límite superior")
colnames(ic_values) <- c("age", "ejection_fraction", "platelets",
                         "serum_sodium")


#Diferencia vector de medias MUERTE

#H0: El vector de medias no difiere entre pacientes que murieron y no
#Ha: El vector de medias difiere entre pacientes que murieron y no

# Filtrar datos de pacientes muertos y vivos
dead <- datos_orig %>% filter(death_event == 1) %>% select(-death_event,-sex)
alive <- datos_orig %>% filter(death_event == 0) %>% select(-death_event,-sex)

# Calcular vectores de medias y matrices de covarianza
media_d <- colMeans(dead) %>% matrix(ncol = 1)
media_a <- colMeans(alive) %>% matrix(ncol = 1)
vd <- cov(dead)
va <- cov(alive)

n1 <- nrow(dead)
n2 <- nrow(alive)

# Calcular matriz de covarianza ponderada
sp <- ((n1 - 1) * vd + (n2 - 1) * va) / (n1 + n2 - 2)
sp_esc <- sp * (1 / n1 + 1 / n2)

# Calcular estadístico T²
t(media_d - media_a) %*% solve(sp_esc) %*% (media_d - media_a)
# EP = 6.74

# Calcular valor crítico F y región de rechazo
valor_f <- qf(1 - 0.05, p, n1 + n2 - p - 1)
region_rechazo <- valor_f * p * (n1 + n2 - 2) / (n1 + n2 - p - 1) # RR 11.00

# Rechazo H0 si 6.74 > 11.00
# No rechazo H0
# El vector de medias no difiere entre pacientes que murieron y no

# Calcular intervalos de confianza
sqrt_aux <- sqrt(region_rechazo * diag(sp_esc))
lim_inf <- media_d - media_a - sqrt_aux
lim_sup <- media_d - media_a + sqrt_aux

data.frame(
  Inferior = lim_inf,
  Media = media_d - media_a,
  Superior = lim_sup
)

#Diferencia vector de medias SEXO

#H0: El vector de medias no difiere entre pacientes masculinos y femeninos
#Ha: El vector de medias difiere entre pacientes masculinos y femeninos

masc <- as.data.frame(read_excel("muestra_heart.xlsx"))%>% 
  clean_names() %>% filter(sex == 1) %>% select(-death_event,-sex)
fem<-as.data.frame(read_excel("muestra_heart.xlsx"))%>% 
  clean_names() %>% filter(sex == 0) %>% select(-death_event,-sex)

media_m<-colMeans(masc) #vector de medias
media_f<-colMeans(fem)
vm<-cov(masc) #matriz de varianza y covarianza
vf<-cov(fem)

n1<-33
n2<-17

sp<-((n1-1)*vm+(n2-1)*vf)/(n1+n2-2)
t(media_m-media_f)%*%solve(((1/n1)+(1/n2))*sp)%*%(media_m-media_f)#EP 8.32

#Region rechazo
qf(1-0.05,4,n1+n2-2)#valor f
qf(1-0.05,4,n1+n2-2)*4*(n1+n2-2)/(n1+n2-4-1)#RR 10.94

#Rechazo H0 si 8.32 > 10.94
#NO rechazo H0
#El vector de medias no difiere entre pacientes masculinos y femeninos

sp_esc<-((n1-1)*vm+(n2-1)*vf)/(n1+n2-2)*(1/n1+1/n2)
p<-4

lim_inf<- (media_m-media_f)-sqrt(qf(1-0.05,p,n1+n2-p-1)*p*(n1+n2-2)/(n1+n2-p-1)*diag(sp_esc))
lim_sup<-(media_m-media_f)+sqrt(qf(1-0.05,p,n1+n2-p-1)*p*(n1+n2-2)/(n1+n2-p-1)*diag(sp_esc))

sex.ints_conf <- data.frame(
  Inferior = lim_inf,
  Media = media_m-media_f,
  Superior = lim_sup
)

## 4. TÉCNICA MULTIVARIADA ##############################################################

#estandarizacion
cp<-scale(caracteristicas)
summary(scale(caracteristicas))

#descripcion
prcomp(cp)
summary(prcomp(cp))


#grafica de codo
fviz_eig(prcomp(cp),addlabels=T)

#Tres componentes
