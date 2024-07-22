# Cargar paquetes necesarios
library(pacman)
packages <- c("tidyverse", "janitor", "readxl", "rstudioapi", "openxlsx",
              "dplyr", "corrplot", "MVN", "factoextra", "ggplot2")
pacman::p_load(char = packages, character.only = TRUE)

# Configurar el directorio de trabajo
setwd(dirname(getActiveDocumentContext()$path))

# Cargar y limpiar datos
datos <- as.data.frame(read_excel("muestra_heart.xlsx")) %>%
  clean_names() #%>%
  #select(-death_event, -sex)

## 1. ANÁLISIS EXPLORATORIO DE LOS DATOS ######################################

# Mostrar la estructura de los datos
str(datos)

# Mostrar los primeros 10 elementos
head(datos, 10)

# Estadísticas descriptivas
summary(datos)

#Histogramas

hist(datos$age,
     main = "Histograma de Eyección",
     xlab = "Porcentaje de Eyección",
     ylab = "Frecuencia",
     col = "royalblue3",
     ylim = c(0, 50),
     border = "black")

hist(datos$ejection_fraction,
     main = "Histograma de Eyección",
     xlab = "Porcentaje de Eyección",
     ylab = "Frecuencia",
     col = "lightblue3",
     ylim = c(0, 50),
     border = "black")

hist_data <- hist(datos$platelets,
                  main = "Histograma de Plaquetas",
                  xlab = "Plaquetas (kiloplaquetas/ml)",
                  ylab = "Frecuencia",
                  col = "lightblue4",
                  border = "black",
                  ylim = c(0, 50),
                  xaxt = "n")
xlim <- range(hist_data$breaks)
axis(1, at = pretty(xlim), labels = format(pretty(xlim), scientific = FALSE))


hist(datos$serum_sodium,
     main = "Histograma de Sodio Sérico",
     xlab = "Sodio Sérico mE/L",
     ylab = "Frecuencia",
     col = "cyan3",
     ylim = c(0, 50),
     border = "black")

hist(datos$serum_creatinine,
     main = "Histograma de Suero de Creatinina",
     xlab = "Suero de Creatinina mE/L",
     ylab = "Frecuencia",
     col = "lightslateblue",
     ylim = c(0, 50),
     border = "black")


#Graficas de pie


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

## 2. PRUEBAS DE BONDAD DE AJUSTE ####################################################

caracteristicas<- datos %>% select(-serum_creatinine)

# Realizar las pruebas de normalidad multivariada
mvn(caracteristicas, mvnTest = "hz")

## 3. PRUEBAS COMPARATIVAS ##########################################################

##Independencia

#H0: independencia para las variables
#Ha: las variables no son independientes

R<- cor(caracteristicas)

-2*(1-((8+11)/(6*50)))*log(det(R)**(50/2)) #EP 8.77

qchisq(1-0.05,4*(1+4)/2) #RR 18.31

#Rechazo H0 si EP = 8.77 > RR = 18.31

#NO rechazo H0, se cumple la igualdad de las varianzas
#Las variables de edad, ejection_fraction, platelets y serum_sodium son independientes 


##Prueba de medias

#H0: Las media para edad = 60, media para ejection = 60, de platelets =275000, de serum_sodium =140
#H0: Se tienen medias distintas a las planteadas

mu_hipotesis <- matrix(c(60, 60, 275000, 140),ncol=1)

x.barras<-matrix(c(mean(datos$age),mean(datos$ejection_fraction),
                   mean(datos$platelets),mean(datos$serum_sodium)),ncol=1)
covs<-cov(caracteristicas)
inv_cov<-solve(cov(caracteristicas))
t2<-50*t(x.barras-mu_hipotesis)%*%inv_cov%*%(x.barras-mu_hipotesis)
n<-50
p<-4
1-pf((n-p)*t2/((n-1)*p),p,n-p)
#Rechazo H0 si p-valor= 6.7 x 10^-17 < alfa = 0.05
#Rechazo H0
#No hay evidencia suficiente para decir que las medias son las sugeridas

##I.C.

aux<-qf(1-0.05,p,n-p)*p*(n-1)/(n*(n-p))

c(x.barras[1]-sqrt(aux*covs[1,1]),x.barras[1]+sqrt(aux*covs[1,1]))    
c(x.barras[2]-sqrt(aux*covs[2,2]),x.barras[2]+sqrt(aux*covs[2,2]))
c(x.barras[3]-sqrt(aux*covs[3,3]),x.barras[3]+sqrt(aux*covs[3,3]))
c(x.barras[4]-sqrt(aux*covs[4,4]),x.barras[4]+sqrt(aux*covs[4,4]))



#Diferencia vector de medias MUERTE

#H0: El vector de medias no difiere entre pacientes que murieron y no
#Ha: El vector de medias difiere entre pacientes que murieron y no

dead <- as.data.frame(read_excel("muestra_heart.xlsx"))%>% 
  clean_names() %>% filter(death_event == 1) %>% select(-death_event,-sex)
alive<-as.data.frame(read_excel("muestra_heart.xlsx"))%>% 
  clean_names() %>% filter(death_event == 0) %>% select(-death_event,-sex)

media_d<-colMeans(dead) #vector de medias
media_a<-colMeans(alive)
vd<-cov(dead) #matriz de varianza y covarianza
va<-cov(alive)

n1<-15
n2<-35

sp<-((n1-1)*vd+(n2-1)*va)/(n1+n2-2)
t(media_d-media_a)%*%solve(((1/n1)+(1/n2))*sp)%*%(media_d-media_a)#EP 6.74

#Region rechazo
qf(1-0.05,4,n1+n2-2)#valor f
qf(1-0.05,4,n1+n2-2)*4*(n1+n2-2)/(n1+n2-4-1)#RR 10.94

#Rechazo H0 si 6.74 > 10.94
#NO rechazo H0
#El vector de medias no difiere entre pacientes que murieron y no

sp_esc<-((n1-1)*vd+(n2-1)*va)/(n1+n2-2)*(1/n1+1/n2)
p<-4

lim_inf<- (media_d-media_a)-sqrt(qf(1-0.05,p,n1+n2-p-1)*p*(n1+n2-2)/(n1+n2-p-1)*diag(sp_esc))
lim_sup<-(media_d-media_a)+sqrt(qf(1-0.05,p,n1+n2-p-1)*p*(n1+n2-2)/(n1+n2-p-1)*diag(sp_esc))

death.ints_conf <- data.frame(
  Inferior = lim_inf,
  Media = media_d-media_a,
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
