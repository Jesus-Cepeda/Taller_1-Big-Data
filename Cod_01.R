############################################### 
##         Taller 1 - Big Data
## 
##
##
##############################################
# Limpiamos nuestro ambiente
rm(list = ls())
setwd("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Taller 1")
getwd()
#######################################

################################################################################
# Paquetes 
library(pacman)
p_load(tidyverse, rio , skimr , fastDummies, caret, glmnet, MLmetrics , janitor)
p_load(tidyverse,data.table,plyr,rvest,XML,xml2,boot, stargazer)
library(survey)
library(ggplot2)
library(stargazer)
library(boot)
################################################################################

################################################################################
#
#                         Punto 1 -Regresion
#
################################################################################


#################################################
### (Punto_1) a - i importar los datos 
##########################################################################
dc_url<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
df<-data.frame()
for (url in dc_url){
  print(url)
  temp<-read_html(url) %>% html_table() 
  temp<-as.data.frame(temp[[1]])
  df<-rbind(df, temp)
}

write_csv(df, file = "datos.csv") # Guardando la base de datos
#################################################

#############################################
# (Punto_1) a - ii Description de la grafica 
#############################################
# """Este punto es charlado """
##############################################


#################################################
# Punto_1) - b
#############################################
rm(list = ls())
setwd("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Taller 1")
getwd()
datos <- read.csv("datos.csv")###############################################

###############################################################3
## Limpieza 

## Limpiesa a la data

# filtro
df <- subset(datos, age >= 18 & ocu == 1) ## filtro 
############################################################################
#### Eleccion de las varibales
## Y : y_total_m
df = select(df, c( "y_total_m" , "clase", 
                   "depto", "estrato1" , "p6090" , "p6210" , "p6920" , "formal" , "cuentaPropia", "microEmpresa", 
                   "cotPension", "regSalud", "p6100", "age" , "sex", "hoursWorkUsual", "p7040", "relab"))

################################################################################
#### Manejo de datos Limpieza
df <- na.omit(df) ## elimina datos nulos de la base de d

y_total_m <- df$y_total_m
quantile( y_total_m, prob = c( .05, .1, .15, .20 , .25 , .30, .35, .40 , .50 , .55 , .60 , .65 , .7 , .75 , .80 , .85 , .90 , .95))
rm(y_total_m)
df <- df %>% dplyr::filter(y_total_m >= 237125  )
df <- df %>% dplyr::filter( y_total_m<= 5000000 )
## Descripcion Los datos

skim(df)
###################################################### 
summary(df)
########################################################
install.packages("vtable")
library("vtable")
sumtable(df)

estrato <-  prop.table(table (df$estrato1))
Nivel_educativo <-  prop.table(table (df$p6210))
Pension <-  prop.table(table (df$p6920))
formal <-  prop.table(table (df$formal))
cuentapropia <-  prop.table(table (df$cuentaPropia))
microEmpresa  <-  prop.table(table (df$microEmpresa))
cotPension  <-  prop.table(table (df$cotPension))
regSalud  <-  prop.table(table (df$regSalud))
tipo_regimen  <-  prop.table(table (df$p6100))
segundo_trabajo  <-  prop.table(table (df$p7040))
Tipo_ocupacion  <-  prop.table(table (df$relab))

write.csv2(estrato, file = "estrato.csv")
write.csv2(Nivel_educativo, file = "Nivel_educativo.csv")
write.csv2(formal, file = "formal.csv")
write.csv2(cuentapropia, file = "cuentapropia.csv")
write.csv2(microEmpresa, file = "microEmpresa.csv")
write.csv2(cotPension, file = "cotPension.csv")
write.csv2(regSalud, file = "regSalud.csv")
write.csv2(tipo_regimen, file = "tipo_regimen.csv")
write.csv2(segundo_trabajo, file = "segundo_trabajo.csv")
write.csv2(Tipo_ocupacion, file = "Tipo_ocupacion.csv")
