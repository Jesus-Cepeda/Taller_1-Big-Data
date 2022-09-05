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
### censurar los datos
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



##### 2 perfil ganancia 

library(sjPlot)
library(sjmisc)
library(sjlabelled)

df2 <- mutate(df, age2=age*age) 
regresion <- lm(y_total_m ~ age + age2 , data = df2)
salida <- summary(regresion)
tab_model(regresion)

# Punto 2 - D  
################################################################

#CI por bootstrap
require("tidyverse")
set.seed(112)
n<-length(df2$y_total_m)
R<-1000 # Number of Repetions
beta_regresion<-matrix(0,R,2)
peak_age<-rep(0, R)
dimnames(beta_regresion)<-list(c(1:R), c("age", "age2"))
for (i in 1:R){
  db_sample<- sample_frac(df2,size=1,replace=TRUE)
  f<- lm(y_total_m ~ age + age2 , data = db_sample)
  coefs<-f$coefficients
  beta_regresion[i,]<-coefs[2:3]
  peak_age[i]<-(-beta_regresion[i, 1])/(2*beta_regresion[i, 2])
}
varianza<-var(peak_age)

#Predicci?n Age-Earnings
age <- seq(18, 90, 0.1)
pred <- predict(regresion, list(age=age, age2=age^2))

#Grafica sin CI
plot(age, pred, type = "l", xlab = "Edad", ylab = "Ingreso", lwd=2)
mod<-lm(y_total_m ~ age + age2 , data = df2)
beta<-mod$coefficients
p <- c(-beta[2]/(2*beta[3]), max(pred))
points(t(p), pch=16)

#Predicci?n Age-Earnings CI
age<-seq(30, 50, 0.1)
pred<-predict(regresion, list(age=age, age2=age^2))

#Grafica CI
dev.new(width=100, height=max(pred)+200000)
plot(age, pred, type = "l", xlab = "Edad", ylab = "Ingreso", lwd=2)
mod<-lm(y_total_m ~ age + age2 , data = df2)
beta<-mod$coefficients
p <- c(-beta[2]/(2*beta[3]), max(pred))
points(t(p), pch=16)
ps <- matrix(c(1, 15/p[2], p[2]/p[1], 1), nrow=2, ncol=2)*p 
points(rbind(c(round(-beta[2]/(2*beta[3])+(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), c(round(-beta[2]/(2*beta[3])-(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), ps[1,]), pch=c("|", "|", "|"), cex=c(1,1,1))
text(rbind(c(round(-beta[2]/(2*beta[3])+(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), c(round(-beta[2]/(2*beta[3])-(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), ps[1,]), col="black", labels=c("UB", "LB", round(ps[1], digits = 2)), pos=c(3, 3, 1))

#Intervalo de Confianza
CI<-matrix(c(-beta[2]/(2*beta[3])-(1.96*sqrt(varianza)), -beta[2]/(2*beta[3])+(1.96*sqrt(varianza))), nrow = 1, ncol = 2)
colnames(CI)<-c("Lower Bound", "Upper_Bound")
CI
###


########################################################################
# Punto 2 - D (Juan Camilo) 
################################################################

#CI por bootstrap
require("tidyverse")
set.seed(112)
n<-length(df2$y_total_m)
R<-1000 # Number of Repetions
beta_regresion<-matrix(0,R,2)
peak_age<-rep(0, R)
dimnames(beta_regresion)<-list(c(1:R), c("age", "age2"))
for (i in 1:R){
  db_sample<- sample_frac(df2,size=1,replace=TRUE)
  f<- lm(y_total_m ~ age + age2 , data = db_sample)
  coefs<-f$coefficients
  beta_regresion[i,]<-coefs[2:3]
  peak_age[i]<-(-beta_regresion[i, 1])/(2*beta_regresion[i, 2])
}
varianza<-var(peak_age)

#Predicci?n Age-Earnings
age <- seq(18, 90, 0.1)
pred <- predict(regresion, list(age=age, age2=age^2))

#Grafica sin CI
plot(age, pred, type = "l", xlab = "Edad", ylab = "Ingreso", lwd=2)
mod<-lm(y_total_m ~ age + age2 , data = df2)
beta<-mod$coefficients
p <- c(-beta[2]/(2*beta[3]), max(pred))
points(t(p), pch=16)

#Predicci?n Age-Earnings CI
age<-seq(30, 50, 0.1)
pred<-predict(regresion, list(age=age, age2=age^2))

#Grafica CI
dev.new(width=100, height=max(pred)+200000)
plot(age, pred, type = "l", xlab = "Edad", ylab = "Ingreso", lwd=2)
mod<-lm(y_total_m ~ age + age2 , data = df2)
beta<-mod$coefficients
p <- c(-beta[2]/(2*beta[3]), max(pred))
points(t(p), pch=16)
ps <- matrix(c(1, 15/p[2], p[2]/p[1], 1), nrow=2, ncol=2)*p 
points(rbind(c(round(-beta[2]/(2*beta[3])+(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), c(round(-beta[2]/(2*beta[3])-(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), ps[1,]), pch=c("|", "|", "|"), cex=c(1,1,1))
text(rbind(c(round(-beta[2]/(2*beta[3])+(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), c(round(-beta[2]/(2*beta[3])-(1.96*sqrt(varianza)), digits = 2), min(pred)-3000), ps[1,]), col="black", labels=c("UB", "LB", round(ps[1], digits = 2)), pos=c(3, 3, 1))

#Intervalo de Confianza
CI<-matrix(c(-beta[2]/(2*beta[3])-(1.96*sqrt(varianza)), -beta[2]/(2*beta[3])+(1.96*sqrt(varianza))), nrow = 1, ncol = 2)
colnames(CI)<-c("Lower Bound", "Upper_Bound")
CI


##################################################################


################################################################################
#
#                         Punto_3 -Regresion
#
################################################################################

#############################################################################
# Punto_3 - a 
#############################################
df2 <- mutate(df, ln_Y_total_m = log(y_total_m), female=1-sex)
regresion_3 <- lm(ln_Y_total_m ~ female , data = df2)
summary(regresion_3)
tab_model(regresion_3)

################################################################################

###################################
# Punto_3 - b 
#############################################

#bootstrap hombres
require("tidyverse")
set.seed(112)
n<-length(df2$ln_Y_total_m)
R<-1000 # Number of Repetions
beta_regresionh<-matrix(0,R,2)
peak_ageh<-rep(0, R)
dimnames(beta_regresionh)<-list(c(1:R), c("age", "age2"))
df3h<-mutate(df2[df2[,"female"]==0,], age2=age^2)
for (i in 1:R){
  db_sampleh<- sample_frac(df3h,size=1,replace=TRUE)
  f<- lm(y_total_m ~ age + age2, data = db_sampleh)
  coefs<-f$coefficients
  beta_regresionh[i,]<-coefs[2:3]
  peak_ageh[i]<-(-beta_regresionh[i, 1])/(2*beta_regresionh[i, 2])
}
varianzah<-var(peak_ageh)
lbh<-dflnpredh$age[which(dflnpredh$ln_Y_total_m==max(dflnpredh$ln_Y_total_m))]-(1.96*sqrt(varianzah))
ubh<-dflnpredh$age[which(dflnpredh$ln_Y_total_m==max(dflnpredh$ln_Y_total_m))]+(1.96*sqrt(varianzah))
CIh<-matrix(c(lbh, ubh), nrow=1, ncol=2)
colnames(CIh)<-c("Lower bound hombre", "Upper bound hombre")

#bootstrap mujeres
require("tidyverse")
set.seed(112)
n<-length(df2$ln_Y_total_m)
R<-1000 # Number of Repetions
beta_regresionm<-matrix(0,R,2)
peak_agem<-rep(0, R)
dimnames(beta_regresionm)<-list(c(1:R), c("age", "age2"))
df3m<-mutate(df2[df2[,"female"]==1,], age2=age^2)
for (i in 1:R){
  db_samplem<- sample_frac(df3m,size=1,replace=TRUE)
  f<- lm(y_total_m ~ age + age2, data = db_samplem)
  coefs<-f$coefficients
  beta_regresionm[i,]<-coefs[2:3]
  peak_agem[i]<-(-beta_regresionm[i, 1])/(2*beta_regresionm[i, 2])
}
varianzam<-var(peak_agem)
#CI mujeres
lbm<-dflnpredm$age[which(dflnpredm$ln_Y_total_m==max(dflnpredm$ln_Y_total_m))]-(1.96*sqrt(varianzam))
ubm<-dflnpredm$age[which(dflnpredm$ln_Y_total_m==max(dflnpredm$ln_Y_total_m))]+(1.96*sqrt(varianzam))
CIm<-matrix(c(lbm, ubm), nrow=1, ncol=2)
colnames(CIm)<-c("Lower bound mujer", "Upper bound mujer")

#Estimacion Age-Earnings
df3 <- mutate(df2, age2=age^2)
regresion_3b <- lm(ln_Y_total_m ~ age + age2 + female, data=df3)
summary(regresion_3b)

#Prediccion Age-Earnings hombres
age <- seq(18, 90, 0.1)
sexh <- rep(0, times = length(age))
lnpredh <- predict(regresion_3b, list(age=age, age2=age^2, female=sexh))
predh <- exp(lnpredh)

#Prediccion Age-Earnings mujeres
age <- seq(18, 90, 0.1) 
sexm <- rep(1, times = length(age))
lnpredm <- predict(regresion_3b, list(age=age, age2=age^2, female=sexm))
predm <- exp(lnpredm)

#Grafica sin CI
lnpred <- cbind(lnpredh, lnpredm)
matplot(age, exp(lnpred), type = c("l"),pch=1,col = 1:2, xlab = "Edad", ylab = "Ingreso")
dflnpredm <- data.frame(cbind(age, lnpredm))
dflnpredh <- data.frame(cbind(age, lnpredh))
colnames(dflnpredm)<-c("age", "ln_Y_total_m")
colnames(dflnpredh)<-c("age", "ln_Y_total_m")
pm <- c(dflnpredm$age[which(dflnpredm$ln_Y_total_m==max(dflnpredm$ln_Y_total_m))], exp(dflnpredm$ln_Y_total_m[which(dflnpredm$ln_Y_total_m==max(dflnpredm$ln_Y_total_m))]))
ph <- c(dflnpredh$age[which(dflnpredh$ln_Y_total_m==max(dflnpredh$ln_Y_total_m))], exp(dflnpredh$ln_Y_total_m[which(dflnpredh$ln_Y_total_m==max(dflnpredh$ln_Y_total_m))]))
points(t(pm), pch=16, col="red")
text(t(pm), col="red", labels=pm[1], pos=1)
points(t(ph), pch=16)
text(t(ph), col="black", labels=ph[1], pos=1)
legend(20, 600000, legend = c("Hombre", "Mujer"), fill=c("black", "red"), col = c("black", "red"), box.lty = 0, cex = 0.7)

#Prediccion Age-Earnings hombres para CI
age <- seq(30, 50, 0.1) #Ventana ampleada para que se distinga CI hombre y mujer
sexh <- rep(0, times = length(age))
lnpredh <- predict(regresion_3b, list(age=age, age2=age^2, female=sexh))
predh <- exp(lnpredh)

#Prediccion Age-Earnings mujeres para CI
age <- seq(30, 50, 0.1) #Ventana ampleada para que se distinga CI hombre y mujer
sexm <- rep(1, times = length(age))
lnpredm <- predict(regresion_3b, list(age=age, age2=age^2, female=sexm))
predm <- exp(lnpredm)

#Grafica CI
lnpred <- cbind(lnpredh, lnpredm)
matplot(age, exp(lnpred), type = c("l"),pch=1,col = 1:2, xlab = "Edad", ylab = "Ingreso")
dflnpredm <- data.frame(cbind(age, lnpredm))
dflnpredh <- data.frame(cbind(age, lnpredh))
colnames(dflnpredm)<-c("age", "ln_Y_total_m")
colnames(dflnpredh)<-c("age", "ln_Y_total_m")
pm <- c(dflnpredm$age[which(dflnpredm$ln_Y_total_m==max(dflnpredm$ln_Y_total_m))], exp(dflnpredm$ln_Y_total_m[which(dflnpredm$ln_Y_total_m==max(dflnpredm$ln_Y_total_m))]))
ph <- c(dflnpredh$age[which(dflnpredh$ln_Y_total_m==max(dflnpredh$ln_Y_total_m))], exp(dflnpredh$ln_Y_total_m[which(dflnpredh$ln_Y_total_m==max(dflnpredh$ln_Y_total_m))]))
points(t(pm), pch=16, col="red")
text(t(pm), col="red", labels=pm[1], pos=1)
points(t(ph), pch=16)
text(t(ph), col="black", labels=ph[1], pos=1)
points(rbind(c(lbm, 1065000), c(ubm, 1065000)), col="red", pch=c("|", "|"), cex=c(1,1))
text(rbind(c(lbm, 1065000), c(ubm, 1065000)), col="black", labels = c("LB", "UB"), pos=c(3, 3), cex = c(0.8, 0.8))
points(rbind(c(lbh, 1065000), c(ubh, 1065000)), col="black", pch=c("|", "|"), cex=c(1,1))
legend(30, 1200000, legend = c("Hombre", "Mujer"), fill=c("black", "red"), col = c("black", "red"), box.lty = 0, cex = 0.7)


###################################

###################################
# Punto_3 - c 
#############################################

#############################################

#Regresion larga con controles
df3c <- mutate(df3, second_job=p7040-1)
long_mod <- lm(ln_Y_total_m ~ age + age2 + female + hoursWorkUsual + second_job + cuentaPropia + microEmpresa + factor(relab), data = df3c)
stargazer(long_mod, type = "text")

#FWL
db <- mutate(df3c, res_y_nui=lm(ln_Y_total_m~age + age2 + hoursWorkUsual + second_job + cuentaPropia + microEmpresa + factor(relab), data=df3c)$residuals, 
             res_female_nui=lm(female~age + age2 + hoursWorkUsual + second_job + cuentaPropia + microEmpresa + factor(relab), data=df3c)$residuals)
FWL <- lm(res_y_nui~res_female_nui, data=db)
stargazer(long_mod, FWL, type="text")

#FWL con bootstrap
set.seed(112)
beta.FWL<-function(data,index){
  coef(lm(res_y_nui~res_female_nui, data=db, subset = index))
}
boot(db, beta.FWL, R = 1000)


################################################################################
#
#                         Punto_4 - Prediccion
#
################################################################################

######################################
#Punto 4 - a
######################################
set.seed(101010) #sets a seed
matchdata <- df3c %>%
  mutate(holdout= as.logical(1:nrow(df3c) %in%
                               sample(nrow(df3c), nrow(df3c)*.3))
         #generates a logical indicator to divide
         #between train and test set
  )
test<-matchdata[matchdata$holdout==T,]
train<-matchdata[matchdata$holdout==F,]


#####################################
#Punto 4 - b
#####################################

#Especificaciones
specification1<-lm(y_total_m~1,data=train)
specification2<-lm(ln_Y_total_m~1, data = train)
specification3<-lm(ln_Y_total_m~log(age) + age2 + female, data = train)
specification4<-lm(ln_Y_total_m~age+female+femalexage, data = train)
specification5<-lm(ln_Y_total_m ~ age + age2 + female + log(hoursWorkUsual) + cuentaPropia, data = train)
specification6<-lm(y_total_m ~ age + age2 , data = train)
specification7<-lm(ln_Y_total_m ~ female, data = train)
specification8<-lm(ln_Y_total_m ~ age + age2 + female + femalexage + femalexage2, data=train)
specification9<-lm(ln_Y_total_m ~ age + age2 + female + hoursWorkUsual + second_job + cuentaPropia + microEmpresa + factor(relab), data = train)

#test
test$specification1<-predict(specification1,newdata = test)
with(test,mean((y_total_m-specification1)^2))

test$specification2<-predict(specification2,newdata = test)
with(test,mean((ln_Y_total_m-specification2)^2))

test$specification3<-predict(specification3,newdata = test)
with(test,mean((ln_Y_total_m-specification3)^2))

test$specification4<-predict(specification4,newdata = test)
with(test,mean((ln_Y_total_m-specification4)^2))

test$specification5<-predict(specification5,newdata = test)
with(test,mean((ln_Y_total_m-specification5)^2))

test$specification6<-predict(specification6,newdata = test)
with(test,mean((y_total_m-specification6)^2))

test$specification7<-predict(specification7,newdata = test)
with(test,mean((ln_Y_total_m-specification7)^2))

test$specification8<-predict(specification8,newdata = test)
with(test,mean((ln_Y_total_m-specification8)^2))

test$specification9<-predict(specification9,newdata = test)
with(test,mean((ln_Y_total_m-specification9)^2))

tests<-matrix(c(test$specification1[1], test$specification2[1], test$specification3[1], test$specification4[1],
                test$specification5[1], test$specification6[1], test$specification7[1], test$specification8[1], 
                test$specification9[1]), ncol = 1, nrow = 9)
rownames(tests)<-c("specification1", "specification2", "specification3", 
                   "specification4", "specification5", "specification6", 
                   "specification7", "specification8", "specification9")
tests



#####################################
#Punto 4 - c
#####################################

#Cual tiene el menor MSE?
which(tests==min(tests))



#####################################
#Punto 4 - d
#####################################

#LOOCV
model2 <- train(ln_Y_total_m~.,
                data = matchdata,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model4 <- train(ln_Y_total_m~age+female+femalexage,
                data = matchdata,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

LOOCV<-matrix(c(model2$results$RMSE, model4$results$RMSE), nrow = 2, ncol = 1)
rownames(LOOCV)<-c("Specification2", "specification4")
colnames(LOOCV)<-"RMSE"
LOOCV

##########################################




