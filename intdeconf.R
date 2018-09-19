x<-c(42.2,42.6,43.3,43.5,43.7,44.1,44.9,45.3,45.7,45.7,45.9,46,46.2,46.2,46.8,46.8,47.1,47.2)
y<-c(44,44,44,45,45,46,46,46,47,48,48,48,47,48,48,49,49,49)
mod<-lm(y~x)
summary(lm(y~x))
anova(lm(y~x))
require(ggplot2)
dat<-data.frame(x,y)
ggplot(dat,aes(x,y))+geom_point()+geom_smooth(method = "lm")
plot(x,y)
abline(lsfit(x,y))
qf(.95,16,1)
qt(.975,16)
###
xa<-c(635,644,711,708,836,820,810,870,856,923,878,937,948)
ya<-c(100,93,88,84,77,75,74,63,57,55,47,43,38)
moda<-lm(ya~xa)
#intervalo de confianza
#> (beta1)-.17563-(2.200985*.01837)(t de tablas*error standart)
#[1] -0.2160621
#> -.17563+(2.200985*.01837)
#[1] -0.1351979
######################################ejercicio2#################################################
#y^+-t(a/2,n-2)*s*sqrt(1+(1/n)+(x*-med)^2/sxx) prediccion
xs<-c(8,15,16.5,20,20,27.5,30,30,35,38,40,45,50,50,55,55,59,65)#profundidad
ys<-c(22.8,27.2,23.7,17.1,21.5,18.6,16.1,23.4,13.4,19.5,12.4,13.2
      ,11.4,10.3,14.1,9.7,12,6.8)#resistencia: calcular un intervalo de confianza al 95% con una
#profundidad de 45
modlin<-lm(ys~xs)
summary(modlin)
#intervalo de prediccion=b1+-(t*sqrt(s^2)*sqrt(y^)
#=-.29756+(2.119905*.04116)=-.2103047
#=-.29756-(2.119905*.04116)=-.3848153
#ejercicio 2
xd<-c(23,45,68,91,114,136,159,182,205,228)
yd<-c(53.3,26.9,54.8,33.8,29.9,8.2,17.2,12.2,3.2,11.1)
#zavala la grafica de dispersion la seleccion del modelo?
#**si, la dispersion esta muy cencana a la recta de regresion
#Obtenga la recta de minimos cuadrados ordinarios
#**y=52.6269-0.2204x
#que proporcion de la variacion puede ser atribuida al modelo
#**el modelo puede predecir el 66.36% de las variables
#sera el mrls un modelo util? utilice prueba de hipotesis al 95%
#**tiene p-value menor al .025 por lo que se rechaza la hipotesis nula
#calcule e interprete un intervalo de confianza de 95% para y cuando
#**(-0.3376895,-0.1030305)
#**Los valores para el estimador B1 se encontraran entre -0.3376895 y -0.1030305, lo cual indica
#**una relacion negativa, es decir por cada aumento de x el valor de y se encontrara en este  
#**intervalo
datos<-data.frame(xd,yd)
ggplot(datos,aes(xd,yd))+geom_point(pch=18,color="darkblue")+geom_smooth(method = "lm",color="red")
cor(xd,yd)^2
######################################ejercicio 3################################################
#bo + b1x*+-t(sbo+b1)
xf<-c(29.8,33.2,33.7,35.3,35.5,36.1,36.2,36.3,37.5,37.7,38.7,38.8,39.6,41,42.8,42.8,43.5,45.6,46
      ,46.9,48,49.3,51.7,62.6,69.8,79.5,80)
yf<-c(5.9,7.2,7.3,6.3,8.1,6.8,7,7.6,6.8,6.5,7,6.3,7.9,9,8.2,8.7,7.8,9.7,7.4,7.7,9.7,7.8,7.7,11.6
      ,11.3,11.8,10.7)
b1<-cov(xf,yf)/var(xf)
bo<-mean(yf)-(b1*mean(xf))
recta<-bo+b1*xf
errores<-yf-recta
val<-data.frame(xf=40)
valo<-data.frame(xf,yf)
predict(lm(yf~xf),val,interval = "prediction")
confint(lm(yf~xf))
require(ggplot2)
ggplot(valo,aes(xf,yf))+geom_point()+geom_smooth(method = "lm")