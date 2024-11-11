library(astsa)
library(forecast)
library(tseries)
######### COMENTARIO  ################
#
# Ejecute el script en Rstudios o bién vs studios.
# Para cada pregunta reutilice la pregunta anterior.
# Hay preguntas que las saque directamente de los ejemplos del texto guia (stoffer)
# Cualquier comentario hacerlo a Eric.Zepeda.14@sansano.usm.cl
#
########## Ejercicio 1     #############
#De acuerdo con el texto guia de la tarea:

adf.test(gnp) # Primero veamos la estacionalidad de esto
adf.test(log(gnp))    #Se realiza el test para validar el hecho de aplicar diferencias.
gnpgr = diff(log(gnp))      # Calculo la diferencia
adf.test(gnpgr) #Veo como se comporta la estacionariedad de la diferencia
ts.plot(gnp,main = "Original time series")#Ploteo la serie original
ts.plot(gnpgr,main = "Transformed time series")# Ploteo la serie transformada

fit <- sarima(gnpgr, 1, 0, 0)      # AR(1) # Realizo el primer ajuste



########## Ejercicio 2     #############
dev.new()

ts.plot(oil,main = "Original time series")
adf.test(oil)
adf.test(log(oil))
poil = diff(log(oil))
adf.test(poil)
# La siguiente linea se mantiene comentado porque por alguna razon me crashea rstudio.

#fit2 <- auto.arima(log(oil),seasonal = FALSE) #lanzo el autoarima

acf2(poil)
#Vemos los modelos
fit1 <- sarima(poil, 1, 0, 1) # Candidato 1
fit2 <- sarima(poil, 0, 0, 3) # Candidato 2
fit3 <- sarima(poil,1,0,3) # Candidato 3

########## Ejercicio 3     #############
#de aqui en adelante, como es lo mismo que antes, dejo de comentar
dev.new()

ts.plot(gtemp_both,main = "Original time series")
adf.test(gtemp_both)
gtamp<-diff(gtemp_both)
adf.test(gtamp)
ts.plot(gtamp,"Transformed time series")
acf2(gtamp)
sarima(gtemp_both, 1, 1, 1)
sarima.for(gtemp_both, 10, 1, 1, 1)#este comando es para hacer la predicción.


########## Ejercicio 4     #############
dev.new()



ts.plot(so2,main = "Original time series")
adf.test(so2)
s<-diff(so2)
adf.test(s)
ts.plot(s,"Transformed time series")
acf2(s)
sarima(so2, 0, 1, 1)
sarima.for(so2, 4, 0, 1, 1,tol=0.05)



########## Ejercicio 5     #############
#Siguiendo el ejemplo 3.47
dev.new()
par(mfrow=1:2)
phi  = c(rep(0,11),.8)
ACF  = ARMAacf(ar=phi, ma=.5, 50)[-1]
PACF = ARMAacf(ar=phi, ma=.5, 50, pacf=TRUE)
 LAG = 1:50/12
tsplot(LAG, ACF, type="h", xlab="LAG", ylim=c(-.4,.8), col=4, lwd=2)
 abline(h=0, col=8)
tsplot(LAG, PACF, type="h", xlab="LAG", ylim=c(-.4,.8), col=4, lwd=2)
 abline(h=0, col=8)

########## Ejercicio 6     #############
dev.new()
 
 ts.plot(chicken,main = "Original time series")
 adf.test(chicken)
 chicken_transformed<-diff(chicken)
 adf.test(chicken_transformed)
 ts.plot(chicken_transformed,"Transformed time series")
 acf2(chicken_transformed)
 
 fit_6 <- auto.arima(chicken,seasonal = FALSE)
 
 sarima(chicken, 2, 1, 1)
 sarima.for(chicken, 12, 2, 1, 1)


########## Ejercicio 7     #############
dev.new()
 
ts.plot(unemp,main = "Original time series")
adf.test(unemp)
unemp_transformed<-diff(unemp)
adf.test(unemp_transformed)
ts.plot(unemp_transformed,"Transformed time series")
acf2(diff(unemp_transformed,max.lag = 48))
acf2(unemp)
 
fit_7 <- auto.arima(unemp,seasonal = FALSE)
 
sarima(unemp, 5, 1, 1)
sarima(unemp, 5, 0, 1)
sarima.for(unemp, 12, 5, 1, 1)


########## Ejercicio 8     #############

ts.plot(UnempRate,main = "Original time series")
adf.test(UnempRate)
UnempRate_transformed<-diff(UnempRate)
adf.test(UnempRate_transformed)
ts.plot(UnempRate_transformed,"Transformed time series")
acf2(UnempRate_transformed,max.lag = 72)


fit_8 <- auto.arima(UnempRate,seasonal = FALSE)

sarima(UnempRate, 5, 1, 0)
sarima(UnempRate, 5, 1, 1)
sarima.for(UnempRate, 12, 5, 1, 1)



########## Ejercicio 9     #############
dev.new()
ts.plot(birth,main = "Original time series")
adf.test(birth)
birth_1 <- diff(birth)
adf.test(birth_1)


acf2(birth_1)

fit_9 <- auto.arima(birth,seasonal = FALSE)

sarima(birth,1,1,0)
sarima(birth,1,1,1)

sarima.for(birth, 12, 1, 1, 0)


########## Ejercicio 10     #############
dev.new()
ts.plot(jj,main = "Original time series")
adf.test(jj)
jj_1 <- diff(jj)
adf.test(jj_1)
acf2(jj_1)


fit_10 <- auto.arima(jj,seasonal = FALSE)

sarima(jj,2,1,2)
sarima(jj,2,1,4)

sarima.for(jj,4,2,1,2)