install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("forecast")
library(readxl)
library(ggplot2)


M<-read_excel(file.choose())
M$`Team/NOC`<-as.factor(M$`Team/NOC`)
head(M)
M

# Estadistica b�sica
summary(M)
str(M)

#Histograma de las Medallas de oro
ggplot(M,aes(x=Gold)) +
  geom_histogram(binwidth=0.8,color="black", fill="gold",
                 alpha=0.5) +
  ggtitle("HISTOGRAMA MEDALLAS DE ORO ")+
  labs(caption = "HISTORIAL JUEGOS OLIMPICOS")+
  theme_classic()

#Grafico de barras del total de medallas por pais
ggplot(M,aes(x=reorder(`Team/NOC`,Total),y=Total, fill=`Team/NOC`)) +
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")+
  ggtitle("GRAFICO BARRAS TOTAL MEDALLAS ")+
  labs(caption = "HISTORIAL JUEGOS OLIMPICOS")+ 
  xlab("Pais")

D<-read_excel(choose.files()) #Importamos datos del archivo de excel "Deportistas"
View(D)
attach(D)

#Grafico de dispersion entre a�o de olimpiadas y la cantidad de deportistas
ggplot(D, aes(A�o, `Deportistas`)) +
  geom_point(color="gold2")+
  geom_smooth(method = "lm")+
  ggtitle("GRAFICO DISPERSION ENTRE A�O DE OLIMPIADAS Y NUMERO DE DEPORTISTAS ")+
  labs(caption = "HISTORIAL JUEGOS OLIMPICOS") + 
  theme_dark()

#Correlaci�n
cor(D$A�o,D$`Deportistas`, method = "pearson") #correlaci�n positiva 

#Regresi�n Lineal simple

D1 <- lm(`A�o`~ Deportistas, data=D)
summary(D1)
AIC(D1) # Para escoger el mejor modelo regresi�n (el valor m�s bajo se selecciona)
plot(D1)
abline(lsfit(A�o, Deportistas)) # Trazamos la recta de regresi�n estimada


#Serie de tiempo  edicion (a�o) de las olimpiadas vs numero de deportistas por a�o
S<-read_excel(choose.files())
S
#Serie de tiempo
ST <- ts(S$Deportistas, start=(1896), end=(2020),deltat = 4)
plot(ST, xlab="A�o de olimpiadas", ylab="Numero de deportistas", main="Serie de tiempo: Numero deportistas por a�o")

acf(ST)
pacf(ST)

library(urca)
summary(ur.kpss(ST))

library(forecast)

STBest <- auto.arima(x=ST);STBest

acf(STBest$residuals)
coef(STBest)

#Predicci�n  de las sigueinte 5 ediciones de los juegos olimpicos 2024, 2028, 2032, 2036, 2040

Forecast <- forecast(object=STBest, h=5)

plot(Forecast,col="darkgoldenrod4", main="Predicci�n")
