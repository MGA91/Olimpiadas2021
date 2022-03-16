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

# Estadistica básica
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

#Grafico de dispersion entre año de olimpiadas y la cantidad de deportistas
ggplot(D, aes(Año, `Deportistas`)) +
  geom_point(color="gold2")+
  geom_smooth(method = "lm")+
  ggtitle("GRAFICO DISPERSION ENTRE AÑO DE OLIMPIADAS Y NUMERO DE DEPORTISTAS ")+
  labs(caption = "HISTORIAL JUEGOS OLIMPICOS") + 
  theme_dark()

#Correlación
cor(D$Año,D$`Deportistas`, method = "pearson") #correlación positiva 

#Regresión Lineal simple

D1 <- lm(`Año`~ Deportistas, data=D)
summary(D1)
AIC(D1) # Para escoger el mejor modelo regresión (el valor más bajo se selecciona)
plot(D1)
abline(lsfit(Año, Deportistas)) # Trazamos la recta de regresión estimada


#Serie de tiempo  edicion (año) de las olimpiadas vs numero de deportistas por año
S<-read_excel(choose.files())
S
#Serie de tiempo
ST <- ts(S$Deportistas, start=(1896), end=(2020),deltat = 4)
plot(ST, xlab="Año de olimpiadas", ylab="Numero de deportistas", main="Serie de tiempo: Numero deportistas por año")

acf(ST)
pacf(ST)

library(urca)
summary(ur.kpss(ST))

library(forecast)

STBest <- auto.arima(x=ST);STBest

acf(STBest$residuals)
coef(STBest)

#Predicción  de las sigueinte 5 ediciones de los juegos olimpicos 2024, 2028, 2032, 2036, 2040

Forecast <- forecast(object=STBest, h=5)

plot(Forecast,col="darkgoldenrod4", main="Predicción")
