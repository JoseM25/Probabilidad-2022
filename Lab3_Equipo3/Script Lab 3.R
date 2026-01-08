install.packages("tidyverse") # instalar si no lo tienen instalado
install.packages("DescTools") # instalar si no lo tienen instalado
install.packages("car") # instalar si no lo tienen instalado

library(tidyverse)
library(DescTools)
library(car)
library(dplyr)

#P1 La desventaja principal de la media es que esta es afectada de manera desproporcional por sus extremos. 

train <- read.csv(
    "C:/Users/josea/Downloads/train.csv",
    stringsAsFactors = FALSE,
    header = TRUE,
    encoding = "UTF-8"
  )
str(train)
head(train)

# encontrar la media de la tarifa (Fare)
mean(train$Fare)

#P2 La media de la tarifa, representa lo que se paga aproximadamente por pasajero lo cual seria $32.20

# usando la funci?n de R mode()
mode(train$Age)

# usando un m?todo simple parecido al del laboratorio 1
actual_mode <- table(train$Age) # crea una tabla de frecuencia de edades

# usamos la funci?n max() para encontrar el valor m?s frecuente
names(actual_mode)[actual_mode == max(actual_mode)]

#P3 Sus valores pueden ser de tipo continuo o discreto, y cualitativos o cuantitativos, ademas puede que no exista o puede hber multiples modas en el caso en que varios datos cuenten con la misma frecuencia. 

#P4 La edad mas com?n en el barco era de 24 a?os por lo que se observa que en el barco se contaba con grupos de pasajeros de edad muy joven

#5 El calculo de la mediana nos lo da summary y sera 14.45
summary(train$Fare)
#6 El valor de la mediana indica que en general que la gente en el barco pago ya sea menos de 14 o mas de esto siendo el centro de dividiendo las tarifas en dos mitades.


fare_max <- max(train$Fare, na.rm = TRUE)
fare_min <- min(train$Fare, na.rm = TRUE)
fare_range <- (fare_max - fare_min)

#P7 el rango del conjunto de datos nos lo da la diferencia entre el min y el max, en el caso de la tarifa se trata de 512.32 - 0 lo que nos indica que el rango es 512.32

# usando R b?sico grafiquemos un boxplot
boxplot(train$Age ~ train$Pclass, xlab = "Class", ylab = "Age", col = c("blue"))

#P8 En el boxplot, a la hora de analizar las medianas de cada grupo, se puede notar como entre más alta sea la clase,(clase 1 siendo la mayor), más alta es la mediana de edades del grupo, además de que, la mediana corresponde aproximadamente al centro de las cajas. Por otro lado, los puntos fuera de las barra de error representan los valores que se alejan del rango normal llamados valores atipicos.
var(train$Fare, na.rm = TRUE)
SD(train$Fare, na.rm = TRUE)

#P9 utilizando la funciones proveidas por R basico, podemos obtener la varianza utilizando la funci?n var() la cual nos da la varianza simple, lo que nos da 2469.44 , por otro lado la deviaci?n estandar la obtenemos utilizando a funci?n sd() la cual nos da 49.69

median(train$Age, na.rm = TRUE)
mean(train$Age, na.rm = TRUE)
var(train$Age, na.rm = TRUE)
sd(train$Age, na.rm = TRUE)

#P10 podemos utilizar las funciones de R basico mean(), median(), var(), sd() para obetner la media, mediana, varianza y desviaci?n estandara respectivamente de la edad de los pasajeros del Titanic. De esto obtenemos que la media seri 29.69, que la mediana corresponderia a 28. la varianza nos daria 211.02 y la desviaci?n estandar seria de 14.53.


hist(train$Fare, main = "Precio de entrada por persona" ,xlab = "Precio", col = "blue", breaks = 40, xlim = c(0,300), ylim = c(0,400))

hist(train$Age, main = "Edad de pasajeros",xlab = "Edad", col = "red", breaks = 40,  xlim = c(0,100), ylim = c(0,60))

fareM <- mean(train$Fare)
ageM <-mean(train$Age, na.rm = TRUE)
fareSD <- SD(train$Fare)
ageSD <- SD(train$Age, na.rm = TRUE)

fareV <- seq(-512,512,0.1)
ageV <- seq(-80,80,0.1)

dStandardNormalFare <- dnorm(fareV, mean=fareM, sd=fareSD)
dStandardNormalAge <- dnorm(ageV, mean=ageM, sd=ageSD)
plot(fareV,dStandardNormalFare,type="l",col="blue",lwd=3)
plot(ageV,dStandardNormalAge,type="l",col="red",lwd=3)


#P11 Gráficos de las distribuciones correspondientes a las variables de precio de entrada y edad del conjunto de datos del Titanic. 

new_data <- train[!(train$Pclass > 1), ]
tail <- 1

#df <- df[!(df$col1==2 & d$col1==3), ]

z.test2 <- function(a, b, n, t) {
  tail = t
  if(tail > 2) {
    tail = 2
  }
  sample_mean = mean(a)
  pop_mean = mean(b)
  c = nrow(n)
  var_b = var(b)
  zeta = (sample_mean - pop_mean) / (sqrt (var_b / c))
  pvalue = tail * (pnorm(q=zeta, lower.tail=FALSE))
  return(pvalue)
  
}

z.test2(new_data$Survived, train$Survived, new_data, tail)


#P12 Función que calcula p-value dependiendo del valor de Z, que incluye como 4to parámetro una variable para indicar si se quiere la prueba de 1 cola o de 2 colas. Contiene un condicional que si se pone en la variable de la cola un valor mayor a 2, se va a hacer una prueba de 2 colas igualmente. En el caso del ejemplo realizado, el p-value en la prueba de 1 cola a la derecha da 5.689134e-14, y en la prueba de 2 colas da 1.137827e-13, ambos valores muy por debajo del nivel del nivel de significancia del 95%. 


#P13 Debido a que se presenta un valor alto en Z, esto significa que el p-value será muy bajo, y este al ser muy bajo (menor a 0.05 si se hace un estudio con base al 95% de confianza, el más común o estándar),  se rechaza la hipótesis nula y por lo tanto, contextualizando con los datos del Titanic, se puede argumentar que sí parece haber una relación significativa entre las posibilidades de supervivencia y la clase a la que pertenecen los pasajeros. 


chisq.test(train$Survived, train$Sex)

summary(table(train$Survived, train$Pclass))

#P14 En ambos casos, al analizar el p-value que devuelven ambas funciones, se puede notar que ambos son menores a 0.05 por mucho, ya que en el caso del sexo, el p-value < 2.2e-16, y en el caso de la clase, el p-value = 4.549e-23, por lo que se puede inferir que la relación entre las variables es significativa con un 95% de confianza, o relacionándolo con el conuunto de datos, se puede observar que existe una relación significativa entre la superviviencia y el sexo, y la supervivencia y la clase a la que pertenecía el pasajero.


plot(train$Fare, train$Age, xlab = "Fare", ylab = "Age")


#P15 Debido  a que el gráfico no presenta una relacion lineal o no forma una línea o patrón evidente entre sus variables, con la información presentada en el grafico no parece que exista una correlación entre las variables de precio de entrada y edad. 

cor.test(train$Age, train$Fare, method = "pearson")

#P16 El cálculo de correlación utilizando el método de Pearson confirma las inferencias realizadas con el análisis visual gráfico hecho en la pregunta anterior, ya que la función demuestra que la correlación entre las variables es muy baja, de un 9.6% aproximadamente. Contextualizando estos datos al conjunto de datos del Titanic, se puede inferir que no existe una asociación significativa entre la edad y el precio pagado por tiquete por los pasajeros.

plot(train$Fare, train$Survived, xlab = "Fare", ylab = "Survived")
cor.test(train$Fare, train$Survived, method = "pearson")
plot(train$Age, train$Survived, xlab = "Age", ylab = "Survived")
cor.test(train$Age, train$Survived, method = "pearson")


#P17 


# 2da Parte Prueba T ------------------------------------------------------

# Datos en 2 vectores numéricos 
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)

# Creamos el conjunto de datos 
my_data <- data.frame(group = rep(c("Woman", "Man"), each = 9), weight = c(women_weight, men_weight))

print(my_data)

# Pregunta 18
pesos <- data.frame(women = c(women_weight), men = c(men_weight))
label = c("Women", "Men")
boxplot(pesos$women,pesos$men, xlab = "Group", ylab = "Weight", col = c("pink","blue"), names = label)

# Pregunta 19
group_by(my_data, group) %>%
  summarize(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE))

# La mediana de los hombres tiene un valor de 67.3 y
# la mediana de las mujeres tiene un valor de 48.8

# Pregunta 20
# Prueba Shapiro-Wilk de normalidad en peso hombres
with(my_data, shapiro.test(weight[group == "Man"]))

# Prueba Shapiro-Wilk de normalidad en peso mujeres
with(my_data, shapiro.test(weight[group == "Woman"]))

# QQPlot en peso hombres
with(my_data, qqPlot(weight[group == "Man"]))

# QQPlot en Peso en peso mujeres
with(my_data, qqPlot(weight[group == "Woman"]))

f_test <- var.test(weight ~ group, data = my_data) 
f_test
# El valor p de la prueba F es 0.1714, esto significa
# que es mayor al nivel de significancia, por lo tanto
# se concluye que las varianzas no tienen diferencia
# entre sí.

# Pregunta 21
# Método 1
t.res <- t.test(weight ~ group, 
                data = my_data, 
                var.equal = TRUE) 
t.res

# Método 2
t.res.2 <- t.test(women_weight, 
                  men_weight, 
                  var.equal = TRUE) 
t.res.2

# Al ejecutar ambos métodos se puede evidenciar que
# ambos métodos producen el mismo resultado, el
# resultado del valor p que producen ambos métodos
# es 0.01327, por lo tanto se rechaza H0 y se puede
# concluir que el peso promedio de las mujeres es
# diferente al de los hombres.
# Se rechaza H0


# Pregunta 22
# La media del grupo de los hombres tiene un
# valor de 68.9, mientras que la media del grupo de
# las mujeres tiene un valor de 52.1, por lo tanto
# el grupo de los hombres es mayor.

# Pregunta 23
# less:
one.tail.test <- t.test(weight ~ group, 
                        data = my_data, 
                        var.equal = TRUE, 
                        alternative = "less")

one.tail.test
# No se rechaza H0 (p-value > 0.05), 
# por lo tanto se puede decir que
# el promedio de peso en los hombres no es menor
# que el de las mujeres.

#greater
one.tail.test2 <- t.test(weight ~ group, 
                         data = my_data, 
                         var.equal = TRUE, 
                         alternative = "greater")

one.tail.test2
# El valor p es 0.006, lo cual es menor que 0.05,
# por lo tanto se rechaza la hipotesis nula y 
# y si se puede decir que el promedio de peso de
# los hombres es mayor que el promedio de peso de 
# las mujeres.
