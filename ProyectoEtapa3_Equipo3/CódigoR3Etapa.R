install.packages("dplyr")
install.packages("nortest")

library(dplyr)
library(writexl)
library(discreteRV)
library(nortest)

df.data <- read.csv("C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/Data.csv")
row.names(df.data) <- df.data$canton
df.data$canton <- NULL
df.data$cod_provin<- NULL
df.data$cod_canton <-NULL
#Matina

matina <- df.data[c('Matina'),]
weeks_of_interest_matina <- select(matina, 'X03.05.2021':'X21.06.2021')
weeks_of_interest_matina <- c(select(matina, 'X03.05.2021':'X21.06.2021'))
cases_matina<-unlist(weeks_of_interest_matina)
cases_matina<-diff(cases_matina)


df.summary <- data.frame(unclass(summary(cases_matina)))
colnames(df.summary) <- c('Matina')




sd.matina <- sd(cases_matina, na.rm = TRUE)
df.sd <- data.frame(c(sd.matina))
colnames(df.sd) <- c('Matina')





#Santa Ana
santa_ana <- df.data[c('Santa Ana'),]

weeks_of_interest_table_santaana <- select(santa_ana, 'X03.05.2021':'X21.06.2021')
weeks_of_interest_santaana <- c(select(santa_ana, 'X03.05.2021':'X21.06.2021'))
cases_santaana<-unlist(weeks_of_interest_santaana)
cases_santaana<-diff(cases_santaana)


df.summary$SantaAna <- unclass(summary(cases_santaana))


sd.santaana <- sd(cases_santaana, na.rm = TRUE)
df.sd$SantaAna <- c(sd.santaana)



is.num<- sapply(df.summary, is.numeric)

#Tablas para documento
df.summary[is.num] <- lapply(df.summary[is.num], round, 2)
row.names(df.summary) <- c('Min', '1st Q', 'Mediana', 'Media', '3rd Q', 'Max')
write_xlsx(df.summary,"C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/Summary.xlsx")


row.names(df.sd) <- c('Varianza', 'Desviación Estandar')
write_xlsx(df.sd,"C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/VSDTable.xlsx")


#Graficos del documento

plot(cases_santaana, 
     main = "Figura 1: Comparación de contagios diarios en Santa Ana y Matina 
     entre las semanas 18 y 24 de 2021", 
     xlab = "Días", 
     ylab = "Cantidad de Contagios",
     xaxt= 'n',
     pch = 20,
     cex = 2)

points(cases_matina,
       col = "blue",
       pch = 20,
       cex = 2)

axis(1, at = seq (1,50, by=1), las= 2, tck = 1, col = "gray")


box()


#Boxplot

df.juntos = data.frame(Matina = c(cases_matina), SantaAna= c(cases_santaana))
boxplot(df.juntos$Matina, df.juntos$SantaAna, xlab = "Cantones", ylab = "Contagios Diarios", main = "Figura3: Comparación de contagios diarios en Santa Ana y Matina 
        entre las semanas 18 y 24 de 2021", names= c("Matina", "Santa Ana") )

#QQplots

qqnorm(cases_matina, pch = 1, main = "Normal Q-Q Plot Matina")
qqline(cases_matina, col = "steelblue", lwd = 2)

qqnorm(cases_santaana, pch = 1, main = "Normal Q-Q Plot Santa Ana")
qqline(cases_santaana, col = "steelblue", lwd = 2)

lillie.test(cases_matina)
lillie.test(cases_santaana)
ftest <- var.test(cases_matina, cases_santaana)
ftest.df <- data.frame(Estadistico = ftest$statistic, valorP = ftest$p.value, IntervaloConfianza = ftest$conf.int, ratioVariancia = ftest$estimate)
write_xlsx(ftest.df,"C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/FtestTable.xlsx")


ttest <- t.test(cases_matina, cases_santaana, var.equal = TRUE)
ttest.df <- data.frame(Estadistico =ttest$statistic, valorP = ttest$p.value, IntervaloConfianza = ttest$conf.int)
write_xlsx(ttest.df,"C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/TtestTable.xlsx")

ttest <- t.test(cases_matina, cases_santaana,alternative = "greater", var.equal = TRUE)
ttest.df <- data.frame(Estadistico =ttest$statistic, valorP = ttest$p.value, IntervaloConfianza = ttest$conf.int)
write_xlsx(ttest.df,"C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/TtestTable.xlsx")

ttest2 <- t.test(cases_matina, cases_santaana, alternative = "less", var.equal = TRUE)
ttest2.df <- data.frame(Estadistico =ttest2$statistic, valorP = ttest2$p.value, IntervaloConfianza = ttest2$conf.int)
write_xlsx(ttest2.df,"C:/Users/marcelo.delgado/Desktop/UCR/Probabilidad/2da Parte Proyecto/TtestTable2.xlsx")


