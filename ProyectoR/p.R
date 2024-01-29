library(MASS)
library(missMDA)
library(dplyr)
library(cluster)
library(ggplot2)
library(hrbrthemes)
library(xtable)
# ---------------------------------------------------------------------------
#                    Proyecto de NBA de Mineria de Datos
#----------------------------------------------------------------------------
# Primero hemos agrupado todos los datos de las últimas 6 temporadas de la NBA
# indicando que jugadores han sido escogidos como ALL-STAR tanto en la temporada
# actual y la siguiente. Importamos los datos como un fichero CSV y lo almacenaos
# en la variable data.

# --------------- Limpieza de Datos  ----------------------------------------
data = read.csv("data.csv", header = T)
complete.cases(data)
data_sin_nulos <- mutate_all(data, ~replace(., is.na(.), 0))
complete.cases(data_sin_nulos)

#Cambiar los valores lógicos por binarios
data_sin_nulos$ASCURR[data_sin_nulos$ASCURR == "VERDADERO"] <- 1
data_sin_nulos$ASCURR[data_sin_nulos$ASCURR == "FALSO"] <- 0
data_sin_nulos$ASNEXT[data_sin_nulos$ASNEXT == "VERDADERO"] <- 1
data_sin_nulos$ASNEXT[data_sin_nulos$ASNEXT == "FALSO"] <- 0


# Guardamos los datos  en la nueva variable my_data,con los valores nulos 
# puesto a cero. Las columnas que tenian nulos eran: X3P, X2P y GS. Como son
# de porcentajes hemos visto que es adecuado marcar los nulos como 0

jugadores2021 = data_sin_nulos[data_sin_nulos$ASNEXT == "",] #jugadores temporada 2020/21
restojugadores = data_sin_nulos[data_sin_nulos$ASNEXT != "",] #resto de jugadores
all_star = restojugadores[restojugadores$ASNEXT == 1,] #All-Star de restojugadores
no_all_star = restojugadores[restojugadores$ASNEXT == 0,] # No All-Star



#ver el número mínimo de partidos jugados para ser all-star
partidos_AS = c()
for (i in 1:82) {
  partidos_AS[i] = length(all_star$G[all_star$G == i])
}
par(mfrow = c(1, 1))
barplot(partidos_AS)
partidos_acumulados_AS = cumsum(partidos_AS)
resAcumulado_AS = barplot(partidos_acumulados_AS, cex.axis = TRUE)
tolerancia = 0.975
umbral = (1 - tolerancia)*sum(partidos_AS)
for (i in 1:82) {
  if(partidos_acumulados_AS[i] > umbral) break
}
requisito_partidos_jugar = i

#quitamos los jugadores que no cumplen el requisito de partidos
data_mucho_jugado = data_sin_nulos[data_sin_nulos$G >= requisito_partidos_jugar,]
data_limpia = data_mucho_jugado[,c(-1, -2, -4)]#Quitamos variables que no interesan
jugadores2021_mucho_jugado = data_mucho_jugado[data_mucho_jugado$ASNEXT == "",]
jugadores2021 = data_limpia[data_limpia$ASNEXT == "",]
restojugadores = data_limpia[data_limpia$ASNEXT != "",]

all_star = restojugadores[restojugadores$ASNEXT == 1,]
no_all_star = restojugadores[restojugadores$ASNEXT == 0,]

# Pasamos de 3107 datos originales a 2412. Eliminamos los jugadores que no han
# influido mucho en la temporada, por lo tanto, no tienen opción a ser
# escogidos como all-star



#------------------- descripción de datos --------------------------------

# ---------------------- analisis clúster 

datos_cluster = restojugadores[,c(-28, -29)]

W<-c()
for(i in 1:5){
  res=pam(daisy(datos_cluster, metric = "gower", type = list("symm" = c("ASCURR"))), k=i)
  W[i] =  res$objective[1]
}
par(mfrow = c(1,1))
plot(W)

# Vemos que el salto esta claramente entre 1 y 2, por lo tanto
# escogemos 2 grupos.

cluster = pam(daisy(datos_cluster, metric = "gower", type = list("symm" = c("ASCURR"))), k=2)
par(mar = rep(2, 4), mfrow=c(4, 3))
for(i in 1:26){
  boxplot(datos_cluster[,i] ~cluster$clustering)
  title(names(datos_cluster)[i])
}

restojugadores_con_nombre = data_mucho_jugado[data_mucho_jugado$ASNEXT != "",]
#Jugadores del resto de temporadas con su grupo asignado
restojugadores_con_nombre["grupos"] = cluster$clustering

media_grupo_1 = colMeans(restojugadores_con_nombre[restojugadores_con_nombre$grupos == 1,c(-1, -2, -4, -30, -31, -32, -33)])
media_grupo_2 = colMeans(restojugadores_con_nombre[restojugadores_con_nombre$grupos == 2,c(-1, -2, -4, -30, -31, -32, -33)])



#------------------ Componentes P, para representar
datos.pca <- princomp(datos_cluster[, -27], cor = T)
summary(datos.pca)
datos.pca$loadings[,1:2]
biplot(datos.pca)
# La primera componente principal nos indica los datos de los jugadores que más tiempo y partidos
# juegan. La segunda componente se centra en jugadores exteriores que se centran más en los triples

par(mfrow = c(1,1))
ylim = c(-10, 8)
xlim = c(-15, 7)
plot(datos.pca$scores[cluster$medoids,1:2], xlab = "Comp 1", 
     ylab = "Comp 2", pch=8, xlim = xlim, ylim = ylim, col='green')
points(datos.pca$scores[cluster$clustering==1,1:2], col="red")
points(datos.pca$scores[cluster$clustering==2,1:2],col='purple')
points(datos.pca$scores[cluster$clustering==2 & datos_cluster$ASCURR == 1,1:2], pch = 13)
points(datos.pca$scores[cluster$medoids,1:2],cex = 1.5,pch=18, col='green')
legend(x = "bottomleft", legend = c("Clúster 1", "Clúster 2", "Medoides", "All-Star"), 
       col = c("red", "purple", "green", "black"), pch =c(1, 1, 18, 13) ,title = "Leyenda", cex = 0.6)


# Vamos a ver en que grupo pertenece los All-Star
grupos_AS = c()
grupos_AS[1] = length(restojugadores_con_nombre[restojugadores_con_nombre$grupos == 1 & restojugadores_con_nombre$ASNEXT == "1",1])
grupos_AS[2] = length(restojugadores_con_nombre[restojugadores_con_nombre$grupos == 2 & restojugadores_con_nombre$ASNEXT == "1",1])
grupos_AS


#Como podemos ver están todos los All-Star clasificados en el grupo 2.

#------------------ descripción ALL-Star 
summary(all_star)
par(mfrow = c(2, 2))
mostrar = seq(1, 29)[c(-27, -28, -29)]
for(i in mostrar){#Gráficas de las variables
  boxplot(all_star[i], xlab=names(all_star)[i])
}


#------------------ descripción totales 
summary(restojugadores)
par(mfrow = c(2, 2))
for(i in mostrar){#Gráficas de las variables
  boxplot(restojugadores[i], xlab=names(restojugadores)[i])
}
porcentaje_all_star = length(all_star[,1])/length(restojugadores[,1])
#alrededor de un 5% de los jugadores son all-star




#------------------------- Analisis discriminante ----------------------------


grupos= as.factor(restojugadores[,28]) #Grupos 1 all_star 0 no
datos_analizar = restojugadores[,1:26]
resL = lda(datos_analizar, grupos)
prediccionesL = predict(resL, datos_analizar)
taL = table(prediccionesL$class, grupos)
(taL[1,1]+taL[2,2])/sum(sum(taL))
taL

#Hacemos el análisis discriminante quadrático para comparar cual funciona mejor
resQ = qda(datos_analizar, grupos)
prediccionesQ = predict(resQ, datos_analizar)
taQ = table(prediccionesQ$class, grupos)
(taQ[1,1]+taQ[2,2])/sum(sum(taQ))
taQ

# Como podemos observar la bondad de ajuste del lineal de Fischer es más elevado, por lo
# que las clasificaciones son más adecuadas.

# predicciones de la temporada 2020-2021
a_predecir_2021 = jugadores2021[,1:26]
predicciones2021 = predict(resL, a_predecir_2021)
predicciones2021$class
predicho = jugadores2021[predicciones2021$class == 1,]
indices = which(predicciones2021$class == 1)
indices
potencial_AS = jugadores2021_mucho_jugado[indices,]

#bucle para calcular los 25 All-star
potencial_AS_limpiadas = potencial_AS[,c(-1, -2, -4, -31, -32)]
all_star_limpiada = all_star[,c(-28, -29)]
dist_AS = c()
for(i in 1:length(potencial_AS_limpiadas[,1])){
  x = rbind(potencial_AS_limpiadas[i,], all_star_limpiada) 
  matX = daisy(x, metric = "gower", type = list("symm" = c("ASCURR")))
  X = as.matrix(matX)
  dist_AS[i] = min(X[1,-1])
}
potencial_AS["distancia_AS"] = dist_AS
potencial_AS[order(-potencial_AS$distancia_AS),]
definitivos_AS = tail(potencial_AS[order(-potencial_AS$distancia_AS),], 26)

print(xtable(definitivos_AS[,c(1, 2, 4, 6, 29, 23, 24)], type = "latex"), file = "tablaAS01.tex")
