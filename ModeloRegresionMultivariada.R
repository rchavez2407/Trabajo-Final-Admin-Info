#2. Regresion multivariada


#Experimentacion . Realizaremos el entrenamiento y la prueba

ids <- sample(1:NROW(dfpr) , NROW(dfpr)*0.85)
#mi data frame de entrenamiento

ini <- grep("N..CASOS.ATENDIDOS...MUJERES...TOTAL" , colnames(dfpr))
fin <- ini + 3

entrenamiento <- dfpr[ids, c(ini:fin)]
#mi data frame de prueba
prueba <- dfpr[-ids,c(ini:fin)]

colnames(entrenamiento) <- c("N.CasosMujeres" , "N.CasosPsico" , "N.CasosFisico", "N.CasosSexual")
colnames(prueba) <- c("N.CasosMujeres" , "N.CasosPsico" , "N.CasosFisico", "N.CasosSexual")

#Una vez tenemos los datasets de entrenamiento y prueba , procederemos a utilizar el de entrenamiento para el aprendizaje

f = lm(N.CasosMujeres~N.CasosPsico+N.CasosFisico+N.CasosSexual, data = entrenamiento)

#Una vez se realize el entrenamiento , utilizamos la funcion para hacer las pruebas

prueba$Prediccion <- predict(f,prueba)

#Ahora procedemos a encontrar un porcentaje de acierto dado el modelo

perError <- abs(100*((prueba$Prediccion - prueba$N.CasosMujeres)/prueba$Prediccion))

PerAciertorModelo <- 100 - mean(perError) # Este resultado es el porcentaje de acierto del modelo

