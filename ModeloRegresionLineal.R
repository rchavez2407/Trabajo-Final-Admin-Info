Regresion_Lineal <- function(var1 , var2 , nvar1 , nvar2){
  resultados <- list()
  
  prediccion <- NA
  
  promvar1 <- mean(var1)
  promvar2 <- mean(var2)
  
  dvar1 <- var1 - promvar1
  dvar2 <- var2 - promvar2
  
  arrayCov <- dvar1 * dvar2
  
  Covarianza <- sum(arrayCov)/(length(arrayCov)-1)
  
  
  desvar1 <- dvar1 * dvar1
  desvar2 <- dvar2 * dvar2
  
  
  VarianzaVar1 <- (sum(desvar1)/(length(desvar1) - 1)) ^ 0.5
  VarianzaVar2 <- (sum(desvar2)/(length(desvar2) - 1)) ^ 0.5
  
  r <- Covarianza/(VarianzaVar1 * VarianzaVar2)
  
  #Prediccion var2 sobre var1
  
  if (is.na(nvar2))
    prediccion <- promvar2 + (Covarianza/(VarianzaVar1 ^ 2))*(nvar1 - promvar1) 
  #Prediccion var1 sobre var2
  if (is.na(nvar1))
    prediccion <- promvar1 + (Covarianza/(VarianzaVar2 ^ 2))*(nvar2 - promvar2)
  
  resultados[[1]] <- data.frame(var1 , var2 , dvar1 , dvar2 , arrayCov ,  desvar1, desvar2)
  resultados[[2]] <- Covarianza
  resultados[[3]] <- r #nos da el porcentaje de la probabilidad de exito
  resultados[[4]] <- prediccion
  
  return (resultados)
  
}







