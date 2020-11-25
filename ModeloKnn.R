DistanciaDM <- function(x1 , y1 , x2 , y2){
  
  return (abs(x2 - x1)+ abs(y2 - y1))
  
}

#saca distancia euclidiana
DistanciaEu <- function(x1 , y1 , x2 , y2){
  
  return (((x2 - x1)^2 + (y2 - y1)^2)^0.5)
  
}

#Funcion para hallar las distancias mas cortas de un punto en la grafica "numero de casos de violencia fisica" vs "numero de
#casos de violencia sexual" 

KNN <- function(df , x , y , k){
  
  dfTemp <- df
  
  #dm <- DistanciaDM(x,y,df$x , df$y) #Distancia Manhattan
  dm <- DistanciaEu(x,y,df$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA , df$N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL) #Distancia Euclidiana
  
  dfTemp$distancias <- dm
  
  dfTemp <- dfTemp[with(dfTemp , order(dfTemp$distancias)), ]
  
  Minimos <- dfTemp[c(1:k) , "DEPARTAMENTO"]
  
  
  return (Minimos)
  
}


Probabilidades <- function(minimos){
  VCat <- unique(minimos)
  
  Departamento <- c()
  freq <- c()
  
  
  for (i in 1:NROW(VCat)){
    
    term <- VCat[i]
    
    Departamento <- c(Departamento, term)
    
    ft <- sum(as.numeric(minimos == term))
    
    freq <- c(freq , ft)
    
    
  }
  
  TabFreq <- data.frame(Departamento , freq)
  total <- sum(TabFreq$freq)
  TabFreq$PorcetajeProb <- TabFreq$freq * 100/total
  return (TabFreq)
  
}


Modelo_KNN <- function(df , x , y , k){
  
  return (Probabilidades(KNN(dfKNN , 125 , 132 , k)))
  
}





