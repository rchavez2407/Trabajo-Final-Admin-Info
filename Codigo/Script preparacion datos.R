#arreglo prueba
arr <- c(4,5,7,7,8,9,45,1,2,NA,NA,8,7)

##PRUEBITAS

arr[!(is.na(arr))]

fprueba <- "10/07/2015"
fprueba <- as.Date(fprueba)

fprueba <- as.character(fprueba)

newFec <- paste("20",substring(fprueba,9,10),"-",
                substring(fprueba,6,7), "-" ,
                substring(fprueba,3,4),sep ="")

class(newFec)

as.Date(newFec)

año <- substring(newFec,1,4)

class(año)


numero <- "1 789"
numero2 <- " 741  "


numero <- gsub(" ",replacement = "", numero2) # para eliminar espacios


arrP <- c("1 789" , "1245" , " 23  1", " 54 ")

EliminaEspacios(arrP)




##Obtenemos el dataframe

DF <- read.csv("Codigo/Datos Fuente/CasosFem.csv",header = T , sep=";" ,
               na.strings = "" , dec = ",")

class(DF)

glimpse(DF)

##Hacemos la preparacion de datos

#Eliminamos als columnas que no nos interesan

PDF1 <- DF[,c(1:16 , 18:22)]
glimpse(PDF1)
#####Ahora solamente tenemos 21 columnas

###Debido que existen numeros en este data frame que fueron anotados con espacios , implementaremos una funcion que elimine
#espacios vacios

EliminaEspacios <- function(texto){
  return (gsub(" ",replacement = "", texto))
}
##Ahora , para cada variable numerica , les quitaremos sus espacios en blanco para posteriormente convertirlos en datos
#numericos
glimpse(PDF1)
PDF1$N..CASOS.ATENDIDOS...MUJERES...TOTAL <- EliminaEspacios(PDF1$N..CASOS.ATENDIDOS...MUJERES...TOTAL)
PDF1$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA <- EliminaEspacios(PDF1$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA)
PDF1$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA <- EliminaEspacios(PDF1$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)


##Borramos aquellos registros donde sus variables numericas esten NA

indNA <- c()
for (i in 1:NROW(PDF1)){
  
  
  if(is.na(PDF1[i, 17]) & is.na(PDF1[i, 18]) & is.na(PDF1[i, 19]) &
     is.na(PDF1[i, 20]) & is.na(PDF1[i, 21])){
    
    indNA <- c(indNA , i)
    
  }
  
  
}

PDF2 <- PDF1[-indNA , ]



PDF2 <- PDF1[!(is.na(PDF1$N..CASOS.ATENDIDOS...HOMBRES...TOTAL)),]

NROW(PDF1)
NROW(PDF2)
###Podemos observar que se eliminaron 12 registros ya que no tenian datos importantes


#Cambiamos los tipos de dato de las columnas
###Cambiando la fecha a as.Date
PDF2$FECHA.ENVIO <- as.Date(PDF2$FECHA.ENVIO)
PDF2$FECHA.ENVIO <- as.character(PDF2$FECHA.ENVIO)

PDF2$FECHA.ENVIO <- paste("20",substring(PDF2$FECHA.ENVIO,9,10),"-",
                          substring(PDF2$FECHA.ENVIO,6,7), "-" ,
                          substring(PDF2$FECHA.ENVIO,3,4),sep ="")

#Cambiando a tipo de dato Date
PDF2$FECHA.ENVIO <- as.Date(PDF2$FECHA.ENVIO)
glimpse(PDF2)

#Cambiando a tipo de dato factor
PDF2$DEPARTAMENTO <- as.factor(PDF2$DEPARTAMENTO)
PDF2$PROVINCIA <- as.factor(PDF2$PROVINCIA)
PDF2$DISTRITO <- as.factor(PDF2$DISTRITO)
PDF2$NOMBRE.CENTRO.ATENCION <- as.factor(PDF2$NOMBRE.CENTRO.ATENCION)


#cAMBIAMOS ALGUNOS DATOS A NUMERICOS
PDF2$N..CASOS.ATENDIDOS...MUJERES...TOTAL <- as.integer(PDF2$N..CASOS.ATENDIDOS...MUJERES...TOTAL)
PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA<- as.integer(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA)
PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA <- as.integer(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)


#Hacemos imputacion de datos para valores NA 

glimpse(PDF2)

promHT <- as.integer(mean(na.omit(PDF2$N..CASOS.ATENDIDOS...HOMBRES...TOTAL))) 
promMT <- as.integer(mean(na.omit(PDF2$N..CASOS.ATENDIDOS...MUJERES...TOTAL))) 
promVP <- as.integer(mean(na.omit(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA)))
promVF <- as.integer(mean(na.omit(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)))
promVS <- as.integer(mean(na.omit(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL)))


##IMpautacion para la variable de n..casos..atendidos.mujeres..total

PDF2$N..CASOS.ATENDIDOS...MUJERES...TOTAL <- ifelse(is.na(PDF2$N..CASOS.ATENDIDOS...MUJERES...TOTAL),
                                                    promMT,PDF2$N..CASOS.ATENDIDOS...MUJERES...TOTAL)

##Imputacion para la variable de numero de casos denunciados de violencia psicologia
PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA <- ifelse(is.na(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA),
                                                    promVP,PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA)


##Imputacion para la variable de numero de casos denunciados de violencia fisica
PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA <- ifelse(is.na(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA),
                                                          promVF,PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)

##Imputacion para la variable de numero de casos denunciados de violencia sexual
PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL <- ifelse(is.na(PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL),
                                                     promVS,PDF2$N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL)


##UNA VEZ TERMINADO DE REALZIAR EL PREPROCESAMIENTO GUARDAMOS EL DATASET EN UN ARCHIVO CSV

write.csv(PDF2, "Codigo/Datos Fuente/DatosPreprocesadosFinal.csv",row.names = FALSE)


###prbamos si recura el data frame ya preprocesado
PruebaObtencion <- read.csv("Codigo/Datos Fuente/DatosPreprocesadosFinal.csv")
glimpse(PruebaObtencion)


##Para comprobar si tiene valores NA
NumeroNA <- function(df , col){
  
  return (NROW(df[is.na(df[,col]), col]))
  
}


NumeroNA(df, 18)







