#Exploracion (Consultas exploratorias y diagramas )

#Obtenemos el data frame ya preprocesado

df <- read.csv("Codigo/Datos Fuente/DatosPreprocesadosFinal.csv")
df$FECHA.ENVIO <- as.Date(df$FECHA.ENVIO) #Convertimos el dato fecha
#df$DEPARTAMENTO <- as.factor(df$DEPARTAMENTO)
#df$PROVINCIA <- as.factor(df$PROVINCIA)
#df$DISTRITO <- as.factor(df$DISTRITO)
#df$NOMBRE.CENTRO.ATENCION <- as.factor(df$NOMBRE.CENTRO.ATENCION)
glimpse(df)


#10 consultas Exploratorias con DPLYR
library(dplyr)

#1.Cuantos registros de numero de casos de violencia se dieron en Lima Metropolitana

 q1 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% summarise(Total = n())

#2.Cuantos casos de denuncia hacia los hombres existieron durante el 2013 y 2020
 
 #q2 <- sum(df %>% select(N..CASOS.ATENDIDOS...HOMBRES...TOTAL))
 q2 <- df %>% summarise(Total_Casos = sum(N..CASOS.ATENDIDOS...HOMBRES...TOTAL))
 
#3.Listado de año,  provincias y numero de casos de violencia hacia mujeres que se dieron en el departamento de Arequipa 
 q3 <- df %>% filter(DEPARTAMENTO == "AREQUIPA") %>% select(AÑO, PROVINCIA, N..CASOS.ATENDIDOS...MUJERES...TOTAL)
 
#4.Numero de casos de violencia fisica en el distrito de Carabayllo 
 q4 <- df %>% filter(DISTRITO == "CARABAYLLO") %>% summarise(Numero_Violencia_fisica
                                                             = sum(N..CASOS.ATENDIDOS...VIOLENCIA.FISICA))
 
 
#5.Listado de departamentos y año cuyo numero de casos de violencias psicologicas son mayor a 100
 q5 <- df %>% select(AÑO , DEPARTAMENTO , N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA) %>% 
   filter( N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA > 100)
 
 
#6.Listado de los 10 primeros registros con el mayor numero de casos de violencia contra la mujer
 q6 <- df %>% select(AÑO,FECHA.ENVIO,DEPARTAMENTO,PROVINCIA,DISTRITO,N..CASOS.ATENDIDOS...MUJERES...TOTAL) %>% 
   arrange(desc(N..CASOS.ATENDIDOS...MUJERES...TOTAL)) %>% top_n(10)

#7.Listado de los 5 primeros registros con el mayor numero de casos de violencia fisica en el distrito de Miraflores
 q7 <- df %>% filter(DISTRITO == "MIRAFLORES") %>%
   select(AÑO , FECHA.ENVIO , N..CASOS.ATENDIDOS...VIOLENCIA.FISICA) %>%
   arrange(desc(N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)) %>% top_n(5)
 
 
#8. Listado de cada departamento con el numero de casos de violencia sexual
 
 q8 <- df %>% select(DEPARTAMENTO , N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL) %>%
   group_by(DEPARTAMENTO) %>% summarise(Total_Casos_ViolenciaSexual = sum(N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL))
   
#9. Listado de cada provincia del departamento de Cusco con el numero de casos de violencia contra la mujer
 q9 <- df %>% filter(DEPARTAMENTO == "CUSCO") %>% select(PROVINCIA , N..CASOS.ATENDIDOS...MUJERES...TOTAL) %>%
   group_by(PROVINCIA) %>% summarise(Total_Casos_violenciaMujer = sum(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
 
#10.Del total del numero de casos de violencia contra la mujer en el departamento de Lima Metropolitana, cual es 
 #el promedio , la mediana , el minimo , el maximo , percentiles y cuartiles
 
 q10 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
   summarise(promedio = mean(N..CASOS.ATENDIDOS...MUJERES...TOTAL)) 
 q11 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
   summarise(mediana = median(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
 q12 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
   summarise(minimo = min(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
 q13 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
   summarise(maximo = max(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
 q14 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
   summarise(cuartil = quantile(N..CASOS.ATENDIDOS...MUJERES...TOTAL))

# 10 DIAGRAMAS CON ggplot2
 library(ggplot2) 
 
 
 #G1: Grafica un histograma donde se puede observar la mayor concentracion de datos por la variable n.casos.atendidos.mujeres

 g1 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL) + geom_histogram(fill = "#12BB00") +
    theme_dark()
 
 #G2: Grafica un histograma donde se puede observar la mayor concentracion de datos por dep para la variable n.casos.mujeres
 g2 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL, fill = DEPARTAMENTO) + geom_histogram(bins = 100) +
    theme_linedraw() + labs(x = "Numero de casos atentidos de violencia contra la mujer" , y = "Numero de registros",
                            title = "Histograma de registros de violencia contra la mujer por Departamento ")
 
 #G3: Grafica el histograma histograma de antes pero en diferentes viewers
 g3 <- g2+facet_wrap(vars(DEPARTAMENTO))
    
 
 #G4: Diagrama de cajas para la variable "numero de casos de violencia atendidos contra la mujer"
 
 g4 <- ggplot(df) + aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL ) + geom_boxplot(fill = "#27CC85") +
    labs(y = "Numero de casos atentidos de violencia contra la mujer", x = "" , title = "Diagrama de cajas")
 
 
 #G5: Diagrama de cajas para la variable "numero de casos de violencia atendidos contra la mujer" para cada departamento
 g5 <- ggplot(df) + aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) + geom_boxplot() +
    labs(y = "Numero de casos atentidos de violencia contra la mujer", x = "" , title = "Diagrama de cajas")
 

 
 #G6: Diagrama de cajas que muestra la dsitribucion de registros de los departamentos Lima y Arequipa con respecto al 
 #numero de casos de denuncia de violencia contra la mujer
 
 g6 <- df %>% filter((DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA") & 
                         AÑO >= 2015 & AÑO <= 2019) %>% ggplot() +
    aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) + 
    geom_boxplot() +
    labs(y = "Numero de casos atentidos de violencia contra la mujer", x = "" , title = "Diagrama de cajas") +
    facet_wrap(facets = vars(DEPARTAMENTO))
 
 
 #G7: Diagrama de densidad que meustra la densidad de los registros para los departamentos de Arequipa , Lima y La Libertad
 #con respecto al numero de casos denunciados contra la mujer
 
 g7 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA" | DEPARTAMENTO == "LA LIBERTAD") %>%
    ggplot() + 
    aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) +
    geom_density(alpha = 0.5) +
    labs(x = "Numero de casos atentidos de violencia contra la mujer", x = "" , title = "Diagrama de densidad")
    
 
 #G8: Diagrama de dispercion del numero de casos atendidos de violencia contra la mujer versus el numero de casos atendidos
 #por violencia
 g8 <- ggplot(df , 
              aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                  y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                  color = DEPARTAMENTO)) +
    geom_point()
 
 
 
 #G9: Diagrama de dispercion del numero de casos atendidos de violencia contra la mujer versus el numero de casos atendidos
 #por violencia en los departamentos de Lima , Arequipa y La Libertad
 g9 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA" | DEPARTAMENTO == "LA LIBERTAD") %>% 
    ggplot( aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                  y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                  color = DEPARTAMENTO)) +
    geom_point()
 
 
 #G10
 g10 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA" | DEPARTAMENTO == "LA LIBERTAD") %>% 
    ggplot( aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                color = DEPARTAMENTO)) +
    geom_point() + geom_smooth()
 
 

 
  
 
