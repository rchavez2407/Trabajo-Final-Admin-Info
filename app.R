library("ggplot2")
library("shinyjs")#para la funcion toggle
library("shiny")
library("readxl")#para lectura de excel
library("dplyr")
library("lubridate")#para fechas
library("sqldf")
library("plotly")#para graficos dinamicos
library("shinythemes")

source("ModeloKnn.R")# Archivo que contiene el modelo
source("ModeloRegresionLineal.R") #Archivo que contiene el modelo 
source("Back up DB/Script Conexion a DB.R") #Para conectarnos a una base de datos
source("www/introduccion.R")
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  navbarPage( "Proyecto final sobre los casos de violencia en el Peru",
    tabPanel("Presentacion",introduccion),
    tabPanel("Recoleccion",
             sidebarLayout (
               sidebarPanel (
             selectInput("select", label = h3("Tipo de archivo: "), 
             choices = c('text'='1','csv'='2','xlsx'='3')),
             hr(),
             fileInput("file1", "Seleccione Archivo",accept = c("text/csv","text/json",
             "text/comma-separated-values,text/plain",".csv","XLSX file", 
             ".json",".xlsx",".xls",".xml")),
             helpText ( " Max. Tamaño de archivo: 30MB " )),
             mainPanel(
             tabsetPanel(  tabPanel("Info",tableOutput("tablaS6")),
                            tabPanel("Data",tableOutput("tablaS2"))
             ))))
    
            ,tabPanel("Preprocesamiento",
                        navlistPanel(
                          tabPanel("Limpieza e Imputacion",
                                 h4("Unir Dataset's Separados"),hr(),checkboxInput("control1", "Mostrar Codigo", FALSE),hr(),
                                   verbatimTextOutput("consulta1"), tableOutput("tablaS5"))
                          
                        )
            )
    
    ,tabPanel("Back Up DB" ,h3("Datos para conexion con una base de datos"),verbatimTextOutput("DatosDB"),h3("Script para conexion a una base de datos "), hr() ,verbatimTextOutput("CodeDB") )
    ,tabPanel("Pruebas",tableOutput("TablaPruebas"))
  
              ,tabPanel('Consultas Exploracion',
                        navlistPanel(
                          tabPanel("Consultas DPLYR",
                      h4("DPLYR"), hr(),selectInput("selectdp", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5','consulta 6'='6','consulta 7'='7','consulta 8'='8','consulta 9'='9','consulta 10'='10')),checkboxInput("control2", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta2"),dataTableOutput("tablaS3"))
                      , tabPanel("Consultas con dbGetSQL", h4("SQL"),hr(),selectInput("selectsq", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3')),checkboxInput("control3", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta3"),dataTableOutput("tablaS4"))
                      
                      )
                      )
            
            ,tabPanel('Graficos',
                      navlistPanel( 
                        tabPanel("Graficos GGPLOT2",h4("GGPLOT2"),hr(),selectInput("selectgg", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5','consulta 6'='6','consulta 7'='7','consulta 8'='8','consulta 9'='9','consulta 10'='10')),checkboxInput("control4", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta4"),plotOutput('plot1'))))
                        
            ,tabPanel('Modelo',
                      navlistPanel(  
                     tabPanel("Regresion Lineal",hr(),checkboxInput("check", "Mostrar Codigo", FALSE),verbatimTextOutput("CodRL"),hr(), plotOutput('plotRL') , sliderInput("xRL" ,"Numero de casos de denuncias de mujeres", min = 50 , max = 500, value = 1 ), sidebarLayout(sidebarPanel(h5("Predicion del numero de casos de denuncia por violencia fisica:")),mainPanel(textOutput("IdPrediccionRL")))  )
                     ,tabPanel("Regresion Multivariada",hr(),checkboxInput("check2", "Mostrar Codigo", FALSE),verbatimTextOutput("CodRM"),hr(), plotOutput("plot8"))
                     ,tabPanel("KNN",h4("Codigo implementacion KNN"),verbatimTextOutput("CodKnn"),sliderInput("xKnn" ,"Numero de casos de denuncias de violencia fisica", min = 50 , max = 500, value = 1 ),sliderInput("yKnn" ,"Numero de casos de denuncia de violencia sexual", min = 50 , max = 500, value = 1 ),sliderInput("kKnn" ,"valor de k", min = 1 , max = 10, value = 1 ) , tableOutput("tablaKNN"))
                      ) )
                      )
            
            
             
    )#Fin de fluid page
  

  

options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {
  getwd()
  
  df <- read.csv('DatosPreprocesadosFinal.csv') #Data set ya preprocesada
  
  
  
  #nos conectamos a db para recupearar una tabla
  output$TablaPruebas <- renderTable({
    
    return (dbReadTable(conexion, "Registros_Violencia"))
    
  })
  
  
  if (!(dbExistsTable(conexion, "Registros_Violencia"))){
    
    dbWriteTable(conexion, name = "Registros_Violencia" , value = df , append = TRUE)
    
  }
  
  
  #output$TablePruebas <- renderTable({
    
    
   # return (df)
    
  #})
  
  
  
  
  
  

  output$tablaS2 <- renderTable({
    tablaS1 <- input$file1
    if (is.null(tablaS1))
      {return(NULL)}
    box<- input$select
    if(box == "1")    {
      read.csv(tablaS1$datapath)
    } else{  if(box == "2") {
      read.csv(tablaS1$datapath)
    } else { if(box == "3") {
      read_xlsx(tablaS1$datapath)
    } 
     } }
  })
  
  output$consulta1 <- renderText({'
  
  ##Obtenemos el dataframe sin procesar

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
  
  
  
   ' })
  
  
  output$DatosDB <- renderText({
    
    '
    driver = MySQL()
    host = "3.235.246.109"
    port = 3306
    user = "administrador"
    password = "adminfo"
    dbname = "Tf_DB"
    
    '
    
  })
  
  output$CodeDB <- renderText({
    
    '
    library(RMySQL)
    library(DBI)


    source("Codigo/Back Up DB/Datos.R")

    ##Nos conectamos a la base de datos
    if(dbCanConnect(drv=driver,port=port,user=user,host=host,
                password=password,dbname=dbname)){
  
  
    conexion <- dbConnect(drv=driver,port = port,user=user,host=host,
                        password = password,dbname = dbname)
      }



    #Ahora el objetivo es poder pasar el data frame utilizado a la base de datos conectada


    if (!(dbExistsTable(conexion, "Registros_Violencia"))){
  
      dbWriteTable(conexion, name = "Registros_Violencia" , value = df , append = TRUE)
  
      }

    ##SERIALIZACION DE LAS TABLAS (RECUPERACION DE DATOS)


    tablas <- dbListTables(conexion) #dEVUELVE EL NOMBRE DE LAS TABLAS DE LA BASE DE DATOS CONECTADA
    lista <- list()
    for (i in tablas){
  
      lista[[i]] <- dbReadTable(conexion, i) #OBTENCION DE LA TABLA COMO TIPO DE DATO data.frame
  
      }

    lista
  
  
    #El df que utilizaremos es el de "Registro_Violencia"

    newdf <- lista[[4]]
    
       '
  })
  
  
  
  ConsultasDplyr <- 
    c(
      "1.Cuantos registros de numero de casos de violencia se dieron en Lima Metropolitana\nq1 <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA') %>% summarise(Total = n())",
      "2.Cuantos casos de denuncia hacia los hombres existieron durante el 2013 y 2020\nq2 <- df %>% summarise(Total_Casos = sum(N..CASOS.ATENDIDOS...HOMBRES...TOTAL))",
      "3.Listado de año,  provincias y numero de casos de violencia hacia mujeres que se dieron en el departamento de Arequipa\nq3 <- df %>% filter(DEPARTAMENTO == 'AREQUIPA') %>% select(AÑO, PROVINCIA, N..CASOS.ATENDIDOS...MUJERES...TOTAL)",
      "4.Numero de casos de violencia fisica en el distrito de Carabayllo\nq4 <- df %>% filter(DISTRITO == 'CARABAYLLO') %>% summarise(Numero_Violencia_fisica= sum(N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)) ",
      "5.Listado de departamentos y año cuyo numero de casos de violencias psicologicas son mayor a 100\nq5 <- df %>% select(AÑO , DEPARTAMENTO , N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA) %>% filter( N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA > 100)",
      "6.Listado de los 10 primeros registros con el mayor numero de casos de violencia contra la mujer\nq6 <- df %>% select(AÑO,FECHA.ENVIO,DEPARTAMENTO,PROVINCIA,DISTRITO,N..CASOS.ATENDIDOS...MUJERES...TOTAL) %>% arrange(desc(N..CASOS.ATENDIDOS...MUJERES...TOTAL)) %>% top_n(10)",
      "7.Listado de los 5 primeros registros con el mayor numero de casos de violencia fisica en el distrito de Miraflores\nq7 <- df %>% filter(DISTRITO == 'MIRAFLORES'') %>%select(AÑO , FECHA.ENVIO , N..CASOS.ATENDIDOS...VIOLENCIA.FISICA) %>%arrange(desc(N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)) %>% top_n(5)",
      "8.Listado de cada departamento con el numero de casos de violencia sexual\nq8 <- df %>% select(DEPARTAMENTO , N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL) %>% group_by(DEPARTAMENTO) %>% summarise(Total_Casos_ViolenciaSexual = sum(N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL))",
      "9.Listado de cada provincia del departamento de Cusco con el numero de casos de violencia contra la mujer\nq9 <- df %>% filter(DEPARTAMENTO == 'CUSCO') %>% select(PROVINCIA , N..CASOS.ATENDIDOS...MUJERES...TOTAL) %>% group_by(PROVINCIA) %>% summarise(Total_Casos_violenciaMujer = sum(N..CASOS.ATENDIDOS...MUJERES...TOTAL))",
      "10.Del total del numero de casos de violencia contra la mujer en el departamento de Lima Metropolitana, cual es el promedio , la mediana , el minimo , el maximo , percentiles y cuartiles\nprom <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA') %>% na.omit() %>%summarise(promedio = mean(N..CASOS.ATENDIDOS...MUJERES...TOTAL))\nmediana <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA') %>% na.omit() %>%summarise(mediana = median(N..CASOS.ATENDIDOS...MUJERES...TOTAL))\nminimo <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA') %>% na.omit() %>%summarise(minimo = min(N..CASOS.ATENDIDOS...MUJERES...TOTAL))\nmaximo <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA') %>% na.omit() %>%summarise(maximo = max(N..CASOS.ATENDIDOS...MUJERES...TOTAL))\nquartil <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA') %>% na.omit() %>%summarise(cuartil = quantile(N..CASOS.ATENDIDOS...MUJERES...TOTAL))\nq10 <- prom %>% bind_cols(mediana,minimo,maximo,quartil)                               "
    )
  
  
  
  output$consulta2 <- renderText({
    
    
    if (input$selectdp == "1"){
      
      ConsultasDplyr[1]
    }
    
    else {if (input$selectdp == "2") {
      ConsultasDplyr[2]
    }
      
    else { if (input$selectdp == "3"){
      ConsultasDplyr[3]
    }
      
    else{ if (input$selectdp == "4"){
      ConsultasDplyr[4]
    }
      
    else{ if (input$selectdp == "5"){
      ConsultasDplyr[5]
    }
    
    else{ if (input$selectdp == "6"){
      ConsultasDplyr[6]
    }
    else{ if (input$selectdp == "7"){
      ConsultasDplyr[7]
    }
      
    else{ if (input$selectdp == "8"){
      ConsultasDplyr[8]
    }
    else {if (input$selectdp == "9"){
      ConsultasDplyr[9]
    }
    else{ if (input$selectdp == "10"){
      ConsultasDplyr[10]
    }
    
      
    }}}}}}}}} #Llaves de los elses
    
    
  })
  
  output$tablaS3 <- renderDataTable({
    box<- input$selectdp
    if (is.null(box))
    {return(NULL)}
  
    if(box == "1")    {
      q1 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% summarise(Total = n())
      return (q1)
    } else{  if(box == "2") {
      q2 <- df %>% summarise(Total_Casos = sum(N..CASOS.ATENDIDOS...HOMBRES...TOTAL))
      return (q2)
    } else { if(box == "3") {
      q3 <- df %>% filter(DEPARTAMENTO == "AREQUIPA") %>% select(AÑO, PROVINCIA, N..CASOS.ATENDIDOS...MUJERES...TOTAL)
      return (q3)
    } else { if(box == "4")
    { q4 <- df %>% filter(DISTRITO == "CARABAYLLO") %>% summarise(Numero_Violencia_fisica
                                                                  = sum(N..CASOS.ATENDIDOS...VIOLENCIA.FISICA))
    return (q4)
    } else { if(box=="5")
    { q5 <- df %>% select(AÑO , DEPARTAMENTO , N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA) %>% 
      filter( N..CASOS.ATENDIDOS...VIOLENCIA.PSICOLOGICA > 100)
    return (q5)
    } 
    else{if(box=="6"){
      q6 <- df %>% select(AÑO,FECHA.ENVIO,DEPARTAMENTO,PROVINCIA,DISTRITO,N..CASOS.ATENDIDOS...MUJERES...TOTAL) %>% 
        arrange(desc(N..CASOS.ATENDIDOS...MUJERES...TOTAL)) %>% top_n(10)
      return (q6)
    }
    else{if(box=="7"){
      q7 <- df %>% filter(DISTRITO == "MIRAFLORES") %>%
        select(AÑO , FECHA.ENVIO , N..CASOS.ATENDIDOS...VIOLENCIA.FISICA) %>%
        arrange(desc(N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)) %>% top_n(5)
      
      return (q7)
      
    }
    else{if(box=="8"){
      
      q8 <- df %>% select(DEPARTAMENTO , N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL) %>%
        group_by(DEPARTAMENTO) %>% summarise(Total_Casos_ViolenciaSexual = sum(N..CASOS.ATENDIDOS...VIOLENCIA.SEXUAL))
      
      return (q8)
      
    }
    else{if(box=="9"){
      
      q9 <- df %>% filter(DEPARTAMENTO == "CUSCO") %>% select(PROVINCIA , N..CASOS.ATENDIDOS...MUJERES...TOTAL) %>%
        group_by(PROVINCIA) %>% summarise(Total_Casos_violenciaMujer = sum(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
      return (q9)
    }
    else{if(box=="10"){
      prom <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
        summarise(promedio = mean(N..CASOS.ATENDIDOS...MUJERES...TOTAL)) 
      mediana <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
        summarise(mediana = median(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
      minimo <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
        summarise(minimo = min(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
      maximo <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
        summarise(maximo = max(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
      quartil <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA") %>% na.omit() %>%
        summarise(cuartil = quantile(N..CASOS.ATENDIDOS...MUJERES...TOTAL))
      
      
      q10 <- prom %>% bind_cols(mediana,minimo,maximo,quartil) 
      
      return(q10)
      
    }
      
    }}}}}}}}} #Llaves de los elses 
    
    } #Final render table
  )
  
  ConsultasSQL <- 
    c("1.sql1 <- dbGetQuery(conexion , 'Select DEPARTAMENTO , PROVINCIA , DISTRITO , `N..CASOS.ATENDIDOS...MUJERES...TOTAL` from Registros_Violencia')" ,
      "2.sql2 <- dbGetQuery(conexion , 'SELECT DISTRITO , `N..CASOS.ATENDIDOS...MUJERES...TOTAL` FROM Registros_Violencia WHERE DEPARTAMENTO = 'LIMA METROPOLITANA' AND PROVINCIA = 'LIMA'')",
      "3.sql3 <- dbGetQuery(conexion , 'SELECT DEPARTAMENTO , AVG(`N..CASOS.ATENDIDOS...MUJERES...TOTAL`) FROM Registros_Violencia GROUP BY DEPARTAMENTO')")
  
  
  output$consulta3 <- renderText({
    box<- input$selectsq
    
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      ConsultasSQL[1]
    } else{  if(box == "2") {
      ConsultasSQL[2]
    } else { if(box == "3") {
      ConsultasSQL[3]}
      
    }}
    
  })
  
  output$tablaS4 <- renderDataTable({
    box<- input$selectsq
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      sql1 <- dbGetQuery(conexion , 'Select DEPARTAMENTO , PROVINCIA , DISTRITO , `N..CASOS.ATENDIDOS...MUJERES...TOTAL` from Registros_Violencia')
      return (sql1)
    } else{  if(box == "2") {
      sql2 <- dbGetQuery(conexion , "SELECT DISTRITO , `N..CASOS.ATENDIDOS...MUJERES...TOTAL` FROM Registros_Violencia WHERE DEPARTAMENTO = 'LIMA METROPOLITANA' AND PROVINCIA = 'LIMA'")
      return (sql2)
    } else { if(box == "3") {
      sql3 <- dbGetQuery(conexion , "SELECT DEPARTAMENTO , AVG(`N..CASOS.ATENDIDOS...MUJERES...TOTAL`) FROM Registros_Violencia GROUP BY DEPARTAMENTO")
      
      return (sql3)}  
    
    }}
    
    
    })
  
  ###Graficas para realizar el analisis

  ConsultaGraficas <- 
    c(
      "1.Grafica un histograma donde se puede observar la mayor concentracion de datos por la variable n.casos.atendidos.mujeres\ng1 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL) + geom_histogram(fill = '#12BB00') + theme_dark() ",
      "2.Grafica un histograma donde se puede observar la mayor concentracion de datos por dep para la variable n.casos.mujeres\n g2 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL, fill = DEPARTAMENTO) + geom_histogram(bins = 100) +theme_linedraw() + labs(x = 'Numero de casos atentidos de violencia contra la mujer' , y = 'Numero de registros',title = 'Histograma de registros de violencia contra la mujer por Departamento ') ",
      "3.Grafica el de antes pero en diferentes viewers\ng3 <- g2+facet_wrap(vars(DEPARTAMENTO))",
      "4.Diagrama de cajas para la variable 'numero de casos de violencia atendidos contra la mujer'\ng4 <- ggplot(df) + aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL ) + geom_boxplot(fill = '#27CC85') +labs(y = 'Numero de casos atentidos de violencia contra la mujer', x = '' , title = 'Diagrama de cajas')",
      "5.Diagrama de cajas para la variable 'numero de casos de violencia atendidos contra la mujer' para cada departamento\ng5 <- ggplot(df) + aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) + geom_boxplot() +labs(y = 'Numero de casos atentidos de violencia contra la mujer', x = '' , title = 'Diagrama de cajas') ",
      "6.Diagrama de cajas que muestra la dsitribucion de registros de los departamentos Lima y Arequipa con respecto al numero de casos de denuncia de violencia contra la mujer\ng6 <- df %>% 
      filter((DEPARTAMENTO == 'LIMA METROPOLITANA' |DEPARTAMENTO == 'AREQUIPA') & AÑO >= 2015 & AÑO <= 2019) %>% ggplot() +aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) + geom_boxplot() +
     labs(y = 'Numero de casos atentidos de violencia contra la mujer', x = '' , title = 'Diagrama de cajas') +
     facet_wrap(facets = vars(DEPARTAMENTO)) ",
      "7.Diagrama de densidad que muestra la densidad de los registros para los departamentos de Arequipa , Lima y La Libertad con respecto al numero de casos denunciados contra la mujer\ng7 <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA' |DEPARTAMENTO == 'AREQUIPA' | DEPARTAMENTO == 'LA LIBERTAD') %>%
    ggplot() + 
    aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) +
    geom_density(alpha = 0.5) +
    labs(x = 'Numero de casos atentidos de violencia contra la mujer', x = '' , title = 'Diagrama de densidad')",
      "8.Diagrama de dispercion del numero de casos atendidos de violencia contra la mujer versus el numero de casos atendidos por violencia\ng8 <- ggplot(df , 
              aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                  y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                  color = DEPARTAMENTO)) +
    geom_point() ",
      "9.Diagrama de dispercion del numero de casos atendidos de violencia contra la mujer versus el numero de casos atendidos por violencia en los departamentos de Lima , Arequipa y La Libertad\ng9 <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA' |DEPARTAMENTO == 'AREQUIPA' | DEPARTAMENTO == 'LA LIBERTAD') %>% 
    ggplot( aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                  y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                  color = DEPARTAMENTO)) +
    geom_point() ",
      "10Diagrama de dispercion con lineas de regresion\ng10 <- df %>% filter(DEPARTAMENTO == 'LIMA METROPOLITANA' |DEPARTAMENTO == 'AREQUIPA' | DEPARTAMENTO == 'LA LIBERTAD') %>% 
    ggplot( aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                color = DEPARTAMENTO)) +
    geom_point() + geom_smooth()  ."
      
    )
  
  
  output$consulta4 <- renderText({
    
    if (input$selectgg == "1"){
      
      ConsultaGraficas[1]
    }
    
    else {if (input$selectgg == "2") {
      ConsultaGraficas[2]
    }
      
      else { if (input$selectgg == "3"){
        ConsultaGraficas[3]
      }
        
        else{ if (input$selectgg == "4"){
          ConsultaGraficas[4]
        }
          
          else{ if (input$selectgg == "5"){
            ConsultaGraficas[5]
          }
            
            else{ if (input$selectgg == "6"){
              ConsultaGraficas[6]
            }
              else{ if (input$selectgg == "7"){
                ConsultaGraficas[7]
              }
                
                else{ if (input$selectgg == "8"){
                  ConsultaGraficas[8]
                }
                  else {if (input$selectgg == "9"){
                    ConsultaGraficas[9]
                  }
                    else{ if (input$selectgg == "10"){
                      ConsultaGraficas[10]
                    }
                      
                      
                    }}}}}}}}} #Llaves de los elses
  
    
  })
  
  output$plot1 <- renderPlot({
    box<- input$selectgg
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      g1 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL) + geom_histogram(fill = "#12BB00") +
        theme_dark()
      return (g1)
    } else{  if(box == "2") {
      g2 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL, fill = DEPARTAMENTO) + geom_histogram(bins = 100) +
        theme_linedraw() + labs(x = 'Numero de casos atentidos de violencia contra la mujer' , y = 'Numero de registros',  
                        title = 'Histograma de registros de violencia contra la mujer por Departamento ')
      return (g2)
    } else { if(box == "3") {
      g3 <- ggplot(df) + aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL, fill = DEPARTAMENTO) + geom_histogram(bins = 100) +
        theme_linedraw() + labs(x = 'Numero de casos atentidos de violencia contra la mujer' , y = 'Numero de registros',  
                                title = 'Histograma de registros de violencia contra la mujer por Departamento ')    +facet_wrap(vars(DEPARTAMENTO))
      return (g3)
    } else { if(box == "4")
    { g4 <- ggplot(df) + aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL ) + geom_boxplot(fill = '#27CC85') +
      labs(y = 'Numero de casos atentidos de violencia contra la mujer', x = "" , title = 'Diagrama de cajas')
    return (g4)
    } else { if(box=="5")
    { g5 <- ggplot(df) + aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) + geom_boxplot() +
      labs(y = 'Numero de casos atentidos de violencia contra la mujer', x = '' , title = 'Diagrama de cajas')
    return (g5)
    }
    else{ if(box == "6"){
      g6 <- df %>% filter((DEPARTAMENTO == 'LIMA METROPOLITANA' |DEPARTAMENTO == 'AREQUIPA') & 
                            AÑO >= 2015 & AÑO <= 2019) %>% ggplot() +
        aes(y = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) + 
        geom_boxplot() +
        labs(y = 'Numero de casos atentidos de violencia contra la mujer', x = '' , title = 'Diagrama de cajas') +
        facet_wrap(facets = vars(DEPARTAMENTO))
      
      return (g6)
    }
    else{ if(box == "7"){
      g7 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA" | DEPARTAMENTO == "LA LIBERTAD") %>%
        ggplot() + 
        aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL,fill = DEPARTAMENTO ) +
        geom_density(alpha = 0.5) +
        labs(x = "Numero de casos atentidos de violencia contra la mujer", x = "" , title = "Diagrama de densidad")
      return (g7)
    }
    else{ if(box == "8"){
      g8 <- ggplot(df , 
                   aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                       y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                       color = DEPARTAMENTO)) +
        geom_point()
      return (g8)
    }
    else{ if(box == "9"){
      g9 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA" | DEPARTAMENTO == "LA LIBERTAD") %>% 
        ggplot( aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                    y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                    color = DEPARTAMENTO)) +
        geom_point()
      return (g9)
    }
    else{ if(box == "10"){
      
      g10 <- df %>% filter(DEPARTAMENTO == "LIMA METROPOLITANA" |DEPARTAMENTO == "AREQUIPA" | DEPARTAMENTO == "LA LIBERTAD") %>% 
        ggplot( aes(x = N..CASOS.ATENDIDOS...MUJERES...TOTAL ,
                    y = N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ,
                    color = DEPARTAMENTO)) +
        geom_point() + geom_smooth()
      return (g10)
    }
      
    }}}}}}}}} 
    })
  
  
 
##Para la parte del modeladp  
  
output$consulta6 <- renderText({
"dtModeloR2<-data.frame(dtFinal%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
dtModeloR2.1<-dtModeloR2%>%select(AHumanitarFam,EDanosVivienda)

regresion2 <- lm(AHumanitarFam  ~  EDanosVivienda, data = dtModeloR2)
summary(regresion2)
    
ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle('Gráfica de Regresion') + xlab('Ayuda Humanitaria por Familia') + ylab('Estimacion de Daños por Vivienda') + geom_smooth(method=lm)
    "
  })

  
  
  output$tablaS6 <- renderTable({
    
    if(is.null(input$file1)){return ()}
    input$file1
    
  })
  
 ##MODELAMIENTO DE DATOS 

  
 ####PARA REGRESION LINEAL
  ##CODIGO REGRESION LINEAL
  
  output$CodRL <- renderText({
    '
    
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

  '
  })
  
  
  #Implementacion de Regresion lineal
  
  output$plotRL <- renderPlot({
    
    dtModeloR2<-data.frame(df %>% select(N..CASOS.ATENDIDOS...MUJERES...TOTAL , N..CASOS.ATENDIDOS...VIOLENCIA.FISICA ))
    
    return( 
      
      ggplot(dtModeloR2, aes(x=N..CASOS.ATENDIDOS...MUJERES...TOTAL, y=N..CASOS.ATENDIDOS...VIOLENCIA.FISICA)) + geom_point() + ggtitle("Gráfica de Regresion") + xlab("Numero de casos denunciados por mujeres") + ylab("Numero de casos denunciados por violencia fisica") + geom_smooth(method=lm)
    )
    
  })
  

  
  output$IdPrediccionRL <- renderText({
    
    v1 <- input$xRL
    
    resultado <- Regresion_Lineal(df$N..CASOS.ATENDIDOS...MUJERES...TOTAL ,df$N..CASOS.ATENDIDOS...VIOLENCIA.FISICA,v1,NA )
    
    as.character(resultado[[4]])
    
  })
  
###Para la regresion multivariada
  
  output$CodRM <- renderText({
    '
    
    #2. Regresion multivariada


#Experimentacion . Realizaremos el entrenamiento y la prueba

ids <- sample(1:NROW(df) , NROW(df)*0.85)
#mi data frame de entrenamiento

ini <- grep("N..CASOS.ATENDIDOS...MUJERES...TOTAL" , colnames(df))
fin <- ini + 3

entrenamiento <- df[ids, c(ini:fin)]
#mi data frame de prueba
prueba <- df[-ids,c(ini:fin)]

colnames(entrenamiento) <- c("N.CasosMujeres" , "N.CasosPsico" , "N.CasosFisico", "N.CasosSexual")
colnames(prueba) <- c("N.CasosMujeres" , "N.CasosPsico" , "N.CasosFisico", "N.CasosSexual")

#Una vez tenemos los datasets de entrenamiento y prueba , procederemos a utilizar el de entrenamiento para el aprendizaje

f = lm(N.CasosMujeres~N.CasosPsico+N.CasosFisico+N.CasosSexual, data = entrenamiento)

#Una vez se realize el entrenamiento , utilizamos la funcion para hacer las pruebas

prueba$Prediccion <- predict(f,prueba)

#Ahora procedemos a encontrar un porcentaje de acierto dado el modelo

perError <- abs(100*((prueba$Prediccion - prueba$N.CasosMujeres)/prueba$Prediccion))

PerAciertorModelo <- 100 - mean(perError) # Este resultado es el porcentaje de acierto del modelo
    
    
    '
    
  })

  
  

###Para KNN
  
  ###Codigo KNN
  

  

  output$tablaKNN <- renderTable({
    
    xknn <- input$xKnn
    yknn <- input$yKnn
    kknn <- input$kKnn
    
    return (Modelo_KNN(df , 150, 320 , 6))
    
    
  })
  
    
    output$tablaS5 <- renderTable({
      Pro<- read.csv(file = 'datasetFinalPre.csv')
      if (is.null(Pro)) 
        return(NULL)
      else head(Pro)
  })
  

  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
