introduccion<-{
  fluidRow(
    column(12,align="center",h3("Universidad Peruana de Ciencias Aplicadas"),
           tags$b(h4("Trabajo final de Administracion de la Informacion")),
            tags$b(h4("Alumno : Ramiro Chavez Caituiro")),
           tags$b(h4("Codigo : U201524658"))
           
           
           
           ),
    
    div(
      column(12,align= "center",tags$img(src="imagen.jpg",width="50%")) ,
      column(12,align= "center",br(),h4("Descripcion del Proyecto"))
    ),
    
    div(
      
      p("El proyecto consiste en desarrollar el ciclo de vida de ciencia de datos . El tema elegido fue casos de violencia
               en el Perú. Para ello se elegió un data set en la pagina nacional de datos abiertos y se escogio un data frame que 
               contiene informacion acerca del numero de casos denunciados por violencia de diferentes tipos en todos los departamentos
               del Peru. Con esta informacion , se podria determinar diferentes indices de violencia y de esa manera tomar deciciones
               al respecto. El proyecto abarca la recolección de datos , el preprocesamiento de datos a datos estructurados , la exploracion,
               el analisis y el modelado de datos . Aparte tambien como parte del trabajo se realizara una conexion a base de datos desde
               R para almacenar la informacion obtenida")  
      
    ),
    
    column(12,align= "center",br(),h4("2020 - 2"))
    
    
    
  )}
