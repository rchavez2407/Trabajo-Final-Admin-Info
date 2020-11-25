library(RMySQL)
library(DBI)


source("Back up DB/Datos.R")

##Nos conectamos a la base de datos
if(dbCanConnect(drv=driver,port=port,user=user,host=host,
                password=password,dbname=dbname)){
  
  
  conexion <- dbConnect(drv=driver,port = port,user=user,host=host,
                        password = password,dbname = dbname)
}



if (!(dbExistsTable(conexion, "Registros_Violencia"))){
  
  dbWriteTable(conexion, name = "Registros_Violencia" , value = df , append = TRUE)
  
}
 
  
  
  
