
#Adquisicion de datos a partir de un dataset
archivo <- read.csv("/home/valeria/Escritorio/BitcoinDataset.csv")
archivo
#Conocimeinto de la estructura del archivo csv
str(archivo)
ncol(archivo)
names(archivo)<- c("Fecha","Precio_promedio", "Bitcoins_minado","Precio_circulacion","Precio_compraventa", "Size_bloque_headersTransa",
"Promedio_sizeMB", "Blocks_Mined", "Promedio_transacciones_bloque", "Time_transaccion","Terapersecond_Bitcoin","Dificultad_nuevobloque",
"Pagos_mineros","Valortotal_mineros","Ingresos_mineros","IngresosMineros_entretransacciones"," Direcciones_BitcoinBlockc",
"Transacciones_diarias","Total_trasacciones","Exlusion_popular","Transacciones_largas","Total_transacciones_dia","Transacciones_blockchain",
"Estimacion_Transacciones_Dolar")

head(archivo)
#---------------------------------





































































#--------------------------
#Se desecha informacion incompleta
archivo<-na.omit(archivo)
plot(archivo$Date,archivo$btc_market_price)
#conocer si existen relaciones lineales entre datos
pairs(archivo)

#Se grafica la correlation que tienen los datos entre ellos
library(corrplot)
  correlacion <- cor(archivo[,2:22])
  corrplot(correlacion)
  getwd()
  
  head(archivo)
  
#Para que exista una estrecha relacion entre datos su correlacion debe tener un valor cercano a 1
##-----------------------------------
  
##Grafica que muestra el costo a traves del tiempo 
  
  
  
  
  
  