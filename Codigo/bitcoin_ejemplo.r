
#Adquisicion de datos a partir de un dataset
archivo <- read.csv("/home/valeria/Escritorio/BitcoinDataset.csv")
archivo

archivo2 <- read.csv("/home/valeria/Escritorio/EthereumDataset.csv")
archivo2

#Conocimeinto de la estructura del archivo csv
str(archivo)

str(archivo2)
# names(archivo)<- c("Fecha","Precio_promedio", "Bitcoins_minado","Precio_circulacion","Precio_compraventa", "Size_bloque_headersTransa",
# "Promedio_sizeMB", "Blocks_Mined", "Promedio_transacciones_bloque", "Time_transaccion","Terapersecond_Bitcoin","Dificultad_nuevobloque",
# "Pagos_mineros","Valortotal_mineros","Ingresos_mineros","IngresosMineros_entretransacciones"," Direcciones_BitcoinBlockc",
# "Transacciones_diarias","Total_trasacciones","Exlusion_popular","Transacciones_largas","Total_transacciones_dia","Transacciones_blockchain",
# "Estimacion_Transacciones_Dolar")

head(archivo)
}head(archivo2)
#--------------------------
#Se desecha informacion si llega a contener un campo "NA"
archivo<-na.omit(archivo)
archivo<-na.omit(archivo2)
#conocer si existen relaciones lineales entre datos
pairs(archivo)
pairs(archivo2)

#Se grafica la correlation que tienen los datos entre ellos
library(corrplot)
correlacion <- cor(archivo[,2:10])
corrplot(correlacion)
getwd()

head(archivo)
ncol(archivo)

#Para que exista una estrecha relacion entre datos su correlacion debe tener un valor cercano a 1
##-----------------------------------

##Grafica que muestra el costo a traves del tiempo con el 
plot(archivo$Date,archivo$btc_market_price, main="Precio a través del tiempo", ylab="Precio", xlab="Fecha")
## Ploteo de precio en el mercado contra 
mod<-lm(btc_market_price~btc_market_cap, data=archivo)
mod1<-lm(btc_market_price~btc_estimated_transaction_volume_usd, data=archivo)
summary(mod)
#-------------Entrenamiento 

set.seed(1)
train.index<-sample(1:nrow(archivo),0.70*nrow(archivo), replace=FALSE)
train <- archivo[train.index, ]
test  <- archivo[-train.index,]





