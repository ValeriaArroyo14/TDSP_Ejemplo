
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
head(archivo2)
#--------------------------
#Se desecha informacion si llega a contener un campo "NA"
archivo<-na.omit(archivo)
str(archivo)
archivo2<-na.omit(archivo2)
#conocer si existen relaciones lineales entre datos
#Como se visualizan muchas graficas es necesario agrandar el panel donde se visualizan los plots(casi al maximo)
#Para poder visualizar los resultados
pairs(archivo)
pairs(archivo2)

#Se grafica la correlation que tienen los datos entre ellos
#Como se visualizan muchas graficas es necesario agrandar el panel donde se visualizan los plots(casi al maximo)
#Para poder visualizar los resultados
library(corrplot)
correlacion <- cor(archivo[,2:24])
corrplot(correlacion)
getwd()

head(archivo)
ncol(archivo)

#Para que exista una estrecha relacion entre datos su correlacion debe tener un valor cercano a 1
##-----------------------------------

##Grafica que muestra el precio del bitcoin a traves del tiempo 
plot(archivo$Date,archivo$btc_market_price, main="Precio a través del tiempo", ylab="Precio", xlab="Fecha")
## Modelo de regresión lineal entre Precio promedio en dólares en las principales bolsas de bitcoins
# y El valor total en dólares del suministro de bitcoin en circulación.
mod<-lm(btc_market_price~btc_market_cap, data=archivo)
summary(mod)
#la intercepción es 3.78, lo que significa que cuando el valor total en USD del suministro de Bitcoin en circulación es 0, se predice que el precio promedio del mercado en USD a través de las principales bolsas de bitcoin será $3.78 
#pendiente es 5.95, nos dice que predecimos que el precio de mercado promedio en USD en las principales bolsas de bitcoin aumentará en 5.95 $ por cada aumento adicional de dólares en el valor total en USD del suministro de bitcoins en circulación

mod1<-lm(btc_market_price~btc_estimated_transaction_volume_usd, data=archivo)
summary(mod1)

#intercept es 7, lo que significa que cuando el valor estimado de transacción en valor USD es 0, se predice que el precio promedio del mercado USD en las principales bolsas de bitcoins será 7 $.
# pendiente es 3.9, nos dice que predecimos que el precio promedio del mercado en USD en las principales bolsas de bitcoins aumentará en 3.9 $ por cada aumento adicional en el valor estimado de transacción.

#-------------Entrenamiento 

set.seed(1)
train.index<-sample(1:nrow(archivo),0.70*nrow(archivo), replace=FALSE)
train <- archivo[train.index, ]
test  <- archivo[-train.index,]


#Entrenamiento del dataset y realizar la prediccion del precio, respecto al valor total en USD del volumen de
#negociacion en las principales bolsas de bitcoins.
model1 <- lm(btc_market_price~btc_trade_volume , train)
summary(model1)
p1 <- predict(model1,test)
p1
#head(p1)
error1 <- p1 - test[["btc_market_price"]]
error1
sqrt(mean(error1^2))
plot(p1, main="Prediccion 1")
#RMSE=977.2906


#Modelo para la relacion del precio con el valor total en dólares del suministro de bitcoin en circulación.

model2 <- lm(btc_market_price~btc_market_cap, train)
summary(model2)
p2 <- predict(model2,test)
head(p2)
error2 <- p2 - test[["btc_market_price"]]
sqrt(mean(error2^2))
plot(p2, main= "Prediccion 2")
#RMSE=51.17771

#-------------------

# Principal Componente de Analisis en el mercado de Bitcoin 
library(FactoMineR)
pc1<-PCA(archivo[,2:24],scale.unit = TRUE, ncp = 23, graph = TRUE)
summary(pc1)
a<-dimdesc(pc1,axes = c(1:2))
a$Dim.1
a$Dim.2


pc<-prcomp(BTC[,c("btc_market_price","btc_miners_revenue")], center = T, scale=T)
summary(pc)
head(pc$x)
#we can see that the first principal component explains about 61.6% of the total variation, and the second principal component an additional 10.98%. 
#btc_n_transactions_total and btc_blocks_size  are the most contributing variables


model3<-lm(btc_market_price~btc_n_transactions_total, data=BTC)
summary(model3)
p3 <- predict(model3,test)
error3 <- p3 - test[["btc_market_price"]]
sqrt(mean(error3^2))
#RMSE=616.3098
#intercept is -1.16, which means that when total number of transactions is 0, the average USD market price across major bitcoin exchanges is predicted to be -1.
# slope is  9.8,  it tells us that we predict the average USD market price across major bitcoin exchanges to increase by 9.8$ for every additional dollar increase in total number of transactions.


model4<-lm(btc_market_price~btc_n_transactions_total+btc_avg_block_size+btc_difficulty+btc_output_volume, data=BTC)
summary(model4)
#Multiple R-squared:  0.9112,	Adjusted R-squared:  0.9111 , all independent variables affect the dependent one.
p4 <- predict(model4,test)
error4 <- p4 - test[["btc_market_price"]]
sqrt(mean(error4^2))
#RMSE=295.8589








