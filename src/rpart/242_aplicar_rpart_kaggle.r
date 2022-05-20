rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui debe cambiar los parametros por los que desea probar

param_basicos  <- list( "cp"=          -1,  #complejidad minima
                        "minsplit"=   900,     #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  300,     #minima cantidad de registros en una hoja
                        "maxdepth"=     11 )    #profundidad máxima del arbol
peso_error = 0.8
proba = 1/60

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:\\data_mining\\")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasets/paquete_premium_202011.csv")
#dtrain[, clase_binaria := ifelse( clase_ternaria=="BAJA+2", 1, 0)][, clase_binaria := factor(clase_binaria)]

matriz_perdida  <- matrix(c( 0,peso_error,1,   1,0,1,   1,peso_error,0), nrow = 3)

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase binaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 parms = list(loss = matriz_perdida),
                 control=  param_basicos )

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply, type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > proba) ]

dapply[, prob_baja2]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/", showWarnings = FALSE  )
dir.create( "./labo/exp/KA2022/", showWarnings = FALSE  )

fwrite( entrega, 
        file= "./labo/exp/KA2022/K242_001.csv", 
        sep= "," )
