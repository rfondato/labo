rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:\\data_mining\\")  #Establezco el Working Directory

#cargo la salida del Grid Seach, verifique que corresponda a la carpeta donde dejó el resultado
dtrain  <- fread("./labo/exp/HT2020/gridsearch.txt")

ordered = dtrain[order(-ganancia_promedio), ]


dataset  <- fread("./datasets/paquete_premium_202011.csv")

dataset[,.N]

dataset[clase_ternaria=="CONTINUA", .N]
dataset[clase_ternaria=="BAJA+1", .N]
dataset[clase_ternaria=="BAJA+2", .N]

dataset[clase_ternaria=="BAJA+1", .N] / (dataset[clase_ternaria=="CONTINUA", .N] + dataset[clase_ternaria=="BAJA+1", .N])
dataset[clase_ternaria=="BAJA+2", .N] / dataset[clase_ternaria=="CONTINUA", .N]

(dataset[clase_ternaria=="CONTINUA", .N] + dataset[clase_ternaria=="BAJA+1", .N]) / dataset[clase_ternaria=="BAJA+2", .N]
244/60
