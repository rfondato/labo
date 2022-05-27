rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:\\data_mining\\")  #Establezco el Working Directory

#cargo la salida del Grid Seach, verifique que corresponda a la carpeta donde dejó el resultado
dtrain  <- fread("./exp/lightGBM_FE_4/lightGBM_FE_4.txt")

plot(dtrain[, ganancia])

ordered = dtrain[order(-ganancia)]


dataset  <- fread("./labo/exp/FE4020/paquete_premium_202011_ext.csv")

dataset[, mv_status01]

importance  <- fread("./labo/exp/lightGBM_FE_4/lightGBM_FE_4_importancia.txt")

importance = importance[, gain_freq := Gain / Frequency]

cbaja1 = dataset[clase_ternaria == "BAJA+1", .N]
cbaja2 = dataset[clase_ternaria == "BAJA+2", .N]

coef = (59000 * cbaja2 - 1000 * cbaja1) / (cbaja1 + cbaja2)

#e1  <- fread("./labo/exp/ensamble1.csv")
#e2  <- fread("./labo/exp/ensamble2.csv")
#e3  <- fread("./labo/exp/ensamble3.csv")

#final = e1
#final = final[e2, on=.(numero_de_cliente)]
#final = final[e3, on=.(numero_de_cliente)]

#final[,final_pred := ifelse((Predicted + i.Predicted + i.Predicted.1) > 1, 1, 0)]

#genero un dataset con las dos columnas que me interesan
#entrega  <- final[   , list(numero_de_cliente, Predicted = final_pred) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
#dir.create( "./labo/exp/", showWarnings = FALSE  )
#dir.create( "./labo/exp/KA2022/", showWarnings = FALSE  )

#fwrite( entrega, 
       # file= "./labo/exp/ensamble_final.csv", 
      #  sep= "," )
