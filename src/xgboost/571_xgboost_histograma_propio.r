# XGBoost  sabor HISTOGRAMA

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\data_mining\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( gamma=                0.0,  #por ahora, lo dejo fijo, equivalente a  min_gain_to_split
                                   alpha=                0.0,  #por ahora, lo dejo fijo, equivalente a  lambda_l1
                                   lambda=               0.0,  #por ahora, lo dejo fijo, equivalente a  lambda_l2
                                   subsample=            1.0,  #por ahora, lo dejo fijo
                                   tree_method=       "hist",  # histograma
                                   grow_policy=  "lossguide",  # lossguide
                                   max_depth=            0,    # NO lo limito por altura
                                   max_bin=            256,    #por ahora fijo
                                   scale_pos_weight=     1.0,   #por ahora, lo dejo fijo
                                   max_leaves=          906,
                                   min_child_weight=    10,
                                   eta=                 0.01000428,
                                   colsample_bytree=    0.7379721
                                   ),
                      nrounds= 244
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "prob"= prediccion ) )

#ordeno por probabilidad descendente
setorder( entrega, -prob )

dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5710/", showWarnings = FALSE )

setwd( "./labo/exp/KA5710/" )

#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
for( envios  in  c( 10000, 10500, 11000, 11500, 12000, 12500, 13000, 13500 ) )
{
  entrega[  , Predicted := 0L ]
  entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0( "KA_571_", envios, ".csv" ),
          sep= "," )
}

