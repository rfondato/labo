#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


ksemilla  <- 240007  #poner aqui la PRIMERA de sus cinco semillas

setwd("c:\\data_mining\\")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


# Paso la clase a binaria que tome valores {0,1} enteros
# Estrategia: Tratar a BAJA+1 y BAJA+2 como positivos (1) porque son parecidos
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
#estos hiperparametros  salieron de una Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=              31,
                                   learning_rate=         0.02067308,
                                   num_iterations=      524,
                                   num_leaves=          702,
                                   min_data_in_leaf=   1047,
                                   feature_fraction=      0.2749327,
                                   max_depth= 8,
                                   neg_bagging_fraction = 0.6624233,
                                   pos_bagging_fraction = 0.8580996,
                                   bagging_freq = 21,
                                   scale_pos_weight = 1.2497213,
                                   seed=               ksemilla   #aqui se utiliza SU primer semilla
                                  )
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente ) ]
tb_entrega[  , prob := prediccion ]

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )

#creo las carpetas para resultados
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/lightGBM hyper/", showWarnings = FALSE )
setwd( "./labo/exp/lightGBM hyper/" )

#genero archivos con los  "envios" mejores
for( envios  in  c( 10000, 10500, 11000, 11500, 12000, 12500, 13000, 13500 ) )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0( "lightGBM_hyper_", envios, ".csv" ),
          sep= "," )
}

#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "lightGBM hyper importancia.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

