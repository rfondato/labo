#Ensemble de arboles de decision

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\data_mining\\")  #Establezco el Working Directory

#cargo los datos donde entreno y convierto en target binario
dtrain  <- fread("./datasets/paquete_premium_202011.csv")
dtrain[, clase_binaria := ifelse( clase_ternaria=="BAJA+2", 1, 0)][, clase_binaria := factor(clase_binaria)]
dtrain = dtrain[, !"clase_ternaria"]

#cargo los datos donde aplico el modelo
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#Establezco cuales son los campos que puedo usar para la prediccion
campos_buenos  <- setdiff(  colnames(dtrain) ,  c("clase_binaria") )

param_buenos  <- list( "cp"=         -1,
                       "minsplit"=  1600,
                       "minbucket"= 400,
                       "maxdepth"=    8 )

num_trees         <-  1000    #voy a generar 20 arboles, a mas arboles mas tiempo de proceso y MEJOR MODELO
feature_fraction  <-   0.3  #entreno cada arbol con solo 50% de las variables variables

# Matriz de loss
peso_error = 250
matriz_perdida  <- matrix(c( 0, 1, peso_error,0), nrow = 2, byrow = TRUE)

set.seed(240007) #Establezco la semilla aleatoria, cambiar por SU primer semilla

#inicializo en CERO el vector de las probabilidades en dapply
#Aqui es donde voy acumulando, sumando, las probabilidades
probabilidad_ensemble  <- rep( 0, nrow(dapply) )

tb_ensembles  <-  copy( dapply[ , list( numero_de_cliente ) ] )

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/", showWarnings = FALSE  )
dir.create( "./labo/exp/KA2101/", showWarnings = FALSE )

#aqui es donde voy a graficar los arboles
pdf( "./labo/exp/KA2101/arbolitos.pdf", paper="a4r" )

#Genero los arboles y voy sumando la probabilidad que el arbol entrenado en 202011 asigna a cada registro de 202101

for(  i in  1:num_trees ) #genero  num_trees arboles
{
  qty_campos_a_utilizar  <- as.integer( length(campos_buenos)* feature_fraction )
  campos_random  <- sample( campos_buenos, qty_campos_a_utilizar )
  
  #paso de un vector a un string con los elementos separados por un signo de "+"
  #este hace falta para la formula
  campos_random  <- paste( campos_random, collapse=" + ")

  #armo la formula para rpart
  formulita  <- paste0( "clase_binaria ~ ", campos_random )

  #genero el arbol de decision
  modelo  <- rpart( formulita,
                    data= dtrain,
                    xval= 0,
                    parms = list(loss = matriz_perdida),
                    control= param_buenos )

  #grafico el modelo
  prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

  #aplico el modelo a los datos que no tienen clase
  prediccion  <- predict( modelo, dapply , type = "prob")
  
  tb_ensembles[  ,  paste0( "arbol", i) :=  prediccion[ , "1"] ]

  #voy acumulando la probabilidad
  probabilidad_ensemble  <- probabilidad_ensemble +  prediccion[, "1"]
}

dev.off()  #dejo de imprimir

#fue sumando las probabilidades, ahora hago el cociente por la cantidad de arboles
#o sea, calculo el promedio
probabilidad_ensemble  <- probabilidad_ensemble / num_trees

#asigngo el promedio y grabo
tb_ensembles[  , prob_promedio := probabilidad_ensemble ]
fwrite( tb_ensembles,
        file="./labo/exp/KA2101/ensemble.csv",
        sep="\t" )

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(probabilidad_ensemble > 1/60) ) ) #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/", showWarnings = FALSE  )
dir.create( "./labo/exp/KA2101/", showWarnings = FALSE )

#grabo el archivo para Kaggle
fwrite( entrega, 
        file= "./labo/exp/KA2101/K311_001.csv", 
        sep= "," )

