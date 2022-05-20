#esqueleto de grid search
#se espera que los alumnos completen lo que falta para recorrer TODOS cuatro los hiperparametros 

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# --- Paquetes para ejecución paralela en windows ---#
# install.packages("doParallel")
library(doParallel)
# --------------------------------------------------#

require("data.table")
require("rpart")
require("parallel")
require("foreach")

ksemillas  <- c(240007, 625643, 825329, 910421, 345689) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N], by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos, peso_error )
{
  dataset = as.data.table(dataset)
  gc()
  
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  matriz_perdida  <- matrix(c( 0,peso_error,1,   1,0,1,   1,peso_error,0), nrow = 3)
  
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   parms = list(loss = matriz_perdida),
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}

#------------------------------------------------------------------------------

CorrerSemillasEnParalelo <- function(cluster, semillas, max_depth, min_split, min_bucket, cp, peso_error)
{
  param_basicos  <- list( "cp"=         cp,         #complejidad minima
                          "minsplit"=   min_split,  #minima cantidad de registros en un nodo para hacer el split
                          "minbucket"=  min_bucket, #minima cantidad de registros en una hoja
                          "maxdepth"=   max_depth ) #profundidad máxima del arbol
  
  ganancias <- clusterMap(cluster, ArbolEstimarGanancia, semillas, MoreArgs=list(param_basicos, peso_error))
  
  # Promedio de ganancias entre todas las corridas
  return (mean(unlist(ganancias)))
}

#------------------------------------------------------------------------------

EscribirEnArchivo <- function(archivo_salida, max_depth, min_split, min_bucket, cp, peso_error, ganancia_promedio) {
   cat(  file=archivo_salida,
         append= TRUE,
         sep= "",
         max_depth, "\t",
         min_split, "\t",
         min_bucket, "\t",
         cp, "\t",
         peso_error, "\t",
         ganancia_promedio, "\n"  )
}

#------------------------------------------------------------------------------

ArbolesMCParalelo  <- function( semillas, params )
{
  archivo_salida  <- "./labo/exp/HT2020/gridsearch.txt"
  
  # Escribo los títulos al archivo donde van a quedar los resultados
  cat( file=archivo_salida,
       sep= "",
       "max_depth", "\t",
       "min_split", "\t",
       "min_bucket", "\t",
       "cp", "\t",
       "peso_error", "\t",
       "ganancia_promedio", "\n")
  
  # Registro cores por cantidad de semillas a ejecutar en paralelo
  cl <- makeCluster(length(semillas))  
  registerDoParallel(cl)  

  # Exportamos las funciones necesarias y el dataset a los clusters para que tengan acceso a ellos
  clusterExport(cl,list('ArbolEstimarGanancia', 'particionar', 'dataset'))
  
  # Cargamos librerías necesarias en clusters
  clusterEvalQ(cl, { 
    library("data.table") 
    library("rpart")
  })
  
  # Establezco una columna con un índice único
  params[,Idx:=.I]
  
  # Usando esa columna agrupo por la misma (es decir creo grupos ficticios de 1 row cada uno)
  # y por cada grupo ejecuto el árbol de decisión para todas las semillas (en paralelo).
  # Este truco me permite hacer una ejecución por row.
  # Luego asigno el promedio de todas las semillas en una nueva columna "ganancia_promedio".
  # Como resultado, el data.table params contendrá para cada combinación de params su ganancia.
  params[,by=Idx, ganancia_promedio := CorrerSemillasEnParalelo(cl, semillas, max_depth, min_split, min_bucket, cp, peso_error)]
  
  # Usando el mismo truco ejecuto "EscribirEnArchivo" por cada row,
  # para grabar en el archivo de salida el resultado de cada combinación de parámetros
  params[,by=Idx, EscribirEnArchivo(archivo_salida, max_depth, min_split, min_bucket, cp, peso_error, ganancia_promedio)]
  
  # Limpio la columna dummy Idx
  params[,Idx:=NULL]

  # Paro el cluster para liberar recursos    
  stopCluster(cl)
}

#------------------------------------------------------------------------------

setwd("C:\\data_mining\\")   #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasets/paquete_premium_202011.csv")


# Creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/HT2020/", showWarnings = FALSE )

# Creo los valores posibles a explorar por cada parámetro
max_depth = c (10)
min_split = c (1600)
min_bucket = c (400)
cp = c(-1)
peso_error = c(0.8, 0.9, 0.95, 1)

# Creo un data.table con un "cross-join", es decir todas las combinaciones de los parámetros
params <- CJ(max_depth=max_depth,min_split=min_split,min_bucket=min_bucket,cp=cp,peso_error=peso_error)

# Filtrar params que no funcionan según relación de min_split y min_bucket
params <- params[min_split >= 2 * min_bucket,]

# Función que procesa el árbol de decisión para todas las combinaciones de parámetros y semillas,
# y graba los resultados en un archivo de salida.
ArbolesMCParalelo(ksemillas, params)
