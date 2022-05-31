#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

dataset_entrada <- "paquete_premium_ext_001.csv.gz"
dataset_salida  <- "paquete_premium_ext_001_trunc.csv.gz"
campos_fijos  <- c( "numero_de_cliente", "clase_ternaria", "foto_mes", "mes" )

ReportarCampos  <- function( dataset )
{
  cat( deparse(sys.calls()[[sys.nframe()-1]]),  #el nombre de la funcion desde donde se llamo
       "La cantidad de campos es ", 
       ncol(dataset) ,
       "\n" )
}

#------------------------------------------------------------------------------
fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")
  
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 60000, -1000 ) ) )
  
  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta
  
  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500
  
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------

Truncar_Hasta  <- function( n_features = 500 )
{
  gc()
  
  cat( "La cantidad de campos inicial es ", ncol(dataset) , "\n" )
  
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
  
  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )
  
  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]
  
  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
  )
  
  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
  )
  
  
  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 240007,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
                 # num_threads= 8,
                 early_stopping_rounds= 200 )
  
  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )
  
  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]
  
  fwrite( tb_importancia, 
          file= paste0( "truncator_importancia.txt"),
          sep= "\t" )
  
  col_utiles  <- tb_importancia[ pos <= n_features,  Feature ]
  col_utiles  <-  unique( c( col_utiles,  campos_fijos ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )
  
  dataset[  ,  (col_inutiles) := NULL ]
  
  cat( "La cantidad de campos final es ", ncol(dataset) , "\n" )
  cat( "Se truncaron las ", length(col_inutiles) , " columnas menos importantes \n" )
  cat( "La ganancia de la columna más importante truncada es: ", tb_importancia[ pos == (n_features + 1),  Gain ], "\n" )
}

#------------------------------------------------------------------------------

setwd( "~/buckets/b1/datasets/" )

dataset <- fread( dataset_entrada )

Truncar_Hasta(500)

#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )

#Grabo el dataset
fwrite( dataset,
        dataset_salida,
        logical01= TRUE,
        sep= "," )
