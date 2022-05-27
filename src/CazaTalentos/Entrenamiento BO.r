# Limpio la memoria
rm( list=ls() )
gc()

require("data.table")

# Para optimización bayesiana
require("rlist")
require("DiceKriging")
require("mlrMBO")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<- sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}

#------------------------------------------------------------------------------

jugar_ronda <- function(n_ronda, planilla_cazatalentos, ids_juegan, n_tiros, percentil)
{
  # Limpio planilla anterior
  planilla_cazatalentos[, aciertos := 0]
  planilla_cazatalentos[, tasa_aciertos := 0]
  
  # Hago que tiren
  resultado  <- gimnasio_tirar( ids_juegan, n_tiros )
  
  # Registro en la planilla los aciertos de los que jugaron
  planilla_cazatalentos[ ids_juegan,  aciertos := resultado ]
  
  # Sumo al total de aciertos y al total de tiros lanzados por jugador
  planilla_cazatalentos[ ids_juegan,  total_aciertos := total_aciertos + resultado ]
  planilla_cazatalentos[ ids_juegan,  total_tiros := total_tiros + n_tiros ]
  
  # Recalculo tasa de aciertos general de cada jugador
  planilla_cazatalentos[ ids_juegan,  tasa_aciertos := total_aciertos / total_tiros ]
  
  # Calculo valor de percentil pasado por param sobre la columa tasa_aciertos
  valor_percentil = quantile(planilla_cazatalentos[ids_juegan, tasa_aciertos], probs=c(percentil))
  
  # Devuelvo los ids de los que pasaron de ronda (arriba del percentil)
  return (planilla_cazatalentos[ ids_juegan ][ tasa_aciertos > valor_percentil, id ])
}

#------------------------------------------------------------------------------

Estrategia_Rodrigo  <- function(rondas)
{
  gimnasio_init()
  
  # Inicializo ids y la planilla del cazatalentos
  ids_juegan = 1:100
  planilla_cazatalentos <- data.table( "id"= 1:100 )
  planilla_cazatalentos[, total_aciertos := 0]
  planilla_cazatalentos[, total_tiros := 0]
  
  for (i in 1:length(rondas))
  {
    r = rondas[[i]]
    tiros = r[1]
    percentil = r[2]
    
    # Esto es para evitar jugar rondas eliminadas con tiros = 0 
    # por la optimización bayesiana
    if (tiros == 0) {
      next
    }
    
    # Juego la ronda y actualizo los ids de los que pasaron
    ids_juegan = jugar_ronda(i, planilla_cazatalentos, ids_juegan, tiros, percentil)
  }
  
  # Calculo el mejor después de todas las rondas (el de mayor tasa de aciertos general)
  pos_mejor <-  planilla_cazatalentos[ , which.max(tasa_aciertos) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}

#------------------------------------------------------------------------------

Montecarlo_Estrategia <- function(rondas, num_experimentos=10000)
{
  tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )
  
  for( experimento  in  1:num_experimentos )
  {
    veredicto  <- Estrategia_Rodrigo(rondas)
    
    tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
  }
  
  tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
  tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
  
  return (c(tiros_total, tasa_eleccion_correcta))
}

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}

#------------------------------------------------------------------------------

Calcular_Tasa_Penalizada <- function(n_tiros, tasa_eleccion_mejor, corte_penalidad, c_penalidad, corte_bonif, c_bonif)
{
  penalidad <- ifelse(n_tiros > corte_penalidad, c_penalidad * (n_tiros - corte_penalidad)/corte_penalidad, 0)
  bonificacion <- ifelse((n_tiros <= corte_penalidad) && ((tasa_eleccion_mejor - corte_bonif) > 0), c_bonif * (tasa_eleccion_mejor - corte_bonif), 0)
  
  return (tasa_eleccion_mejor - penalidad + bonificacion)
}

Estimar_Tasa_Eleccion_Correcta <- function(x)
{
  GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
  
  rondas = list(
    unlist(c(x["tiros_ronda_1"], 0.5)),
    unlist(c(x["tiros_ronda_2"], 0.5)),
    unlist(c(x["tiros_ronda_3"], 0.5)),
    unlist(c(x["tiros_ronda_4"], 0.5)),
    unlist(c(x["tiros_ronda_5"], 0)) # En la ultima ronda no hace falta cortar
  )
  
  resultado = Montecarlo_Estrategia(rondas, 1000)
  
  x$cantidad_tiros  <- resultado[1]
  x$tasa_eleccion  <- resultado[2]
  
  # MUY IMPORTANTE!: Tasa corregida penalizando excesos de tiros 
  x$ganancia <- Calcular_Tasa_Penalizada(resultado[1], resultado[2], 15000, 1, 0.95, 10)
  
  x$iteracion <- GLOBAL_iteracion
  loguear( x,  arch= archivo_log )
  
  return(x$ganancia)
}

Buscar_Mejor_Modelo <- function()
{
  # Defino la  Optimizacion Bayesiana
  kBO_iter  <- 1000   # Cantidad de iteraciones de la Optimizacion Bayesiana
  archivo_BO   <- "CazaTalentos.RDATA" # Archivo para retomar ejecución
  
  hs  <- makeParamSet(
    makeIntegerParam("tiros_ronda_1" , lower= 5L, upper= 100L),
    makeIntegerParam("tiros_ronda_2" , lower= 0L, upper= 200L),
    makeIntegerParam("tiros_ronda_3" , lower= 0L, upper= 400L),
    makeIntegerParam("tiros_ronda_4" , lower= 0L, upper= 400L),
    makeIntegerParam("tiros_ronda_5" , lower= 0L, upper= 400L)
    )
  
  configureMlr( show.learner.output= FALSE)
  
  # Configuro la busqueda bayesiana
  obj.fun  <- makeSingleObjectiveFunction(
    fn=       Estimar_Tasa_Eleccion_Correcta,
    minimize= FALSE, # Maximizar tasa de elección correcta
    noisy=    TRUE,
    par.set=  hs,
    has.simple.signature = FALSE
  )
  
  ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
  ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
  ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())
  
  surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))
  
  #inicio la optimizacion bayesiana
  if( !file.exists( archivo_BO ) ) {
    run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
  } else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista
  
}

# Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia

# set.seed( 625643 )

setwd( "C:\\data_mining\\" )

#creo la carpeta donde va el experimento
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/CazaTalentos/", showWarnings = FALSE )
setwd("C:\\data_mining\\labo\\exp\\CazaTalentos\\") # Establezco el Working Directory DEL EXPERIMENTO

archivo_log  <- "HT_CazaTalentos.txt"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
  tabla_log  <- fread( archivo_log )
  GLOBAL_iteracion  <- nrow( tabla_log )
}

Buscar_Mejor_Modelo()

