##############################################################
######## SOLUCIÓN CAZATALENTOS 15K - Rodrigo Fondato #########
##############################################################

# Limpio la memoria
rm( list=ls() )
gc()

require("data.table")

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

jugar_ronda <- function(planilla_cazatalentos, n_tiros, percentil = 0)
{
  # Inicializo ids_juegan con todos los jugadores por defecto
  ids_juegan = planilla_cazatalentos[, id]
  
  # Si hay un percentil > 0, selecciono los jugadores que van a tirar en ésta ronda
  if (percentil > 0) {
    # Calculo valor de percentil pasado por parámetro sobre la columa tasa_aciertos
    # en TODA la tabla
    valor_percentil = quantile(planilla_cazatalentos[, tasa_aciertos], probs=c(percentil))
    
    # Calculo ids que van a jugar la ronda, todos los de tasa de acierto por encima del percentil
    ids_juegan = planilla_cazatalentos[tasa_aciertos >= valor_percentil, id ]
  }
  
  # Protección contra ronda innecesaria. Si hay uno no se juega y no se tira (la ronda sobra)
  if (length(ids_juegan) <= 1) {
    return (ids_juegan)
  }
  
  # Limpio aciertos de la ronda anterior
  planilla_cazatalentos[, aciertos := 0]

  # Hago que tiren
  resultado  <- gimnasio_tirar( ids_juegan, n_tiros )
  
  # Registro en la planilla los aciertos de los que jugaron
  planilla_cazatalentos[ ids_juegan,  aciertos := resultado ]
  
  # Sumo al total de aciertos y al total de tiros lanzados por jugador
  planilla_cazatalentos[ ids_juegan,  total_aciertos := total_aciertos + resultado ]
  planilla_cazatalentos[ ids_juegan,  total_tiros := total_tiros + n_tiros ]
  
  # Recalculo tasa de aciertos general de cada jugador
  planilla_cazatalentos[ ids_juegan,  tasa_aciertos := total_aciertos / total_tiros ]
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
  planilla_cazatalentos[, tasa_aciertos := 0]
  
  for (r in rondas)
  {
    tiros = r[1]
    percentil = r[2]
    
    # Juego la ronda
    jugar_ronda(planilla_cazatalentos, tiros, percentil)
  }
  
  # Calculo el mejor después de todas las rondas,
  # entre TODOS los jugadores (el de mayor tasa de aciertos general)
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


# set.seed( 625643 )

#rondas = list(
 # c(20, 0),
#  c(20, 0.1),
#  c(20, 0.2),
#  c(20, 0.3),
#  c(20, 0.4),
#  c(20, 0.5),
#  c(20, 0.6),
#  c(20, 0.65),
#  c(20, 0.7),
#  c(20, 0.75),
#  c(20, 0.8),
#  c(20, 0.85),
#  c(20, 0.9),
#  c(100, 0.95)
#)

rondas = list(
  c(10, 0),
  c(10, 0.05),
  c(10, 0.1),
  c(10, 0.15),
  c(10, 0.2),
  c(10, 0.25),
  c(10, 0.3),
  c(10, 0.35),
  c(10, 0.4),
  c(10, 0.45),
  c(1000, 0.5)
  #c(15, 0.5),
  #c(15, 0.55),
  #c(15, 0.6),
  #c(15, 0.65),
  #c(20, 0.7),
  #c(20, 0.75),
  #c(30, 0.8),
  #c(30, 0.85),
  #c(50, 0.9),
  #c(100, 0.95)
)

for (i in 1:5) {
  resultados = Montecarlo_Estrategia(rondas, 1000)
    
  print(paste("Cantidad total de tiros:", resultados[1]))
    
  print(paste("Tasa de Elección Correcta:", resultados[2]))
}