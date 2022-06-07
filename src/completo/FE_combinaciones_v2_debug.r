
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}

#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  gc()
  cat( "Agregando Mes\n")
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
  cat( "Mes Agregado\n")
}

#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan dañadas

Corregir  <- function( dataset )
{
  gc()
  cat( "Corrigiendo Variables\n")
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_trx   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_trx   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_trx   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_trx   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mtarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_trx  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_trx   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_trx   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_trx   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_trx   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_trx   := NA ]
  dataset[ foto_mes==202006,  ccajas_trx   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
  cat( "Variables Corregidas\n")
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  cat( "Agregando Variables Manuales\n")
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a NA
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.nan, dataset)] <<- NA
  }

  ReportarCampos( dataset )
  cat( "Variables Manuales Agregadas\n")
}

#------------------------------------------------------------------------------
#calcula el ranking de la funcion

Rankeador  <- function( dataset, cols )
{
  gc()
  cat( "Agregando Rankings\n")
  sufijo  <- "_rank"
  
  nuevas_cols = c()
  
  for( vcol in cols )
  {
    dataset[ , paste0( vcol, sufijo) := frank( get(vcol) )/ .N, 
             by= foto_mes ]
    
    nuevas_cols = c(nuevas_cols, paste0( vcol, sufijo))
  }
  
  ReportarCampos( dataset )
  
  cat( "Rankings Agregados\n")
  
  return (nuevas_cols)
}

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( dataset, cols, nlag, deltas )
{
  gc()
  cat( "Agregando Lags de orden ", nlag, "\n")
  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
  cat( "Lags de orden ", nlag, " agregados\n")
}

#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()
  cat( "Agregando tendencias y más con ventana ", ventana, "\n")
  ReportarCampos( dataset )
  
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana
  
  last  <- nrow( dataset )
  
  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente
  
  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1
  
  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
  
  nuevas_cols = c()
  
  for(  campo  in   cols )
  {
    nueva_col <- fhistC( dataset[ , get(campo) ], vector_desde ) 
    
    if(tendencia) {
      nuevas_cols = union(nuevas_cols, paste0( campo, "_tend", ventana))
      dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    }
    if(minimo){
      nuevas_cols = union(nuevas_cols, paste0( campo, "_min", ventana))
      dataset[ , paste0( campo, "_min", ventana) := nueva_col[ (1*last +1):(2*last) ]  ]
    }
    if(maximo){ 
      nuevas_cols = union(nuevas_cols, paste0( campo, "_max", ventana))
      dataset[ , paste0( campo, "_max", ventana) := nueva_col[ (2*last +1):(3*last) ]  ]
    }
    if(promedio){
      nuevas_cols = union(nuevas_cols, paste0( campo, "_avg", ventana))
      dataset[ , paste0( campo, "_avg", ventana) := nueva_col[ (3*last +1):(4*last) ]  ]
    }
    if(ratioavg){
      nuevas_cols = union(nuevas_cols, paste0( campo, "_ratioavg", ventana))
      dataset[ , paste0( campo, "_ratioavg", ventana) := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    }
    if(ratiomax){
      nuevas_cols = union(nuevas_cols, paste0( campo, "_ratiomax", ventana))
      dataset[ , paste0( campo, "_ratiomax", ventana) := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
    }
  }
  
  ReportarCampos( dataset )
  
  cat( "Tendencias y más con ventana ", ventana, " agregadas\n")
  
  return ( list("dataset" = dataset, "cols" = nuevas_cols) )
}

#------------------------------------------------------------------------------

VPOS_CORTE  <- c()

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

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos
#se llama varias veces, luego de agregar muchas variables nuevas, para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

GVEZ <- 1 

CanaritosImportancia  <- function( dataset, canaritos_ratio=0.2 )
{
  gc()
  cat("Aplicando canaritos con ratio: ", canaritos_ratio, "\n")
  ReportarCampos( dataset )
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

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
                 seed= 999983,
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
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]

  fwrite( tb_importancia, 
          file= paste0( "impo_", GVEZ ,".txt"),
          sep= "\t" )

  GVEZ  <<- GVEZ + 1

  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + sd(pos) ]  #Atencion corto en la mediana !!

  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )

  dataset[  ,  (col_inutiles) := NULL ]

  ReportarCampos( dataset )
  
  cat("Finalizado canaritos con ratio: ", canaritos_ratio, "\n")
  cat("Eliminadas: ", length(col_inutiles[which(!(col_inutiles %like% "canarito"))]), " columnas menos importantes\n")
  
  tb_importancia = tb_importancia[ Feature %in% col_utiles ]
  tb_importancia[  , pos := .I ]
  
  return (tb_importancia)
}

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# CruzarVariablesImportantes:
# 
# Cruza las n variables mas importantes del dataset, utilizando la funcion_cruza pasada como parámetro.
#
# Parámetros:
# dataset: El dataset sobre el cual se van a procesar las cruzas
# tb_importancia: Tabla de importancias de features.
# funcion_cruza: Función que realiza la operación de cruza entre 2 columnas.
#   * Debe recibir los parámetros: dataset, columna 1, columna2.
#   * Debe devolver el nombre de la columna recién agregada.
# n: Top n variables a cruzar entre sí.
# cruzar_con_bajas:
#   * Si es TRUE (defecto), además agrega una columna con el count de bajas del mes anterior
#   * Luego cruza esta nueva columna con las demás top n columnas.
#
# Devuelve una lista con:
# * dataset: El dataset modificado
# * cols: Un vector con los nombres de las nuevas columnas creadas
#------------------------------------------------------------------------------

CruzarVariablesImportantes <- function(dataset, tb_importancia, funcion_cruza, n=100, cruzar_con_bajas=TRUE)
{
  gc()
  cat("Cruzando las: ", n, " variables más importantes\n")
  
  # Tomo las n variables más importantes
  cols = tb_importancia[pos <= n, Feature]
  
  if (cruzar_con_bajas) {
    # Calculo numero de (BAJA+1 + BAJA+2) del mes anterior. Si es el primero lo dejo en NA.
    bajas_dt = dataset[clase_ternaria != "CONTINUA", .(bajas_del_mes = shift(.N, 1, NA, "lag")), keyby= foto_mes]
    
    # Joineo el dataset con la nueva columna
    setkey(dataset,foto_mes)
    setkey(bajas_dt,foto_mes)
    dataset = dataset[bajas_dt]
    
    # Agrego a cols esta nueva columna
    cols = union(cols, "bajas_del_mes")
  }
  
  # Solo voy a cruzar las columnas que sean numéricas
  num_cols = colnames(dataset[,sapply(dataset,is.numeric),with=FALSE])
  cols = intersect(cols, num_cols)
  
  nuevas_cols = c()
  
  for( i in 1:(length(cols) - 1) ) {
    for(j in (i+1):length(cols)) {
      nuevas_cols = union(nuevas_cols, funcion_cruza(dataset, cols[i], cols[j]))
    }
  }
  
  # Nuevo intersect como protección
  nuevas_cols = intersect( colnames(dataset), nuevas_cols )
  
  ReportarCampos( dataset )
  cat("Fin de cruce de variables. Agregadas: ", length(nuevas_cols), "\n")
  
  return (list("dataset"= dataset, "cols" = nuevas_cols))
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# AgregarRankingsTendenciasLags:
# 
# Procesa agregado de rankings, tendencias y lags en ese orden sobre el dataset
# y columnas especificados como parámetros.
#
# Parámetros:
# dataset: El dataset a procesar
# cols: Columnas sobre las cuales realizar el procesamiento.
# rankear: Si es TRUE calcula el ranking de las variables especificadas.
# tendencias: Objeto configuración de las tendencias.
# lags: Vector de lags a aplicar.
# canaritos_ratio: Ratio de canaritos a aplicar en cada paso
#
# Devuelve una lista con:
# * dataset: El dataset modificado
# * importancia: La tabla de importancia final
#------------------------------------------------------------------------------

AgregarRankingsTendenciasLags <- function(dataset, cols, rankear, tendencias, lags, canaritos_ratio = 0.2) {
  if( rankear ) {
    setorder( dataset, foto_mes, numero_de_cliente )
    cols = union(cols, Rankeador(dataset, cols))
    cols = intersect( colnames(dataset), cols )
    setorder( dataset, numero_de_cliente, foto_mes )
  }
  
  if( (tendencias$correr == TRUE) && ( length(tendencias$ventanas) > 0 ) )
  {
    nuevas_cols = c()
    for (ventana in tendencias$ventanas) {
      cat("Campos antes de llamar a tendencias con ventana: ", ventana, "\n")
      ReportarCampos(dataset)
      
      resultados = TendenciaYmuchomas( dataset, 
                                       cols= cols,
                                       ventana=   ventana,
                                       tendencia= tendencias$tendencia,
                                       minimo=    tendencias$minimo,
                                       maximo=    tendencias$maximo,
                                       promedio=  tendencias$promedio,
                                       ratioavg=  tendencias$ratioavg,
                                       ratiomax=  tendencias$ratiomax )
      
      nuevas_cols = union(nuevas_cols, resultados$cols)
      dataset = resultados$dataset
      
      cat("Campos después de llamar a tendencias con ventana: ", ventana, "\n")
      ReportarCampos(dataset)
      
      tb_importancia = CanaritosImportancia( dataset, canaritos_ratio= canaritos_ratio )
      cols = intersect( colnames(dataset), cols ) # Luego de canaritos, quito las columnas que fueron removidas del dataset
    }
    
    cols = union(cols, nuevas_cols)
    cols = intersect( colnames(dataset), cols ) # Quito las nuevas cols que fueron removidas por canaritos
  }
  
  for( lag in lags )
  {
    Lags( dataset, cols, lag, TRUE ) # calculo los lags de orden "lag"
    
    tb_importancia = CanaritosImportancia( dataset, canaritos_ratio= canaritos_ratio )
    cols = intersect( colnames(dataset), cols ) # Quito las cols que fueron removidas por canaritos
  }
  
  return ( list("dataset" = dataset, "importancia" = tb_importancia) )
}

#------------------------------------------------------------------------------
# Aquí empieza el programa

setwd("~/buckets/b1/")

EXP <<- read_yaml("./exp/debug/debug.yml")
PARAM  <<- EXP$param

# Cargo el dataset
nom_arch = "./datasets/paquete_premium.csv.gz"
dataset   <- fread( nom_arch )

# Seteo el wd en el experimento
setwd("~/buckets/b1/exp/debug/")

# Ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
setorderv( dataset, PARAM$const$campos_sort )

AgregarMes( dataset )  # Agrego el mes del año

if( PARAM$corregir ) Corregir( dataset )

if( PARAM$variablesmanuales )  AgregarVariables( dataset )

cols_a_procesar  <- copy( setdiff( colnames(dataset), PARAM$const$campos_fijos ) )

# Calculo rankings, tendencias, lags
resultados = AgregarRankingsTendenciasLags( dataset, cols_a_procesar, PARAM$rankear, PARAM$tendencias, PARAM$lags, PARAM$canaritos_ratio)

# Actualizo el dataset y la tabla de importancia
dataset = resultados$dataset
tb_importancia = resultados$importancia

# Realizar cruzas de variables si corresponde. En éste caso hago una simple división entre variables.
if ( PARAM$cruzas > 0 ) {
  resultado_cruzas = CruzarVariablesImportantes(
    dataset,
    tb_importancia,
    function(dataset, c1, c2) {
      if (!(c1 %in% colnames(dataset)) || !(c2 %in% colnames(dataset)))
        return(c())
      
      nombre = paste0("cruza_ratio_", c1, '_vs_', c2)
      dataset[get(c2) > 0, eval(nombre) := (get(c1) / get(c2))]
      return (nombre)
    },
    PARAM$cruzas,
    PARAM$agregar_bajas
  )
  
  # Actualizo dataset y obtengo nuevas columnas
  dataset = resultado_cruzas$dataset
  cols_cruzas = resultado_cruzas$cols
  
  # Ahora calculo rankings, tendencias, lags para las nuevas variables cruza
  resultados = AgregarRankingsTendenciasLags( dataset, cols_cruzas, PARAM$rankear, PARAM$tendencias, PARAM$lags, PARAM$canaritos_ratio)
  
  # Actualizo el dataset y la tabla de importancia
  dataset = resultados$dataset
  tb_importancia = resultados$importancia
}

# Trunco las X variables más importantes para achicar el dataset
if ( PARAM$truncar > 0 ) {
  cat("Truncando variables a: ", PARAM$truncar, "\n")
  cols_finales = union(tb_importancia[pos <= PARAM$truncar, Feature], PARAM$const$campos_fijos)
  dataset = dataset[, ..cols_finales]
  ReportarCampos(dataset)
  cat("Variables truncadas\n")
}

# Dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , PARAM$const$clase ) , PARAM$const$clase )
setcolorder( dataset, nuevo_orden )

# Seteo wd en datasets
setwd("~/buckets/b1/datasets/")

# Grabo el dataset
fwrite( dataset,
        "debug.csv.gz",
        logical01= TRUE,
        sep= "," )
