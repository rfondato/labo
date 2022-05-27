rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
library(tibble)
library(dplyr)
library(tidyr)

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:\\data_mining\\")  #Establezco el Working Directory

#Cargo dataset
dataset  <- fread("./datasets/paquete_premium_202011.csv")

dataset[, clase_numerica := as.numeric(ifelse(clase_ternaria == "CONTINUA", 0, ifelse(clase_ternaria == "BAJA+2", 1, 2)))]

correlaciones = cor(dataset[, !c("clase_ternaria", "foto_mes")])

correlaciones = correlaciones %>% as.data.frame %>% rownames_to_column(var = 'var1') %>% gather(var2, value, -var1)

correlaciones = as.data.table(correlaciones)

mayor_cor_positiva = correlaciones[var1 != var2][order(-value)]
mayor_cor_negativa = correlaciones[var1 != var2][order(value)]

cor_ctrx_quarter = correlaciones[var1 == "ctrx_quarter"]
cor_mpasivos_margen = correlaciones[var1 == "mpasivos_margen"]
cor_cpayroll_trx = correlaciones[var1 == "cpayroll_trx"]

cor_clase_numerica = correlaciones[var1 == "clase_numerica"]

dataset[  , ctrx_quarter_normalizado := ctrx_quarter * 1.0 ]
dataset[ cliente_antiguedad== 1 , ctrx_quarter_normalizado := ctrx_quarter * 5.0 ]
dataset[ cliente_antiguedad== 2 , ctrx_quarter_normalizado := ctrx_quarter * 2.0 ]
dataset[ cliente_antiguedad== 3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

dataset[ , var_porcentual_egreso_ingreso := (mtransferencias_emitidas - mtransferencias_recibidas) / mtransferencias_recibidas ]
dataset[ , transf_emitidas_vs_ctrx_quarter := ctransferencias_emitidas / ctrx_quarter_normalizado ]
dataset[ , pasivos_vs_activos := mpasivos_margen / mactivos_margen ]
dataset[ , pasivos_vs_ctrx_quarter := mpasivos_margen / ctrx_quarter_normalizado ]
dataset[ , mpayroll_vs_cpayroll := mpayroll / cpayroll_trx ]
dataset[ , visa_consumo_vs_trx := mtarjeta_visa_consumo / ctarjeta_visa_trx ]
dataset[ , visa_consumo_vs_ctrx_quarter := mtarjeta_visa_consumo / ctrx_quarter_normalizado ]
dataset[ , mv_tarjeta_consumo := rowSums( cbind( mtarjeta_master_consumo,  mtarjeta_visa_consumo) , na.rm=TRUE ) ]
dataset[ , mv_tarjeta_ctrx := rowSums( cbind( ctarjeta_master_trx,  ctarjeta_visa_trx) , na.rm=TRUE ) ]
dataset[ , mv_consumo_vs_trx := mv_tarjeta_consumo / mv_tarjeta_ctrx ]
dataset[ , mv_consumo_vs_ctrx_quarter := mv_tarjeta_consumo / ctrx_quarter_normalizado ]
dataset[ , mrentabilidad_vs_anual := mrentabilidad / mrentabilidad_annual ]

#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos      <- lapply( names(dataset),
                          function(.name) dataset[ , sum(is.infinite( get(.name) )) ]  )

infinitos_qty  <- sum( unlist( infinitos ) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  dataset[mapply(is.infinite, dataset)] <- NA
}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans      <- lapply( names(dataset),
                     function(.name) dataset[ , sum( is.nan( get(.name) )) ] )

nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el script!\n\n")
  dataset[mapply(is.nan, dataset)] <- NA
}

means_per_class = dataset[, lapply(.SD, mean, na.rm=T), by=.(clase_ternaria)]
sd_per_class = dataset[, lapply(.SD, sd, na.rm=T), by=.(clase_ternaria)]

means_per_class[, ctrx_quarter] - 2 * sd_per_class[, ctrx_quarter]
means_per_class[, ctrx_quarter] + 2 * sd_per_class[, ctrx_quarter]

dataset[, ctrx_quarter]
