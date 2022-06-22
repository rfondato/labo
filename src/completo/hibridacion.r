#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("c:\\data_mining\\predicciones\\")

p_start = 1000
p_envios = 11000
p_salida = paste0("hibridacion_", p_envios, ".csv")

pred_files = dir(".", pattern = ".*\\.csv$", full.names = TRUE, ignore.case = TRUE)

all_preds = NULL

for (file in pred_files) {
  pred_data <- fread( file )
  
  # Normalizo: Esto es porque usé diferente cantidad de semillas en cada semillerío,
  # y me interesa el peso relativo que cada modelo le puso a cada cliente,
  # pero luego tratar a todos los semilleríos con el mismo peso de decisión,
  # en vez de darle mas peso a los semilleríos con más semillas.
  max_pred_acum = pred_data[, max(pred_acumulada)]
  pred_data[ , pred_normalizada := (pred_acumulada / max_pred_acum)] 
  
  # pred_data[ , pred_rank := as.numeric( frank(pred_acumulada, ties.method= "random") )]

  pred_data = pred_data[, .(numero_de_cliente, pred_normalizada)]
  
  if (is.null(all_preds)) {
    all_preds <- pred_data
  } else {
    setkey(all_preds, numero_de_cliente)
    setkey(pred_data, numero_de_cliente)
    
    all_preds <- all_preds[pred_data, nomatch=0]
  }
}

all_preds[, pred_final := rowSums(.SD), .SDcols = !c("numero_de_cliente")]
setorder( all_preds, -pred_final )

all_preds[  , Predicted := 0L ]
all_preds[ p_start:p_envios, Predicted := 1L ]
all_preds = all_preds[, .(numero_de_cliente, Predicted)]

dir.create( "./salida_hibridacion/",  showWarnings = FALSE )
setwd("c:\\data_mining\\predicciones\\salida_hibridacion\\")

fwrite(  all_preds,
         file= p_salida,
         sep= "," )
