#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("c:\\data_mining\\submits_to_mix\\")

p_salida = "submits_final.csv"

submit_files = dir(".", pattern = ".*\\.csv$", full.names = TRUE, ignore.case = TRUE)
n_files = length(submit_files)

all_submits = NULL

for (file in submit_files) {
  submit_data <- fread( file )

  if (is.null(all_submits)) {
    all_submits <- submit_data
  } else {
    setkey(all_submits, numero_de_cliente)
    setkey(submit_data, numero_de_cliente)
    
    all_submits <- all_submits[submit_data, nomatch=0]
  }
}

all_submits[, pred_acum := rowSums(.SD), .SDcols = !c("numero_de_cliente")]

setorder( all_submits, -pred_acum )

all_submits[  , Predicted := 0L ]
all_submits[ pred_acum > (3/4 * n_files), Predicted := 1L ]

# Chequeo numero de envíos
cat("Se van a enviar ", all_submits[Predicted == 1, .N], " envíos sobre un total de ", all_submits[, .N], " clientes.")

fwrite(  all_submits[, .(numero_de_cliente, Predicted)],
         file= p_salida,
         sep= "," )
