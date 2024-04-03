# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
install.packages(c("data.table", "rpart", "rpart.plot"))


# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/Elisabeth/Desktop/MAESTRIA_AUSTRAL/Labo_I/labo2024v1") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("C:/Users/Elisabeth/Desktop/MAESTRIA_AUSTRAL/Labo_I/datasets/dataset_pequeno.csv")

#creo clase binaria1

dataset<- dataset[clase_ternaria == "BAJA+2",clase_binaria1:="pos"]
dataset<- dataset[clase_ternaria != "BAJA+2",clase_binaria1:="neg"]

#elimino clase ternaria para que solo trabaje con clase binaria
dataset[, clase_ternaria := NULL]


dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo



# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
  formula = "clase_binaria1 ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -0.5, # esto significa no limitar la complejidad de los splits
  minsplit = 600, # minima cantidad de registros para que se haga el split
  minbucket = 150, # tamaño minimo de una hoja [minsplit >= 2* minbucket] 
  maxdepth = 8
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_pos := prediccion[, "pos"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_pos > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("C:/Users/Elisabeth/Desktop/MAESTRIA_AUSTRAL/Labo_I/exp/")
dir.create("C:/Users/Elisabeth/Desktop/MAESTRIA_AUSTRAL/Labo_I/exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "C:/Users/Elisabeth/Desktop/MAESTRIA_AUSTRAL/Labo_I/exp/KA2001/K103_001.csv",
       sep = ","
)
