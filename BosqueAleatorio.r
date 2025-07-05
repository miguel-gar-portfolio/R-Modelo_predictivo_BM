
#############################  INSTALACION DE LIBRERIAS   #################################################
# Lista de paquetes requeridos
paquetes <- c("corrplot", "pROC","rpart","rpart.plot","caret","openxlsx","DBI","RSQLite","odbc","randomForest","lubridate")

# Instalar los que falten
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if(length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

# Cargar todos los paquetes
lapply(paquetes, library, character.only = TRUE)

#############################  LECTURA DE DATOS   #################################################
# Conexión a SQL Server
conection <- dbConnect(odbc(),
                 Driver   = "...",    
                 Server   = "...",   # ej: "localhost" o "192.168.1.100"
                 Database = "...",
                 UID      = "...",     # Usuario 
                 PWD      = "...",  # Contraseña
                 Port     = ...)
datos_originales <- dbGetQuery(conection, "SELECT * FROM table")
head(datos_originales)
dbDisconnect(conection)

#ELIMINAMOS COLUMNAS INNECESARIAS
datos_originales<-datos_originales[,-c(1,3,4,6)] 

#CONJUNTO DE ENTRENAMIENTO
datos<-datos_originales


############################# CONTROL DE DATOS #####################################################
## Eliminacion de columnas sin valor estadistico
## Deteccion y administracion de nulls  
## Reconversion de tipo de columnas al tipo adecuado



################# ELIMINACION DE COLUMNAS SIN VALOR ESTADISTICO 
datos<-datos[,-13] 

################# DETECCION Y ADMINISTRACION DE VALORES NULL 
{  ### VERIFICACION DE VALORES NA 
  sapply(datos, function(x) sum(is.na(x)))  
  
  ### REASIGNACION DE VALORES NA
  datos <- datos[!is.na(datos$col), ]
  datos <- datos[!is.na(datos$col2), ]
  }

################# RECONVERSION DE TIPO DE COLUMNAS AL TIPO ADECUADO
{  #### CONVERSION DE COLUMNAS CATEGORICAS A FACTORES 
  factores <- c("...")
  datos[factores] <- lapply(datos[factores], as.factor)
}


############################# ANALISIS EXPLORATORIO #####################################################
## Matriz de correlaciones en variables continuas
## Test de correlacion con prueba chi cuadrado en variables categoricas 


################# MATRIZ DE CORRELACIONES EN VARIABLES CONTINUAS 
{
  #### LISTA DE COLUMNAS CON VARIABLES CONTINUAS 
  col_var_continuas=c("...")
  
  #### MATRIZ DE CORRELACIONES
  matrizcor <- cor(datos[col_var_continuas])  
  corrplot(matrizcor,
           method = "color",        # Muestra el color del coeficiente
           type = "upper",          # Solo triángulo superior
           col = colorRampPalette(c("red", "white", "blue"))(200),  # Paleta de colores
           addCoef.col = "black",   # Añade coeficientes en negro
           number.cex = 0.8,        # Tamaño de número de correlación
           tl.cex = 1,            # Tamaño del texto (nombres de variables)
           tl.col = "black",        # Color del texto
           tl.srt = 45,             # Rotación de las etiquetas
           mar = c(0, 0, 1, 0)      # Márgenes para más espacio
  )
}

################# TEST DE CORRELACION CON PRUEBA CHI CUADRADO EN VARIABLES CATEGORICAS
{
  correlaciones_factores <- data.frame(Variable_1 = character(0), Variable_2 = character(0), Cor = numeric(0))
  for (i in 1:(length(factores) - 1)) {
    for (j in (i + 1):length(factores)) {
      
      tabla_contingencia <- table(datos[[factores[i]]], datos[[factores[j]]])
      resultado_chi <- chisq.test(tabla_contingencia)
      
      # Agregar a tabla resultados
      correlaciones_factores <- rbind(correlaciones_factores, data.frame(Variable_1 = factores[i], Variable_2 = factores[j], Cor = resultado_chi$p.value))
    }
  }
  
  correlaciones_factores[correlaciones_factores$Cor>0.05,]
}


############################# MODELADO PREDICTIVO #####################################################
## Seleccion de muestra 
## Entrenamiento del modelo 
## Bondad del ajute

################# SELECCION DE MUESTRA
{
  #### ESTUDIO DE LA ESTRATIFICACION
  ...
  
  #### DIVISION DE LA MUESTRA EN CONJUNTO DE ENTRENAMIENTO Y CONJUNTO DE TEST 
  ...
  
  # Índices aleatorios para entrenamiento (70%)
  n <- nrow(datos)
  indices_entrenamiento <- sample(1:n, size = 0.7 * n)
  
  indices_entrenamiento <- (1:floor(n*0.7))
  
  # Crear subconjuntos
  muestra_entrenamiento <- datos[indices_entrenamiento, ]
  muestra_test <- datos[-indices_entrenamiento, ]
  
  
  muestra_entrenamiento<-rbind(...)
  muestra_test <- datos[!(rownames(datos) %in% rownames(muestra_entrenamiento)), ]
}

################# ENTRENAMIENTO DEL MODELO 
muestra_entrenamiento$Objetivo <- as.factor(muestra_entrenamiento$Objetivo)  # Solo si es clasificación

modelo_rf <- randomForest(Objetivo ~ ., 
                          data = muestra_entrenamiento, 
                          ntree = 200,        # Número de árboles
                          mtry = 6,           # Número de variables aleatorias por split (puedes ajustar)
                          maxnodes = 10,
                          importance = TRUE)  # Para ver la importancia de variables
importance(modelo_rf)
varImpPlot(modelo_rf)
plot(modelo_rf)

# GUARDO EL MODELO CON SUS DATOS DE ENTRENAMIENTO 
save(modelo_rf,muestra_entrenamiento, muestra_test, file = "...")

################# BONDAD DEL AJUSTE

#### MATRIZ DE CONFUSION DE LA MUESTRA DE ENTRENAMIENTO  
predicciones_test <- predict(modelo_rf, muestra_test, type = "class")

confusionMatrix<-table(prediccion = predicciones_test,real = muestra_test$Objetivo)
confusionMatrix

#### RATIOS DE LA MUESTRA DE TEST
medida_bienclasificados_entre_total<-(sum(predicciones_test==1 & muestra_test$Objetivo==1)+sum(predicciones_test==0 & muestra_test$Objetivo==0))/length(muestra_test[,1])
medida_bienclasificados_entre_total

medida_verdaderospos_entre_totalpos<-(sum(predicciones_test==1 & muestra_test$Objetivo==1)/sum(predicciones_test==1))
medida_verdaderospos_entre_totalpos

medida_verdaderosneg_entre_totalneg<-1-(sum(predicciones_test==0 & muestra_test$Objetivo==1)/sum(predicciones_test==0))
medida_verdaderosneg_entre_totalneg

### VALIDACIONES POR MES PARA ESTIMAR COEFICIENTES 
# REASIGNACION DE VALORES NA
datos_originales <- datos_originales[!is.na(datos_originales$col), ]


fechabase<-min(datos_originales$fechaMuestra)
tabla_ratios <- data.frame(fechabase = as.Date(character()) , medida_bienclasificados_entre_total = numeric(),medida_verdaderospos_entre_totalpos = numeric(),medida_verdaderosneg_entre_totalneg  = numeric())

for (i in 1:46){
#VALIDACIONES MENSUALES BOSQUE ALEATORIO 
muestra_test_mes<-datos_originales[datos_originales$fechaMuestra == fechabase,]
muestra_test_mes$fechaMuestra<-NULL

# CONVERSION DE COLUMNAS CATEGORICAS A FACTORES 
factores <- c("...")
muestra_test_mes[factores] <- lapply(muestra_test_mes[factores], as.factor)

# Forzar mismos niveles de factor en '...'
muestra_test_mes$Turno <- factor(
  muestra_test_mes$col,
  levels = levels(muestra_entrenamiento$col)
)

predicciones_test_mes <- predict(modelo_rf,  newdata = muestra_test_mes,type = "class")

confusionMatrix<-table(prediccion = predicciones_test_mes,real = muestra_test_mes$Objetivo)
confusionMatrix

medida_bienclasificados_entre_total<-(sum(predicciones_test_mes==1 & muestra_test_mes$Objetivo==1)+sum(predicciones_test_mes==0 & muestra_test_mes$Objetivo==0))/length(muestra_test_mes[,1])
medida_bienclasificados_entre_total

medida_verdaderospos_entre_totalpos<-(sum(predicciones_test_mes==1 & muestra_test_mes$Objetivo==1)/sum(predicciones_test_mes==1))
medida_verdaderospos_entre_totalpos

medida_verdaderosneg_entre_totalneg<-1-(sum(predicciones_test_mes==0 & muestra_test_mes$Objetivo==1)/sum(predicciones_test_mes==0))
medida_verdaderosneg_entre_totalneg

print(fechabase)
print(confusionMatrix)
print(medida_bienclasificados_entre_total)
print(medida_verdaderospos_entre_totalpos)
print(medida_verdaderosneg_entre_totalneg)


# nueva_fila <- data.frame(fechabase = fechabase, medida_bienclasificados_entre_total = medida_bienclasificados_entre_total, medida_verdaderospos_entre_totalpos = medida_verdaderospos_entre_totalpos,medida_verdaderosneg_entre_totalneg=medida_verdaderosneg_entre_totalneg)
# tabla_ratios<-rbind(tabla_ratios,nueva_fila)

  
# Convertir a tipo Date y sumar un mes
fechabase <- fechabase %m+% months(1)
}

#primer insertado de ratios para PBI
# tabla_ratios_trucanda<-tabla_ratios[tabla_ratios$fechabase>="...",]
# tabla_ratios_trucanda$fechabase<-tabla_ratios_trucanda$fechabase + 9
# 
# 
# conection <- dbConnect(odbc(),
#                        Driver   = "...",   
#                        Server   = "...",   # ej: "localhost" o "192.168.1.100"
#                        Database = "...",
#                        UID      = "...",     # Usuario 
#                        PWD      = "...",  # Contraseña
#                        Port     = ...)
# 
# dbExecute(conection,"drop table if exists ...")
# dbWriteTable(conection,
#              name = "...",  # Debe existir ya en la base de datos
#              value = tabla_ratios_trucanda,              # Tu data frame en R
#              append = TRUE,              # ← importante: NO sobrescribe, solo agrega
#              row.names = FALSE)          # ← evita que escriba nombres de fila como una columna
# dbExecute(conection,"insert into mitabla select * from ...")
# dbExecute(conection,"drop table if exists  ...")
# dbDisconnect(conection)


############################# PREDICCIONES MENSUALES #####################################################
################# EXTRACCION DE DATOS
fechaMuestra<-"..."
         
# Conexión a SQL Server
conection <- dbConnect(odbc(),
                 Driver   = "...",    
                 Server   = "...",   # ej: "localhost" o "192.168.1.100"
                 Database = "...",
                 UID      = "...",     # Usuario 
                 PWD      = "...",  # Contraseña
                 Port     = ...)
datos_a_predecir <- dbGetQuery(conection, "SELECT * FROM table")
head(datos_a_predecir)
dbDisconnect(conection)

# SELECCION DE FECHA
datos_a_predecir<-datos_a_predecir[datos_a_predecir$fechaMuestra==fechaMuestra,]

################# CONTROL DE DATOS
### ELIMINACION DE COLUMNAS 
datos_a_predecir<-datos_a_predecir[,-c(3,4,6,16)]

### ADMINISTRACION DE VALORES NA
sapply(datos_a_predecir, function(x) sum(is.na(x)))  

#### REASIGNACION DE VALORES NA
datos_a_predecir <- datos_a_predecir[!is.na(datos_a_predecir$col), ]

#### CONVERSION DE COLUMNAS CATEGORICAS A FACTORES 
factores <- c("...")
datos_a_predecir[factores] <- lapply(datos_a_predecir[factores], as.factor)

# Forzar mismos niveles de factor en 'Turnos'
datos_a_predecir$col <- factor(
  datos_a_predecir$col,
  levels =c("...") 
)

################# CALCULO DE LA PREDICCION
### CLASIFICACION 
predicciones_clasificacion <- predict(modelo_rf, datos_a_predecir, type = "class")
predicciones_clasificacion<-as.integer(predicciones_clasificacion)-1

sum(predicciones_clasificacion)

predicciones_probabilidad<- predict(modelo_rf, datos_a_predecir, type = "prob")
predicciones_probabilidad <- predicciones_probabilidad[,2]

predicciones<- cbind(id=datos_a_predecir$ConductorID,clasificacion=predicciones_clasificacion,probabilidad=predicciones_probabilidad)
predicciones<- as.data.frame(predicciones)
predicciones$probabilidad[predicciones$clasificacion==0] <- 1-predicciones$probabilidad[predicciones$clasificacion==0]


### AÑADIR COLUMNA DE FIABILIDAD Y FECHA MUESTRA
predicciones$Fiabilidad <- ifelse(predicciones$probabilidad >= 0.5 & predicciones$probabilidad < 0.7, "Baja",
                                        ifelse(predicciones$probabilidad >= 0.7 & predicciones$probabilidad < 0.85, "Media",
                                               "Alta"))

predicciones$FechaMuestra <- fechaMuestra
predicciones$FechaMuestra <- as.Date(predicciones$FechaMuestra)


### GUARDAR PREDICCIONES EN TABLA SQL 
conection <- dbConnect(odbc(),
                 Driver   = "...",    
                 Server   = "...",   # ej: "localhost" o "192.168.1.100"
                 Database = "...",
                 UID      = "...",     # Usuario 
                 PWD      = "...",  # Contraseña
                 Port     = ...)

dbExecute(conection,"drop table if exists ...")
dbWriteTable(conection,
             name = "...",  # Debe existir ya en la base de datos
             value = predicciones,              # Tu data frame en R
             append = TRUE,              # ← importante: NO sobrescribe, solo agrega
             row.names = FALSE)          # ← evita que escriba nombres de fila como una columna
dbExecute(conection,"insert into mitabla select * from ....")
dbExecute(conection,"drop table if exists  #PrediccionesTemporal")
dbDisconnect(conection)
