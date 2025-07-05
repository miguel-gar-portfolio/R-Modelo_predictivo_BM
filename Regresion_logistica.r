#############################  INSTALACION DE LIBRERIAS   #################################################
# Lista de paquetes requeridos
paquetes <- c("corrplot", "pROC","rpart","part.plot","caret")

# Instalar los que falten
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if(length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

# Cargar todos los paquetes
lapply(paquetes, library, character.only = TRUE)



#############################  LECTURA DE DATOS   #################################################
datos<-read.csv("...",header=TRUE,stringsAsFactors = FALSE,sep=";")

############################# CONTROL DE DATOS #####################################################
  ## Eliminacion de columnas sin valor estadistico
  ## Deteccion y administracion de nulls  
  ## Reconversion de tipo de columnas al tipo adecuado



################# ELIMINACION DE COLUMNAS SIN VALOR ESTADISTICO 
datos<-datos[,-c(3,4,6)] 
datos$col<-NULL


################# DETECCION Y ADMINISTRACION DE VALORES NULL 
{
#### VERIFICACION DE COLUMNAS CON VALORES CHAR 
datos$col[datos$col == "NULL"] #NO TIENE NULLS

#### REASIGNACION DE VALORES NULL
mean(as.numeric(datos$col[datos$col != "NULL"])) #tomamos 1 como media
datos$col[datos$col == "NULL"]<-"1"
}

################# RECONVERSION DE TIPO DE COLUMNAS AL TIPO ADECUADO
{
#### CAMBIO DE COLUMNAS CHAR A VALORES NUMERICOS 
datos$col<- as.integer(datos$col)
datos$col <- gsub("\\.", "", datos$col)  # quitar puntos
datos$col <- as.numeric(datos$col) 


#### CONVERSION DE COLUMNAS CATEGORICAS A FACTORES 
factores <- c("...")
datos[factores] <- lapply(datos[factores], as.factor)


#### CONVERSION DE VARIABLE OBJETIVO A NUMERICA
datos$Objetivo <- as.numeric(datos$Objetivo)
}


############################# ANALISIS EXPLORATORIO #####################################################
  ## Matriz de correlaciones en variables continuas
  ## Test de correlacion con prueba chi cuadrado en variables categoricas 


################# MATRIZ DE CORRELACIONES EN VARIABLES CONTINUAS 
{
#### LISTA DE COLUMNAS CON VARIABLES CONTINUAS 
col_var_continuas=c("...")

####MATRIZ DE CORRELACIONES
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


muestra_entrenamiento<-rbind(...)
muestra_test <- datos[!(rownames(datos) %in% rownames(muestra_entrenamiento)), ]
  }

################# ENTRENAMIENTO DEL MODELO 
{
#### CREACION DEL MODELO 
  modelo_nulo <- glm(Objetivo ~ 1, data = muestra_entrenamiento[,2:13], family = binomial)
  modelo_completo<- glm(Objetivo ~ ., data = muestra_entrenamiento[,2:13],family = binomial)
#### SELECCION PASO A PASO 
  modelo_step <- step(modelo_nulo,
                      scope = list(lower = modelo_nulo, upper = modelo_completo),
                      direction = "forward")
#### ESTADISTICAS DEL MODELO 
  summary(modelo_step)
}


################# BONDAD DEL AJUSTE

#### MATRIZ DE CONFUSION DE LA MUESTRA DE ENTRENAMIENTO  
predicciones_entrenamiento <- predict(modelo_step,type = "response")
clase_predicha_entrenamiento <-ifelse(predicciones_entrenamiento > 0.5, 1, 0)
table(Predicho = clase_predicha_entrenamiento, Real = muestra_entrenamiento$Objetivo)

#### RATIOS DE LA MUESTRA DE ENTRENAMIENTO  
medida_bienclasificados_entre_total<-(sum(clase_predicha_entrenamiento==1 & muestra_entrenamiento$Objetivo==1)+sum(clase_predicha_entrenamiento==0 & muestra_entrenamiento$Objetivo==0))/length(muestra_entrenamiento[,1])
medida_bienclasificados_entre_total

medida_verdaderospos_entre_totalpos<-1-(sum(clase_predicha_entrenamiento==1 & muestra_entrenamiento$Objetivo==0)/sum(clase_predicha_entrenamiento==1))
medida_verdaderospos_entre_totalpos

medida_verdaderosneg_entre_totalneg<-1-(sum(clase_predicha_entrenamiento==0 & muestra_entrenamiento$Objetivo==1)/sum(clase_predicha_entrenamiento==0))
medida_verdaderosneg_entre_totalneg


#### MATRIZ DE CONFUSION DE LA MUESTRA DE TEST
predicciones_test <- predict(modelo_step, newdata =muestra_test[,2:13], type = "response")
clase_predicha_test <-ifelse(predicciones_test > 0.5, 1, 0)
table(Predicho = clase_predicha_test, Real = muestra_test$Objetivo)

#### RATIOS DE LA MUESTRA DE TEST 
medida_bienclasificados_entre_total<-(sum(clase_predicha_test==1 & muestra_test$Objetivo==1)+sum(clase_predicha_test==0 & muestra_test$Objetivo==0))/length(muestra_test[,1])
medida_bienclasificados_entre_total

medida_verdaderospos_entre_totalpos<-1-(sum(clase_predicha_test==1 & muestra_test$Objetivo==0)/sum(clase_predicha_test==1))
medida_verdaderospos_entre_totalpos

medida_verdaderosneg_entre_totalneg<-1-(sum(clase_predicha_test==0 & muestra_test$Objetivo==1)/sum(clase_predicha_test==0))
medida_verdaderosneg_entre_totalneg

#### CALCULO DE ROC
roc_obj <- roc(muestra_test$Objetivo, predicciones_test)
plot(roc_obj)
auc(roc_obj)
