unlist(lapply(c('haven', 'magrittr', 'data.table', 'stringr', 'clipr', 'vtable','readxl', 'ggplot2','dplyr'), require, character.only = TRUE))

df_cancer <- read.csv("/home/angsmb16/R/Modulo_Mineria/data.csv")

df_cancer %>% nrow()
#569
df_cancer$X <- NULL 
df_cancer %>% length()
#32
str(df_cancer)
#id es int, diganosis es chr y los demas son num 
#solo tenemos un categorico y las demas son numericos 
unique(df_cancer$id) %>% length
#La columna id si es una columna unica que puede ser usa como id pues todos son unicos

#Verificamos si tiene NA 
df_cancer %>% naniar::vis_miss(., warn_large_data = F)
#0
#contamos los na
df_cancer %>% is.na() %>% sum()
#0

#cambiamos a chr la columa id para checar los outliers
df_cancer$id <- as.character(df_cancer$id)

#verifiquemos outliers

# Función para contar outliers 
contar_outliers <- function(x) {
  if(is.numeric(x)) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lim_inf <- q1 - 1.5 * iqr
    lim_sup <- q3 + 1.5 * iqr
    return(sum(x < lim_inf | x > lim_sup, na.rm = TRUE)
    )
  } else {
    return(NA)
  }
}

sapply(df_cancer, contar_outliers)

#Funcion para identificar las filas con outliers
es_outlier <- function(x) {
  if(is.numeric(x)) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lim_inf <- q1 - 1.5 * iqr
    lim_sup <- q3 + 1.5 * iqr
    return(x < lim_inf | x > lim_sup)
  } else {
    return(rep(FALSE, length(x))) # Si no es numérico, nada es outlier
  }
}
#Creamos una matriz
matriz_outliers <- sapply(df_cancer, es_outlier)
#Sumamos
df_cancer$total_outliers <- rowSums(matriz_outliers, na.rm = TRUE)
#Sumamos los ususairios que cumplan con outliers > 5
total_usuarios_con_outliers <- sum(df_cancer$total_outliers, na.rm = TRUE)
#Total de ususarios con outliers son 171 es el 30.05%
#Total de usuairios con mas de 1 outlier son 112 es el 19.68 %
#Total de ususarios con mas de 5 outliers son 33 es el 5.8% 

# para imprimir cuantos son usamos: total_usuarios_con_outliers
total_usuarios_con_outliers

#Vamos a elimiar los usuarios con outliers >5 por que resultan ser casos
#anomalos o extraordinarios o anomalos, todo el cancer en general tienen caracterisiticas
#que no son comparables ante las demas muestras, si bien no todos los humanos somos iguales
#estos datos no nos servirian pues son casos poco comunes, y con una muestra de solo 33 pacientes
#tampoco se puede crear un modelo.

#Filtro para quedarnos solo con los outlieres <= 5
df_cancer_5 <- df_cancer[df_cancer$total_outliers <= 5, ]
df_cancer_5$total_outliers <- NULL

sapply(df_cancer_5, contar_outliers)

#volvemos a aplicar
total_usuarios_con_outliers <- sum(df_cancer_5$total_outliers > 0, na.rm = TRUE)
total_usuarios_con_outliers
#138 id con outliers que son el  24.5% de nuestra muestra

#Para tratar estos outliers vamos a tratar las columnas que tienen el mayor numero de ouliers en esta caso son:
#area_mean, radius_se, perimeter_se, area_se, smoothness_se, compacness_se,
#symmetry_se, fractal_dimension_se,area_worst,fractal_dimension_worst

cols_mean <- c(
  "area_mean","radius_se","perimeter_se","area_se","smoothness_se","compactness_se",
  "symmetry_se","fractal_dimension_se","area_worst","fractal_dimension_worst"
)

# Reemplazar valores atípicos por la media o mediana
replace_outliers <- function(column, method = "mean") {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Calcular reemplazo según el método
  if (method == "mean") {
    replacement <- mean(column, na.rm = TRUE)
  } else if (method == "median") {
    replacement <- median(column, na.rm = TRUE)
  } else {
    stop("Método no reconocido. Usa 'mean' o 'median'.")
  }
  
  # Reemplazar valores atípicos
  column[column < lower_limit | column > upper_limit] <- replacement
  return(column)
}

df_cancer_5[cols_mean] <- lapply(df_cancer_5[cols_mean], replace_outliers, method = "mean")

sapply(df_cancer_5, contar_outliers)

#de nuevo para ver que cantidad de usurios tenemos con outlieres
matriz_outliers_5 <- sapply(df_cancer_5, es_outlier)
#Sumamos
df_cancer_5$total_outliers <- rowSums(matriz_outliers_5, na.rm = TRUE)
#Sumamos los ususairios que cumplan con outliers > 5
total_usuarios_con_outliers <- sum(df_cancer_5$total_outliers, na.rm = TRUE)
total_usuarios_con_outliers

#outliers sadisfactoriamente corregidos

df_cancer_pruebas <- df_cancer[df_cancer$total_outliers <= 5, ]

cols_pruebas <- c( "radius_mean", "texture_mean", "perimeter_mean",
  "area_mean","smoothness_mean","compactness_mean","concavity_mean","concave.points_mean",
  "symmetry_mean","fractal_dimension_mean","radius_se","texture_se",
  "perimeter_se","area_se","smoothness_se","compactness_se", "concavity_se","concave.points_se",
  "symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst",
  "area_worst","smoothness_worst","compactness_worst","concavity_worst","concave.points_worst",
  "symmetry_worst","fractal_dimension_worst"
)

df_cancer_pruebas[cols_pruebas] <- lapply(df_cancer_pruebas[cols_pruebas], replace_outliers, method = "median")
df_cancer_pruebas$total_outliers <- NULL

#de nuevo para ver que cantidad de usurios tenemos con outlieres
matriz_outliers_pruebas <- sapply(df_cancer_pruebas, es_outlier)
#Sumamos
df_cancer_pruebas$total_outliers <- rowSums(matriz_outliers_pruebas, na.rm = TRUE)
#Sumamos los ususairios que cumplan con outliers > 5
total_usuarios_con_outliers <- sum(df_cancer_pruebas$total_outliers, na.rm = TRUE)
total_usuarios_con_outliers

sapply(df_cancer_pruebas, contar_outliers)

#pasamos de obtener 608 datos con outlier de la tabla df_cancer 
# a 498 datos outlier con la tabla df_cancer_5
# y con 268 datos con outlier para la tabla df_pruebas asi que trabajaremos con estas dos


#================================================================================================
#Regrecion Logistica para df_cancer_5
#================================================================================================

#Definimos una sola seed
set.seed(123)
#factores
df_cancer_5$diagnosis <- factor(df_cancer_5$diagnosis, levels = c("B", "M"))

# Selección de variables para el modelo

model_df <- df_cancer_5[,c( "diagnosis",
  "radius_mean", "texture_mean", "perimeter_mean",
  "area_mean","smoothness_mean","compactness_mean","concavity_mean","concave.points_mean",
  "symmetry_mean","fractal_dimension_mean","radius_se","texture_se",
  "perimeter_se","area_se","smoothness_se","compactness_se", "concavity_se","concave.points_se",
  "symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst",
  "area_worst","smoothness_worst","compactness_worst","concavity_worst","concave.points_worst",
  "symmetry_worst","fractal_dimension_worst"
)]

#Divicion de datos de entrenamiento y test

ind <- sample(seq_len(nrow(model_df)), size = 0.7 * nrow(model_df))

train_data <- model_df[ind, ]
test_data  <- model_df[-ind, ]

modelo_logit <- glm(
  diagnosis ~ .,
  data = train_data,
  family = binomial
)

# Predicciones
logit_prob <- predict(
  modelo_logit,
  newdata = test_data,
  type = "response"
)


pred_test <- ifelse(logit_prob > 0.5, "M", "B")
pred_test <- factor(pred_test, levels = c("B","M"))

table(Predicho = pred_test, Real = test_data$diagnosis)
mean(pred_test == test_data$diagnosis)


#================================================================================================
#Regrecion Logistica para df_cancer_prueba
#================================================================================================

#factores
df_cancer_pruebas$diagnosis <- factor(df_cancer_pruebas$diagnosis, levels = c("B", "M"))

# Selección de variables para el modelo

model_df_pruebas <- df_cancer_pruebas[,c( "diagnosis",
                            "radius_mean", "texture_mean", "perimeter_mean",
                            "area_mean","smoothness_mean","compactness_mean","concavity_mean","concave.points_mean",
                            "symmetry_mean","fractal_dimension_mean","radius_se","texture_se",
                            "perimeter_se","area_se","smoothness_se","compactness_se", "concavity_se","concave.points_se",
                            "symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst",
                            "area_worst","smoothness_worst","compactness_worst","concavity_worst","concave.points_worst",
                            "symmetry_worst","fractal_dimension_worst"
)]

#Divicion de datos de entrenamiento y test

ind_pruebas <- sample(seq_len(nrow(model_df_pruebas)), size = 0.7 * nrow(model_df_pruebas))

train_data_pruebas <- model_df_pruebas[ind_pruebas, ]
test_data_pruebas  <- model_df_pruebas[-ind_pruebas, ]

modelo_logit_pruebas <- glm(
  diagnosis ~ .,
  data = train_data_pruebas,
  family = binomial
)

# Predicciones
logit_prob_pruebas <- predict(
  modelo_logit_pruebas,
  newdata = test_data_pruebas,
  type = "response"
)


pred_test_pruebas <- ifelse(logit_prob_pruebas > 0.5, "M", "B")
pred_test_pruebas <- factor(pred_test_pruebas, levels = c("B","M"))

table(Predicho = pred_test_pruebas, Real = test_data_pruebas$diagnosis)
mean(pred_test_pruebas == test_data_pruebas$diagnosis)

# ======================================================================
# Evaluación del modelo
# ======================================================================
library(caret)
library(pROC)

# Matriz de confusión
confusionMatrix(
  pred_test,
  test_data$diagnosis,
  positive = "M"
)

# ROC y AUC
logit_roc <- roc(test_data$diagnosis, logit_prob)
logit_auc <- auc(logit_roc)

cat("AUC Regresión Logística:", logit_auc, "\n")

# Curva ROC
plot(logit_roc, col = "blue", lwd = 2)

# ======================================================================
# SVM
# ======================================================================
library(e1071)

modelo_svm <- svm(diagnosis ~  ., data = train_data,
                  kernel = "linear",
                  scale = TRUE,
                  probability = TRUE)

svm_pred <- predict(modelo_svm, test_data)

# Matriz de confusión
confusionMatrix(svm_pred,test_data$diagnosis,
  positive = "M"
)

# Probabilidades para ROC
pred_svm_prob <- attr(
  predict(modelo_svm, test_data, probability = TRUE),
  "probabilities"
)[, "M"]

# ROC y AUC
svm_roc <- roc(test_data$diagnosis, pred_svm_prob)
svm_auc <- auc(svm_roc)

cat("AUC SVM:", svm_auc, "\n")

# Curva ROC
plot(svm_roc, col = "red", lwd = 2)

