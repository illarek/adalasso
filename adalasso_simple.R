
library(tidyverse)
library(magrittr)
library(glmnet)
library(pROC)

data(QuickStartExample)
x_cont <- x
y_cont <- y

# ---------------------------------------------------------------------------------------------------------
## PROBANDO CON UNA RIDGE
ridge1 <- glmnet(x = x_cont, y = y_cont,
                 alpha = 0)
plot(ridge1, xvar = "lambda")

## Regresion Ridge 10-veces validacion cruzada
ridge1_cv <- cv.glmnet(x = x_cont, y = y_cont,
                       type.measure = "mse",
                       nfold = 10,
                       alpha = 0)

## Penalidad vs CV MSE plot
plot(ridge1_cv)

## Extrayendo coeficiente (lambda minimo)
ridge1_cv$lambda.min


## s: Valores de los parametros de penalidad
coef(ridge1_cv, s = ridge1_cv$lambda.min)

## La estimación de la intersección debe descartarse.
best_ridge_coef <- as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1]
best_ridge_coef


# ----------------------------------------------------------------------------------------
## Ejemplo de LASSO adaptativo
alasso1 <- glmnet(x = x_cont, y = y_cont,
                  alpha = 1,
                  penalty.factor = 1 / abs(best_ridge_coef))

plot(alasso1, xvar = "lambda")


## Ejemplo de LASSO adaptativo con 10-repeticiones - CV
alasso1_cv <- cv.glmnet(x = x_cont, y = y_cont,
                        type.measure = "mse",
                        K = 10,
                        nfold = 10,
                        alpha = 1,
                        penalty.factor = 1 / abs(best_ridge_coef),
                        keep = TRUE)

## Penalidad vs CV MSE plot
plot(alasso1_cv)

alasso1_cv$lambda.min

best_alasso_coef1 <- coef(alasso1_cv, s = alasso1_cv$lambda.min)
best_alasso_coef1


## R cuadrado
r_squared <- function(y, yhat) {
  ybar <- mean(y)
  ss_tot <- sum((y - ybar)^2)
  ss_res <- sum((y - yhat)^2)
  1 - (ss_res / ss_tot)
}

## n tamaño de la muestra, p numero de parametros
adj_r_squared <- function(r_squared, n, p) {
  1 - (1 - r_squared) * (n - 1) / (n - p - 1)
}

r_squared_alasso1 <- r_squared(as.vector(y_cont), as.vector(predict(alasso1, newx = x_cont, s = alasso1_cv$lambda.min)))
r_squared_alasso1

## R-cuadrado ajustado
adj_r_squared(r_squared_alasso1, n = nrow(y_cont), p = sum(as.vector(coef(alasso1, s = alasso1_cv$lambda.min)) > 0))

##  Conjunto de prueba de Cross-validated R^2
## alasso1_cv $ cvm [1] es el error cuadrático medio del conjunto de pruebas con validación cruzada del modelo de solo intercepción.
1 - alasso1_cv$cvm[alasso1_cv$lambda == alasso1_cv$lambda.min] / alasso1_cv$cvm[1]


lapply(unique(alasso1_cv$foldid), function(id) {
  fit <- glmnet(x = x_cont[alasso1_cv$foldid != id,],
                y = y_cont[alasso1_cv$foldid != id],
                alpha = 1,
                penalty.factor = 1 / abs(best_ridge_coef))
  y_pred <- predict(fit, newx = x_cont[alasso1_cv$foldid == id,], s = alasso1_cv$lambda.min)
  y <- y_cont[alasso1_cv$foldid == id]
  1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
}) %>%
  unlist %T>%
  print %>%
  
  mean
