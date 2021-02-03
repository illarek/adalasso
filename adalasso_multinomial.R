
library(tidyverse)
library(magrittr)
library(glmnet)
library(pROC)

data(MultinomialExample)
x_multi  <- x
y_multi  <- y

x_multi
y_multi

## Regresion RIDGE
ridge2 <- glmnet(x = x_multi, y = y_multi,
                 family = "multinomial",
                 alpha = 0)

plot(ridge2, xvar = "lambda")

## regresion RIDGE con 10-repeticiones CV
ridge2_cv <- cv.glmnet(x = x_multi, y = y_multi,
                       type.measure = "deviance",
                       K = 10,
                       nfold = 10,
                       family = "multinomial",
                       alpha = 0)

## Penalidad vs CV MSE plot
plot(ridge2_cv)

# Lambda que minimiza el error
ridge2_cv$lambda.min

# penalidades para cada X_i
best_ridge_coef2 <- do.call(cbind, coef(ridge2_cv, s = ridge2_cv$lambda.min))
best_ridge_coef2

best_ridge_weights2 <- 1 / abs(as.matrix(best_ridge_coef2)[-1,])
best_ridge_weights2


# LASSO ADAPTATIVO

alasso2 <- glmnet(x = x_multi, y = y_multi,
                  family = "multinomial",
                  alpha = 1,
                  penalty.factor = best_ridge_weights2,
                  type.multinomial = "grouped")

plot(alasso2, xvar = "lambda")


alasso2_cv <- cv.glmnet(x = x_multi, y = y_multi,
                        type.measure = "deviance",
                        K = 10,
                        nfold = 10,
                        family = "multinomial",
                        alpha = 1,
                        penalty.factor = best_ridge_weights2,
                        type.multinomial = "grouped",
                        keep = TRUE)

## Penalidad vs CV MSE plot
plot(alasso2_cv)

alasso2_cv$lambda.min
best_alasso_coef2 <- do.call(cbind, coef(alasso2_cv, s = alasso2_cv$lambda.min))
best_alasso_coef2


## Tasa de clasificación correcta del modelo final
y_multi_pred_class <- as.numeric(predict(alasso2, newx = x_multi, type = "class", s = alasso2_cv$lambda.min))
xtabs(~ y_multi_pred_class + y_multi)


##  Conjunto de prueba de Cross-validated R^2
lapply(unique(alasso2_cv$foldid), function(id) {
  ## Fit excluding test set (foldid == id)
  fit <- glmnet(x = x_multi[alasso2_cv$foldid != id,],
                y = y_multi[alasso2_cv$foldid != id],
                family = "multinomial",
                alpha = 1,
                penalty.factor = best_ridge_weights2,
                type.multinomial = "grouped")
  ## Test-set Y_hat using model fit at best lambda
  y_pred <- as.numeric(predict(fit, newx = x_multi[alasso2_cv$foldid == id,], type = "class",
                               s = alasso2_cv$lambda.min))
  ## Test-set Y
  y <- y_multi[alasso2_cv$foldid == id]
  ## Test-set CCR
  mean(y == y_pred)
}) %>%
  unlist %T>%
  print %>%
  mean