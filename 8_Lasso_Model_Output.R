library(dplyr)
library(caret)
library(fastDummies)
library(doParallel)
library(Metrics)
library(MASS)
library(glmnet)

# Script that estimates the Lasso Linear model on the entire data set
# NB:   - This script requires the workspace resulting from running (5) and
#         also requires 'linGrid', 'trcontrollin' and functions 'data_pp', 
#         'train_lin', 'transform_dummy' and 'normalise' from (7) to run properly
#         (it is not necessary to run the entire nested 10-fold CV in script (7))

# Auxiliary function for reporting output 
get_lasso_out <- function(lasso){
  print(round(log(lasso$bestTune, base = 10 ),2))
  coeff <- coef(lasso$finalModel, lasso$bestTune$lambda) %>% as.matrix() %>% round(., digits = 9)
  print(round(coeff,4))
}

set.seed(1913)

# Estimate extended model
data_full <- data_pp(df_full)

X_full <- data_full[,-c(1:2)] %>% as.matrix 
y_abs_full <- data_full$absolute_effect
y_rel_full <- data_full$relative_effect

lm_abs_full <- train_lin(y_abs_full, X_full, trcontrollin, linGrid)
lm_rel_full <- train_lin(y_rel_full, X_full, trcontrollin, linGrid)

# Extended Model - Absolute Effectiveness
get_lasso_out(lm_abs_full) 

# Extended Model - Relative Effectiveness
get_lasso_out(lm_rel_full)


# Estimate baseline model
data_base <- data_pp(df_base)

X_base <- data_base[,-c(1:2)] %>% as.matrix 
y_abs_base <- data_base$absolute_effect
y_rel_base <- data_base$relative_effect

lm_abs_base <- train_lin(y_abs_base, X_base, trcontrollin, linGrid)
lm_rel_base <- train_lin(y_rel_base, X_base, trcontrollin, linGrid)

# Baseline Model - Absolute Effectiveness
get_lasso_out(lm_abs_base)

# Baseline Model - Relative Effectiveness
get_lasso_out(lm_rel_base)




