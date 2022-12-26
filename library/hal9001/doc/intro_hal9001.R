## ----sim-data-----------------------------------------------------------------
library(data.table)
library(ggplot2)
# simulation constants
set.seed(467392)
n_obs <- 500
n_covars <- 3

# make some training data
x <- replicate(n_covars, rnorm(n_obs))
y <- sin(x[, 1]) + sin(x[, 2]) + rnorm(n_obs, mean = 0, sd = 0.2)

# make some testing data
test_x <- replicate(n_covars, rnorm(n_obs))
test_y <- sin(x[, 1]) + sin(x[, 2]) + rnorm(n_obs, mean = 0, sd = 0.2)

## ----sim-view-----------------------------------------------------------------
head(x)
head(y)

## -----------------------------------------------------------------------------
library(hal9001)

## ----fit-hal-glmnet-----------------------------------------------------------
hal_fit <- fit_hal(X = x, Y = y)
hal_fit$times

## ----results-hal-glmnet-------------------------------------------------------
print(summary(hal_fit))

## ----eval-mse-----------------------------------------------------------------
# training sample prediction for HAL vs HAL9000
mse <- function(preds, y) {
    mean((preds - y)^2)
}

preds_hal <- predict(object = hal_fit, new_data = x)
mse_hal <- mse(preds = preds_hal, y = y)
mse_hal

## ----eval-oob-----------------------------------------------------------------
oob_hal <- predict(object = hal_fit, new_data = test_x)
oob_hal_mse <- mse(preds = oob_hal, y = test_y)
oob_hal_mse

## ----fit-hal-reduced----------------------------------------------------------
hal_fit_reduced <- fit_hal(X = x, Y = y, reduce_basis = 0.1)
hal_fit_reduced$times

## ----results-hal-reduced------------------------------------------------------
summary(hal_fit_reduced)$table

## -----------------------------------------------------------------------------
set.seed(98109)
num_knots <- 100 # Try changing this value to see what happens.
n_covars <- 1
n_obs <- 250
x <- replicate(n_covars, runif(n_obs, min = -4, max = 4))
y <- sin(x[, 1]) + rnorm(n_obs, mean = 0, sd = 0.2)
ytrue <- sin(x[, 1])

hal_fit_0 <- fit_hal(
  X = x, Y = y, smoothness_orders = 0, num_knots = num_knots
)

hal_fit_smooth_1 <- fit_hal(
  X = x, Y = y, smoothness_orders = 1, num_knots = num_knots
)

hal_fit_smooth_2_all <- fit_hal(
  X = x, Y = y, smoothness_orders = 2, num_knots = num_knots, 
  fit_control = list(cv_select = FALSE)
)

hal_fit_smooth_2 <- fit_hal(
  X = x, Y = y, smoothness_orders = 2, num_knots = num_knots
)

pred_0 <- predict(hal_fit_0, new_data = x)
pred_smooth_1 <- predict(hal_fit_smooth_1, new_data = x)
pred_smooth_2 <- predict(hal_fit_smooth_2, new_data = x)

pred_smooth_2_all <- predict(hal_fit_smooth_2_all, new_data = x)
dt <- data.table(x = as.vector(x))
dt <- cbind(dt, pred_smooth_2_all)
long <- melt(dt, id = "x")
ggplot(long, aes(x = x, y = value, group = variable)) + geom_line()

## -----------------------------------------------------------------------------
mean((pred_0 - ytrue)^2)
mean((pred_smooth_1- ytrue)^2)
mean((pred_smooth_2 - ytrue)^2)

dt <- data.table(x = as.vector(x),
                 ytrue = ytrue,
                 y = y,
                 pred0 = pred_0,
                 pred1 = pred_smooth_1,
                 pred2 = pred_smooth_2)
long <- melt(dt, id = "x")
ggplot(long, aes(x = x, y = value, color = variable)) + geom_line()
plot(x, pred_0, main = "Zero order smoothness fit")
plot(x, pred_smooth_1, main = "First order smoothness fit")
plot(x, pred_smooth_2, main = "Second order smoothness fit")

## -----------------------------------------------------------------------------
set.seed(98109)
n_covars <- 1
n_obs <- 250
x <- replicate(n_covars, runif(n_obs, min = -4, max = 4))
y <- sin(x[, 1]) + rnorm(n_obs, mean = 0, sd = 0.2)
ytrue <- sin(x[, 1])

hal_fit_0 <- fit_hal(
  X = x, Y = y, smoothness_orders = 0, num_knots = 100
)
hal_fit_smooth_1 <- fit_hal(
  X = x, Y = y, smoothness_orders = 1, num_knots = 100
)

hal_fit_smooth_2 <- fit_hal(
  X = x, Y = y, smoothness_orders = 2, num_knots = 100
)

pred_0 <- predict(hal_fit_0, new_data = x)
pred_smooth_1 <- predict(hal_fit_smooth_1, new_data = x)
pred_smooth_2 <- predict(hal_fit_smooth_2, new_data = x)

## -----------------------------------------------------------------------------
mean((pred_0 - ytrue)^2)
mean((pred_smooth_1- ytrue)^2)
mean((pred_smooth_2 - ytrue)^2)
plot(x, pred_0, main = "Zero order smoothness fit")
plot(x, pred_smooth_1, main = "First order smoothness fit")
plot(x, pred_smooth_2, main = "Second order smoothness fit")

## -----------------------------------------------------------------------------
set.seed(98109)
num_knots <- 100

n_obs <- 500
x1 <-  runif(n_obs, min = -4, max = 4)
x2 <- runif(n_obs, min = -4, max = 4)
A <- runif(n_obs, min = -4, max = 4)
X <- data.frame(x1 = x1, x2 = x2, A = A)
Y <- rowMeans(sin(X)) + rnorm(n_obs, mean = 0, sd = 0.2)

## -----------------------------------------------------------------------------
# The `h` function is used to specify the basis functions for a given term
# h(x1) generates one-way basis functions for the variable x1
# This is an additive model:
formula <- ~h(x1) + h(x2) + h(A)
#We can actually evaluate the h function as well. We need to specify some tuning parameters in the current environment:
smoothness_orders <- 0
num_knots <- 10
# It will look in the parent environment for `X` and the above tuning parameters
form_term <- h(x1) + h(x2) + h(A)
form_term$basis_list[[1]]
# We don't need the variables in the parent environment if we specify them directly:
rm(smoothness_orders)
rm(num_knots)
# `h` excepts the arguments `s` and `k`. `s` stands for smoothness and is equivalent to smoothness_orders in use. `k` specifies the number of knots. ` 
form_term_new <- h(x1, s = 0, k = 10) + h(x2, s = 0, k = 10) + h(A, s = 0, k = 10)
# They are the same!
length( form_term_new$basis_list) == length(form_term$basis_list)

#To evaluate a unevaluated formula object like:
formula <- ~h(x1) + h(x2) + h(A)
# we can use the formula_hal function:
formula <- formula_hal(
  ~ h(x1) + h(x2) + h(A), X = X, smoothness_orders = 1, num_knots = 10
)
# Note that the arguments smoothness_orders and/or num_knots will not be used if `s` and/or `k` are specified in `h`.
formula <- formula_hal(
  Y ~ h(x1, k=1) + h(x2,  k=1) + h(A, k=1), X = X, smoothness_orders = 1, num_knots = 10
)
 
 
 
 

## -----------------------------------------------------------------------------
smoothness_orders <- 1
num_knots <- 5
# A additive model
colnames(X)
# Shortcut:
formula1 <- h(.)
# Longcut:
formula2 <- h(x1) + h(x2) + h(A)
# Same number of basis functions
length(formula1$basis_list ) == length(formula2$basis_list)

# Maybe we only want an additive model for x1 and x2
# Use the `.` argument
formula1 <- h(., . = c("x1", "x2"))
formula2 <- h(x1) + h(x2) 
length(formula1$basis_list ) == length(formula2$basis_list)


## -----------------------------------------------------------------------------
#  Two way interactions
formula1 <-  h(x1) + h(x2)  + h(A) + h(x1, x2)
formula2 <-  h(.)  + h(x1, x2)
length(formula1$basis_list ) == length(formula2$basis_list)
#
formula1 <-  h(.) + h(x1, x2)    + h(x1,A) + h(x2,A)
formula2 <-  h(.)  + h(., .)
length(formula1$basis_list ) == length(formula2$basis_list)

#  Three way interactions
formula1 <-  h(.) + h(.,.)    + h(x1,A,x2)  
formula2 <-  h(.)  + h(., .)+ h(.,.,.)  
length(formula1$basis_list ) == length(formula2$basis_list)

 

## -----------------------------------------------------------------------------
# Write it all out
formula <-  h(x1) + h(x2) + h(A) + h(A, x1) + h(A,x2)
 

# Use the "h(.)" which stands for add all additive terms and then manually add
# interactions
formula <- y ~ h(.) + h(A,x1) + h(A,x2)

 

# Use the "wildcard" feature for when "." is included in the "h()" term. This
# useful when you have many variables and do not want to write out every term.
formula <-  h(.) + h(A,.) 


formula1 <-   h(A,x1) 
formula2 <-   h(A,., . = c("x1")) 
 length(formula1$basis_list) == length(formula2$basis_list)

## -----------------------------------------------------------------------------
# An additive monotone increasing model
formula <- formula_hal(
  y ~ h(., monotone = "i"), X, smoothness_orders = 0, num_knots = 100
)

# An additive unpenalized monotone increasing model (NPMLE isotonic regressio)
# Set the penalty factor argument `pf` to remove L1 penalization
formula <- formula_hal(
  y ~ h(., monotone = "i", pf = 0), X, smoothness_orders = 0, num_knots = 100
)

# An additive unpenalized convex model (NPMLE convex regressio)
# Set the penalty factor argument `pf` to remove L1 penalization
# Note the second term is equivalent to adding unpenalized and unconstrained main-terms (e.g. main-term glm)
formula <- formula_hal(
  ~ h(., monotone = "i", pf = 0, k=200, s=1) + h(., monotone = "none", pf = 0, k=1, s=1), X)
 
# A bi-additive monotone decreasing model
formula <- formula_hal(
  ~ h(., monotone = "d") + h(.,., monotone = "d"), X, smoothness_orders = 1, num_knots = 100
)
 

 
 

## -----------------------------------------------------------------------------
# Additive glm
# One knot (at the origin) and first order smoothness
formula <- h(., s = 1, k = 1, pf = 0)
# Running HAL with this formula will be equivalent to running glm with the formula Y ~ .

# intraction glm
formula <- h(., ., s = 1, k = 1, pf = 0) + h(., s = 1, k = 1, pf = 0)
# Running HAL with this formula will be equivalent to running glm with the formula Y ~ .^2


## -----------------------------------------------------------------------------
# get formula object
fit <- fit_hal(
  X = X, Y = Y, formula = ~ h(.), smoothness_orders = 1, num_knots = 100
)
print(summary(fit), 10) # prints top 10 rows, i.e., highest absolute coefs
plot(predict(fit, new_data = X), Y)

