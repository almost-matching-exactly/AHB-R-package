# for(i in 1:10){
#   MIP_out <- AHB_MIP_match(data = data, holdout = 0.5, cv = T)
#   fast_out <- AHB_fast_match(data = data, holdout = 0.5, cv = T)
#   print(ATE(MIP_out))
#   print(ATT(MIP_out))
#   print(ATE(fast_out))
#   print(ATT(fast_out))
# }
#
# set.seed(11)
# n  = 200
# p = 5
# X = data.frame(matrix(runif(n * p), ncol = p))
# y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#
# ##build BART regression model
# k_folds <- 5
# k_cvs<- c(2, 3, 5)
# num_tree_cvs<- c(100, 200)
# nu_q_cvs<-list(c(3, 0.9), c(3, 0.99), c(10, 0.75))
#
#
# # bart_machine_cv = bartMachineCV(X, y,k_folds = k_folds, k_cvs = k_cvs, num_tree_cvs =num_tree_cvs, nu_q_cvs = nu_q_cvs, verbose = False)
# bart_machine = bartMachine(X,y, num_trees = 200, num_burn_in = 500,num_iterations_after_burn_in = 1000)
#
# f0 = predict(bart_machine, new_data = X)
#
#
# bart_fit0<- dbarts::bart(x.train = X,
#              y.train = y,
#              keeptrees = TRUE,
#              keepevery = 10,
#              verbose = FALSE,
#              ntree = 200)
#
# fhat0 = colMeans(predict(bart_fit0, newdata = X))
#
# for(i in 1:5){
#   p <- 4
#   data <- gen_data(n_units = 50, p = p)
#   # fast_out <- AHB_fast_match(data = data, holdout = data,cv = T,black_box = 'xgb')
#   # fast_out <- AHB_fast_match(data = data, holdout = data,cv = T,black_box = 'LASSO')
#   MIP_out <- AHB_MIP_match(data = data, holdout = data, cv = F,black_box = 'BART',MIP_solver = "Rcplex")
#   MIP_out <- AHB_MIP_match(data = data, holdout = data, cv = T,black_box = 'xgb',MIP_solver ="Rglpk")
# }

# library("glmnet")
# library("mvtnorm")
# ## generate the data
#
# # Loading the data
# data(swiss)
#
# x_vars <- model.matrix(Fertility~. , swiss)[,-1]
# y_var <- swiss$Fertility
#
#
# # Splitting the data into test and train
# set.seed(86)
# train = sample(1:nrow(x_vars), nrow(x_vars)/2)
# x_test = (-train)
# y_test = y_var[x_test]

# cv_output <- glmnet::cv.glmnet(x_vars[train,], y_var[train],
#                        alpha = 1, lambda = lambda_seq,
#                        nfolds = 5)
# # identifying best lamda
# best_lam <- cv_output$lambda.min
# best_lam
# lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = 100)
# pred <- predict(lasso_best, x_vars[x_test,])

