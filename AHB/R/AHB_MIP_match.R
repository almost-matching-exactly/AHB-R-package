# author: Xian Sun, Duke University, 2020



#'A MIP Implementation of AHB Matching
#'
#'\code{AHB_MIP_match} runs a MIP Implementation of AHB Matching
#'
#'@section Introduction: \code{AHB_MIP_match} is a matching algorithm that
#'  matches units with others in unit-specific, hyper-box-shaped regions of the
#'  covariate space.These regions are large enough that many matches are created
#'  for each unit and small enough that the treatment effect is roughly constant
#'  throughout. The regions are found as the solution to using a Mixed Integer
#'  Program algorithm. For more details, please see the AHB paper
#'  \href{https://arxiv.org/pdf/2003.01805.pdf}{here}.
#'
#'@param data Either a data frame or a path to a .csv file to be read. If
#'  holdout is not a numeric value, this is the data to be matched. If holdout
#'  is a numeric scalar between 0 and 1, that proportion of data will be made
#'  into a holdout set and only the remaining proportion of data will be
#'  matched. Covariates can either be categorical, continuous or mixed
#'  (categorical and continuous). Outcome must be either binary or continuous
#'  (both numeric) with name \code{outcome_column_name} . Treatment must be
#'  described by a logical or binary column with name
#'  \code{treated_column_name}.
#'
#'@param holdout A data frame, a path to a .csv file to be read or a numeric
#'  scalar between 0 and 1 .  If a numeric scalar, that proportion of data will
#'  be made into a holdout set and only the remaining proportion of data will be
#'  matched. Otherwise, a dataframe or a path to a .csv file. Must have the same
#'  column names and order as data. This data will not be matched. Covariates
#'  can either be categorical, continuous or mixed (categorical and
#'  continuous).Outcome must be either binary or continuous (both numeric) with
#'  name \code{outcome_column_name} . Treatment must be described by a logical
#'  or binary column with name \code{treated_column_name}.
#'
#'@param treated_column_name A character with the name of the treatment column
#'  in data and holdout. Defaults to 'treated'.
#'
#'@param outcome_column_name A character with the name of the outcome column in
#'  holdout and also in data, if supplied in the latter. Defaults to 'outcome'.
#'
#'@param black_box Denotes the method to be used to generate outcome model Y.
#' If "BART", use bartMachine as ML model to do prediction.
#' If "xgb, use xgboost as ML model to do prediction.
#'  Defaults to '"xgb"'.
#'
#'@param cv A logical scalar. If \code{TURE}, do cross-validation on the train
#'  set to generate outcome model Y . Defaults to \code{TRUE}.
#'
#'@param gamma0 A numeric value, one of hyperparameters in global MIP that
#'  controls the weight placed on the outcome function portion of the loss.
#'  Defaults to 3.
#'
#'@param gamma1 A numeric value, one of hyperparameters in global MIP that
#'  controls the weight placed on the outcome function portion of the loss.
#'  Defaults to 3.
#'
#'@param beta A numeric value, one of hyperparameters in global MIP that
#'  controls the weight placed on the number of units in the box. Defaults to 2.
#'
#'@param m A numeric value, the at least number of control units that the box
#'  contains when estimating causal effects for a single treatment unit.
#'  Defaults to 1.
#'
#'@param M A numeric value, a large positive constant that controls the weight
#'  placed on decision variable wij, which is an indicator for whether a unit is
#'  in the box.Defaults to 1e5.
#'
#'@param n_prune A numeric value, the number of candidate units selected to run
#'  the mip on for constructing the box. Dataset mentioned below is refered to
#'  the dataset for matching. If you match a small dataset with the number of
#'  units smaller than 400, it will run MIP on all dataset for each treated
#'  unit. If you match larger dataset and your memory of your computer cannot
#'  support such much computation, plase adjust n_prune below 400 or even
#'  smaller. The smaller number of candidate units selected to run the mip on
#'  for constructing the box, the faster this program runs.Defualt n_prune =
#'  0.1* nrow(dataset).
#'
#'@param MIP_solver Denotes the method to be used to solve MIP problem.
#'  Optional, if "Rcplex", use Rcplex as solver, which is faster than Rglpk.
#'  if "Rglpk", use Rglpk as solver, which is free and easy to be installed.
#'
#'@return The basic object returned by \code{AHB_MIP_match} is a list of 5
#'  entries: \describe{\item{data}{Data set was matched by \code{AHB_MIP_match}.
#'  If holdout is not a numeric value, the \code{AHB_MIP_out$data} is the same
#'  as the data input into \code{AHB_MIP_match}.  If holdout is a numeric scalar
#'  between 0 and 1, \code{AHB_MIP_out$data} is the remaining proportion of data
#'  that were matched.} \item{units_id}{A integer vector with unit_id for test
#'  treated units} \item{CATE}{A numeric vector with the conditional average
#'  treatment effect estimates for every test treated unit in its matched group
#'  in \code{MGs}} \item{bins}{ An array of two lists where the first one
#'  contains lower bounds and the other contains upper bounds for each test
#'  treated unit. Each row of each list is a vector corresponding to a test
#'  treated unit.} \item{MGs}{A list of all the matched groups formed by
#'  AHB_fast_match. For each test treated unit, each row contains all unit_id of
#'  the other units that fall into its box, including itself. } }

#'@importFrom stats  predict rbinom rnorm var formula runif sd
#'@importFrom utils combn flush.console read.csv
#'@export


AHB_MIP_match<-function(data,
                        holdout = 0.1,
                        treated_column_name = 'treated',
                        outcome_column_name = 'outcome',
                        black_box="xgb",
                        cv=F,
                        gamma0=3,
                        gamma1=3,
                        beta=2,
                        m=1,
                        M=1e5,
                        n_prune = ifelse(is.numeric(holdout),
                                         round(0.1*(1-holdout) * nrow(data)),
                                         round(0.1*nrow(data))),
                        MIP_solver = "Rglpk"
                        ){

  df <- read_data(data = data, holdout = holdout,
                  treated_column_name = treated_column_name, outcome_column_name = outcome_column_name)

  check_args_MIP(data = df[[1]], holdout = df[[2]],
                 treated_column_name = treated_column_name, outcome_column_name = outcome_column_name,
                 black_box = black_box, cv = cv, gamma0 = gamma0, gamma1 = gamma1, beta = beta, m = m, M = M)

  # get outcome model with train set
  inputs <- estimator_inputs(train_df = df[[2]], test_df = df[[1]],
                             treated_column_name= treated_column_name, outcome_column_name=outcome_column_name,
                             black_box =  black_box, cv = cv)
  f = inputs[[1]]
  n = inputs[[2]]
  n_train = inputs[[3]]
  p = inputs[[4]]
  train_df = inputs[[5]]
  train_covs = inputs[[6]]
  train_control = inputs[[7]]
  train_treated= inputs[[8]]
  test_df = inputs[[9]]
  test_covs = inputs[[10]]
  test_control = inputs[[11]]
  test_treated = inputs[[12]]
  n_test_control = inputs[[13]]
  n_test_treated = inputs[[14]]
  bart_fit0 = inputs[[15]]
  bart_fit1 = inputs[[16]]


  ind_treated <- which(colnames(test_df) == treated_column_name)
  ind_outcome <- which(colnames(test_df) == outcome_column_name)
  ind_integer <- c()
  ind_other <- c()

  #get the indexes of types of covariates to add different tolerance to them
  count <- 1
  for(x in 1:ncol(test_df)){
    if(x != ind_treated  && x != ind_outcome){
      if(all.equal(test_df[,x], as.integer(test_df[,x])) == TRUE){
        ind_integer <- append(ind_integer, count)
        count <- count + 1
      }
      else{
        ind_other <- append(ind_other, count)
        count <- count + 1
      }
    }
  }


  ## BART
  # fhat1 = colMeans(predict(bart_fit1, newdata = test_covs))
  # fhat0 = colMeans(predict(bart_fit0, newdata = test_covs))
  # fhat1 = predict(bart_fit1, test_covs)
  # fhat0 = predict(bart_fit0, test_covs)
  fhat1 = 0
  fhat0 = 0
  if (black_box=='xgb' | black_box=='LASSO'){
    fhat1 = predict(bart_fit1, as.matrix(test_covs))
    fhat0 = predict(bart_fit0, as.matrix(test_covs))
  }
  else if(black_box=='BART'){
    fhat1 = predict(bart_fit1, test_covs)
    fhat0 = predict(bart_fit0, test_covs)
  }
  # print(fhat1)
  # print(fhat0)
  #MIP
  mip_cates = numeric(n_test_treated[1])
  mip_bins = array(NA, c(n_test_treated, p, 2))
  mip_groups = list()
  sol = NULL

  message("Running AHB_MIP_matching")

  if(n_prune >= 200){
    message(paste0("If there is an error indicating that momery is not enough or your computer
gets stuck, please adjust the parameter 'n_prune' into smaller values.
For now, n_prune = ", n_prune, ". Try to set n_prune below 400 or even smaller"))}

  start_time <- Sys.time()
  for (l in 1:n_test_treated){
    i = test_treated[l]
    message(paste("Matching unit", l, "of", n_test_treated), "\r", appendLF = FALSE)
    test_df_treated = test_df[,which(colnames(test_df) == treated_column_name)]
    test_df_outcome = test_df[,which(colnames(test_df) == outcome_column_name)]

    #Do preprocessing, only do MIP on varibales with most similar outcome
    ord = order(abs(fhat1[i] - fhat1) * gamma1/sd(fhat1) + abs(fhat0[i] - fhat0) * gamma0/sd(fhat0))
    cands = c(ord[test_df$treated[ord]==1][2:n_prune], ord[test_df$treated[ord]==0][1:n_prune])
    cands = cands[cands != i]

    #do MIP without preprocessing
    if(nrow(test_df) <= 400){cands = -i}

    mip_pars =  setup_miqp_fhat(xi = as.numeric(test_covs[i, ]),
                                x_test = as.matrix(test_covs[cands, ]),
                                z_test = test_df_treated[cands],
                                fhati1=fhat1[i],
                                fhati0=fhat0[i],
                                fhat1=fhat1[cands],
                                fhat0=fhat0[cands],
                                alpha=0, lambda0=0, lambda1=0,
                                gamma0=gamma0/sd(fhat0), gamma1=gamma1/sd(fhat1),
                                beta=beta, m=m, M=M)

    if(MIP_solver == "Rcplex"){
      #The following will enbale users to install MIP_solver optionally
      if (!requireNamespace("Rcplex", quietly = TRUE)) {
           warning("The Rcplex package must be installed if you use Rcplex solver")
           return(NULL)
       }
      sol <- do.call(Rcplex::Rcplex, c(mip_pars, list(objsense="max", control=list(trace=0))))
      sol <- sol$xopt
    }

    if(MIP_solver == "Rglpk"){
      if (!requireNamespace("Rglpk", quietly = TRUE)) {
           warning("The Rglpk package must be installed if you use Rglpk solver")
           return(NULL)
       }
      dir = mip_pars$sense
      dir[which(dir =="G")] <- ">="
      dir[which(dir =="L")] <- "<="
      #get bounds
      ind <- 1:length(mip_pars$cvec)
      bounds <- list(lower = list(ind = ind, val = mip_pars$lb), upper = list(ind = ind, val = mip_pars$ub))
      sol <-Rglpk::Rglpk_solve_LP(obj = mip_pars$cvec, mat = mip_pars$Amat, dir = dir, rhs = mip_pars$bvec,
                           bounds = bounds, types = mip_pars$vtype, max = TRUE,control=list(verbose=FALSE, presolve=TRUE))
      sol <- sol$solution
    }

    mip_out = recover_pars(sol, n_train, nrow(test_covs), p)

    mip_bins[l, ,1] = mip_out$a
    mip_bins[l, ,2] = mip_out$b
    #Add tolerance to different types
    # add 1e-5 to integer type
    if(length(ind_integer)!=0){
      mip_bins[l,ind_integer,1] = mip_bins[l,ind_integer,1]- 1e-5
      mip_bins[l,ind_integer,2] = mip_bins[l,ind_integer,2]+ 1e-5
    }
    # add 1e-10 to other type
    if(length(ind_other)!=0){
      mip_bins[l, ind_other,1] = mip_bins[l, ind_other,1] - 1e-10
      mip_bins[l, ind_other,2] = mip_bins[l, ind_other,2] + 1e-10
    }

    mg = make_mg(test_covs, mip_bins[l, ,1], mip_bins[l, ,2])
    mg = as.integer(mg)
    mip_groups[[l]] = mg
    mip_cates[l] = mean(test_df_outcome[mg][test_df_treated[mg]]) - mean(test_df_outcome[mg][!test_df_treated[mg]])
  }
  end_time <- Sys.time()
  t = difftime(end_time, start_time, units = "auto")
  message(paste0("Time to match ", length(test_treated), " units: ", format(round(t, 2), nsmall = 2)))

  return(list(data = test_df,units_id = test_treated, CATE = mip_cates, bins = mip_bins, MGs = mip_groups,verbose = c(treated_column_name,outcome_column_name)))
}
