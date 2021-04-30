test_that("check_hpyerparameter_fast", {
  expect_error(check_hpyerparameter_fast("black_box",T, 0.1))
  expect_error(check_hpyerparameter_fast("BART",1, 0.1))
  expect_error(check_hpyerparameter_fast("BART",F, "0.1"))
  check_hpyerparameter_fast("BART",F, 0.1)
  check_hpyerparameter_fast("xgb",T, 0.1)
})


test_that("check_hpyerparameter_MIP", {
  PE_method = "BART"
  cv=F
  gamma0=3
  gamma1=3
  beta=2
  m=1
  M=1e5
  MIP_solver = "Rglpk"
  expect_error(check_hpyerparameter_MIP("PE_method",cv,gamma0,gamma1,beta,m,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,"cv",gamma0,gamma1,beta,m,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,1,gamma0,gamma1,beta,m,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,cv,"s",gamma1,beta,m,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,cv,gamma0,"gamma1",beta,m,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,"beta",m,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,beta,0.1,M,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,beta,m,1,MIP_solver))
  expect_error(check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,beta,m,M,"safdasf"))
  check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,beta,m,M,"Rcplex")
  check_hpyerparameter_MIP(PE_method,cv,gamma0,gamma1,beta,m,M,"Rglpk")
})




test_that("check_dataset_type", {
  data<-gen_data()
  holdout<-gen_data()
  check_dataset_type(data, "treated", "outcome")
  expect_error( check_dataset_type(data,"asdf", "outcome"))
  expect_error( check_dataset_type(data,"treated", "sa"))
})

test_that("check_args_dataset", {
  data<-gen_data()
  holdout<-gen_data()
  expect_error(check_args_dataset(data,holdout,"safdadsf", "outcome"))
  expect_error(check_args_dataset(data,holdout,123, "outcome"))
  expect_error(check_args_dataset(data,holdout,"treated", 123))
  expect_error(check_args_dataset(data,holdout,"treated", "asdf"))
  data[,"treated"]<-21
  expect_error(check_args_dataset(data,holdout,"treated", "oucome"))
  data<-gen_data()
  data[,"outcome"]<-"2"
  expect_error(check_args_dataset(data,holdout,"treated", "oucome"))
})

test_that("check_users_PE", {
  check_users_PE("f",list(),"f",list())
  expect_error(check_users_PE(NULL,list(),"Asf",list()))
  expect_error(check_users_PE("a",list(),NULL,list()))
  expect_error(check_users_PE("a",1,"v",list()))
  expect_error(check_users_PE("a",list(),"v",1))
})





