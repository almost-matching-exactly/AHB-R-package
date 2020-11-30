
setup_miqp_fhat = function(xi, fhati1, fhati0, fhat0, fhat1, x_test, z_test,
                           lambda1=1, lambda0=0, alpha=1, beta=1, gamma1=1, gamma0=0, m=1, M=1e10){

  n_test = length(fhat1)
  p = length(xi)

  #Unit level mip:
  # Objective: max c'X
  cvec = c(alpha * rep(-1, p),                      # aij
           alpha * rep(1, p),                       # bij
           beta * rep(1, n_test) +
             - gamma1 * abs(fhati1 - fhat1)
           - gamma0 * abs(fhati0 - fhat0),           # wij
           rep(0, n_test * p),                      # qij
           rep(0, n_test * p)                       # rij
  )

  a_start = 0
  b_start = p
  w_start = b_start + p
  q_start = w_start + n_test
  r_start = q_start + n_test * p
  n_vars = length(cvec)

  Qmat = matrix(0, n_vars, n_vars)
  Qmat[(w_start+1):q_start, (w_start+1):q_start] =
    outer(fhat0, fhat0, FUN=function(y1, y2) - lambda0 * abs(y1 - y2)) +
    outer(fhat1, fhat1, FUN=function(y1, y2) - lambda1 * abs(y1 - y2))

  # Constraint 2 a_j < x_j
  a2 = matrix(0, p, n_vars)
  for (j in 1:p){
    a2[j, a_start + j] = 1
  }
  rownames(a2) = rep("C2", nrow(a2))
  b2 = xi
  names(b2) = rep("C2", length(b2))
  s2 = rep("L", p)

  # Constraint 3 -b_j < -x_j
  a3 =  matrix(0, p, n_vars)
  for (j in 1:p){
    a3[j, b_start + j] = 1
  }
  rownames(a3) = rep("C3", nrow(a3))
  b3 = xi
  names(b3) = rep("C3", length(b3))
  s3 = rep("G", p)

  ### Test set constraints ###
  # Constraint 10: qikj = 0 if xkj < aij, either 0 or 1 otherwise
  a10 = matrix(0, n_test * p, n_vars)
  b10 = rep(NA, n_test * p)
  l = 1
  for (k in 1:n_test){
    for (j in 1:p){
      a10[l, q_start + l] = M
      a10[l, a_start + j] = 1
      b10[l] = x_test[k, j] + M - 1/M
      l = l + 1
    }
  }
  rownames(a10) = rep("C10", nrow(a10))
  names(b10) = rep("C10", length(b10))
  s10 = rep("L", n_test * p)

  # Constraint 11: qikj = 1 if xkj > aij, either 0 or 1 otherwise
  a11 = matrix(0, n_test * p, n_vars)
  b11 = rep(NA, n_test * p)
  l = 1
  for (k in 1:n_test){
    for (j in 1:p){
      a11[l, q_start + l] = -M
      a11[l, a_start + j] = -1
      b11[l] = - x_test[k, j] - 1/M
      l = l + 1
    }
  }
  rownames(a11) = rep("C11", nrow(a11))
  names(b11) = rep("C11", length(b11))
  s11 = rep("L", n_test * p)

  # Constraint 12: rikj = 0 if xkj > bij, either 0 or 1 otherwise
  a12 = matrix(0, n_test * p, n_vars)
  b12 = rep(NA, n_test * p)
  l = 1
  for (k in 1:n_test){
    for (j in 1:p){
      a12[l, r_start + l] = M
      a12[l, b_start + j] = -1
      b12[l] = -x_test[k, j] + M - 1/M
      l = l + 1
    }
  }
  rownames(a12) = rep("C12", nrow(a12))
  names(b12) = rep("C12", length(b12))
  s12 = rep("L", n_test * p)

  # Constraint 13: rikj = 1if xkj < bij, either 0 or 1 otherwise
  a13 = matrix(0, n_test * p, n_vars)
  b13 = rep(NA, n_test * p)
  l = 1
  for (k in 1:n_test){
    for (j in 1:p){
      a13[l, r_start + l] = -M
      a13[l, b_start + j] = 1
      b13[l] = x_test[k, j] - 1/M
      l = l + 1
    }
  }
  rownames(a13) = rep("C13", nrow(a13))
  names(b13) = rep("C13", length(b13))
  s13 = rep("L", n_test * p)

  # Constraint 14: wik = 1 if sum over j uijk + sum over j vijk = 4P,
  # either 0 or 1 otherwise
  a14 = matrix(0, n_test,  n_vars)
  b14 = rep(NA, n_test)
  for (k in 1:n_test){
    a14[k, w_start + k] = -M
    a14[k, (r_start + 1 + p*(k-1)):(r_start + p*k)] = 1
    a14[k, (q_start + 1 + p*(k-1)):(q_start + p*k)] = 1
    b14[k] = 2 * p - 1
  }
  rownames(a14) = rep("C14", nrow(a14))
  names(b14) = rep("C14", length(b14))
  s14 = rep("L", n_test)

  # Constraint 15: wik = 0 if sum over j uijk + sum over j vijk < 2P,
  # either 0 or 1 otherwise
  a15 = matrix(0, n_test, n_vars)
  b15 = rep(NA, n_test)
  for (k in 1:n_test){
    a15[k, w_start + k] = M
    a15[k, (r_start + 1 + p*(k-1)):(r_start + p*k)] = -1
    a15[k, (q_start + 1 + p*(k-1)):(q_start + p*k)] = -1
    b15[k] = -2 * p + M
  }
  rownames(a15) = rep("C15", nrow(a15))
  names(b15) = rep("C15", length(b15))
  s15 = rep("L", n_test)

  # Constraint 16 sum(wik) >= m
  a16 = matrix(0, 1, n_vars)
  a16[1, (w_start+1):(w_start + n_test)] = 1-z_test
  rownames(a16) = "C16"
  b16 = m
  names(b16) = "C16"
  s16 = "G"

  Amat = rbind(a2, a3, a10, a11, a12, a13, a14, a15, a16)
  bvec = c(b2, b3, b10, b11, b12, b13, b14, b15, b16)
  svec = c(s2, s3, s10, s11, s12, s13, s14, s15, s16)

  lbs = c(sapply(1:p, function(j) min(c(x_test[, j], xi[j])))-1/M,
          sapply(1:p, function(j) min(c(x_test[, j], xi[j])))-1/M,
          rep(0, n_test),  rep(0, n_test * p), rep(0, n_test * p))
  ubs = c(sapply(1:p, function(j) max(c(x_test[, j], xi[j])))+1/M,
          sapply(1:p, function(j) max(c(x_test[, j], xi[j])))+1/M,
          rep(1, n_test),  rep(1, n_test * p), rep(1, n_test * p))

  vtype = c(rep("C", p), rep("C", p), rep("B", n_test),
            rep("B", n_test * p), rep("B", n_test * p))
  list(Amat=Amat, bvec=bvec, cvec=cvec, Qmat=Qmat, lb=lbs, ub=ubs, vtype=vtype, sense=svec)
}


recover_pars = function(sol, n_train, n_test, p){
  a_start = 0
  b_start = p
  w_start = b_start + p
  q_start = w_start + n_test
  r_start = q_start + n_test * p
  u_start = r_start + n_test * p
  v_start = u_start + n_train * p
  s_start = v_start + n_train * p

  list(
    a = sol[(a_start+1):b_start],
    b = sol[(b_start+1):w_start],
    w = sol[(w_start+1):(q_start)],
    q = matrix(sol[(q_start+1):(r_start)], n_test, p, byrow=T),
    r = matrix(sol[(r_start+1):(u_start)], n_test, p, byrow=T),
    u = matrix(sol[(u_start+1):(v_start)], n_train, p, byrow=T),
    v = matrix(sol[(v_start+1):s_start], n_train, p, byrow=T),
    s = sol[(s_start + 1):(n_train + s_start)]
    #obj = sol$obj
  )
}

make_mg <- function(X, lbs, ubs){
  return(which(colMeans((t(X) <= ubs)*(t(X) >= lbs))==1))
}
