# Lp_solve is freely available (under LGPL 2) software for
# solving linear, integer and mixed integer programs

library(lpSolve)

optmodule <- function(y_pred, s, r, b, theta) {
  h = length(y_pred)
  if (theta < sum(y_pred)) {
    R = integer(h + 1)
    Y = integer(h)
    for (i in 1:h) {
      R[i] = sum(r[i:h])
      Y[i] = sum(y_pred[i:h])
    }
    matr_constr = integer(3*h*2*h)
    for (i in 1:h) {
      matr_constr[(i - 1)*2*h + i] = 1
      if (i > 1) {
        for (j in 1:(i - 1)) {
          matr_constr[(i - 1)*2*h + j] = -1
          matr_constr[(i - 1)*2*h + j + h] = 1
        }
      }
    }
    for (i in 1:h) {
      for (j in 1:i) {
        matr_constr[2*h*(h + i - 1) + j] = -1
        matr_constr[2*h*(h + i - 1) + j + h] = 1
      }
    }
    for (i in 1:h) {
      matr_constr[(2*h + i - 1) * 2*h + i + h] = 1
    }
    f.con <- matrix(matr_constr, nrow = 3*h, byrow = TRUE)
    f.dir <- rep("<=", 3*h)
    f.rhs <- (c(Y - theta, rep(theta, h), y_pred))
    f.obj <- c(-(b + R[1:h]), s + R[2:(h + 1)])
    sol = lp("max",f.obj,f.con,f.dir,f.rhs,all.int = TRUE)$solution
    new_theta = max(0, theta + sum(sol[1:h]) - sum(sol[(h + 1):(2*h)]))
  }
  else
  {
    sol = c(integer(h), y_pred)
    new_theta = theta - sum(y_pred)
  }
  return(list(sol, new_theta))
}
  
  