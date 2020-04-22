
abc(585000, Parameters[1,1], Parameters[2,1], Parameters[3,1]) + 
  abc(74000, Parameters[1,2], Parameters[2,2], Parameters[3,2]) + 
  abc(147000, Parameters[1,3], Parameters[2,3], Parameters[3,3]) + 
  abc(194000, Parameters[1,4], Parameters[2,4], Parameters[3,4]) + 
  abc(0, Parameters[1,5], Parameters[2,5], Parameters[3,5])



a <- c(Parameters[1, 1], Parameters[1,2], Parameters[1,3], Parameters[1,4], Parameters[1,5])
b <- c(Parameters[2, 1], Parameters[2,2], Parameters[2,3], Parameters[2,4], Parameters[2,5])
c <- c(Parameters[3, 1], Parameters[3,2], Parameters[3,3], Parameters[3,4], Parameters[3,5])

eval_f <- function(x) {
  obj <- 
    abc(x[1], a[1], b[1], c[1]) + 
    abc(x[2], a[2], b[2], c[2]) + 
    abc(x[3], a[3], b[3], c[3]) +
    abc(x[4], a[4], b[4], c[4]) +
    abc(x[5], a[5], b[5], c[5])
  gr <-
    c(-dabc(x[1], a[1], b[1], c[1]), 
      -dabc(x[2], a[2], b[2], c[2]),
      -dabc(x[3], a[3], b[3], c[3]), 
      -dabc(x[4], a[4], b[4], c[4]),
      -dabc(x[5], a[5], b[5], c[5]))
  
  return(list("objective" = -obj,
              "gradient" = gr))
}


# constraint functions
# inequalities
eval_g_ineq <- function( x ) {
  constr <- 
    c( x[1]+x[2]+x[3]+x[4]+x[5]-1000000  )
  grad <- rep(1,5)
  
  return( list( "constraints"=constr, "jacobian"=grad ) )
}


# initial values
x0 <- rep(100, 5)
# lower and upper bounds of control
lb <- rep(100, 5)
ub <- rep(1000000, 5)
local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                    "xtol_rel" = 1.0e-7 )
opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
              "xtol_rel" = 1.0e-7,
              "maxeval" = 10000000,
              "local_opts" = local_opts )


res <- nloptr( x0=x0,
               eval_f=eval_f,
               lb=lb,
               ub=ub,
               eval_g_ineq=eval_g_ineq,
               #               eval_g_eq=eval_g_eq,
               opts=opts)


abc(res$solution[1], Parameters[1,1], Parameters[2,1], Parameters[3,1]) + 
  abc(res$solution[2], Parameters[1,2], Parameters[2,2], Parameters[3,2]) + 
  abc(res$solution[3], Parameters[1,3], Parameters[2,3], Parameters[3,3]) + 
  abc(res$solution[4], Parameters[1,4], Parameters[2,4], Parameters[3,4]) + 
  abc(res$solution[5], Parameters[1,5], Parameters[2,5], Parameters[3,5])

dabc(res$solution[1], Parameters[1,1], Parameters[2,1], Parameters[3,1])
dabc(res$solution[2], Parameters[1,1], Parameters[2,1], Parameters[3,1])
dabc(res$solution[3], Parameters[1,1], Parameters[2,1], Parameters[3,1])
dabc(res$solution[4], Parameters[1,1], Parameters[2,1], Parameters[3,1])
dabc(res$solution[5], Parameters[1,1], Parameters[2,1], Parameters[3,1])
