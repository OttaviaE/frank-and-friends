# prova stima di theta --- 
# log likelihood 
loglik <- function(theta,a,b,c,delta,D){
  #u e Pij sono matrici, la somma ci dà la log-verosimiglianza (Lord, p. 58)
  sum(u*log(P) + (1-u)*log(1-P))
}
# derivata di theta
dP_dtheta <- function(theta, a=1, b=.5, c=0, D = 1.7, delta = 1) {
  dP <- (D*(delta - c) * exp(-D * a*(theta - b)))/(exp(-D* a*(theta - b)) +1)^2
  return(dP)
}
# derivata
dl_dtheta <- function(theta, u, a,b,c,delta,D,dP_dtheta) {
  n <- length(u)
  temp=sapply(1:n, function(i) (u[1,i]*2-1)* dP_dtheta(theta,a[i],b[i],c[i],D,delta[i]))
  sum(temp)
}

set.seed(132)
n=100 # item
N=1 # sogg
# dP_dtheta(theta, a = a, b = b, c = c, D = D, delta = delta)

a=rep(1,n)
b=runif(n, -3,3)
c=rep(0,n) 
D = 1.7
delta = rep(1,n)
thetas = numeric(1000)
hat_theta = numeric(1000)
# u=matrix(rbinom(n*N,size = 1,P),N,n)  
for (j in 1:1000) {
  theta = rnorm(N) 
  thetas[j]= theta
  P <- c + (delta - c) / (1 + exp(-D *  
                                    matrix(a,N,n,byrow = TRUE) * (matrix(theta,N,n) - matrix(b,N,n,byrow = TRUE)))) # sogg x item:  N x n
  u=matrix(rbinom(n*N,size = 1,P),N,n) 
  
  hat_theta[j] = uniroot(dl_dtheta, c(-4, 4), 
                         a = a, u = u, b = b, c = c, 
                         delta = delta, 
          D = D, dP_dtheta)$root
  cat("iter", j)
}
uniroot(dl_dtheta, c(-4, 4), a = a, u = u, b = b, c = c, delta = delta, 
        D = D, dP_dtheta)$root



theta_seq <- seq(-4, 4, length.out = 1000)
y_vals <- sapply(theta_seq, function(theta) {
  val <- tryCatch(dl_dtheta(theta, a = a, u = u, b = b, c = c, delta = delta), 
                  error = function(e) NA)
  return(val)
})

plot(theta_seq, y_vals, type = "l")
