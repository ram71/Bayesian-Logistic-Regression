


setwd("E:\\iu\\fa18\\b555\\pp03\\pp3data")
  
conv.matr = function(x)
{
  rownames(x) = NULL
  colnames(x) = NULL
  return(as.matrix(x))
}


train_x = conv.matr(read.csv("A.csv", header = FALSE))
train_y = conv.matr(read.csv("labels-A.csv", header = FALSE))

mean_vector = function(X)
{
  return(as.matrix(apply(X, FUN = mean, MARGIN = 2)))
}

center = function(x, mu)
{
  return(x - mu)
}

center_matrix = function(X)
{
  t(apply(t(X), mean_vector(X), FUN = center, MARGIN = 2))
}

cov_matrix = function(X)
{
  phi = center_matrix(X)
  n = dim(phi)[1]
  return((t(phi) %*% phi)/n)
}

gen_model = function(y, X)
{
  c1_index = which(y == 1)
  c2_index = which(y == 0)
  N1 = length(c1_index)
  N2 = length(c2_index)
  N = N1 + N2
  X1 = X[c1_index, ]
  X2 = X[c2_index, ]
  mu1 = mean_vector(X1)
  mu2 = mean_vector(X2)
  S1 = cov_matrix(X1)
  S2 = cov_matrix(X2)
  S = (N1/N) * S1 + (N2/N) * S2
  p = N1/(N1+N2)
  Sinv = solve(S, tol = 1e-80)
  w = Sinv %*%(mu1 - mu2)
  w0 = (-0.5 * t(mu1) %*% Sinv %*% mu1) + (0.5 * t(mu2) %*% Sinv %*% mu2) + log(p/(1 - p))
  return(list(w, w0))
}



gen_test = function(X, w, w0)
{
  t = rep(NA, dim(X)[1])
  for(i in 1:dim(X)[1])
  {
    post = t(w) %*% as.matrix(X[i, ]) + w0
    t[i] = 1 * (post >= 0.5) + 0 * (post < 0.5)
  }
  return(as.matrix(t))
}


logistic_model = function(t, X, alpha, w0)
{
  phi = cbind(1, X)
  N = dim(phi)[1]
  d = dim(phi)[2]
  w0 = as.matrix(w0)
  
  r = rep(NA, N)
  y = rep(NA, N)
  
  stop = FALSE
  n = 0
  
  while(stop == FALSE)
  {
    for(i in 1:N)
    {
      x = as.matrix(phi[i, ])
      a = as.numeric(t(w0) %*% x)
      r[i] = (1/(1 + exp(-a))) * (1/(1 + exp(a)))
    }
    
    R = diag(r)
    
    for(i in 1:N)
    {
      x = as.matrix(phi[i, ])
      a = as.numeric(t(w0) %*% x)
      y[i] = (1/(1 + exp(-a)))
    }
    
    w1 = w0 - solve((alpha * diag(1, d)) + t(phi) %*% R %*% phi, tol = 1e-80) %*% (t(phi) %*% (y - t) + alpha * w0)
    
    n = n + 1
    
    crit = sqrt(t(w1 - w0) %*% (w1 - w0))/sqrt(t(w0) %*% w0)
    
    if(crit < 1e-3 || n >= 100)
    {
      stop = TRUE
    }
    
    w0 = w1
  }
  return(w1)
}

logistic_test = function(X, w)
{
  phi = cbind(1, X)
  t = rep(NA, dim(phi)[1])
  for(i in 1:dim(phi)[1])
  {
    a = t(w) %*% as.matrix(phi[i, ])
    y = 1/(1 + exp(-a))
    t[i] = as.numeric(1 * (y >= 0.5) + 0 * (y < 0.5))
  }
  return(as.matrix(t))
}



a = "A.csv"
b = "labels-A.csv"
X = conv.matr(read.csv(a, header = FALSE))
t = conv.matr(read.csv(b, header = FALSE))
s = ceiling(dim(X)[1]/3)
rand = sample(1:dim(X)[1], s, replace = FALSE)
train_X = X[-rand, ]
train_t = t[-rand, ]
test_X = X[rand, ]
test_t =  t[rand, ] 
  
w0 = rep(0, (dim(train_X)[2] + 1))
w = logistic_model(train_t, train_X, 0.1, w0)
pred_logit = logistic_test(test_X, w)
l = gen_model(train_t, train_X)
pred_gen = gen_test(test_X, l[[1]], l[[2]])







