


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
  return(apply(X, FUN = mean, MARGIN = 2))
}

center = function(x, mu)
{
  return(x - mu)
}

center_matrix = function(X)
{
  t(apply(X, t(mean_vector(X)), FUN = center, MARGIN = 1))
}

cov_matrix = function(X)
{
  phi = center_matrix(X)
  n = dim(phi)[1]
  return((phi %*% t(phi))/n)
}



model.disc = function(y, X)
{
  c1_index = which(y == 1)
  c2_index = which(y == 0)
  X1 = X[c1_index, ]
  X2 = X[c2_index, ]
  mu1 = mean_vector(X1)
  mu2 = mean_vector(X2)
}

