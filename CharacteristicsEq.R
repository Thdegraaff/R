counteq <- function(mat){
  # Counts the number of various equilibria per iteration
  #
  # Args:
  #   mat: a matrix with equilibria
  #
  # Returns:
  #   a share of the neighborhoods with three equilibria
  fun1 <- function(x) sum (x==1)
  fun3 <- function(x) sum (x==3)
  vec1 <- apply(mat, 1, fun1)
  vec3 <- apply(mat, 1, fun3)
  return(vec3/(vec1+vec3))
}

counteqlow <- function(mat){
  # Counts the number of low equilibria per iteration
  #
  # Args:
  #   mat: a matrix with the estimated equilibria
  #
  # Returns:
  #   a share of the neighborhoods with lowest equilibria
  fun <- function(x) sum (x<=0.5)
  vec <- apply(mat, 1, fun)
  return(vec/ncol(mat))
}

percclose <- function(cr, mat, epsilon) {
  # Counts the number of equilibria per iteration close to 
  # real crime rate
  #
  # Args:
  #   cr : real crime rate
  #   mat: a matrix with the estimated equilibria
  #   epsilon: error margin allowed
  #
  # Returns:
  #   a share of the neighborhoods per iteration close to the
  #   real crime rate
  mattemp <- mat - cr/100
  fun <- function(x) sum (abs(x)<=epsilon)
  vec <- apply(mattemp, 1, fun)
  return(vec/ncol(mat))
}

perceqlower <- function(cr, mat) {
  # Counts the number of equilibria lower than the 
  # real crime rate
  #
  # Args:
  #   cr : real crime rate
  #   mat: a matrix with the estimated equilibria
  #
  # Returns:
  #   a share of the neighborhoods per iteration with equilibrium lower than the
  #   real crime rate
  mattemp <- mat - cr/100
  fun <- function(x) sum (x>0)
  vec <- apply(mattemp, 1, fun)
  return(vec/ncol(mat))
}