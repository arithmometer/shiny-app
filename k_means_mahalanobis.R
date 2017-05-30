# source: https://github.com/Bakeforfun/CMF/blob/d5a250dfb6a41023ea282f97538ea41f343f09eb/Courses/Fall%20term/Machine%20Learning/4.%20K-means%20algorithm/KMeans.r

initMeans <- function(X, K) X[sample(1:nrow(X), size = K),]

distMatr <- function(X, Mu, distFunc, Sigma) {
  K <- nrow(Mu); m <- nrow(X); n <- ncol(X)
  dist <- vector("list", K)
  for (j in 1:K) dist[[j]] <- function(x) distFunc(x, Mu[j,], Sigma[[j]])
  Matr <- matrix(nrow = m, ncol = K)
  for (j in 1:K) Matr[,j] <- apply(X, 1, dist[[j]])
  Matr
}

calcMeans <- function(X, class) {
  u <- unique(class)
  K <- length(u); m <- nrow(X); n <- ncol(X)
  Mu <- matrix(nrow = K, ncol = n)
  for (i in 1:length(u)) Mu[i,] <- apply(rbind(X[class == u[i],]), 2, mean)
  Mu
}

covMatr <- function(X, class) {
  u <- unique(class)
  K <- length(u)
  cm <- vector("list", K)
  for (i in 1:K) cm[[i]] <- cov(X[class == class[i],])
  cm
}

distEuclid <- function(x, y, Sigma) (sum((x-y)^2))^0.5

distMahalanobis <- function(x, y, Sigma) {
  solveSigma <- try(solve(Sigma), silent = TRUE)
  if (class(solveSigma) == "try-error") solveSigma <- diag(nrow(Sigma))
  as.vector((rbind(x-y) %*% solveSigma %*% cbind(x-y))^0.5)
}

assignClass <- function(X, Mu, distFunc, Sigma) {
  Matr <- distMatr(X, Mu, distFunc, Sigma)
  class <- apply(Matr, 1, which.min)
  u <- unique(class)
  for (i in 1:length(u)) class[class == u[i]] <- i
  class
}

calcTSS <- function(X) {
  m <- nrow(X); n <- ncol(X)
  mu <- apply(X, 2, mean)
  TSS <- matrix(0, nrow = n, ncol = n)
  for (i in 1:m) TSS <- TSS + cbind(X[i,]-mu) %*% rbind(X[i,]-mu)
  TSS
}

calcWSS <- function(X, Mu, class) {
  m <- nrow(X); n <- ncol(X)
  WSS <- matrix(0, nrow = n, ncol = n)
  for (i in 1:m) WSS <- WSS + cbind(X[i,]-Mu[class[i],]) %*% rbind(X[i,]-Mu[class[i],])
  WSS
}

KMeans <- function(X, K, distFunc = distEuclid, nstart = 10, maxIter = 20, tol = 0.001) {
  model <- NULL; err <- Inf
  m <- nrow(X); n <- ncol(X)
  
  for (s in 1:nstart) {
    Mu <- initMeans(X, K)
    Sigma <- vector("list", K); for (k in 1:K) Sigma[[k]] <- diag(rep(1,n))
    class <- assignClass(X, Mu, distFunc, Sigma)
    
    iter <- 0
    TSS <- calcTSS(X); WSS <- calcWSS(X, Mu, class)
    dTSS <- det(TSS); dWSS <- det(WSS)
    ratio <- dWSS / dTSS; deltaRatio <- -Inf
    
    optTrace <- rbind(c(0, dWSS, round(ratio*100,1)))
    dimnames(optTrace) <- list(NULL, c("iteration", "wss", "wss.tss"))
    
    while(iter <= maxIter & deltaRatio < -tol) {
      iter <- iter + 1
      Mu <- calcMeans(X, class)
      Sigma <- covMatr(X, class)
      class <- assignClass(X, Mu, distFunc, Sigma)
      WSS <- calcWSS(X, Mu, class); dWSS <- det(WSS)
      newRatio <- dWSS / dTSS; deltaRatio <- newRatio - ratio
      ratio <- newRatio
      optTrace <- rbind(optTrace, c(iter, dWSS, round(ratio*100,1)))
    }
    
    if (dWSS < err) {
      model <- list(optTrace = optTrace, means = Mu, class = class, err = dWSS)
      err <- dWSS
    }
  }
  
  model
}

km <- KMeans(as.matrix(mtcars[, 4:7]), 2, distFunc = distMahalanobis)
km
