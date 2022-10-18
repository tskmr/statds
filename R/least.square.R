calcX = function(x){
  if(!is.matrix(x)){
    n = length(x)
    k= 1
  }else {
    n = nrow(x)
    k = ncol(x)
  }
  X = matrix(c(rep(1, n), x), ncol=k+1)
  XXinv = solve(t(X) %*% X)
  return(list(x=x, n=n, k=k, X=X, XXinv=XXinv))
}

calcXY = function(x, y){
  resX =calcX(x)
  Y = matrix(y)
  return(list(resX, y=y, Y=Y))
}

least.square = function(x, y){
  XY = calcXY(x, y)
  X = XY$X
  Y = XY$Y
  XXinv = XY$XXinv
  n = XY$n
  k = XY$k
  beta = XXinv %*% t(X) %*% Y
  sigma2 = t(Y-X %*% beta) %*% (Y-X %*% beta) / (n-k-1)
  res = c(beta, sigma2)
  names(res) = c(paste0("beta", 0:k), "sigma2")
  return(list(estimates = res, XY=XY))
}

aov = function(x, y){
  XY = calcXY(x, y)
  X = XY$X
  Y = XY$Y
  XXinv = XY$XXinv
  n = XY$n
  k = XY$k
  ### variance covariance analysis
  df.reg = k+1; df.sig = n-k-1; df.all = n
  least.square(x, y)
  estY= X%*%beta; eps = Y-estY
  ss.reg = t(estY)%*%estY; ss.sig = t(eps)%*%eps; ss.all = t(Y)%*%Y
  mean.ss.reg = ss.reg/df.reg; mean.ss.sig = ss.sig/df.sig
  valueF = mean.ss.reg/mean.ss.sig
  vca = data.frame(name=c("reg", "err", "total"),
             df = c(df.reg, df.sig, df.all),
             ss = c(ss.reg, ss.sig, ss.all),
             mean.ss = c(mean.ss.reg, mean.ss.sig, NA),
             valueF = c(valueF, NA, NA))
  R2 = (t(estY)%*%estY)/(t(Y)%*%Y)
  
  ad.df.reg = k; ad.df.sig = n-k-1; ad.df.all = n-1
  estY= X%*%beta; eps = Y-estY; bary = mean(y);
  ad.ss.reg = t(estY)%*%estY - n*bary^2; ad.ss.sig = t(eps)%*%eps; ad.ss.all = t(Y)%*%Y - n*bary^2
  ad.mean.ss.reg = ad.ss.reg/ad.df.reg; ad.mean.ss.sig = ad.ss.sig/ad.df.sig
  ad.valueF = ad.mean.ss.reg/ad.mean.ss.sig
  ad.vca = data.frame(name=c("reg", "err", "total"),
                   df = c(ad.df.reg, ad.df.sig, ad.df.all),
                   ss = c(ad.ss.reg, ad.ss.sig, ad.ss.all),
                   mean.ss = c(ad.mean.ss.reg, ad.mean.ss.sig, NA),
                   valueF = c(ad.valueF, NA, NA))
  ad.R2 = (t(estY)%*%estY - n*bary^2)/(t(Y)%*%Y - n*bary^2)
  
  varBeta = XXinv*as.numeric(sigma2)
  rBeta = sapply(1:(k+1), function(i){
    sapply(1:(k+1), function(j){
      varBeta[i,j]/sqrt(varBeta[i,i]*varBeta[j,j])
    })})
  seBeta = sqrt(diag(XXinv)*as.numeric(sigma2))
  tBeta = beta/seBeta
  
  return(list(ad.vca=ad.vca, ad.R2=ad.R2, vca=vca, R2=R2, seBeta=seBeta, tBeta=tBeta))
}

multi.collinearity= function(x){
  ### multi-collinearity
  # VIF
  # t(rBeta)%*%rBeta
  if(is.null(ncol(x))) return(NULL)
  x1 = x[, 1]; 
  x2 = x[,-1]
  r1 = aov(x2, x1)
  VIF = 1/(1-rBeta^2)

  return(res)
}

x1 = c(8, 5, 7, 7, 7, 7, 5)
x2 = c(29, 4, 9, 18, 54, 7, 8)
x3 = c(150, 144, 134, 150, 130, 130, 120)
x4 = c(18, 20, 30, 40, 30, 30, 100)
x5 = c(49, 24, 39, 58, 84, 37, 108)
x = matrix(c(x1, x2, x3, x4), ncol=4)
y = c(7.390, 7.3, 7.215, 7.162, 5.193, 4.654, 2.708)

least.square(x, y)
aov(x, y)
