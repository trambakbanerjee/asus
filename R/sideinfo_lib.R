#' SureShrink estimator
#'
#' SureShrink estimator of a high dimensional sparse parameter from Donoho and Johnstone (1995)
#'
#' @import wavethresh
#' @importFrom stats rnorm
#'
#' @param d an n vector of observations
#' @param v.d an n vector of variances for each component of d
#'
#' @return
#' \enumerate{
#' \item est - an n vector holding the estimates
#' \item t - estimated threshold
#' }
#'
#' @details Estimates a threshold t by minimizing the SURE function and then soft thresholds
#'     d using t.
#'
#' @seealso \code{\link{sureshrink.mse}}
#'
#' @examples
#' library(asus)
#' set.seed(42)
#' d<-rnorm(10,2,1)
#' v.d<- rep(1,10)
#' theta.hat<-sureshrink(d,v.d)
#'
#' @references
#' David L Donoho and Iain M Johnstone. Adapting to unknown smoothness via wavelet shrinkage.
#' Journal of the american statistical association, 90(432):1200-1224, 1995
#'
#' @export

sureshrink<- function(d,v.d){
  p<- length(d)
  if (p>0){
    dd<- d/sqrt(v.d)
    td<- sqrt(2*log(p))
    gam<- ((log2(p))^(1.5))/sqrt(p)
    ts<- sure(dd)
    sd2<- (1/p)*(sum(dd^2)-p)
    tt<- ts
    if(sd2<=gam){
      tt<-td
    }
    d.ss<-sqrt(v.d)*softTh(dd,tt)
    return(list("est"=d.ss,"t"=tt))
  }
  if (p==0){
    return(list("est"=0,"t"=0))
  }
}

#' SURE estimate of risk
#'
#' Stein's Unbiased Risk Estimate for the sureshrink estimator
#'
#' @import wavethresh
#' @importFrom stats rnorm
#'
#' @param d an n vector of observations
#' @param v.d an n vector of variances for each component of d
#' @param type set type=1 if you want the thresholding parameter t to be estimated. Otherwise
#'     set type = 0 in which case you must provide t. Default is type = 1
#' @param t soft thresholding parameter. If type = 1, then t is estimated whereas if type = 0
#'     then you must provide t. Default is t = 0 (and type = 1)
#'
#' @return
#' \enumerate{
#' \item sure.est - SURE estimate of risk
#' \item t - estimated threshold (meaningless if type = 0)
#' }
#'
#' @details Estimates the risk of the surehsrink estimator of Donoho and Johnstone (1995).
#'
#' @seealso \code{\link{sureshrink}},\code{\link{asus}}
#'
#' @examples
#' library(asus)
#' set.seed(42)
#' d<-rnorm(10,2,1)
#' v.d<- rep(1,10)
#' mse<-sureshrink.mse(d,v.d)
#'
#' @references
#' \enumerate{
#' \item Charles M Stein. Estimation of the mean of a multivariate normal distribution. The annals of
#' Statistics, pages 1135-1151, 1981
#' \item David L Donoho and Iain M Johnstone. Adapting to unknown smoothness via wavelet shrinkage.
#' Journal of the american statistical association, 90(432):1200-1224, 1995
#' }
#'
#' @export

sureshrink.mse<- function(d,v.d,type=1,t=0){

  pp<- length(d)
  if (pp==0){
    return(list("sure.est"=0,"t"=0))
  }
  if(type==1){
    if (pp>0){
      dd<-d/sqrt(v.d)
      td<- sqrt(2*log(pp))
      gam<- ((log2(pp))^(1.5))/sqrt(pp)
      ts<- sure(dd)
      sd2<- (1/pp)*(sum(dd^2)-pp)
      tt<- ts
      if (sd2<=gam){
        tt<-td
      }
      sure.est<- sum(v.d)-2*sum(v.d*(abs(dd)<=tt))+
        sum(v.d*(pmin(tt,abs(dd)))^2)
      return(list("sure.est"=sure.est,"t"=tt))
    }
  }
  if(type==2){
    if (pp>0){
      dd<-d/sqrt(v.d)
      tt<-t
      sure.est<- sum(v.d)-2*sum(v.d*(abs(dd)<=tt))+
        sum(v.d*(pmin(tt,abs(dd)))^2)
      return(list("sure.est"=sure.est,"t"=tt))
    }
  }
}

#' Extended James-Stein (ejs) estimator
#'
#' Extended James-Stein estimator of a high dimensional sparse parameter.
#'
#' @importFrom stats rnorm
#'
#' @param d an n vector of observations
#' @param v.d an n vector of variances for each component of d
#
#' @return est - an n vector holding the estimates
#'
#' @details Extended James-Stein estimator of mean from Brown (2008) and equation (7.3) in Xie et al. (2012)
#'
#' @seealso \code{\link{sureshrink}},\code{\link{asus}}
#'
#' @examples
#' library(asus)
#' set.seed(42)
#' d<-rnorm(10,2,1)
#' v.d<- rep(1,10)
#' theta.hat<-ejs(d,v.d)
#'
#' @references
#' \enumerate{
#' \item Brown, L.D. (2008). In-Season Prediction of Batting Averages: A Field Test of Empirical
#' Bayes and Bayes Methodologies. The Annals of Applied Statistics, 2, 113-152
#' \item Xie, X. C., Kou, S. C., and Brown, L. D. (2012). SURE Estimates for a Heteroscedastic
#' Hierarchical Model. Journal of the American Statistical Association, 107, 1465-1479.
#' }
#'
#' @export

ejs <- function(d,v.d){
  p <- length(d)
  muhat <- sum( d*(1/v.d) )/sum(1/v.d)
  b <- 1 - (p-3) / sum( (d-muhat)^2/v.d )
  return( muhat + max(0,b) * (d-muhat) )
}

#' Adaptive SURE thresholding with side information (asus)
#'
#' ASUS procedure for shrinkage estimation of a high dimensional sparse parameter.
#'
#' @import wavethresh
#' @importFrom stats rnorm
#' @importFrom stats quantile
#' @importFrom utils combn
#'
#' @param d an n vector of primary observations
#' @param v.d an n vector of variances for each component of d
#' @param s an n vector of side information
#' @param k number of groups. Default is k=2
#' @param m partitions the support of \eqn{|s|} into \eqn{m} equidistant points.
#'     Default is \eqn{m=50}
#
#' @return
#' \enumerate{
#' \item est - an n vector holding the estimates
#' \item mse - estimate of risk
#' \item tau - k-1 vector of grouping parameters if k>=2
#' \item t - k vector of thresholding parameters
#' \item size - k vector of group sizes
#' }
#'
#' @details Estimates a sparse high dimensional vector using the ASUS procedure described in Banerjee et al. (2017).
#'     If k = 1 then ASUS is the SureShrink estimator. The current implementation of ASUS estimates the grouping thresholds
#'     based on the magnitude of \eqn{|s|}. See the reference for more details.
#'
#' @seealso \code{\link{sureshrink}},\code{\link{ejs}},\code{\link{sureshrink.mse}}
#'
#' @examples
#' library(asus)
#' set.seed(42)
#' d<-rnorm(10,2,1)
#' v.d<- rep(1,10)
#' set.seed(42)
#' s<-rnorm(10,3,0.1)
#' asus.out<-asus(d,v.d,s)
#'
#' @references Banerjee. T, Mukherjee. G and Sun. W. Adaptive Sparse Estimation with Side Information.
#' Journal of the American Statistical Association 115, no. 532 (2020): 2053-2067.
#'
#' @export

asus <- function(d,v.d,s,k=2,m=50){

  n<- length(d)

  if (k==1){
    # simple SureShrink (no side info)
    out<-sureshrink.mse(d,v.d,1,0)
    est<-sureshrink(d,v.d)
    return(list("est"=est,"mse"=out$sure.est,"t"=out$t,"size"=c(n,0)))
  }

  if (k==2){
    # 2 group ASUS
    V <- abs(s)
    ttau<- seq(min(V),max(V),length.out = m)
    temp<-matrix(0,length(ttau),1)
    for(kk in 1:length(ttau)){
      tau<- ttau[kk]
      i1<-(V<=tau)
      i2<- (V>tau)
      U1<- d[i1]
      sigma21<-v.d[i1]
      U2<- d[i2]
      sigma22<- v.d[i2]
      temp[kk]<- max(0,sureshrink.mse(U1,sigma21,1,0)$sure.est)+
        max(0,sureshrink.mse(U2,sigma22,1,0)$sure.est)
    }
    tau<- ttau[which(temp==min(temp))]
    sure.est<-min(temp)
    n1<-length(d[V<=tau])
    n2<-n-n1
    i1<-(V<=tau)
    i2<- (V>tau)
    U1<- d[i1]
    sigma21<-v.d[i1]
    U2<- d[i2]
    sigma22<- v.d[i2]
    t1<- sureshrink.mse(U1,sigma21,1,0)$t
    t2<- sureshrink.mse(U2,sigma22,1,0)$t
    est1<- sureshrink(U1,sigma21)$est
    est2<- sureshrink(U2,sigma22)$est
    est<- matrix(0,n,1)
    est[i1]<-est1
    est[i2]<-est2
    return(list("est"=est,"mse"=sure.est,"tau"=tau,"t"=c(t1,t2),"size"=c(n1,n2)))
  }

  if (k>2){

    V <-abs(s)
    vec<- 1:m
    len<- k-1
    a<-t(combn(length(vec), len, function(x) vec[x]))
    ttau.temp<- seq(quantile(V,0.01),quantile(V,0.99),length.out = m)
    ttau<- matrix(0,nrow(a),ncol(a))
    for (kk in 1:ncol(a)){
      ttau[,kk]<-ttau.temp[a[,kk]]
    }
    rm("ttau.temp","a","vec")

    temp<-matrix(0,nrow(ttau),1)
    UU<- d
    VV<- V
    for(i in 1:nrow(ttau)){
      tau<- ttau[i,]
      temp[i]<-asus.cuts(UU,v.d,VV,tau)$mse
    }
    tau<- ttau[min(which(temp==min(temp))),]
    sure.est<-min(temp)
    t<- matrix(0,k,1)
    nn = matrix(0,k,1)
    est<- matrix(0,n,1)
    i1 <- (VV<=tau[1])
    ik<- (VV>tau[(k-1)])
    Ui1<- UU[i1]
    sigmai1<-v.d[i1]
    Uik<- UU[ik]
    sigmaik<-v.d[ik]
    outi1<-sureshrink(Ui1,sigmai1)
    outik<- sureshrink(Uik,sigmaik)
    est[i1]<- outi1$est
    est[ik]<- outik$est
    t[1]<- outi1$t
    t[k]<-outik$t
    nn[1]<-length(Ui1)
    nn[k]<-length(Uik)

    for (kk in 2:(k-1)){
      ikk<- (VV>tau[kk-1] & VV<=tau[kk])
      Ukk<- UU[ikk]
      sigmakk<- v.d[ikk]
      nn[kk]<- length(Ukk)
      out<- sureshrink(Ukk,sigmakk)
      est[ikk]<-out$est
      t[kk]<-out$t
    }
    return(list("est"=est,"mse"=sure.est,"tau"=tau,"t"=t,"n"=nn))
  }
}

#' Risk of asus with pre-defined grouping thresholds
#'
#' Estimates the risk of asus when there are k(>2) groups with pre-defined grouping thresholds
#'
#' @import wavethresh
#' @importFrom stats rnorm
#'
#' @param d an n vector of primary observations
#' @param v.d an n vector of variances for each component of d
#' @param s an n vector of side information
#' @param cutpoints k-1 pre-defined grouping thresholds for k groups. k must be bigger than 2.
#'
#' @return mse - estimate of risk
#'
#' @details Estimates the risk of asus when there are k(>2) groups
#'     with k pre-defined grouping thresholds. This function is called when \code{\link{asus}}
#'     executes.
#'
#' @seealso \code{\link{asus}},\code{\link{sureshrink}},\code{\link{ejs}},\code{\link{sureshrink.mse}}
#'
#' @examples
#' library(asus)
#' set.seed(42)
#' d<-rnorm(10)
#' v.d<- rep(1,10)
#' set.seed(42)
#' s<-rnorm(10)
#' out<-asus.cuts(d,v.d,s,c(0.1,0.5,1))
#'
#' @references Banerjee. T, Mukherjee. G and Sun. W. Adaptive Sparse Estimation with Side Information.
#' Journal of the American Statistical Association 115, no. 532 (2020): 2053-2067.
#'
#' @export

asus.cuts<- function(d,v.d,s,cutpoints){

  VV<-abs(s)
  UU<-d
  var.UU<-v.d
  K = length(cutpoints)+1

  temp<- matrix(0,K,1)
  i.1 = matrix(FALSE,length(VV),K)
  i.1[,1] <- (VV<=cutpoints[1])
  temp[1]<- max(0,sureshrink.mse(UU[i.1[,1]],var.UU[i.1[,1]],1,0)$sure.est)
  i.1[,K]<- (VV>cutpoints[K-1])
  temp[K]<- max(0,sureshrink.mse(UU[i.1[,K]],var.UU[i.1[,K]],1,0)$sure.est)
  for (k in 2:(K-1)){
    i.1[,k]<- (VV>cutpoints[k-1] & VV<=cutpoints[k])
    temp[k]<- max(0,sureshrink.mse(UU[i.1[,k]],var.UU[i.1[,k]],1,0)$sure.est)
  }
  return(list("mse"=sum(temp)))
}

#' Soft Thresholding estimator
#'
#' Soft thresholds the input signal y with the threshold value thld
#'
#' @param y 1D signal to be thresholded
#' @param thld numeric threshold value
#'
#' @return a numeric vector of thresholded values of the same length as y.
#'
#' @examples
#' library(asus)
#' set.seed(42)
#' y<-rnorm(10,2,1)
#' thld<- 3
#' x<-softTh(y,thld)
#'
#' @references
#' Donoho, David L. "De-noising by soft-thresholding."
#' IEEE transactions on information theory 41, no. 3 (1995): 613-627.
#'
#' @export

softTh <- function(y, thld) {
  x <- abs(y)
  x <- sign(y) * (x >= thld) * (x - thld)
  return(x)
}
