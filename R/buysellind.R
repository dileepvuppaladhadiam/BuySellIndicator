#' @title Creation of buy/sell indicator using Technical Analysis
#' @author Dileep Vuppaladhadiam
#' @description This package creates buy/sell indicator using Daily Stock quotes
#' @param (data, n)
#' @examples buysellind(mydata, 14)
#' @import dplyr
#' @export buysellind

high.n <- function(data, n){

  rows = nrow(data)
  cols = 1
  x = matrix(data=NA, nrow=rows, ncol=cols)
  n = n
  for (i in 1:rows){
    m = n+i-1
    x[i,1] = max(data[m:i,"High"])
  }
  High.n = dplyr::lag(x,n, na.pad = TRUE)
  return(High.n)

}

low.n <- function(data, n){

  rows = nrow(data)
  cols = 1
  x = matrix(data=NA, nrow=rows, ncol=cols)
  n = n
  for (i in 1:rows){
    m = n+i-1
    x[i,1] = min(data[m:i,"Low"])
  }
  Low.n = dplyr::lag(x,n, na.pad = TRUE)
  return(Low.n)

}

truehigh <- function(data){

  rows = nrow(data)
  cols = 1
  y=matrix(data = NA, nrow=rows, ncol = cols)
  for (i in 1:rows){
    j=i+1
    y[i,1]=max(data[i,"Close"],data[j,"High"])
  }

  TrueHigh = dplyr::lag(y,1,na.pad=TRUE)
  return(TrueHigh)
}

truelow <- function(data){

  rows = nrow(data)
  cols = 1
  y=matrix(data = NA, nrow=rows, ncol = cols)
  for (i in 1:rows){
    j=i+1
    y[i,1]=min(data[i,"Close"],data[j,"Low"])
  }

  TrueLow = dplyr::lag(y,1,na.pad=TRUE)
  return(TrueLow)
}

tr <- function(data){
  TR = truehigh(data) - truelow(data)
  return (TR)
}

atr <- function(data,n){
  truerange = tr(data)
  rows = nrow(truerange)
  cols = 1
  z = matrix(data = NA, nrow=rows, ncol = cols)
  for (i in 1:rows){
    j = i+1
    z[i,1] = (truerange[i] * (n-1) + truerange[j])/n
  }
  ATR = dplyr::lag(z,1,na.pad=TRUE)
  return(ATR)
}

rwilow <- function(data, n){
  RWIlow = (high.n(data, n) - data[,'Low'])/(atr(data,n) * sqrt(n))
  return (RWIlow)
}

rwihigh <- function(data, n){
  RWIhigh = (data[,'High'] - low.n(data, n))/(atr(data,n) * sqrt(n))
  return (RWIhigh)
}

#Buy/Sell Indicator

buysellind <- function(data,n) {
  rows = nrow(data)
  cols = 1
  a = matrix(data = NA, nrow=rows, ncol = cols)
  myrwihigh = rwihigh(data,n)
  myrwilow = rwilow(data,n)
  for (i in 1:rows) {
    if (is.na(myrwihigh[i,1]) & is.na(myrwilow[i,1])) {
      a[i,1] = 0
    }else  if ((myrwihigh[i,1]>1) & (myrwilow[i,1]<1)) {
      a[i,1] = 1
    }else if ((myrwihigh[i,1]<1) & (myrwilow[i,1]>1)){
      a[i,1] = -1
    }else {
      a[i,1] = 0
    }
  }
  return(a)
}
