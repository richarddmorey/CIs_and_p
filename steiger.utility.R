


steigerCI.omega2 = function(Fstat, df1, df2, conf.level=.95){
  Ntot = df1 + df2 + 1
  ci = steigerCI(Fstat, df1, df2, conf.level)
  return(ci / (ci + Ntot))
}


steigerCI = function(Fstat, df1, df2, conf.level=.95){
  alpha = 1-conf.level
  
  ncp.est = Fstat * df1
  pval = 1 - pf(Fstat, df1, df2)
  if( (1-pval) < alpha/2 ){
    return(c(lo=0,up=0))
  }else if((1-pval) < (1-alpha/2)){
    lower = c(lo=0)
    upper = upperSteigerCI(Fstat, alpha, df1, df2)
  }else{
    lower = lowerSteigerCI(Fstat, alpha, df1, df2)
    upper = upperSteigerCI(Fstat, alpha, df1, df2)
  }
  return(c(lower,upper))
}


lowerSteigerCI = function(Fstat, alpha, df1, df2)
{
  targetp = 1 - alpha/2
  lo = optimize(function(q) (pf(Fstat, df1, df2, ncp = q) - targetp)^2,
                interval = c(0, Fstat * df1))$minimum
  return(c(lo = lo))
}

upperSteigerCI = function(Fstat, alpha, df1, df2)
{
  targetp = alpha/2
  up = optimize(function(q) (pf(Fstat, df1, df2, ncp = q) - targetp)^2,
                interval = c(Fstat * df1, Fstat * df1 * 10))$minimum
  return(c(up = up))
}
