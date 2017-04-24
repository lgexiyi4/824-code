## stat 824, final
rm(list=ls())
## data input
LSAT = c(576, 635, 558, 578, 666, 580, 555, 661, 651, 
         605, 653, 575, 545, 572, 594)
GPA = c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 
        3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 3.96)
n = length(LSAT)

## Q9
ecor = function(x, y){ 
  # estimator of correlation coefficient
  xbar = mean(x); ybar = mean(y)
  xsd = sqrt(sum((x-xbar)**2))
  ysd = sqrt(sum((y-ybar)**2))
  res = sum((x-xbar)*(y-ybar))/(xsd*ysd)
  return(res)
}

#a) estimate of correlation coefficient
rho.est = ecor(LSAT, GPA); rho.est

#b) bias of Jacknife
rho.removes = c()
for(i in 1:n){
  rho.removes = c(rho.removes, ecor(LSAT[-i],GPA[-i]))
}
rho.Jack = mean(rho.removes)
bias.Jack = (n-1)*(rho.Jack-rho.est); bias.Jack

#c) Jacknife bias-corrected estimate based on bias
rho.Jack.corrected = n*rho.est-(n-1)*rho.Jack; rho.Jack.corrected

#d) Jacknife estimate of the standard error of the estimator
rho.removes.corrected = n*rho.est-(n-1)*rho.removes
se.Jack = sqrt(var(rho.removes.corrected)/n)

#e) Jacknife CI
lower.Jack = rho.Jack.corrected+qnorm(0.025)*se.Jack
upper.Jack = rho.Jack.corrected+qnorm(0.975)*se.Jack
CI.Jack = c(lower.Jack,upper.Jack); CI.Jack

## Q10

#a) estimator of the correlation coefficient by bootstrap
ecor.bootstrap = function(B, x, y){
  datalist = list()
  for(i in 1:B){
    ind.sample = sample(1:n, size=n, replace=TRUE)
    datalist[[i]] = cbind(x[ind.sample], y[ind.sample])
  }
  rho.boots = sapply(datalist, function(z){ecor(z[,1],z[,2])})
  rho.b = mean(rho.boots)
  return(list('estimate'=rho.b, 'samples'=rho.boots))
}

#b) B=10000
boots = ecor.bootstrap(10000, LSAT, GPA)
rho.boots = boots$estimate; rho.boots

#c) bootstrap estimate of the bias of the estimator
bias.boots = rho.boots-rho.est; bias.boots

#d) bootstrap se
se.boots = sqrt(var(boots$samples)); se.boots

#e) bootstrap CI
# 0.025&0.975 quantile
lower.boots = quantile(boots$samples,0.025)
upper.boots = quantile(boots$samples,0.975)
CI.boots = c(lower.boots,upper.boots); CI.boots

lower.boots = rho.est+qnorm(0.025)*se.boots
upper.boots = rho.est+qnorm(0.975)*se.boots
CI.boots = c(lower.boots,upper.boots); CI.boots

#f) Comparision
data.frame(CI.Jack, CI.boots, row.names = c("Lower", "Upper"))
