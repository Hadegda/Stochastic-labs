ExpNum = 1000
N = 15
mean = 0
sd = 2.5
a = -5.0
b = 5.0

k = 1 + floor(log2(N))
x.int = seq(a, b, length.out=k+1)

x.int[1]<-(-Inf);x.int[k+1]<-(+Inf)
x.norm.p.theor<-pnorm(x.int,mean=mean,sd=sd)
x.norm.p.theor<-(x.norm.p.theor[2:(k+1)]-x.norm.p.theor[1:k])

x.int[1]<-(-20);x.int[k+1]<-(+20)
r = k - 2 + 1

# alpha = 0.05 (right value -> 0.95)
chisqRQuant = switch(r, 3.8415, 5.9915, 7.8147, 9.4877, 11.0705, 12.5916, 14.0671, 15.5073, 16.9190, 18.3070, 19.6751, 21.0261, 22.3620)

H0Norm = 0
H1Norm = 0
for (i in 1:ExpNum) 
{
  # generates random deviates 
  x.norm = rnorm(N, mean=mean, sd=sd)

  x.norm.hist = hist(x.norm,breaks=x.int, plot=FALSE)
  Xsq = chisq.test(x.norm.hist$counts,p=x.norm.p.theor)
  
  if (Xsq$statistic < chisqRQuant)
  {
    H0Norm = H0Norm + 1
  }
  else
  {
    H1Norm = H1Norm + 1
  }
}

H0Unif = 0
H1Unif = 0
for (i in 1:ExpNum) 
{
  # generates random deviates 
  x.unif = runif(N, min=a, max=b)
  
  x.unif.hist = hist(x.unif,breaks=x.int, plot=FALSE)
  Xsq = chisq.test(x.unif.hist$counts,p=x.norm.p.theor)
  
  if (Xsq$statistic < chisqRQuant)
  {
    H0Unif = H0Unif + 1
  }
  else
  {
    H1Unif = H1Unif + 1
  }
}

normPercent = H0Norm/ExpNum * 100
unifPercent = H0Unif/ExpNum * 100