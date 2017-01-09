#Bootstrap
#Book paragraph 5.2

alpha <- function(x,y){
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X , Portfolio$Y) #Say we have a portfolio we run x and y and alpha gives us the alpha value.

alpha.fn <- function(data,index){
  with(data[index,],alpha(X,Y))
  }
alpha.fn(Portfolio, 1:100)

#Now we run bootstrap.
set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace = TRUE)) 

boot.out <- boot(Portfolio, alpha.fn, R = 1000)
boot.out
plot(boot.out)
