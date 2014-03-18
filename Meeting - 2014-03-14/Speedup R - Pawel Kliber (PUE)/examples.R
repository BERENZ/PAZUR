### author Pawel Kliber
### p.kliber@ue.poznan.pl
library(microbenchmark)

# Przekazywanie warto?ci do funkcji - warto?? i referencja
# Przekazywanie ,,obietnicy'' argumentu
fun<-function(mode, A) {
  if (mode==0)
    return(0)
  if (mode==1) {
    A[1,1] <- A[1,1]+1
    return(1)
  }
}

A <- array(0, dim=c(50, 50))
mb <- microbenchmark(fun(0,A), fun(1,A))
mb
plot(mb)



#U?ywaj dzia?a? macierzowych i wektorowych - unikaj p?tli

fun1 <- function(x) {
  for(i in 1:length(x))
    x[i] <- x[i]*5
  return(x)
}

fun2 <- function(x) {
  return(5*x)
}

x <- rnorm(10000)
mb <- microbenchmark(fun1(x), fun2(x))
mb
plot(mb)



#U?ywaj funkcji wbudowanych
ccsum1 <- function(x) {
  y <- numeric(length(x))
  y[1] <- x[1]
  for(i in 2:length(y))
    y[i] <- y[i-1] + y[i]
  return(y)
}

ccsum2 <- function(x) {
  return(cumsum(x))
}


mb <- microbenchmark(ccsum1(x), ccsum2(x))
mb
plot(mb)
cumsum


#por?wnanie dodawanie przyrostowego z pocz?tkow? deklaracj? wielko?ci wektora
fun1 <- function(n) {
  x <- numeric()
  for(i in 1:n)
    x <- c(x, i)
  return(x)
}

fun2 <- function(n) {
  x <- numeric(n)
  for(i in 1:n)
    x[i] <- i
  return(x)
}

#Gdy obiekt jest kr?tki, nie ma to znaczenia
mb <- microbenchmark(fun1(1000), fun2(1000))
mb
plot(mb)
#Dopiero przy d?u?szych jest istotne
mb <- microbenchmark(fun1(10000), fun2(10000))
mb
plot(mb)



#Podobnie dla macierzy
fun1 <- function(n) {
  A <- NULL
  for(i in 1:n) {
    x <- rnorm(n)
    A <- cbind(A, x)
  }
  return(A)
}

fun2 <- function(n) {
  A <- matrix(0, ncol=n, nrow=n)
  for(i in 1:n) {
    A[,i] <- rnorm(n)
  }
  attr(A, "dimnames") <- NULL
  return(A)
}

#Tym istotniejsze, im wi?kszy obiekt (realokacja)
mb <- microbenchmark(fun1(100), fun2(100))
mb
plot(mb)

mb <- microbenchmark(fun1(200), fun2(200))
mb
plot(mb)



#Wykorzystanie apply
A<-array(rnorm(1000*2000), dim=c(1000,2000))

mmean1 <- function(A) {
   n_col <- dim(A)[2]
   y <- numeric(n_col)
   for(i in 1:n_col)
     y[i] <- sum(A[,i])
   return(y)
}

mmean2 <- function(A) {
  return(apply(A,2,sum))
}


### little change by Maciej Beresewicz (compare colMeans function)
mmean3 <- function(A) {
  return(colMeans(A))
}


mb <- microbenchmark(mmean1(A), mmean2(A),mmean3(A))
mb
plot(mb)


#funkcja lapply!
x1 <- rnorm(100) 
y <- 2 + 3 * x1 + 0.1*rnorm(100)
X <- list(model1=x1, model2=rnorm(100), model3=rnorm(100), model4=rnorm(100), model5=rnorm(100))

f <- function(x)(data.frame(y=y, x=x))
df.list <- lapply(X, f)   #lista data.frame'?w
str(df.list)

f <- function(df) lm(y ~ x, df)
mm <- lapply(df.list, f)
mm

sapply(mm, function(ll) sum(residuals(ll)^2))



#Korzystanie z funkcji 'Reduce'
#Wygladzanie wykladnicze

x <- 5+rnorm(1000)

smooth1 <- function(x, lambda=0.9) {
  y <- numeric(length(x))
  y[1] <- x[1]
  for(i in 2:length(y))
    y[i] <- lambda*y[i-1] + (1-lambda)*x[i]
  return(y)
}

smooth2 <- function(x, lambda=0.9) {
  f <- function(a, b)
    lambda*a + (1-lambda)*b
  out <- Reduce(f, x, accumulate=T)
  return(out)
}

mb <- microbenchmark(smooth1(x), smooth2(x))
mb
plot(mb)

#Pakiet data.table
library(data.table)
df <- data.frame(x=rnorm(1000000), y=1:1000000)
dt <- data.table(df)

fun1 <- function(df) {
  df$y <- 2*df$y
  return(df)
}

fun2 <- function(dt) {
  dt[, y:=2*y]
}

mb <- microbenchmark(fun1(df), fun2(dt))
mb
plot(mb)

#Przekazywanie argumentu przez referencj?
df <- data.frame(x=rnorm(1000000), y=1:1000000)
dt <- data.table(df)

fun1(dt)
dt            #nic si? nie zmieni?o

fun2(dt)
dt            # a teraz?


#Kompilacja do kodu po?redenigo
f<-function(x) {
  for (i in 1:length(x)) 
    if(x[i]<0) 
        x[i]<-5
}

library(compiler)

fc <- cmpfun(f)


x<-rnorm(10000)
mb <- microbenchmark(f(x), fc(x))
mb
plot(mb)



#Funkcja resample
setwd('Meeting - 2014-03-14/Speedup R - Pawel Kliber (PUE)') #change by Maciej Beresewicz
source("resample.R")
load("ago.Rdata")
dd <- ago[ago$d==1,]
mb <- microbenchmark(resample_rr(dd,30,1,1), resample(dd,30,1,1))
mb
plot(mb)
#stosunek ?rednich szyko?ci oblicze?
mean(mb$time[mb$expr==levels(mb$expr)[1]])/mean(mb$time[mb$expr==levels(mb$expr)[2]])



#Rcpp - "przykladyRcpp.cpp
library(Rcpp)
library(RcppArmadillo)

sourceCpp("przykladyRcpp.cpp")

dodaj(5,6)
x <- c(1, 2, 3, 4, 5)
x
add10(x)
x
add10a(x)
x
add10b(x)
x

#U?ycie funkcji R w C++
uniqueCpp(c(1,1,2,2,1,1,3,1,2,3,1,2))


#U?ycie biblioteki STL
sumCpp(c(1,2,3,4))

#Wielowymiarowy rozklad normalny - o ?redniej 0
#wykorzystanie biblioteki Amarilldo
Z<-rmnorm(100000, c(1,2), matrix(c(1,1,1,4), ncol=2))
mean(Z[,1])
mean(Z[,2])
sd(Z[,1])
sd(Z[,2])
cor(Z[,1], Z[,2])


#Generowanie rozk?adu normalnego, wielowymiarowego
# Funkcja R oraz C++ z bibliotek? Armadillo
rmnorm_r <- function(m, mu, Sigma) {
  n <- ncol(Sigma)
  D <- chol(Sigma)
  X <- matrix(rnorm(n*m), ncol=n)
  Y <- matrix(rep(mu, each=m), ncol=n) + X %*% D
  return(Y)
}
mb <- microbenchmark(rmnorm_r(1000, c(1,2), matrix(c(4,3,3,9), ncol=2)), rmnorm(1000, c(1,2), matrix(c(4,3,3,9), ncol=2)), control=list(warmup=20))
mb
plot(mb)


#Resample w Rcpp
sourceCpp("resample.cpp")
source("resample.R")
mb <- microbenchmark(resample_rr(dd,30,1,1), resample(dd,30,1,1), resampleRcpp(dd,30,1,1), control=list(warmup=10))
mb
plot(mb)

