library(ggplot2)
# Zad 1

#####
g1 <- 1 - pnorm(vec)

g2 <- dnorm(vec)/vec
  
g3 <- dnorm(vec)* vec / (vec^2 +1)

dframe <- data.frame(vec, g1, g2, g3, g1/g2, g1/g3)
#####

set.seed(100)
vec <- seq(.2, 4, .001) 

g1 <- function(vec){1 - pnorm(vec)}
g2 <- function(vec){dnorm(vec)/vec}
g3 <-  function(vec){ dnorm(vec)* vec / (vec^2 +1)}

create <- function(vec){
  v1 <- g1(vec)
  v2 <- g2(vec)
  v3 <- g3(vec)
  v4 <- v1/v2
  v5 <- v1/v3
  data.frame(v1, v2, v3, v4, v5)
}

dframe <- create(vec)

qplot(vec, dframe$v1, data = dframe, main = "g1", geom =  "smooth")
qplot(vec, dframe$v2, data = dframe, main = "g2", geom =  "smooth")
qplot(vec, dframe$v3, data = dframe, main = "g3", geom =  "smooth")
qplot(vec, dframe$v4, data = dframe, ylim = c(0.2, 1), main = "g1/g2", geom =  "smooth")
qplot(vec, dframe$v5, data = dframe, main = "g1/g3", geom =  "smooth")

# Zad 2
set.seed(100)
vec <- c(seq(10^2, 10^3, 10), seq(10^3, 10^4, 10^2), seq(10^4, 10^5, 10^3), seq(10^5, 10^6, 10^4), seq(10^6, 10^7, 10^5), seq(10^7, 10^8, 10^6), seq(10^8, 10^9, 10^7))
c <- sqrt(2*log(vec))

g1 <- function(vec, alpha){qnorm(1-alpha/2/vec)}
g2 <- function(vec, alpha){
  B <- 2 * log(2*vec/alpha) - log(2*pi)
  sqrt(B - log(B))
}
create <- function(vec, alpha){
  v11 <- g1(vec, alpha)
  v12 <- g2(vec, alpha)
  v13 <- v11/c
  v14 <- v11/v12
  dframe1 <- data.frame(vec, v11, v12, v13, v14)
}

dframe1 <- create(vec, .01)
qplot(vec, v11, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v12, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v13, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v14, data = dframe1, geom = "smooth", log = "x")

dframe2 <- create(vec, .1)
qplot(vec, v11, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v12, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v13, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v14, data = dframe1, geom = "smooth", log = "x")

dframe3 <- create(vec, .5)
qplot(vec, v11, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v12, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v13, data = dframe1, geom = "smooth", log = "x")
qplot(vec, v14, data = dframe1, geom = "smooth", log = "x")

#####
alpha <- .01
g1 <- qnorm(1-alpha/2/vec)
B <- 2 * log(2*vec/alpha) - log(2*pi)
g2 <- sqrt(B - log(B))

dframe <- data.frame(vec, g1, g2, g1/c, g1/g2)

#qplot(g1, c, data = dframe, main = "g1 vs c", geom =  "smooth")
qplot(vec, g1, data = dframe, main = "g1, alpha = .01", log = "x", geom =  "smooth")
qplot(vec, g1, data = dframe, main = "g1, alpha = .01", log = "x", geom =  "smooth")
qplot(vec, g1/c, data = dframe, log = "x", geom =  "smooth")
qplot(vec, g1/g2, data = dframe, log = "x", geom =  "smooth")

alpha <- .1
g1 <- qnorm(1-alpha/2/vec)
c <- sqrt(2*log(vec))
qplot(g1, c, type = "l", main = "g1 vs c", geom =  "smooth")
B <- 2 * log(2*vec/alpha) - log(2*pi)
g2 <- sqrt(B - log(B))
qplot(vec, g1, type = "l", main = "g1, alpha = .1", log = "x", geom =  "smooth")
qplot(vec, g1, type = "l", main = "g1, alpha = .1", log = "x", geom =  "smooth")
qplot(vec, g1/c, type = "l", log = "x", geom =  "smooth")
qplot(vec, g1/g2, type = "l", log = "x", geom =  "smooth")

alpha <- .5
g1 <- qnorm(1-alpha/2/vec)
c <- sqrt(2*log(vec))
qplot(g1, c, type = "l", main = "g1 vs c", geom =  "smooth")
B <- 2 * log(2*vec/alpha) - log(2*pi)
g2 <- sqrt(B - log(B))
qplot(vec, g1, type = "l", main = "g1, alpha = .5", log = "x", geom =  "smooth")
qplot(vec, g1, type = "l", main = "g1, alpha = .5", log = "x", geom =  "smooth")
qplot(vec, g1/c, type = "l", log = "x", geom =  "smooth")
qplot(vec, g1/g2, type = "l", log = "x", geom =  "smooth")
#####

# Zad 3
set.seed(100)
p <- 10^8
traj <- function(i){
  vec <- rnorm(p)
  M <- sapply(seq(8), function(k, vec){max(abs(vec[1:10^k]))}, vec)
  M
}
gk <- sqrt(2*log(10^(seq(8))))
trac <- sapply(seq(5), traj)
plot(10^seq(8), trac[,1], type = "l", main = "trajectories", log = "x", ylim = c(1, 6.4))
lines(10^seq(8), trac[,2], type = "l", main = "trajectory 2")
lines(10^seq(8), trac[,3], type = "l", main = "trajectory 3")
lines(10^seq(8), trac[,4], type = "l", main = "trajectory 4")
lines(10^seq(8), trac[,5], type = "l", main = "trajectory 5")

plot(trac[,1]/gk[1], type = "l", main = "trajectory i / gi", log = "x", ylim = c(0.2, 3))
lines(trac[,2]/gk[2], type = "l", main = "trajectory 2 / g2")
lines(trac[,3]/gk[3], type = "l", main = "trajectory 3 / g3")
lines(trac[,4]/gk[4], type = "l", main = "trajectory 4 / g4")
lines(trac[,5]/gk[5], type = "l", main = "trajectory 5 / g5")

# Zad 4 - next week bo nic nie kumam :)
