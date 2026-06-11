rpois(20,2)
?data


# install.packages("pscl")
library(pscl)
data("prussian")  # Horse kick data

p <- prussian$y
mean(p)
var(p)
ks.test(p, "ppois", lambda = mean(p))

dpois(x = 0 , lambda = 1 )
dpois(x = 0 , lambda = 2 )


dpois(x = 2 , lambda = 1 )
dpois(x = 2 , lambda = 2 )


dpois(x = 0 , lambda = 1 ) * dpois(x = 2 , lambda = 1 )
dpois(x = 0 , lambda = 2 ) * dpois(x = 2 , lambda = 2 )

dpois(x = 0 , lambda = 3 ) * dpois(x = 1 , lambda = 3 )



n_lambda = 2000
lambdas = seq(0.01, 5, length = n_lambda)
jointprob = rep(NA, length(n_lambda))
for (i in 1:nlambdas){
  jointprob[i] = dpois(x = 0, lambda=lambdas[i])*dpois(x = 2,lambda=lambdas[i])
}

plot(lambdas, jointprob, type = "l", 
     col = "purple", lwd = 2, 
     xlab = 'lambda')
?plot




n_lambda = 2000
lambdas = seq(0.01, 5, length = n_lambda)
jointprob = rep(NA, length(n_lambda))
for (i in 1:nlambdas){
  jointprob[i] = dpois(x = 0, lambda=lambdas[i])*dpois(x = 2,lambda=lambdas[i], log = T)
}

plot(lambdas, jointprob, type = "l", 
     col = "purple", lwd = 2, 
     xlab = 'lambda',
     ylab = 'LogJointProb')


log_lik_ve <- dpois(x = prussian$y, lambda = 1, log=T)
sum(log_lik_ve)

nlambdas = 2000
lambdas = seq(0.01, 5, length = nlambdas)
loglik = rep(NA, length(nlambdas))
for (i in 1:nlambdas){
  loglik[i] = sum(dpois(x = prussian$y, lambda = lambdas[i], log = TRUE))
}

plot(lambdas, loglik, type = "l", col = "purple", lwd = 2, ylab = "log-likelihood",
     xlab = 'lambda')
lambdas[which.max(loglik)]


mean(prussian$y)
sum(prussian$y)
