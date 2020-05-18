
f <- BooleanFunction("01011010")
p <- f$ANF()
data.class(p)
# [1] "Polynomial"
q <- Polynomial("0101")
p
# [1] "x1 + x3"
q
# [1] "x1*x2 + x1"
p * q
# [1] "x1*x2*x3 + x1*x2 + x1*x3 + x1"
p * q + q
# [1] "x1*x2*x3 + x1*x3"
deg(p * q + q)
# [1] 3
r <- p * q + q
x <- c(0, 1, 1) # x1=0, x2=1, x3=1
if( length(x) != r$n() ) stop("this is an error")
p[[x]]
# [1] 1
x[3] <- 0
p[[x]]
# N <- 2^2^n # number of functions with n var
# allRes <- vector("integer", N)
# allAIs <- vector("integer", N)
# for( i in 1:N ) { # forall Boolean function
# f <- BooleanFunction( toBin(i-1,2^n) )
# allRes[i] <- res(f) # get its resiliency
# allAIs[i] <- ai(f) # and algebraic immunity
# }
# xlabel <- "Truth tables as integers"
# plot( x=1:N, y=allRes, type="b",
# xlab=xlabel, ylab="Resiliency" )
# plot( x=1:N, y=allAIs, type="b",
# xlab=xlabel, ylab="Algebraic immunity" )
# plot( 1:N, allRes+allAIs, type="b",
# xlab="f", ylab="ai(f)+res(f)" )
# n <- 8
# data <- data.frame( matrix(nrow=0,ncol=4) )
# names(data) <- c( "deg", "ai", "nl", "res" )
# for( i in 1:1000 ) { # for 1000 random functions
# randomTT <- round(runif(2^n, 0, 1))
# randomBF <- BooleanFunction(randomTT)
# data[i,] <-c( deg(randomBF), ai(randomBF),
# nl(randomBF), res(randomBF))
# }
mean(data)
# deg ai nl res
# 7.479 3.997 103.376 -0.939
sd(data)
# deg ai nl res
# 0.5057814 0.0547174 3.0248593 0.2476698
# data <- getTheBooleanFunctions()
# chistat <- computeChiStat(data)
# outcome <- "random"
# if(chistat > qchisq(0.99, df=n))
# outcome <- "cipher"
# print(outcome)
# frederic.lafitte@rma.ac.be
# M
# M
# 0.0 0.5 1.0 1.5 2.0
# Algebraic immunity</div>
