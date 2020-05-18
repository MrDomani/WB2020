install.packages("smoof")
library(smoof)
fn <- makeSingleObjectiveFunction(
name = "2D-Sphere",
fn = function(x) x[1]^2 + x[2]^2,
par.set = makeNumericParamSet(
len = 2L, id = "x",
lower = c(-10, -10), upper = c(10, 10),
vector = TRUE
),
tags = "unimodal",
global.opt.param = c(0, 0),
global.opt.value = 0
)
print(fn)
# Single-objective function
# Name: 2D-Sphere
# Description: no description
# Tags:
# Noisy: FALSE
# Minimize: TRUE
# Constraints: TRUE
# Number of parameters: 2
# Type len Def Constr Req Tunable Trafo
# x numericvector 2 - -10,-10 to 10,10 - TRUE -
# Global optimum objective value of 0.0000 at
# x1 x2
# 1 0 0
fn2 <- makeSingleObjectiveFunction(
name = "Shifted-Sphere",
fn = function(x) {
shift = which(x$disc == letters[1:3]) * 2
return(x$num^2 + shift)
},
par.set = makeParamSet(
makeNumericParam("num", lower = -5, upper = 5),
makeDiscreteParam("disc", values = letters[1:3])
),
has.simple.signature = FALSE
)
print(fn2)
# Single-objective function
# Name: Shifted-Sphere
# Description: no description
# Tags:
# Noisy: FALSE
# Minimize: TRUE
# Constraints: TRUE
# Number of parameters: 2
# Type len Def Constr Req Tunable Trafo
# num numeric - - -5 to 5 - TRUE -
# disc discrete - - a,b,c - TRUE -
fn2(list(num = 3, disc = "c"))
# [1] 15
plot(fn, render.contours = TRUE, render.levels = TRUE)
plot3D(fn, contour = TRUE)
library(ggplot2)
pl <- autoplot(fn2, use.facets = TRUE) # basic call
pl + ylim(c(0, 35)) + facet_grid(. ~ disc) # (one column per discrete value)
getGlobalOptimum(fn)$param
# x1 x2
# 1 0 0
getGlobalOptimum(fn)$value
# [1] 0
getGlobalOptimum(fn)$is.minimum
# [1] TRUE
getNumberOfParameters(fn)
# [1] 2
getNumberOfObjectives(fn)
# [1] 1
getLowerBoxConstraints(fn)
# x1 x2
# -10 -10
fn = makeDTLZ2Function(dimensions = 2L, n.objectives = 2L)
visualizeParetoOptimalFront(fn, show.only.front = TRUE)
fn.names <- filterFunctionsByTags(tags = "multimodal")
head(fn.names)
# [1] "Ackley" "Adjiman" "Alpine N. 1" "Alpine N. 2" "Bartels Conn"
# [6] "Bird"
print(length(fn.names))
# [1] 46
# Type len Def Constr Req Tunable Trafo
# x1 x2
# 1 0 0
fns <- makeFunctionsByName(fn.names, dimensions = 2)
all(sapply(fns, isSmoofFunction))
# [1] TRUE
print(length(fns))
# [1] 46
print(fns[[1L]])
# Single-objective function
# Name: 2-d Ackley Function
# Description: no description
# Tags: single-objective, continuous, multimodal, differentiable, non-separable, scalable
# Noisy: FALSE
# Minimize: TRUE
# Constraints: TRUE
# Number of parameters: 2
# x numericvector 2 - -32.8,-32.8 to 32.8,32.8 - TRUE -
# Global optimum objective value of 0.0000 at
set.seed(123)
fn <- makeBraninFunction()
fn <- addCountingWrapper(fn)
fn <- addLoggingWrapper(fn, logg.x = TRUE, logg.y = TRUE)
par.set <- getParamSet(fn)
lower <- getLower(par.set); upper = getUpper(par.set)
res <- optim(c(0, 0), fn = fn, method = "Nelder-Mead")
res$counts[1L] == getNumberOfEvaluations(fn)
# [1] TRUE
head(getLoggedValues(fn, compact = TRUE))
# x1 x2 y1
# 1 0.00 0.00 55.60
# 2 0.10 0.00 53.68
# 3 0.00 0.10 54.41
# 4 0.10 0.10 52.53
# 5 0.15 0.15 51.01
# 6 0.25 0.05 50.22
# bossek@wi.uni-muenster.de
# z</div>
# 20
# 40
# 60
# 80</div>
# z</div>
# 20
# 40
# 60</div>
# z</div>
# 200
# 400
# 600
# 800</div>
# z</div>
# 5000
# 10000
# 15000
# 20000
# 25000
# 30000</div>
# z</div>
# 500
# 1000
# 1500</div>
# z</div>
# −0.8
# −0.6
# −0.4
# −0.2</div>
# −10 −5 0 5 10
# −10 −5 0 5 10
# x1
# x2</div>
#  20 
#  40 
#  60 
#  80 
#  100 
#  120 
#  120 
#  120 
#  120 
#  140 
#  140 
#  140 
#  140 
#  160 
#  160 
#  160 
#  160 
#  180 
#  180 
#  180 
#  180 </div>
# a</div>
# b</div>
# c</div>
# 0
# 10
# 20
# 30
# −5.0 −2.5 0.0 2.5 5.0 −5.0 −2.5 0.0 2.5 5.0 −5.0 −2.5 0.0 2.5 5.0
# num
# y
# Shifted−Sphere</div>
# x
# y
# z
# 50
# 100
# 150</div>
# ∗
# ∗
# ∗
# ∗
