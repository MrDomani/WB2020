var.eqn.x <- "(alpha * x) * (1 - (x / beta)) - ((delta * (x^2) * y) / (kappa + (x^2)))"
var.eqn.y <- "((gamma * (x^2) * y) / (kappa + (x^2))) - mu * (y^2)"
model.parms <- c(alpha = 1.54, beta = 10.14, delta = 1, gamma = 0.476,
kappa = 1, mu = 0.112509)
parms.eqn.x <- Model2String(var.eqn.x, parms = model.parms)
## Do not print to screen.
parms.eqn.y <- Model2String(var.eqn.y, parms = model.parms, supress.print = TRUE)
model.state <- c(x = 1, y = 2)
model.sigma <- 0.05
model.time <- 1000 # we used 12500 in the figures
model.deltat <- 0.025
ts.ex1 <- TSTraj(y0 = model.state, time = model.time, deltat = model.deltat,
x.rhs = parms.eqn.x, y.rhs = parms.eqn.y, sigma = model.sigma)
## Could also use TSTraj to combine equation strings and parameter values.
## ts.ex1 <- TSTraj(y0 = model.state, time = model.time, deltat = model.deltat,
## x.rhs = var.eqn.x, y.rhs = var.eqn.y, parms = model.parms, sigma = model.sigma)
TSPlot(ts.ex1, deltat = model.deltat) # Figure 2
TSPlot(ts.ex1, deltat = model.deltat, dim = 2) # Figure 3a
TSDensity(ts.ex1, dim = 1) # like Figure 2 histogram
TSDensity(ts.ex1, dim = 2) # Figure 3b
Φ
## If not done in a previous step.
parms.eqn.x <- Model2String(var.eqn.x, parms = model.parms)
## Do not print to screen.
parms.eqn.y <- Model2String(var.eqn.y, parms = model.parms, supress.print = TRUE)
## Could also input the values by hand and use this version.
## parms.eqn.x <- "1.54 * x * (1.0 - (x / 10.14)) - (y * (x^2)) / (1.0 + (x^2))"
## parms.eqn.y <- "((0.476 * (x^2) * y) / (1 + (x^2))) - 0.112509 * (y^2)"
eq1.x <- 1.40491
eq1.y <- 2.80808
eq2.x <- 4.9040
eq2.y <- 4.06187
[
]
[
]
bounds.x <- c(-0.5, 20.0)
bounds.y <- c(-0.5, 20.0)
=
=
step.number.x <- 1000
step.number.y <- 1000 # we used 4100 in the figures
eq1.local <- QPotential(x.rhs = parms.eqn.x, x.start = eq1.x, x.bound = bounds.x,
x.num.steps = step.number.x, y.rhs = parms.eqn.y, y.start = eq1.y,
y.bound = bounds.y, y.num.steps = step.number.y)
eq2.local <- QPotential(x.rhs = parms.eqn.x, x.start = eq2.x, x.bound = bounds.x,
x.num.steps = step.number.x, y.rhs = parms.eqn.y, y.start = eq2.y,
y.bound = bounds.y, y.num.steps = step.number.y)
Φ
(
)
= Φ
ex1.global <- QPGlobal(local.surfaces = list(eq1.local, eq2.local),
unstable.eq.x = c(0, 4.2008), unstable.eq.y = c(0, 4.0039),
x.bound = bounds.x, y.bound = bounds.y)
QPContour(surface = ex1.global, dens = c(1000, 1000), x.bound = bounds.x,
y.bound = bounds.y, c.parm = 5) # right side of Figure 4
∂Φ
∂Φ
+
## Calculate all three vector fields.
VDAll <- VecDecomAll(surface = ex1.global, x.rhs = parms.eqn.x, y.rhs = parms.eqn.y,
x.bound = bounds.x, y.bound = bounds.y)
## Plot the deterministic skeleton vector field.
VecDecomPlot(x.field = VDAll[, , 1], y.field = VDAll[, , 2], dens = c(25, 25),
x.bound = bounds.x, y.bound = bounds.y, xlim = c(0, 11), ylim = c(0, 6),
arrow.type = "equal", tail.length = 0.25, head.length = 0.025)
## Plot the gradient vector field.
VecDecomPlot(x.field = VDAll[, , 3], y.field = VDAll[, , 4], dens = c(25, 25),
x.bound = bounds.x, y.bound = bounds.y, arrow.type = "proportional",
tail.length = 0.25, head.length = 0.025)
## Plot the remainder vector field.
VecDecomPlot(x.field = VDAll[, , 5], y.field = VDAll[, , 6], dens = c(25, 25),
x.bound = bounds.x, y.bound = bounds.y, arrow.type = "proportional",
tail.length = 0.35, head.length = 0.025)
var.eqn.x <- "- (y - beta) + mu * (x - alpha) * (1 - (x - alpha)^2 - (y - beta)^2)"
var.eqn.y <- "(x - alpha) + mu * (y - beta) * (1 - (x - alpha)^2 - (y - beta)^2)"
model.state <- c(x = 3, y = 3)
model.parms <- c(alpha = 4, beta = 5, mu = 0.2)
model.sigma <- 0.1
model.time <- 1000 # we used 2500 in the figures
model.deltat <- 0.005
ts.ex2 <- TSTraj(y0 = model.state, time = model.time, deltat = model.deltat,
x.rhs = var.eqn.x, y.rhs = var.eqn.y, parms = model.parms, sigma = model.sigma)
TSPlot(ts.ex2, deltat = model.deltat) # Figure 8
TSPlot(ts.ex2, deltat = model.deltat, dim = 2, line.alpha = 25) # Figure 9a
TSDensity(ts.ex2, dim = 1) # Histogram
TSDensity(ts.ex2, dim = 2) # Figure 9b
eqn.x <- Model2String(var.eqn.x, parms = model.parms)
eqn.y <- Model2String(var.eqn.y, parms = model.parms)
eq1.qp <- QPotential(x.rhs = eqn.x, x.start = 4.15611, x.bound = c(-0.5, 7.5),
x.num.steps = 4000, y.rhs = eqn.y, y.start = 5.98774, y.bound = c(-0.5, 7.5),
y.num.steps = 4000)
Φ
QPContour(eq1.qp, dens = c(1000, 1000), x.bound = c(-0.5, 7.5),
y.bound = c(-0.5, 7.5), c.parm = 10)
(
)
(
)
var.eqn.x <- "x * ((1 + alpha1) - (x^2) - x * y - (y^2))"
var.eqn.y <- "y * ((1 + alpha2) - (x^2) - x * y - (y^2))"
model.state <- c(x = 0.5, y = 0.5)
model.parms <- c(alpha1 = 1.25, alpha2 = 2)
model.sigma <- 0.8
model.time <- 5000
model.deltat <- 0.01
ts.ex3 <- TSTraj(y0 = model.state, time = model.time, deltat = model.deltat,
x.rhs = var.eqn.x, y.rhs = var.eqn.y, parms = model.parms, sigma = model.sigma)
TSPlot(ts.ex3, deltat = model.deltat) # Figure 12
TSPlot(ts.ex3, deltat = model.deltat, dim = 2 , line.alpha = 25) # Figure 13a
TSDensity(ts.ex3, dim = 1) # Histogram of time series
TSDensity(ts.ex3, dim = 2 , contour.levels = 20 , contour.lwd = 0.1) # Figure 13b
equation.x <- Model2String(var.eqn.x, parms = model.parms)
equation.y <- Model2String(var.eqn.y, parms = model.parms)
bounds.x <- c(-3, 3); bounds.y <- c(-3, 3)
step.number.x <- 6000; step.number.y <- 6000
eq1.x <- 0; eq1.y <- -1.73205
eq2.x <- 0; eq2.y <- 1.73205
eq1.local <- QPotential(x.rhs = equation.x, x.start = eq1.x, x.bound = bounds.x,
x.num.steps = step.number.x, y.rhs = equation.y, y.start = eq1.y,
y.bound = bounds.y, y.num.steps = step.number.y)
eq2.local <- QPotential(x.rhs = equation.x, x.start = eq2.x, x.bound = bounds.x,
x.num.steps = step.number.x, y.rhs = equation.y, y.start = eq2.y,
y.bound = bounds.y, y.num.steps = step.number.y)
(
)
(
)
Φ
(
Φ
)
(
Φ
)
= Φ
= Φ
ex3.global <- QPGlobal(local.surfaces = list(eq1.local, eq2.local),
unstable.eq.x = c(0, -1.5, 1.5), unstable.eq.y = c(0, 0, 0), x.bound = bounds.x,
y.bound = bounds.y)
QPContour(ex3.global, dens = c(1000, 1000), x.bound = bounds.x, y.bound = bounds.y,
c.parm = 5)
(
)
(
)
(
)
(
)
(
)
(
)
