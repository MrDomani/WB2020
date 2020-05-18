library(rPref)
p <- high(mpg) * high(hp)
psel(mtcars, p)
p <- high(mpg, df = mtcars) * high(hp) * low(qsec)
p
# [Preference] high(mpg) * high(hp) * low(qsec)
# * associated data source: data.frame "mtcars" [32 x 11]
library(dplyr)
select(peval(p), mpg, hp, qsec)
# mpg hp qsec
# Mazda RX4 21.0 110 16.46
# Merc 450SE 16.4 180 17.40
# Merc 450SL 17.3 180 17.60
# Fiat 128 32.4 66 19.47
# Toyota Corolla 33.9 65 19.90
# Porsche 914-2 26.0 91 16.70
# Lotus Europa 30.4 113 16.90
# Ford Pantera L 15.8 264 14.50
# Ferrari Dino 19.7 175 15.50
# Maserati Bora 15.0 335 14.60
mtcars %>% filter(am == 0) %>% psel(p)
p <- true(am == 1) & high(gear)
mtcars0 <- select(mtcars, am, gear, hp, cyl)
p <- p * high(hp/cyl)
psel(mtcars0, p)
# am gear hp cyl
# Maserati Bora 1 5 335 8
psel(mtcars0, p, top = 3)
# am gear hp cyl .level
# Maserati Bora 1 5 335 8 1
# Ford Pantera L 1 5 264 8 2
# Duster 360 0 3 245 8 3
psel(mtcars0, p, at_least = 3)
# am gear hp cyl .level
# Maserati Bora 1 5 335 8 1
# Ford Pantera L 1 5 264 8 2
# Duster 360 0 3 245 8 3
# Camaro Z28 0 3 245 8 3
# Ferrari Dino 1 5 175 6 3
p <- -(true(wt > mean(wt)) & (high(qsec) * low(hp/wt)))
p
# [Preference] high(mpg) * high(hp) * high(cyl)

grouped_cars <- group_by(mtcars, cyl)
opt_cars <- psel(grouped_cars, high(wt) * low(qsec))
as.data.frame(summarize(opt_cars, n()))
# cyl n()
# 1 4 3
# 2 6 4
# 3 8 7
p <- high(mpg, df = mtcars[1:10,]) * low(wt)
p
# [Preference] high(mpg) * low(wt)
# * associated data source: data.frame "mtcars[1:10, ]" [10 x 11]
p <- high(mpg/max(mtcars$mpg) + hp/max(mtcars$hp), df = mtcars)
p
# [Preference] high(mpg/33.9 + hp/335)
# * associated data source: data.frame "mtcars" [32 x 11]
gen_data <- function(N, cor) {
rndvals <- matrix(runif(2 * N), N, 2)
corvals <- runif(N)
corvals <- cbind(corvals, 1 - corvals)
df <- as.data.frame((1 - abs(cor)) * rndvals + abs(cor) * (1 - corvals))
return(df)
}
df <- gen_data(1E6, -0.7)
result1 <- psel(df, low(x) * low(y))
result2 <- as.data.frame(t(nondominated_points(t(as.matrix(df)))))
setequal(result1, result2)
TRUE
options(rPref.parallel = FALSE)
time_rpref_serial <- vapply(1:10, function(i)
system.time({ psel(gen_data(5E6, -0.7), low(x) * low(y)) })[3], 0)
options(rPref.parallel = TRUE)
time_rpref_parallel <- vapply(1:10, function(i)
system.time({ psel(gen_data(5E6, -0.7), low(x) * low(y)) })[3], 0)
time_emoa <- vapply(1:10, function(i)
system.time({ nondominated_points(t(as.matrix(gen_data(5E6, -0.7)))) })[3], 0)
p <- (true(am == 1) & high(gear)) * high(mpg)
df <- psel(mtcars, p, top_level = 5)
plot_btg(df, p, labels)
res <- psel(mtcars, high(mpg) * high(hp), top = nrow(mtcars))
ggplot(res, aes(x = mpg, y = hp, color = factor(.level))) +
res <- mtcars %>% psel(high(mpg) | high(hp), top = nrow(mtcars)) %>%
arrange(mpg, -hp)
ggplot(res, aes(x = mpg, y = hp, color = factor(.level))) +
