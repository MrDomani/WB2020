cube_iterate <- function(p) {
if (p == 1) {
return(rbind(0, 1))
}
lower_dim_cube <- cube_iterate(p - 1)
rbind(
cbind(lower_dim_cube, 0),
cbind(lower_dim_cube, 1)
)
}
cube_permute <- function(p) {
as.matrix(
do.call(
expand.grid,
rep(list(c(0, 1)), p)
)
)
}
cube_edges_length1 <- function(cube) {
p <- ncol(cube)
num_points <- 2 ^ p
from_to <- matrix(NA, nrow = num_points * p / 2, ncol = 2)
next_store_position <- 1
for (i in 1:(num_points - 1)) {
for (j in (i + 1):num_points) {
d1 <- sum((cube[i, ] - cube[j, ]) ^ 2)
if (d1 == 1) {
from_to[next_store_position, ] <- c(i,j)
next_store_position <- next_store_position + 1
}
}
}
from_to
}
cube_edges_binomial <- function(cube) {
p <- ncol(cube)
num_points <- 2 ^ p
from_to <- matrix(NA, nrow = num_points * p / 2, ncol = 2)
next_store_position <- 1
for (i in 1:(num_points - 1)) {
for (j in 1:p) {
if (cube[i, j] == 0) {
from_to[next_store_position, ] <- c(i, 2 ^ (j - 1) + i)
next_store_position <- next_store_position + 1
}
}
}
from_to
}
library(bitops)
cube_edges_binary <- function(p) {
vertices <- 0:(2 ^ p - 1)
from_verts <- vertices[
rep(1:(2 ^ p), each = p)
]
from_to <- data.frame(
from = from_verts,
to = bitXor(from_verts, 2 ^ (0:(p - 1)))
)
from_to <- subset(from_to, from < to) + 1
from_to
}
cube_solid_random <- function(p, n = 850 * 2 ^ p) {
matrix(runif(n * p), ncol = p)
}
cube_solid_grid <- function(p, n) {
grid <- list(seq(0, 1, length = n))
do.call(expand.grid, rep(grid, p))
}
cube_face_grid <- function(p, n = 10) {
face <- cube_solid_grid(p - 1, n)
face_n <- nrow(face)
faces <- do.call(data.frame, rep(list(X = rep(0:1, each = p * face_n)), p))
for(i in seq_len(p)) {
faces[(face_n * (i - 1) + 1):(face_n * i), -i] <- face
faces[(face_n * (i - 1) + 1):(face_n * i) + (p * face_n), -i] <- face
}
return(as.matrix(faces))
}
cube_face_random <- function(p, n = 850 * 2 ^ (p - 1)) {
faces <- cube_solid_random(p, 2 * p * n)
for (i in seq_len(p)) {
faces[(n * (i - 1) + 1):(n * i), i] <- 0
faces[(n * (i - 1) + 1):(n * i) + (p * face_n), i] <- 1
}
faces
}
norm_vec <- function(x) {
x / sqrt(sum(x ^ 2))
}
sphere_hollow <- function(p, n = p * 500) {
x <- matrix(rnorm(n * p), ncol = p)
t(apply(x, 1, norm_vec))
}
sphere_solid_random <- function(p, n = p * 500) {
sphere_hollow(p, n) * runif(n) ^ (1 / p)
}
helmert <- function(d) {
helmert_mat <- matrix(NA, nrow = d, ncol = d)
helmert_mat[1, ] <- rep(1 / sqrt(d), d)
for (i in 1:(d - 1)) {
helmert_mat[i + 1, ] <- c(
rep(1 / sqrt(i * (i + 1)), i),
-i / sqrt(i * (i + 1)),
rep(0, d - i - 1)
)
}
helmert_mat
}
simplex <- function(p) {
x <- diag(p)
# center simplex
x <- x - matrix(1 / p, p, p)
hm <- helmert(p)
final <- (x %*% t(hm))[, -1]
final
}
simplex_wires <- function(simplex) {
wires <- do.call(
expand.grid,
list(
c(1:nrow(simplex)),
c(1:nrow(simplex))
)
)
wires[!wires[,1] == wires[,2],]
}
torus<-c(
rep(cos(theta[p - 1]) * radius[p - 1], p - 1),
sin(theta[p - 1]) * radius[p - 1]
)
for (i in (p - 1):2) {
for (j in (i - 1):1) {
torus[j] <- (torus[j] + radius[i - 1]) * cos(theta[i - 1])
}
torus[i] <- (torus[i] + radius[i - 1]) * sin(theta[i - 1])
}
finished <- rbind(finished, torus.row)
matrix(
do.call(rbind, as.list(
replicate(
n,
torus.row(radius, p)
)
)),
ncol = p, byrow = TRUE
)
theta <- runif(p - 1, min = 0, max = 2 * pi)
radius <- 2 ^ ((p - 2):0)
