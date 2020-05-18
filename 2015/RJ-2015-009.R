●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●</div>
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●</div>
●
●
●
●
●
●
●●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●</div>
●●
●
●
●
●
●
●
●
●
●
●
● ●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●</div>
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●</div>
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●</div>
●
●
●
●
●
●
●
●
●
●●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●</div>
ˆ
ˆ
ˆ
ˆ
ˆ
ˆ
ˆ
ˆ
ˆ
# pull the next location to try from stack
L <- peek_top(V)
## Maze generation pseudocode
V <- rstack()
set the current location L to the start of the maze at S
while(any cell is unvisited) {
find a random unvisited neighbor N of L
if(N is not NULL) {
V <- insert_top(V, L)
build a corridor between N and L
L <- N
mark L visited
else if(the stack is not empty)
V <- without_top(V)
else
set L to a random unvisited cell in the maze
solve_ascii_maze_dfs <- function(maze) {
## find the start and end of the maze
end <- data.frame(which(maze == "E", arr.ind = TRUE))
start <- data.frame(which(maze == "S", arr.ind = TRUE))
maze[maze == " " | maze == "E" | maze == "S"] <- "." ## mark corridors unvisited
loc <- start ## initialize the solution stack and start location
path <- rstack()
path_history <- rstack()
path <- insert_top(path, loc)
step <- 1
loc <- peek_top(path)
## if the current location is unvisited, mark it visited with the current
## timestep
if(maze[loc$row, loc$col] == ".") {maze[loc$row, loc$col] <- step}
## grab a random unvisited neighbor; if there is one push it on the current
## path
nextloc <- random_unvisited_neighbor(maze, peek_top(path), dist = 1)
if(!is.null(nextloc)) {
path <- insert_top(path, nextloc)
} else {
# otherwise backtrack and try again
path <- without_top(path)
}
step <- step + 1
path_history <- insert_top(path_history, path)
}
end <- peek_top(path) ## mark solution by negating visit numbers
maze[end$row, end$col] <- step
while(!empty(path)) {
loc <- peek_top(path)
path <- without_top(path)
maze[loc$row, loc$col] <- -1 * as.numeric(maze[loc$row, loc$col])
}
return(list(maze, path_history))
}
solve_ascii_maze_bfs <- function(maze) {
## find the start and end of the maze
end <- data.frame(which(maze == "E", arr.ind = TRUE))
start <- data.frame(which(maze == "S", arr.ind = TRUE))
maze[maze == " " | maze == "E" | maze == "S"] <- "." ## mark corridors unvisited
loc <- start ## initialize the solution stack and start location
visits <- rdeque()
visits_history <- rstack()
visits <- insert_back(visits, loc)
## keep a stack to remember where each visit came from in the BFS
camefrom <- rstack()
step <- 1
while(!empty(visits)) { ## while there are still cells we can visit
loc <- peek_front(visits)
visits <- without_front(visits)
neighbors <- random_unvisited_neighbor(maze, loc, dist = 1, all = TRUE)
for(neighbor in neighbors) {
camefrom <- insert_top(camefrom, list(from = loc, to = neighbor))
## push neighbors on the queue and mark them visited with the timestep
visits <- insert_back(visits, neighbor)
maze[neighbor$row, neighbor$col] <- step
step <- step + 1
visits_history <- insert_top(visits_history, visits)
}
}
loc <- end ## set loc to the end and track the camefrom path back to find the solution
while(any(loc != start)) {
maze[loc$row, loc$col] <- as.numeric(maze[loc$row, loc$col]) * -1
pathpart <- peek_top(camefrom)
while(any(pathpart$to != loc)) {
camefrom <- without_top(camefrom)
pathpart <- peek_top(camefrom)
}
loc <- pathpart$from
}
maze[loc$row, loc$col] <- as.numeric(maze[loc$row, loc$col]) * -1
return(list(maze, visits_history))
}
shawn.oneil@cgrb.oregonstate.edu
rstack</div>
rdeque</div>
rpqueue</div>
0
20
40
0
20
40
0
20
40</div>
Inserts</div>
Removes</div>
Mix</div>
Number Operations Completed
Seconds</div>
NULL</div>
z</div>
x</div>
y
x <- as.rstack(c("A", "B", "C")) 
y <- insert_top(x, "D") 
z <- without_top(x)</div>
x <- as.rdeque(c("A", "B", "C", "D",         
               "E", "F", "G", "H")) 
y <- insert_back(x, "I") 
z <- without_front(x)</div>
x</div>
y
z</div>
w <- as.rpqueue(c("A", "B", "C", "D")) 
x <- insert_back(w, "E") 
y <- insert_back(x, "F") 
z <- without_front(y)
Delayed/Lazy Evaluation
:
