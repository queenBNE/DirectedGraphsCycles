# Examples 
source('cycles.johnson.R')

g <- graph.data.frame(matrix(c(2,1,2,4,3,2,3,5,4,3,4,5,5,6,6,5), ncol=2, byrow=TRUE),directed=T, data.frame("name"=1:6))
plot(g)
get.elementary.circuits(g)

g <- graph.data.frame(matrix(c(1,2,1,4,2,4,3,2,3,8,3,4,4,1,4,2,4,5,5,7,6,3,7,6,8,5,8,7,8,9,9,7,9,6),  ncol=2, byrow=TRUE),directed=T, data.frame("name"=1:9))
plot(g)
get.elementary.circuits(g)

g <- graph.data.frame(matrix(c(1,2,1,3,2,3,3,4,4,1), ncol=2, byrow=TRUE),directed=T, data.frame("name"=1:4))
plot(g)
get.elementary.circuits(g)