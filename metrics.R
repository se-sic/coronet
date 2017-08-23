
requireNamespace("igraph")





hub.indegree = function(network){
    degrees = igraph::degree(network, mode = c("in"))
    vertex = which.max(degrees)
    return(igraph::V(network)[vertex])
}

density = function(network) {
    density = igraph::graph.density(network)
    return(density)
}

avg.outdegree = function(network) {
    outdegrees = igraph::degree(network, mode = c("out"))
    avg = mean(outdegrees)
    return(avg)
}

avg.pathlength = function(network) {
    lengths = igraph::shortest.paths(network, V(network), mode = "out", weights = NA)
    lengths = unname(lengths





}
