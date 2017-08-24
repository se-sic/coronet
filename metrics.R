
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
    return(igraph::average.path.length(network, directed = TRUE, unconnected = FALSE))
}

clustering.coeff = function(network) {
    local.cc = igraph::transitivity(network, type = "local", vids = NULL)
    cc = mean(local.cc, na.rm = TRUE)
    return(cc)
}

# Not sure if this is correct
modularity = function(network) {
    comm = igraph::cluster_walktrap(network)
    mod = igraph::modularity(network, igraph::membership(comm))
    return(mod)
}

smallworldness = function(network) {
    smallworldness <- determine.smallworldness(network) # smallworldness(nw.data$nw) #
    return(smallworldness)
}


determine.smallworldness = function(g) {

    # construct Erdös-Renyi network with same number of nodes and edges as g
    h = igraph::erdos.renyi.game(n=igraph::vcount(g), p.or.m=igraph::gsize(g), type="gnm", directed=TRUE)

    ## compute clustering coefficients
    g.cc = igraph::transitivity(g)
    h.cc = igraph::transitivity(h)

    ## compute average shortest-path length
    g.l = igraph::average.path.length(g)
    h.l = igraph::average.path.length(h)

    ## binary decision
    # intermediate variables
    gamma = g.cc / h.cc
    lambda = g.l / h.l

    # indicator s.delta
    s.delta = gamma / lambda

    # if s.delta > 1, then the network is a small-world network
    #is.smallworld = ifelse(s.delta > 1, TRUE, FALSE)

    return (s.delta)
}
