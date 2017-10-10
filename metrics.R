requireNamespace("igraph")

metrics.hub.degree = function(network, project){
    degrees = igraph::degree(network, mode = c("total"))
    vertex = which.max(degrees)
    df = data.frame("name" = names(vertex), "degree" = unname(degrees[vertex]), "project" = project)
    return(df)
}

metrics.avg.degree = function(network, project) {
    degrees = igraph::degree(network, mode = c("total"))
    avg = mean(degrees)
    df = data.frame("project" = project, "avg.degree" = avg)
    return(df)
}

metrics.node.degrees = function(network) {
    degrees = sort(igraph::degree(network, mode="total"), decreasing = TRUE)
    return(data.frame("name" = names(degrees), "degree" = unname(degrees)))
}

metrics.density = function(network, project) {
    density = igraph::graph.density(network)
    return(data.frame("project" = project, "density" = unname(density)))
}

metrics.avg.pathlength = function(network, project) {
    return(data.frame("project" = project, "avg.pathlength" = igraph::average.path.length(network, directed = FALSE, unconnected = TRUE)))
}

metrics.clustering.coeff = function(network, project) {
    cc = igraph::transitivity(network, type = "localaverage", vids = NULL)
    return(data.frame("project" = project, "clustering.coeff" = cc))
}

metrics.clustering.coeff.global = function(network, project) {
    cc = igraph::transitivity(network, type = "global", vids = NULL)
    return(data.frame("project" = project, "clustering.coeff" = cc))
}

metrics.modularity = function(network, project) {
    comm = igraph::cluster_walktrap(network)
    mod = igraph::modularity(network, igraph::membership(comm))
    return(data.frame("project" = project, "modularity" = mod))
}

metrics.amount.nodes = function(network, project) {
    return(data.frame("project" = project, "amount.nodes" = igraph::vcount(network)))
}

# requires simplified network
metrics.smallworldness = function(network, project) {

    # construct Erdös-Renyi network with same number of nodes and edges as g
    h = igraph::erdos.renyi.game(n=igraph::vcount(network), p.or.m=igraph::gsize(network), type="gnm", directed=FALSE)

    ## compute clustering coefficients
    g.cc = igraph::transitivity(network)
    h.cc = igraph::transitivity(h)
    ## compute average shortest-path length
    g.l = igraph::average.path.length(network, unconnected = TRUE)
    h.l = igraph::average.path.length(h, unconnected = TRUE)

    ## binary decision
    # intermediate variables
    gamma = g.cc / h.cc
    lambda = g.l / h.l

    # indicator s.delta
    s.delta = gamma / lambda

    # if s.delta > 1, then the network is a small-world network
    #is.smallworld = ifelse(s.delta > 1, TRUE, FALSE)

    return (data.frame("project" = project, "smallworldness" = s.delta))
}

metrics.power.law.fitting = function(network, project) {
    v.degree <- sort(igraph::degree(network, mode="all"), decreasing=TRUE)

    ## Power-law fiting
    ## (from  Mitchell Joblin <mitchell.joblin.ext@siemens.com>, Siemens AG,  2012, 2013)
    p.fit = igraph::power.law.fit(v.degree, implementation="plfit")
    param.names = c("alpha", "xmin", "KS.p")
    res = list()
    res[param.names] = p.fit[param.names]

    ## Check percent of vertices under power-law
    res$num.power.law = length(which(v.degree >= res$xmin))
    res$percent.power.law = 100 * (res$num.power.law / length(v.degree))
    df = data.frame(res$alpha,res$xmin,res$KS.p,res$num.power.law,res$percent.power.law)
    return(data.frame("project" = project, "KS.p" = res$KS.p))
}

metrics.hierarchy = function(network) {
    degrees = igraph::degree(network, mode="total")
    cluster.coeff = igraph::transitivity(network, type = "local", vids = NULL)

    degrees.without.cc = subset(degrees, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    cluster.coeff = subset(cluster.coeff, !(is.nan(cluster.coeff) | cluster.coeff == 0))

    return(data.frame(log.deg = log(degrees.without.cc), log.cc = log(cluster.coeff)))
}

