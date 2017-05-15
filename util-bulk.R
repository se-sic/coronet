library(parallel) # for parallel computation


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bipartite networks
##

collect.bipartite.networks = function(project.conf, network.conf) {

    ## get argument
    author.relation = network.conf$get.variable("author.relation")
    artifact.relation = network.conf$get.variable("artifact.relation")

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, range)

        ## get the bipartite network
        bp.network = range.data$get.bipartite.network(network.conf = network.conf)

        ## set range attribute
        bp.network = set.graph.attribute(bp.network, "range", range)

        # add to global list
        return(bp.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author networks
##

collect.author.networks = function(conf, network.conf) {

    ## get argument
    author.relation = network.conf$get.variable("author.relation")

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, range)

        ## get the author network
        author.network = range.data$get.author.network(network.conf = network.conf)

        ## set range attribute
        author.network = set.graph.attribute(author.network, "range", range)

        # add to global list
        return(author.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Artifact networks
##

collect.artifact.networks = function(conf, network.conf) {

    ## get argument
    artifact.relation = network.conf$get.variable("artifact.relation")

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, range)

        ## get the artifact network
        artifact.network = range.data$get.artifact.network(network.conf = network.conf)

        ## set range attribute
        artifact.network = set.graph.attribute(artifact.network, "range", range)

        # add to global list
        return(artifact.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Construct data
##

construct.data = function(conf, callgraphs = FALSE, step = 1) {

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    data = lapply(ranges, function(range) {

        revision.callgraph = ifelse(callgraphs,
                                    conf$get.callgraph.revision.from.range(range),
                                    "")

        ## construct range data
        range.data = CodefaceRangeData$new(conf, range, revision.callgraph)

        # add to global list
        return(range.data)
    })

    ## set names of list to range values
    names(data) = ranges

    return(data)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Run function on list of CodefaceRangeData
##

run.lapply = function(data, fun) {
    res = mclapply(data, function(dat) dat[[fun]]())
    return(res)
}




