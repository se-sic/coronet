## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de

## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de

## (c) Christian Hechtl 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bipartite networks
##


collect.bipartite.networks = function(conf, network.conf, step = 1) {

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, network.conf, range)

        ## get the bipartite network
        bp.network = range.data$get.bipartite.network()

        ## set range attribute
        bp.network = igraph::set.graph.attribute(bp.network, "range", range)
        attr(bp.network, "range") = range

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


collect.author.networks = function(conf, network.conf, step = 1) {


    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, network.conf, range)

        ## get the author network
        author.network = range.data$get.author.network()

        ## set range attribute
        author.network = igraph::set.graph.attribute(author.network, "range", range)
        attr(author.network, "range") = range

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


collect.artifact.networks = function(conf, network.conf, step = 1) {

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, network.conf, range)

        ## get the artifact network
        artifact.network = range.data$get.artifact.network()

        ## set range attribute
        artifact.network = igraph::set.graph.attribute(artifact.network, "range", range)
        attr(artifact.network, "range") = range

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

construct.data = function(conf, network.conf, callgraphs = FALSE, step = 1) {

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
        range.data = CodefaceRangeData$new(conf, network.conf, range, revision.callgraph)
        attr(range.data, "range") = range

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
    res = parallel::mclapply(data, function(dat) dat[[fun]]())
    return(res)
}
