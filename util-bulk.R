library(parallel)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bipartite networks
##

collect.bipartite.networks = function(conf, author.relation = c("mail", "cochange"), artifact.relation = c("cochange", "callgraph"),
                            simple.network = TRUE, author.directed = FALSE, artifact.extra.edge.attr = c(),
                            artifact.filter = TRUE, artifact.filter.base = TRUE) {

    ## get argument
    author.relation = match.arg(author.relation)
    artifact.relation = match.arg(artifact.relation)

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, range)

        ## get the bipartite network
        bp.network = range.data$get.bipartite.network(
            author.relation = author.relation, artifact.relation = artifact.relation,
            simple.network = simple.network, author.directed = author.directed, artifact.extra.edge.attr = artifact.extra.edge.attr,
            artifact.filter = artifact.filter, artifact.filter.base = artifact.filter.base
        )

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

collect.author.networks = function(conf, author.relation = c("mail", "cochange"), author.directed = FALSE, simple.network = TRUE) {

    ## get argument
    author.relation = match.arg(author.relation)

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, range)

        ## get the author network
        author.network = range.data$get.author.network(author.relation, directed = author.directed, simple.network = simple.network)

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

collect.artifact.networks = function(conf, artifact.relation = c("cochange", "callgraph"),
                                     filter.artifact = TRUE, filter.base.artifact = TRUE, extra.edge.attr = c()) {

    ## get argument
    artifact.relation = match.arg(artifact.relation)

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = CodefaceRangeData$new(conf, range)

        ## get the artifact network
        artifact.network = range.data$get.artifact.network(artifact.relation, filter.artifact = filter.artifact,
                                                            filter.base.artifact = filter.base.artifact, extra.edge.attr = extra.edge.attr)

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

construct.data = function(conf, callgraphs = FALSE) {

    ## we need to iterate over all ranges
    ranges = conf$get.ranges()

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




