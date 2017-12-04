## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de

#' Utility function to compute vertex attributes for a list of network
#'
#' Important: This function only works for lists of networks which have timestamps used in their range names.
#'
#' @param list.of.networks The list of networks to add vertex attributes to
#' @param project.data The entire project date
#' @param attr.name The name of the attribute to add
#' @param aggretation One of range, cumulative or all. Determines the data to use for the attribute calculation.
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                  with the names being the name of the vertex.
#'
compute.vertex.attributes = function(list.of.networks, project.data, attr.name, aggretation = c("range", "cumulative", "all"), compute.attr) {
    net.to.range.list = networks.to.ranges(list.of.networks, project.data, aggretation)

    return (compute.vertex.attributes.with.n2r(net.to.range.list, attr.name, compute.attr))
}

compute.vertex.attributes.with.n2r = function(net.to.range.list, attr.name, compute.attr) {
    return (lapply(net.to.range.list, function(net.to.range) {

        current.network = net.to.range[["network"]]
        range.data = net.to.range[["data"]]

        attr.df = compute.attr(range.data, current.network)

        attributes = lapply(igraph::V(current.network)$name, function(x) attr.df[[x]])
        return (igraph::set_vertex_attr(current.network, attr.name, value = attributes))
    }))
}

networks.to.ranges = function(list.of.networks, project.data, aggretation = c("range", "cumulative", "all")) {
    list.of.ranges = as.list(names(list.of.networks))

    aggretation = match.arg(aggretation)

    net.to.range.list = lapply(list.of.ranges, function(range) {
        start.end = get.range.bounds(range)

        if(aggretation == "cumulative" || aggretation == "all") {
            start.end[1] = as.POSIXct("1970-01-01 00:00:00")
        }

        if(aggretation == "all") {
            start.end[2] = as.POSIXct("9999-01-01 00:00:00")
        }

        range.data = split.data.time.based(project.data, bins = start.end, sliding.window = FALSE)[[1]]

        return (list("network" = list.of.networks[[range]], "data" = range.data))
    })

    names(net.to.range.list) = names(list.of.networks)

    return(net.to.range.list)
}

add.vertex.attributes.commit.count = function(list.of.networks, data, name = "commit.count", aggretation = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, name, aggretation, function(range.data, net) {
        commit.count.df = get.author.commit.count(range.data)[c("author.name", "freq")]
        commit.count.list = structure(commit.count.df["freq"], names = commit.count.df["author.name"])

        return (commit.count.list)
    })
}



add.vertex.attributes.author.email = function(list.of.networks, data, name = "author.email", aggretation = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, name, aggretation,
                              function(range.data, net) {
                                  authors = range.data$get.authors()

                                  return (structure(names = authors[["author.name"]],
                                                    authors[["author.email"]]))
                              })
}

add.vertex.attributes.artifact.count = function(list.of.networks, data, aggretation = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "artifact.count", aggretation,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2artifact(), function(x) length(unique(x))))
                              })
}

add.vertex.attributes.first.activity = function(list.of.networks, data, aggretation = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "first.activity", aggretation,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2commit(),
                                         function(x) { return ( min(x["date"]) ) }))
                              })
}

add.vertex.attributes.active.ranges = function(list.of.networks, data) {
    net.to.range.list = networks.to.ranges(list.of.networks, data, "range")

    range.to.authors = lapply(net.to.range.list,
                                function(net.to.range)
                                    names(net.to.range[["data"]]$get.author2commit())
                            )

    author.names = unique(unlist(range.to.authors))

    active.ranges = lapply(author.names,
                           function(author)
                               names(Filter(function(range)
                                   author %in% range, range.to.authors)))

    names(active.ranges) = author.names

    compute.vertex.attributes.with.n2r(net.to.range.list, "active.ranges",
                              function(range.data, net) active.ranges)
}

add.vertex.attributes.author.role = function(list.of.networks, data, aggretation = c("range", "cumulative", "all"),
                                     type = c("network.degree", "network.eigen", "commit.count", "loc.count")) {
    compute.vertex.attributes(list.of.networks, data, "classification", aggretation,
                              function(range.data, net) {
                                  author.class = get.author.class.by.type(net, range.data, type)
                                  author.class = plyr::ldply(author.class, .id = "type")

                                  return (structure(author.class["type"], names= author.class["author.name"]))
                              })
}
