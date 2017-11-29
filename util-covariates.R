## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de

#' Utility function to compute vertex attributes for a list of network
#'
#' Important: This function only works for lists of networks which have timestamps used in their range names.
#'
#' @param list.of.networks The list of networks to add vertex attributes to
#' @param project.data The entire project date
#' @param attr.name The name of the attribute to add
#' @param what One of range, cumulative or all. Determines the data to use for the attribute calculation.
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                  with the names being the name of the vertex.
#'
compute.vertex.attributes = function(list.of.networks, project.data, attr.name, what = c("range", "cumulative", "all"), compute.attr) {
    list.of.ranges = as.list(names(list.of.networks))
    what = match.arg(what)

    return (lapply(list.of.ranges, function(range) {
        start.end = get.range.bounds(range)

        if(what == "cumulative" || what == "all") {
            start.end[1] = as.POSIXct("1970-01-01 00:00:00")
        }

        if(what == "all") {
            start.end[2] = as.POSIXct("9999-01-01 00:00:00")
        }

        range.data = split.data.time.based(project.data, bins = start.end, sliding.window = FALSE)[[1]]
        current.network = list.of.networks[[range]]

        attr.df = compute.attr(range.data, current.network)

        attributes = lapply(igraph::V(current.network)$name, function(x) { return (attr.df[[x]]) })
        return (igraph::set_vertex_attr(current.network, attr.name, value = attributes))
    }))
}

add.vertex.attributes.commit.count = function(list.of.networks, data, name = "commit.count", what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, name, what, function(range.data, net) {
        commit.count.df = get.author.commit.count(range.data)[c("author.name", "freq")]
        commit.count.list = structure(commit.count.df["freq"], names = commit.count.df["author.name"])

        return (commit.count.list)
    })
}



add.vertex.attributes.author.email = function(list.of.networks, data, name = "author.email", what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, name, what,
                              function(range.data, net) {
                                  author.to.mail = range.data$get.author2commit()
                                  author.to.mail = lapply(author.to.mail, function(a) {
                                      return ( unique(a["author.email"]) )
                                      })

                                  return (author.to.mail)
                              })
}

add.vertex.attributes.artifact.count = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "artifact.count", what,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2artifact(), function(x) length(unique(x))))
                              })
}

add.vertex.attributes.first.activity = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "first.activity", what,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2commit(),
                                         function(x) { return ( min(x["date"]) ) }))
                              })
}

add.vertex.attributes.active.ranges = function(list.of.networks, data) {
    compute.vertex.attributes(list.of.networks, data, "active.ranges", "range",
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2commit(),
                                                 function(x) {
                                                     return ( c(start = min(x["date"]), end = max(x["date"])) )
                                                 }))
                              })
}

add.vertex.attributes.author.role = function(list.of.networks, data, what = c("range", "cumulative", "all"),
                                     type = c("network.degree", "network.eigen", "commit.count", "loc.count")) {
    compute.vertex.attributes(list.of.networks, data, "classification", what,
                              function(range.data, net) {
                                  author.class = get.author.class.by.type(net, range.data, type)
                                  author.class = plyr::ldply(author.class, .id = "type")

                                  return (structure(author.class["type"], names= author.class["author.name"]))
                              })
}
