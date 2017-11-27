## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de

#' Utility function to compute vertex attributes for a list of network
#'
#' Only network ranges with dates work!
#'
#' @param list.of.networks The list of networks to add vertex attributes to
#' @param project.data The entire project date
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                  with the names being the name of the vertex.
#' @param cumulative Should the attribute be calculated from project start to end of range?
#'
compute.vertex.attributes = function(list.of.networks, project.data, attr.name, what = c("range", "cumulative", "all"), compute.attr) {
    list.of.ranges = as.list(names(list.of.networks))
    what = match.arg(what)

    return (lapply(list.of.ranges, function(range) {
        start.end = get.range.bounds(range)

        if(what == "cumulative" || what == "all") {
            start.end[1] = as.POSIXct("1970-01-01 00:00:00", "GMT")
        }

        if(what == "all") {
            start.end[2] = as.POSIXct("9999-01-01 00:00:00", "GMT")
        }

        range.data = split.data.time.based(project.data, bins = start.end, sliding.window = FALSE)[[1]]
        current.network = list.of.networks[[range]]

        attr.df = compute.attr(range.data, current.network)

        attributes = lapply(igraph::V(current.network)$name, function(x) { return (attr.df[[x]]) })
        return (igraph::set_vertex_attr(current.network, attr.name, value = attributes))
    }))
}

add.commit.count = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "commit.count", what, function(range.data, net) {
        commit.count.df = get.author.commit.count(range.data)[c("author.name", "freq")]
        commit.count.list = structure(commit.count.df$freq, names = commit.count.df$author.name)

        return (commit.count.list)
    })
}



add.mail.attribute = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "email.addresses", what,
                              function(range.data, net) {
                                  author.to.mail = range.data$get.author2commit()
                                  author.to.mail = lapply(author.to.mail, function(a) {
                                      return ( unique(a$author.email) )
                                      })

                                  return (author.to.mail)
                              })
}

add.artifact.count = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "artifact.count", what,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2artifact(), length))
                              })
}

add.first.activity = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "first.activity", what,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2commit(),
                                         function(x) { return ( min(x$date) ) }))
                              })
}

add.activity.range = function(list.of.networks, data, what = c("range", "cumulative", "all")) {
    compute.vertex.attributes(list.of.networks, data, "activity.range", what,
                              function(range.data, net) {
                                  return (lapply(range.data$get.author2commit(),
                                                 function(x) {
                                                     return ( c(start = min(x$date), end = max(x$date)) )
                                                 }))
                              })
}

add.author.classification = function(list.of.networks, data, what = c("range", "cumulative", "all"),
                                     type = c("network.degree", "network.eigen", "commit.count", "loc.count")) {
    compute.vertex.attributes(list.of.networks, data, "classification", what,
                              function(range.data, net) {
                                  author.class = get.author.class.by.type(net, range.data, type)
                                  author.class = plyr::ldply(author.class, .id = "type")

                                  return (structure(author.class$type, names= author.class$author.name))
                              })
}
