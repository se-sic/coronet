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
compute.vertex.attributes = function(list.of.networks, project.data, attr.name, compute.attr, cumulative = FALSE) {
    list.of.ranges = as.list(names(list.of.networks))

    return (lapply(list.of.ranges, function(range) {
        start.end = get.range.bounds(range)

        if(cumulative) {
            start.end[1] = as.POSIXct("1970-01-01 00:00:00", "GMT")
        }

        range.data = split.data.time.based(project.data, bins = start.end, sliding.window = FALSE)[[1]]
        current.network = list.of.networks[[range]]

        attr.df = compute.attr(range.data)

        attributes = lapply(igraph::V(current.network)$name, function(x) { return (attr.df[[x]]) })
        return (igraph::set_vertex_attr(current.network, attr.name, value = attributes))
    }))
}

get.commit.count.per.range = function(data) {
    commit.count.df = get.author.commit.count(data)[c("author.name", "freq")]
    commit.count.list = structure(commit.count.df$freq, names = commit.count.df$author.name)

    return (commit.count.list)
}

add.commit.count = function(list.of.networks, data, cumulative = FALSE) {
    compute.vertex.attributes(list.of.networks, data, "commit.count", get.commit.count.per.range, cumulative)
}
