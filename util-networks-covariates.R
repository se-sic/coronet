## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de

#' Utility function to compute vertex attributes for a list of network
#'
#' Important: This function only works for lists of networks which have timestamps used in their range names.
#'
#' This method is a wrapper combining the steps of splitting the project data and calculating the attribute.
#'
#' @param list.of.networks The list of networks to add vertex attributes to
#' @param project.data The entire project data
#' @param attr.name The name of the attribute to add
#' @param aggregation.level One of range, cumulative or project. Determines the data to use for the attribute calculation.
#' @param default.value The default value to add if a vertex has no matching value
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                  with the names being the name of the vertex.
#' @return A list of networks with the added attribute
#'
split.and.add.vertex.attribute = function(list.of.networks, project.data, attr.name, aggregation.level = c("range", "cumulative", "project"), default.value, compute.attr) {
    net.to.range.list = split.data.by.networks(list.of.networks, project.data, aggregation.level)

    nets.with.attr = add.vertex.attribute(net.to.range.list, attr.name, default.value, compute.attr)
    return(nets.with.attr)
}

#' Utility function to compute vertex attributes for a list of network to range tuples.
#'
#' @param net.to.range.list A list containing tuples with networks and corresponding range data.
#' @param attr.name The name of the attribute to add
#' @param default.value The default value to add if a vertex has no matching value
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                  with the names being the name of the vertex.
#' @return A list of networks with the added attribute
#'
add.vertex.attribute = function(net.to.range.list, attr.name, default.value, compute.attr) {
    nets.with.attr = lapply(net.to.range.list, function(net.to.range) {

        current.network = net.to.range[["network"]]
        range.data = net.to.range[["data"]]

        attr.df = compute.attr(range.data, current.network)

        get.or.default = function(name, data, default) {
            if(name %in% names(data)) {
                return(data[[name]])
            } else {
                return(default)
            }
        }

        attributes = lapply(igraph::V(current.network)$name,
                            function(x) get.or.default(x, attr.df, default.value))

        net.with.attr = igraph::set_vertex_attr(current.network, attr.name, value = attributes)

        return(net.with.attr)
    })

    return (nets.with.attr)
}



#' Add commit count attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add
#' @param aggregation.level One of range, cumulative or project. Determines the data to use for the attribute calculation.
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.commit.count = function(list.of.networks, project.data, name = "commit.count", aggregation.level = c("range", "cumulative", "project"), default.value = 0) {
    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, aggregation.level, default.value, function(range.data, net) {
        commit.count.df = get.author.commit.count(range.data)[c("author.name", "freq")]
        commit.count.list = structure(commit.count.df[["freq"]], names = commit.count.df[["author.name"]])

        return(commit.count.list)
    })

    return(nets.with.attr)
}


#' Add author email attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.author.email = function(list.of.networks, project.data, name = "author.email", default.value = NA) {
    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, "project", default.value,
                              function(range.data, net) {
                                  authors = range.data$get.authors()
                                  author.to.mail = structure(names = authors[["author.name"]],
                                                             authors[["author.email"]])

                                  return(author.to.mail)
                              })
    return(nets.with.attr)
}

#' Add unique artifact count attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add
#' @param aggregation.level One of range, cumulative or project. Determines the data to use for the attribute calculation.
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.artifact.count = function(list.of.networks, project.data, name = "artifact.count", aggregation.level = c("range", "cumulative", "project"), default.value = 0) {
    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, aggregation.level, default.value,
                              function(range.data, net)
                                  lapply(range.data$get.author2artifact(), function(x) length(unique(x[["artifact"]])))
                             )
    return(nets.with.attr)
}

#' Add first activity attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param activity.type What kind of activity? (mail, commit)
#' @param name The attribute name to add
#' @param aggregation.level One of range, cumulative or project. Determines the data to use for the attribute calculation.
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.first.activity = function(list.of.networks, project.data, activity.type = c("mail", "commit", "issue"), name = "first.activity",
                                               aggregation.level = c("range", "cumulative", "project"), default.value = NA) {
    activity.type.function = paste("get.author2", match.arg(activity.type), sep = "")

    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, aggregation.level, default.value,
                              function(range.data, net)
                                  lapply(range.data[[activity.type.function]](),
                                         function(x) min(x[["date"]]))
                              )
    return(nets.with.attr)
}

#' Add active ranges attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.active.ranges = function(list.of.networks, project.data, name = "active.ranges", default.value = list()) {
    net.to.range.list = split.data.by.networks(list.of.networks, project.data, "range")

    range.to.authors = lapply(net.to.range.list,
                                function(net.to.range)
                                    names(net.to.range[["data"]]$get.author2commit())
                            )

    author.names = unique(unlist(range.to.authors))

    active.ranges = lapply(author.names,
                           function(author) {
                               filter.by.author = Filter(function(range) author %in% range,
                                                 range.to.authors)

                               active.ranges.of.author = names(filter.by.author)

                               return(active.ranges.of.author)
                               })

    names(active.ranges) = author.names

    nets.with.attr = add.vertex.attribute(net.to.range.list, name, default.value,
                              function(range.data, net) active.ranges)

    return(nets.with.attr)
}

#' Add author role attribute
#'
#' Uses the classification method get.author.class.by.type to provide the attributes
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add
#' @param aggregation.level One of range, cumulative or project.
#'                          Determines the data to use for the attribute calculation.
#' @param type The type of classification
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.author.role.simple = function(list.of.networks, project.data, name = "author.role",
                                                   aggregation.level = c("range", "cumulative", "project"),
                                     type = c("network.degree", "network.eigen", "commit.count", "loc.count"),
                                     default.value = NA) {

    classification.function = function(network, range.data) {
        classification = get.author.class.by.type(network, range.data, type)
        return(classification)
    }

    nets.with.attr = add.vertex.attribute.author.role(list.of.networks, project.data, classification.function,
                                     name, aggregation.level, default.value)

    return(nets.with.attr)
}

#' Add author role attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param classification.function Function taking a network and it's corresponding range data as parameters.
#'                                Must return a tuple of two lists containing the authors named "core" and "peripheral"
#' @param name The attribute name to add
#' @param aggregation.level One of range, cumulative or project.
#'                          Determines the data to use for the attribute calculation.
#' @param default.value The default value to add if a vertex has no matching value
#' @return A list of networks with the added attribute
#'
add.vertex.attribute.author.role = function(list.of.networks, project.data, classification.function, name ="author.role",
                                            aggregation.level = c("range", "cumulative", "project"), default.value = NA) {
    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, aggregation.level, default.value,
                                   function(range.data, net) {
                                       author.class = classification.function(net, range.data)
                                       author.class = plyr::ldply(author.class, .id = "type")

                                       author.to.role = structure(author.class[["type"]], names= author.class[["author.name"]])
                                       return(author.to.role)
                                   })
    return(nets.with.attr)
}
