## This file is part of coronet, which is free software: you
## can redistribute it and/or modify it under the terms of the GNU General
## Public License as published by  the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018-2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018-2019 by Klara Schlüter <schluete@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2020 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr") # for ldply function
requireNamespace("igraph") # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility functions -------------------------------------------------------

#' Utility function to compute vertex attributes for a list of network
#'
#' Important: This function only works for lists of networks which have timestamps used in their range names.
#'
#' This method is a wrapper combining the steps of splitting the project data and calculating the attribute.
#'
#' Important: If a vertex attribute can have multiple values per vertex, the \code{compute.attr} function
#' has to wrap all attribute vectors of a single vertex into a list. In that case, also the
#' \code{default.value} needs to be put into a list. By that, this function can differentiate between
#' attributes having multiple values and attributes having, at most, one value.
#'
#' @param list.of.networks The list of networks to add vertex attributes to
#' @param project.data The entire project data
#' @param attr.name The name of the attribute to add
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                     with the names being the name of the vertex.
#'
#' @return A list of networks with the added attribute
split.and.add.vertex.attribute = function(list.of.networks, project.data, attr.name,
                                          aggregation.level = c("range", "cumulative", "all.ranges",
                                                                "project.cumulative", "project.all.ranges",
                                                                "complete"),
                                          default.value, compute.attr, list.attributes = FALSE) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    net.to.range.list = split.data.by.networks(list.of.networks, project.data, aggregation.level)

    nets.with.attr = add.vertex.attribute(net.to.range.list, attr.name, default.value, compute.attr, list.attributes)
    return(nets.with.attr)
}

#' Utility function to compute vertex attributes for a list of network-to-range tuples.
#'
#' Important: If a vertex attribute can have multiple values per vertex, the \code{compute.attr} function
#' has to wrap all attribute vectors of a single vertex into a list. In that case, also the
#' \code{default.value} needs to be put into a list. By that, this function can differentiate between
#' attributes having multiple values and attributes having, at most, one value.
#'
#' @param net.to.range.list A list containing tuples with networks and corresponding range data.
#' @param attr.name The name of the attribute to add
#' @param default.value The default value to add if a vertex has no matching value
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                     with the names being the name of the vertex.
#'
#' @return A list of networks with the added attribute
add.vertex.attribute = function(net.to.range.list, attr.name, default.value, compute.attr, list.attributes = FALSE) {

    nets.with.attr = mapply(
        names(net.to.range.list), net.to.range.list,
        SIMPLIFY = FALSE, FUN = function(range, net.to.range) {

            current.network = net.to.range[["network"]]
            range.data = net.to.range[["data"]]

            attrs.by.vertex.name = compute.attr(range, range.data, current.network)

            ## catch the primitive case that the list of given attributes is empty or not set
            if (length(attrs.by.vertex.name) == 0 || is.null(attrs.by.vertex.name)) {
                ## construct list of attribute name and type
                attributes = structure(class(default.value), names = attr.name)
                ## add the vertex attribute (default value = 'NA')
                net.with.attr = add.attributes.to.network(current.network, "vertex", attributes)
                ## overwrite set vertex attribute with 'default.value', given the case that there are indeed vertices
                ## in the current network
                net.with.attr = igraph::set.vertex.attribute(net.with.attr, attr.name, value = default.value)
                ## return immediately
                return(net.with.attr)
            }

            get.or.default = function(name, data, default) {
                if (name %in% names(data)) {
                    return(data[[name]])
                } else {
                    return(default)
                }
            }

            attributes = lapply(igraph::V(current.network)$name,
                                function(x) get.or.default(x, attrs.by.vertex.name, default.value))

            ## simplify the list of attributes to a vector if all its elements are just vectors (not lists)
            if (length(attributes) > 0 && !any(lapply(attributes, is.list))) {
                attributes = unlist(attributes)
            }
            ## otherwise, the list of attributes contains lists, so we can only remove the outermost list
            else if (!list.attributes) {
                attributes = unlist(attributes, recursive = FALSE)
            }

            net.with.attr = igraph::set.vertex.attribute(current.network, attr.name, value = attributes)

            return(net.with.attr)
        }
    )

    return (nets.with.attr)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author network functions ------------------------------------------------

## * Commit count ----------------------------------------------------------

#' Add commit-count attribute based on author name
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.author = function(list.of.networks, project.data, name = "commit.count",
                                                    aggregation.level = c("range", "cumulative", "all.ranges",
                                                                          "project.cumulative", "project.all.ranges",
                                                                          "complete"),
                                                    default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.commit.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on author name where author is not committer
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.author.not.committer"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.author.not.committer = function(list.of.networks, project.data,
                                                                  name = "commit.count.author.not.committer",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.not.author.commit.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on comitter name
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer = function(list.of.networks, project.data, name = "commit.count.committer",
                                                       aggregation.level = c("range", "cumulative", "all.ranges",
                                                                             "project.cumulative", "project.all.ranges",
                                                                             "complete"),
                                                       default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on comitter name where committer is not author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.not.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.not.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.not.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.not.author.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on committer name where the committer equals the author.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.and.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.and.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.and.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.and.author.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on commits where the person represented by the vertex is the committer
#' or the author.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.or.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.or.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.or.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.or.author.commit.count, "name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based using \code{commit.count.method}
#'
#' Note: This is a helper function for all other functions adding a commit-count-related
#' vertex attribute.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#' @param commit.count.method The method reference for counting the commits
#' @param name.column The name of the author or committer column
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.helper = function(list.of.networks, project.data, name = "commit.count",
                                             aggregation.level = c("range", "cumulative", "all.ranges",
                                                                   "project.cumulative", "project.all.ranges",
                                                                   "complete"),
                                             default.value = 0L, commit.count.method, name.column) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            commit.count.df = commit.count.method(range.data)[c(name.column, "freq")]

            if (!is.data.frame(commit.count.df)) {
                return(list())
            }

            commit.count.list = structure(commit.count.df[["freq"]], names = commit.count.df[[name.column]])

            return(commit.count.list)
        }
    )

    return(nets.with.attr)
}


## * Mail count ----------------------------------------------------------

#' Add mail-count attribute based on the total number of mails send where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "mail.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.mail.count = function(list.of.networks, project.data,
                                           name = "mail.count",
                                           aggregation.level = c("range", "cumulative", "all.ranges",
                                                                 "project.cumulative", "project.all.ranges",
                                                                 "complete"),
                                           default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.mail.count, "author.name"
    )

    return(nets.with.attr)
}
#' Add mail-count attribute based on the number of mail threads participated in where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "mail.thread.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.mail.thread.count = function(list.of.networks, project.data,
                                           name = "mail.thread.count",
                                           aggregation.level = c("range", "cumulative", "all.ranges",
                                                                 "project.cumulative", "project.all.ranges",
                                                                 "complete"),
                                           default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.mail.thread.count, "author.name"
    )

    return(nets.with.attr)
}

## * Issue count --------------------------------------------------------------

#' Add issues-count attribute based on the number of issues participated in where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "issues.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.issue.count = function(list.of.networks, project.data,
                                            name = "issues.count",
                                            aggregation.level = c("range", "cumulative", "all.ranges",
                                                                  "project.cumulative", "project.all.ranges",
                                                                  "complete"),
                                            default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.issues.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add issues-count attribute based on the number of issues participated by commenting in where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "issues.count.by.commenting"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.issue.count.by.commenting = function(list.of.networks, project.data,
                                                          name = "issues.count.by.commenting",
                                                          aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                "project.cumulative", "project.all.ranges",
                                                                                "complete"),
                                                          default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.issues.commented.in.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add issues-count attribute based on the number of issues participated by commenting in where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "issue.comment.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.issue.comment.count = function(list.of.networks, project.data,
                                                          name = "issue.comment.count",
                                                          aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                "project.cumulative", "project.all.ranges",
                                                                                "complete"),
                                                          default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.issue.comments.count, "author.name"
    )

    return(nets.with.attr)
}

## * Pull request count -------------------------------------------------------------

#' Add PR-count attribute based on the number of PR created where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "pull.request.creation.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.pull.request.creation.count = function(list.of.networks, project.data,
                                                            name = "pull.request.creation.count",
                                                            aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                  "project.cumulative", "project.all.ranges",
                                                                                  "complete"),
                                                            default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.pull.requests.created.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add PR-count attribute based on the number of PRs participated in where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "mail.thread.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.pull.request.count = function(list.of.networks, project.data,
                                                   name = "pull.request.count",
                                                   aggregation.level = c("range", "cumulative", "all.ranges",
                                                                         "project.cumulative", "project.all.ranges",
                                                                         "complete"),
                                                   default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.pull.requests.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add PR-count attribute based on the number of PRs commits were added to where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "mail.thread.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.pull.request.count.by.commits = function(list.of.networks, project.data,
                                                              name = "pull.request.count.by.commits",
                                                              aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                    "project.cumulative", "project.all.ranges",
                                                                                    "complete"),
                                                              default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.pull.requests.commited.in.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add PR-comment-count attribute based on the number of PRs comments written where the person represented by the vertex is the author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "mail.thread.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.pull.request.count.by.commits = function(list.of.networks, project.data,
                                                              name = "pull.request.comments",
                                                              aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                    "project.cumulative", "project.all.ranges",
                                                                                    "complete"),
                                                              default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.pull.request.comments.count, "author.name"
    )

    return(nets.with.attr)
}

## * Meta-data -------------------------------------------------------------

#' Add author email attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "author.email"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.email = function(list.of.networks, project.data, name = "author.email", default.value = NA) {
    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, "complete", default.value,
        function(range, range.data, net) {
            authors = range.data$get.authors()
            author.to.mail = structure(names = authors[["author.name"]],
                                       authors[["author.email"]])

            return(author.to.mail)
        }
    )

    return(nets.with.attr)
}

#' Add unique artifact count attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "artifact.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.count = function(list.of.networks, project.data, name = "artifact.count",
                                               aggregation.level = c("range", "cumulative", "all.ranges",
                                                                     "project.cumulative", "project.all.ranges",
                                                                     "complete"),
                                               default.value = 0) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            ## FIXME we need to implement this also for the other kinds of artifacts
            lapply(range.data$group.artifacts.by.data.column("commits", "author.name"),
                   function(x) {
                       length(unique(x[["artifact"]]))
                   }
            )
        }
    )

    return(nets.with.attr)
}

## * Activity --------------------------------------------------------------

#' Add first activity attribute.
#'
#' @param list.of.networks The network list.
#' @param project.data The project data.
#' @param activity.types The kinds of activity to use as basis: One or more of \code{mails}, \code{commits} and
#'                       \code{issues}. [default: c("mails", "commits", "issues")]
#' @param name The attribute name to add. [default: "first.activity"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "complete"]
#' @param default.value The default value to add if a vertex has no matching value. [default: NA].
#' @param combine.all.activity.types Flag indicating that one value, computed over all given
#'                                           \code{activity.types} is of interest (instead of one value per type).
#'                                           [default: FALSE]
#'
#' @return A list of networks with the added attribute.
add.vertex.attribute.first.activity = function(list.of.networks, project.data,
                                               activity.types = c("mails", "commits", "issues"),
                                               name = "first.activity",
                                               aggregation.level = c("range", "cumulative", "all.ranges",
                                                                     "project.cumulative", "project.all.ranges",
                                                                     "complete"),
                                               default.value = NA,
                                               combine.activity.types = FALSE) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "complete")
    parsed.activity.types = match.arg.or.default(activity.types, several.ok = TRUE)

    ## the given default.value is interpreted as default per author and type.
    type.default = default.value

    ## the default value appended to vertices where no data is available is structured
    ## and named analogously to the vertex attributes containing available data.
    vertex.default = default.value
    if (!combine.activity.types) {
        vertex.default = rep(list(vertex.default), length(parsed.activity.types))
        names(vertex.default) = parsed.activity.types
    }

    compute.attr = function(range, range.data, net) {
        data = get.first.activity.data(range.data, parsed.activity.types, type.default)

        ## If configured, find minimum over all activity types per author, for example:
        ## data
        ##      list(authorA = list(mails = 1, commits = 2), authorB = list(mails = 3, commits = 3))
        ## yields
        ##      list(authorA = list(all.activities = 1), authorB = list(all.activities = 3))
        if (combine.activity.types) {
            data = parallel::mclapply(data, function(item.list) {
                min.value = min(do.call(base::c, item.list), na.rm = TRUE)
                return(list(all.activities = min.value))
            })
        }
        return(data)
    }

    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, aggregation.level, vertex.default,
                                                    compute.attr, list.attributes = TRUE)
    return(nets.with.attr)
}

#' Add active-ranges attribute
#'
#' Notice: One vertex can be active in multiple ranges, therefore there may be a vector of ranges as
#' active-ranges attribute.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "active.ranges"]
#' @param activity.types The kinds of activity to use as basis: One or more of \code{mails}, \code{commits} and
#'                       \code{issues}. [default: c("mails", "commits", "issues")]
#' @param default.value The default value to add if a vertex has no matching value [default: list()]
#' @param combine.activity.types Flag indicating that one value, computed over all given
#'                                           \code{activity.types} is of interest (instead of one value per type).
#'                                           [default: FALSE]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.active.ranges = function(list.of.networks, project.data, name = "active.ranges",
                                              activity.types = c("mails", "commits", "issues"),
                                              default.value = list(),
                                              combine.activity.types = FALSE) {
    net.to.range.list = split.data.by.networks(list.of.networks, project.data, "range")
    parsed.activity.types = match.arg.or.default(activity.types, several.ok = TRUE)

    ## the given default.value is interpreted as default per author and type.
    type.default = default.value

    compute.attr = function(range, range.data, net) {
        data = get.active.ranges.data(parsed.activity.types, net.to.range.list, type.default)

        ## combine the active ranges of all attributes to one list if desired
        if (combine.activity.types) {
            data = lapply(data, function(person) {
                flattened.person = (list("all.activity.types" = as.list(unique(unlist(person)))))
                return(list(flattened.person))
             })
        }

        return(data)
    }

    ## the default value appended to vertices where no data is available is structured
    ## and named analogously to the vertex attributes containing available data.
    vertex.default = default.value
    if (combine.activity.types) {
        vertex.default = list(default.value)
        names(vertex.default) = c("all.activity.types")
    } else {
        vertex.default = rep(list(default.value), length(parsed.activity.types))
        names(vertex.default) = parsed.activity.types
    }
    vertex.default = list(vertex.default)

    nets.with.attr = add.vertex.attribute(net.to.range.list, name, vertex.default, compute.attr)
    return(nets.with.attr)
}

## * Role ------------------------------------------------------------------

#' Add author role attribute, while using the classification method
#' \code{get.author.class.by.type} to provide the attributes
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "author.role"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param type The type of author classification. One of \code{"network.degree"},
#'             \code{"network.eigen"}, \code{"commit.count"}, and \code{"loc.count"}.
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.role.simple = function(list.of.networks, project.data, name = "author.role",
                                                   aggregation.level = c("range", "cumulative", "all.ranges",
                                                                         "project.cumulative", "project.all.ranges",
                                                                         "complete"),
                                                   type = c("network.degree", "network.eigen",
                                                            "commit.count", "loc.count"),
                                                   default.value = NA) {

    classification.function = function(network, range.data) {
        classification = get.author.class.by.type(network, range.data, type)
        return(classification)
    }

    nets.with.attr = add.vertex.attribute.author.role.function(
        list.of.networks, project.data, classification.function, name,
        aggregation.level, default.value
    )

    return(nets.with.attr)
}

#' Add author role attribute using a specified classification function
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param classification.result A name of an author-classification function. Must return a tuple
#'                              of two lists containing the authors named "core" and "peripheral".
#'                              See the functions \code{get.author.class.*}.
#' @param name The attribute name to add [default: "author.role"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.role.function = function(list.of.networks, project.data, classification.function,
                                                     name = "author.role",
                                                     aggregation.level = c("range", "cumulative", "all.ranges",
                                                                          "project.cumulative", "project.all.ranges",
                                                                          "complete"),
                                                     default.value = NA) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    net.to.range.list = split.data.by.networks(list.of.networks, project.data, aggregation.level)

    classification.results = lapply(
        net.to.range.list,
        function(net.to.range) {
            author.class = classification.function(net.to.range[["network"]], net.to.range[["data"]])
            return(author.class)
        }
    )

    nets.with.attr = add.vertex.attribute.author.role(
        list.of.networks, classification.results, name, default.value
    )

    return(nets.with.attr)
}

#' Add author role attribute by classification results
#'
#' Important: The lists \code{list.of.networks} and \code{classification.results} needs to be of the same
#' length for this to work properly.
#'
#' @param list.of.networks The network list
#' @param classification.results A list of author-classification results. Each item needs to contain
#'                               a tuple of two lists containing the authors named "core" and "peripheral"
#'                               (see the functions \code{get.author.class.*}).
#'                               The list needs to be of the same length as \code{list.of.networks} and use
#'                               the same names.
#' @param name The attribute name to add [default: "author.role"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.role = function(list.of.networks, classification.results,
                                            name = "author.role", default.value = NA) {

    if (length(list.of.networks) != length(classification.results) ||
        !identical(names(list.of.networks), names(classification.results))) {
        logging::logwarn(paste("Adding author-classification vertex attribute: The classification",
                               "results do not match with the list of networks. Please see the",
                               "documentation of the function 'add.vertex.attribute.author.role'."))
    }

    ## provide a list whose elements only contain the network as no data is needed here
    net.list = lapply(list.of.networks, function(net) {
        n = list()
        n[["network"]] = net
        return(n)
    })

    nets.with.attr = add.vertex.attribute(
        net.list, name, default.value,
        function(range, range.data, net) {
            classification = classification.results[[range]]
            author.class = plyr::ldply(classification, .id = NA)

            author.to.role = structure(author.class[[".id"]], names = author.class[["author.name"]])
            return(author.to.role)
        }
    )

    return(nets.with.attr)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Artifact network functions ----------------------------------------------

## * Change count ----------------------------------------------------------

#' Add the count of unique editors (i.e., authors) that worked on an artifact
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "editor.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param editor.definition Determines, who is counted as editor of an artifact (one ore more of
#'                          \code{c("author", "committer")}). [default: "author"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.editor.count = function(list.of.networks, project.data, name = "editor.count",
                                                      aggregation.level = c("range", "cumulative", "all.ranges",
                                                                            "project.cumulative", "project.all.ranges",
                                                                            "complete"),
                                                      editor.definition = c("author", "committer"),
                                                      default.value = 0) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    ## match editor definitions to column name in commit dataframe
    if (missing(editor.definition)) {
        editor.definition = "author"
    } else {
        editor.definition = match.arg.or.default(editor.definition, choices = c("author", "committer"), several.ok = TRUE)
    }
    editor.definition = paste0(editor.definition, ".name")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            vertex.attributes = lapply(range.data$group.authors.by.data.column("commits", "artifact"),
                   function(artifact.commits) {
                       editor.count = length(unique(unlist(artifact.commits[editor.definition])))
                       return(editor.count)
                   }
            )
            return(vertex.attributes)
        }
    )

    return(nets.with.attr)
}

#' Add the amount of times the artifact was changed
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "change.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.change.count = function(list.of.networks, project.data, name = "change.count",
                                                      aggregation.level = c("range", "cumulative", "all.ranges",
                                                                            "project.cumulative", "project.all.ranges",
                                                                            "complete"),
                                                      default.value = 0) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            artifact.to.commit = get.key.to.value.from.df(range.data$get.commits.filtered(), "artifact", "hash")
            artifact.change.count = lapply(artifact.to.commit, function(x) {
                length(unique(x[["hash"]]))
            })

            return(artifact.change.count)
        }
    )

    return(nets.with.attr)
}

## * Activity --------------------------------------------------------------

#' Add the first occurrence of the artifact
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "first.occurrence"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "complete"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.first.occurrence = function(list.of.networks, project.data, name = "first.occurrence",
                                                          aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                "project.cumulative", "project.all.ranges",
                                                                                "complete"),
                                                          default.value = NA) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "complete")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            artifact.to.dates = get.key.to.value.from.df(range.data$get.commits.filtered(), "artifact", "date")
            artifact.to.first = lapply(artifact.to.dates, function(a) {
                min(a[["date"]])
            })
            return(artifact.to.first)
        }
    )
    return(nets.with.attr)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper ------------------------------------------------------------------

#' Helper function for first activity: computes first activity information per person and activity type.
#'
#' @param activity.types The activity types to compute information for. [default: c("mails", "commits", "issues")]
#' @param range.data The data to base the computation on.
#' @param default.value The default value to add if no information is available per author and activity type. [default: NA]
#'
#' @return A list containing per author a list of first activity values named with the corresponding activity type.
get.first.activity.data = function(range.data, activity.types = c("commits", "mails", "issues"),
                                   default.value = NA) {

    ## get data for each activity type and extract minimal date for each author in each type,
    ## resulting in a list of activity types with each item containing a list of authors
    ## mapped to their first activity for the current activity type; for example:
    ##    list(
    ##        commits = list(authorA = list(commits = 1), authorB = list(commits = 0)),
    ##        mails   = list(authorB = list(mails = 2), authorC = list(mails = 3)),
    ##        issues  = list(authorA = list(issues = 2), authorD = list(issues = 2))
    ##    )
    activity.by.type = parallel::mclapply(activity.types, function(type) {
        ## compute minima
        minima.per.person = lapply(
            range.data$group.artifacts.by.data.column(type, "author.name"),
            function(x) {
                ## get first date
                m = list(min(x[["date"]]))
                ## add activity type as name to the list
                names(m) = type
                return(m)
            }
        )
        return(minima.per.person)
    })

    ## accumulate/fold lists 'activity.by.type' by adding all values of each list
    ## to an intermediate list (start with first list); for example:
    ##     list(
    ##        authorA = list(commits =  1, mails = NA, issues =  2),
    ##        authorB = list(commits =  0, mails =  2, issues = NA),
    ##        authorC = list(commits = NA, mails =  3, issues = NA),
    ##        authorD = list(commits = NA, mails = NA, issues =  2)
    ##     )
    result = Reduce(function(x, y) {
        ## get names from both lists, representing author names for the current activity types
        keys = union(names(x), names(y))
        ## get current step (i.e., "loop" index or, rather, "how many lists have we merged already?")
        depth = max(lengths(x), lengths(y))
        ## get the list of activity types to use on sublists (after merging next list (i.e., y)!)
        names.list = activity.types[seq_len(depth + 1)]

        ## actually combine values for each key
        result = parallel::mclapply(keys, function(key) {
            ## get value from intermediate list (pre-fill with right length if not existing)
            value.x = if (is.null(x[[key]])) rep(list(default.value), times = depth) else x[[key]]
            ## get value from current list (use default.value as default if not existing)
            value.y = if (is.null(y[[key]])) default.value else y[[key]]

            ## combine values and name them appropriately
            combined.values = c(value.x, value.y)
            names(combined.values) = names.list

            ## convert to POSIXct object again (which can be lost if is.null(x[[key]]) == TRUE)
            combined.values = lapply(combined.values, get.date.from.unix.timestamp)

            return(combined.values)
        })
        names(result) = keys
        return(result)

    }, activity.by.type)

    return(result)
}

#' Helper function for active ranges: computes the active ranges per author and activity.type.
#'
#' @param activity.types The activity types to compute information for. [default: c("mails", "commits", "issues")]
#' @param net.to.range.list The data to base the computation on, split by networks.
#' @param default.value The default value to add if no information is available per author and activity type. [default: list()]
#'
#' @return A list with elements representing the authors, each containing a list of elements representing the activity
#'         types, each containing a list of active ranges.
get.active.ranges.data = function(activity.types = c("mails", "commits", "issues"), net.to.range.list, default.value = list()) {

    ## a list with elements representing the parsed activity types, each containing a list of elements
    ## representing the ranges the data was split by, each containing a list of authors who were active
    ## for the corresponding activity type and range.
    range.to.authors.per.activity.type = lapply(activity.types, function(type) {
        type.function = paste0("get.", type)
        lapply(net.to.range.list, function(net.to.range) {

            ## get author information per activity.type
            type.data = net.to.range[["data"]][[type.function]]()

            ## remove unnecessary information and potentially resulting duplicats
            clean.type.data = unique(type.data[["author.name"]])

            return(as.list(clean.type.data))
        })
    })
    names(range.to.authors.per.activity.type) = activity.types

    ## Switch the list levels from "type - range - author" to "author - range - type".
    active.ranges = transpose.nested.list.by.innermost.values(range.to.authors.per.activity.type)

    ## For every author: if there's no activity data for a type, the default.value named with the type is added instead.
    active.ranges = lapply(active.ranges, function(ranges.per.type) {
        ranges.for.all.types = lapply(activity.types, function(type) {
            if (type %in% names(ranges.per.type)) {
                return(ranges.per.type[[type]])
            } else {
                return(default.value)
            }
        })
        names(ranges.for.all.types) = activity.types
        return(list(ranges.for.all.types))
    })

    return(active.ranges)
}


#' This function takes a nested list and switches the order of the nesting levels: the innermost level is moved to the
#' outside. This is done by reproducing the given list structure for every element occuring in one of the innermost lists
#' and then deleting every sublist in which the element does not occur. For example, on input
#'
#' type.range.author = list(
#'     "type1" = list(
#'         "range1" = list("authorB", "authorC", "authorD"),
#'         "range3" = list("authorA", "authorC")
#'     ),
#'     "type2" = list(
#'         "range2" = list("authorB"),
#'         "range3" = list("authorC", "authorD")
#'     )
#' )
#'
#' the function will return a new list
#'
#' author.type.range = list(
#'     "authorB" = list(
#'         "type1" = list("range1"),
#'         "type2" = list("range2")
#'     ),
#'     "authorC" = list(
#'         "type1" = list("range1", "range3"),
#'         "type2" = list("range3")
#'     ),
#'     "authorD" = list(
#'         "type1" = list("range1"),
#'         "type2" = list("range3")
#'     ),
#'     "authorA" = list(
#'         "type1" = list("range3")
#'     )
#' )
#'
#' @param nested.list A list nested AT LEAST ONCE, that means: the elements of the outermost list are also lists. The
#'                    nesting depth of all inner lists must be the same and the lists must be named at every nesting level.
#'
#' @return The nested list with the innermost level as new outermost level.
transpose.nested.list.by.innermost.values = function(nested.list) {
    list.by = unique(unlist(nested.list, use.names = FALSE))

    ## Returns the given structure of nested lists with all parts removed, that do not contain the given
    ## innerst.element.
    get.structure.for = function(innerst.element, structure, name = "default") {

        ## Base case 1: an empty list is returned as it is.
        if (length(structure) == 0) {
            return(list())

        ## Base case 2: if the structure isn't nested itself, it is only returned, if it contains the given innerst.element.
        ##           Otherwise, NA is returned.
        } else if (!is.list(structure[[1]])) {
            if (innerst.element %in% structure) {
                return(name)
            } else {
                return(NA)
            }

        ## Recursive case: for every substructure, the function is called recursively. From the results, the list is
        ##                 reconstructed, named and returned.
        } else {
            result.of.recursive.calls = lapply(names(structure), function(substructure.name) {
                substructure = structure[[substructure.name]]
                call.result = get.structure.for(innerst.element, substructure, substructure.name)
                return(call.result)
            })
            names(result.of.recursive.calls) = names(structure)

            ## remove NA values from recursively obtained structure
            na.removed = result.of.recursive.calls[sapply(result.of.recursive.calls, function(x) any(!is.na(x)))]

            ## if element names equal content values in the recursive result, remove names
            if (identical(unlist(na.removed, use.names = FALSE), names(na.removed))) {
                na.removed = unname(na.removed)
            }

            return(na.removed)
        }
    }

    result = lapply(list.by, function(element) get.structure.for(element, nested.list))
    names(result) = list.by
    return(result)
}
