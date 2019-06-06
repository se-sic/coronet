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
## Copyright 2015+2017 by Claus Hunsen <hunsen@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("plyr") # for rbind.fill.matrix
requireNamespace("igraph") # graphs, baby!


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Motifs ------------------------------------------------------------------

## * Line motif -----------------------------------------------------------
## (i.e., two authors connected)

MOTIFS.LINE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", type = c(TYPE.AUTHOR, TYPE.AUTHOR)) +
    igraph::edges("D1", "D2", type = c(TYPE.EDGES.INTRA))

## * Triangle motif --------------------------------------------------------
## (two authors are connected to one artifact)

## positive triangle motif (including communication)
MOTIFS.TRIANGLE.POSITIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT)) +
    igraph::edges("D1", "A" , "D2", "A", "D1", "D2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA))

## negative triangle motif (excluding communication)
MOTIFS.TRIANGLE.NEGATIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A", type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT)) +
    igraph::edges("D1", "A" , "D2", "A", type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER))


## * Square motif ----------------------------------------------------------
## (two authors are connected to two distinct artifact that are coupled)

## positive square motif (including communication)
MOTIFS.SQUARE.POSITIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A1", "A2",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT, TYPE.ARTIFACT)) +
    igraph::edges("D1", "A1" , "D2", "A2", "A1", "A2", "D1", "D2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA))

## negative square motif (excluding communication)
MOTIFS.SQUARE.NEGATIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A1", "A2",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT, TYPE.ARTIFACT)) +
    igraph::edges("D1", "A1" , "D2", "A2", "A1", "A2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Mapping of vertex and edge types to numerics ----------------------------

## Map vertex and edge types to numerics
MOTIF.TYPE.MAPPING = as.data.frame(rbind(
    c(character = TYPE.AUTHOR, numeric = 1),
    c(character = TYPE.ARTIFACT, numeric = 2),
    c(character = TYPE.EDGES.INTRA, numeric = 3),
    c(character = TYPE.EDGES.INTER, numeric = 4)
))


#' Get numeric vertex types from the given \code{network}.
#'
#' For this, the actual vertex types are mapped to numerics using the pre-defined
#' mapping \code{MOTIF.TYPE.MAPPING}.
#'
#' @param network the network from which to get the vertex types
#' @param index the subset of vertices to consider [default: igraph::V(network)]
#'
#' @return the vector of numeric vertex types for the (subset of) vertices in the network
#'
#' @seealso \code{MOTIF.TYPE.MAPPING}
get.vertex.types.as.numeric = function(network, index = igraph::V(network)) {

    ## get the vertex attribute as factor
    attr.factor = factor(igraph::get.vertex.attribute(network, "type", index))

    ## replace factor levels with corresponding numerics
    levels(attr.factor) = sapply(levels(attr.factor), function(f) {
        ## get the numeric from MOTIF.TYPE.MAPPING
        num = subset(MOTIF.TYPE.MAPPING, character == f, numeric, drop = TRUE)
        return(num)
    })
    ## re-create vertex-attribute vector with the numerics
    attr.numeric = as.numeric(levels(attr.factor))[attr.factor]

    return(attr.numeric)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Motif-identification functions ------------------------------------------

#' Search for the given \code{motif} in the given \code{network}.
#'
#' The vertex attribute *"type"* is used to match the vertices in the \code{motif} with
#' the ones in the given \code{network}. This means basically that a vertex with
#' a certain value for its attribute "type" in the \code{motif} is only matched with
#' vertices in \code{network} that have the same attribute values.
#'
#' @param network The network in which the given \code{motif} is searched
#' @param motif The motif to search for in the given \code{network}
#' @param remove.duplicates If \code{remove.duplicates == TRUE}, any duplicate matched motifs are
#'                          removed. This logical value basically resembles the idea of respecting
#'                          the order within a matched motif or not.
#'
#' @return A list of vertex sequences denoting the matched motifs, ordered by "type" and "name"
#'         vertex attributes
#'
#' @seealso \code{igraph::subgraph_isomorphisms} (method "vf2")
#'
#' @examples
#' motifs.search.in.network(get.sample.network(), MOTIFS.TRIANGLE.NEGATIVE, remove.duplicates = TRUE)
motifs.search.in.network = function(network, motif, remove.duplicates = TRUE) {

    ## find motif in network
    vs = igraph::subgraph_isomorphisms(target = network, pattern = motif, method = "vf2",
                                       vertex.color1 = get.vertex.types.as.numeric(network),
                                       vertex.color2 = get.vertex.types.as.numeric(motif))

    ## normalize found vertex sequences (sort vertices)
    vs.cleaned = lapply(vs, function(seq) {
        ## get types and names of vertices
        types = get.vertex.types.as.numeric(network, index = seq)
        names = igraph::get.vertex.attribute(network, "name", index = seq)

        ## sort vertex sequence by types and names
        seq = seq[ order(types, names) ]

        return(seq)
    })

    ## if we do not remove duplicated, return immediately
    if (!remove.duplicates) {
        return(vs.cleaned)
    }

    ## identify duplicates in list of found vertex sequences
    vs.matrix = lapply(vs.cleaned, function(vs) {
        ## return the vertex sequence as matrix
        vs.matrix = matrix(vs, nrow = 1)
        return(vs.matrix)
    })
    vs.matrix = plyr::rbind.fill.matrix(vs.matrix)
    vs.duplicates = duplicated(vs.matrix)

    ## remove duplicates from list of vertex sequences
    vs.final = vs.cleaned[ !vs.duplicates ]

    return(vs.final)
}

#' Retrieve the motif count for the given group of motifs.
#'
#' @param network The network to use for the search
#' @param motifs a (named) list of network motifs to search for; the names are re-used in the
#'               return value (see below)
#' @param remove.duplicates If \code{remove.duplicates == TRUE}, any duplicate matched motifs are
#'                          removed. This logical value basically resembles the idea of respecting
#'                          the order within a matched motif or not. [default: TRUE]
#' @param raw.data Whether to add attribute 'raw' to the result list, containing the raw vertex
#'                 sequences describing the matched motifs [default: FALSE]
#'
#' @return A named list,
#'         the item 'authors': the total number of authors in the network,
#'         the item 'artifacts': the total number of artifacts in the network,
#'         the item 'complete': the total number of author pairs, and
#'         one item for each motif given with the argument \code{motifs}:
#'                       the names are taken from the list \code{motifs} or, if they do not exist,
#'                       replaced with a number sequence.
#'
#'         The list get the attribute "raw", when \code{raw.data} is enabled; then,
#'         for each motif in the list \code{motifs}, the raw matched motifs are stored.
motifs.count = function(network, motifs, remove.duplicates = TRUE, raw.data = FALSE) {

    ## get names of motifs
    if (is.null(names(motifs))) {
        names(motifs) = seq_along(motifs)
    }
    motif.names = names(motifs)

    ## run search for all motifs
    res = lapply(motifs, function(motif) {
        motifs.search.in.network(
            network = network,
            motif = motif,
            remove.duplicates = remove.duplicates
        )
    })

    ## construct result list:
    result = list()
    ## (1) basic numbers
    author.count = length(igraph::V(network)[ type == TYPE.AUTHOR ])
    artifact.count = length(igraph::V(network)[ type == TYPE.ARTIFACT ])
    result = c(result, list(
        "authors" = author.count, # total number of authors
        "artifacts" = artifact.count, # total number of artifacts
        "complete" = choose(author.count, 2) # total number of author pairs
    ))
    ## (2) motif counts
    for (name in motif.names) {
        result[name] = length(res[[name]])
    }

    ## add raw data as attribute if wanted
    if (raw.data) {
        attr(result, "raw") = res
    }

    return(result)
}

#' Retrieve the motif count for a predefined list of motifs.
#'
#' @param network The network to use for the search
#' @param remove.duplicates If \code{remove.duplicates == TRUE}, any duplicate matched motifs are
#'                          removed. This logical value basically resembles the idea of respecting
#'                          the order within a matched motif or not. [default: TRUE]
#' @param raw.data Whether to add attribute 'raw' to the result list, containing the raw vertex
#'                 sequences describing the matched motifs [default: FALSE]
#'
#' @return A named list,
#'         the item 'triangle': the motif data for the triangle motifs,
#'         the item 'square': the motif data for the square motifs.
#'
#'         The items' data is preserved as returned from \code{motifs.count}.
#'
#'         Each sublist is augmented with the following items:
#'         - the item 'p1': the fraction of author pairs communicating,
#'         - the item 'p2': the fraction of collaborating author pairs that are also
#'           communicating (i.e., the fraction of fulfilled coordination requirements),
motifs.count.all = function(network, remove.duplicates = TRUE, raw.data = FALSE) {
    ## (1) triangle motif
    triangle = motifs.count(
        network,
        motifs = list(
            collaborating = MOTIFS.TRIANGLE.NEGATIVE,
            communicating = MOTIFS.LINE,
            collaborating.and.communicating = MOTIFS.TRIANGLE.POSITIVE
        ),
        remove.duplicates = remove.duplicates,
        raw.data = raw.data
    )
    ## fractions
    triangle["p1"] = triangle[["communicating"]] / triangle[["complete"]]
    triangle["p2"] = triangle[["collaborating.and.communicating"]] / triangle[["collaborating"]]

    ## (2) square motif
    square = motifs.count(
        network,
        motifs = list(
            collaborating = MOTIFS.SQUARE.NEGATIVE,
            communicating = MOTIFS.LINE,
            collaborating.and.communicating = MOTIFS.SQUARE.POSITIVE
        ),
        remove.duplicates = remove.duplicates,
        raw.data = raw.data
    )
    ## fractions
    square["p1"] = square[["communicating"]] / square[["complete"]]
    square["p2"] = square[["collaborating.and.communicating"]] / square[["collaborating"]]

    ## return data
    return(list(
        "triangle" = triangle,
        "square" = square
    ))
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Low-level functionality -------------------------------------------------

#' Remove vertices of type \code{TYPE.ARTIFACT} from the given vertex sequence(s) \code{vs}.
#'
#' @param network The network from the the vertex sequence comes from
#' @param vs The vertex sequences to modify
#'
#' @return A list with the vertex sequences \code{vs} with the vertices of type \code{TYPE.ARTIFACT} removed
motifs.remove.artifacts.from.matched.motifs = function(network, vs) {
    ## if we do not have a list here, we need to encapsulate the parameter since
    ## we would iterate over the individual vertices in the lapply later
    if (!is.list(vs)) {
        vs = list(vs)
    }

    ## iterate over all vertex sequences to remove artifacts
    vs.cleaned = lapply(vs, function(seq) {
        ## get types of vertices
        types = igraph::get.vertex.attribute(network, "type", index = seq)
        ## remove artifact vertices
        seq = seq[ types != TYPE.ARTIFACT ]
        return(seq)
    })

    return(vs.cleaned)
}
