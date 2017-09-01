## Claus Hunsen, 2015, 2017
## hunsen@fim.uni-passau.de


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
    igraph::edges("D1","D2", type = c(TYPE.EDGES.INTRA))

## * Triangle motif --------------------------------------------------------
## (two authors are connected to one artifact)

## positive triangle motif (including communication)
MOTIFS.TRIANGLE.POSITIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT)) +
    igraph::edges("D1","A" , "D2","A", "D1","D2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA))

## negative triangle motif (excluding communication)
MOTIFS.TRIANGLE.NEGATIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A", type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT)) +
    igraph::edges("D1","A" , "D2","A", type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER))


## * Square motif ----------------------------------------------------------
## (two authors are connected to two distinct artifact that are coupled)

## positive square motif (including communication)
MOTIFS.SQUARE.POSITIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A1", "A2",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT, TYPE.ARTIFACT)) +
    igraph::edges("D1","A1" , "D2","A2", "A1","A2", "D1","D2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA))

## negative square motif (excluding communication)
MOTIFS.SQUARE.NEGATIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A1", "A2",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT, TYPE.ARTIFACT)) +
    igraph::edges("D1","A1" , "D2","A2", "A1","A2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Motif-identification functions ------------------------------------------

#' Search for the given \code{motif} in the given \code{network}.
#'
#' @param network The network in which the given \code{motif} is searched
#' @param motif The motif to search for in the given \code{network}
#' @param color.attr The the vertex and edge attribute that is used to match the vertices
#'                   and the edges in the \code{motif} with the ones in the given \code{network}.
#'                   This means that a vertex A with the attribute declared in \code{color.attr}
#'                   in the \code{motif} is only matched with vertices with the same attribute in
#'                   the network. The same holds for edges.
#' @param remove.duplicates If \code{remove.duplicates == TRUE}, any duplicate matched motifs are
#'                          removed. This logical value basically resembles the idea of respecting
#'                          the order within a matched motif or not.
#'
#' @return A list of vertex sequences denoting the matched motifs, ordered by \code{color.attr}
#'         and "name" vertex attributes
#'
#' @examples
#' motifs.search.in.network(get.sample.network(), MOTIFS.TRIANGLE.NEGATIVE, color.attr = "type", remove.duplicates = TRUE)
#'
#' FIXME use 'lad' method for igraph::subgraph_isomorphisms as 'induced == TRUE' already filters false-positives?
motifs.search.in.network = function(network, motif, color.attr = "type", remove.duplicates = TRUE) {

    ## find motif in network
    vs = igraph::subgraph_isomorphisms(target = network, pattern = motif, method = "vf2",
                                       vertex.color1 = igraph::get.vertex.attribute(network, color.attr),
                                       vertex.color2 = igraph::get.vertex.attribute(motif, color.attr))

    ## normalize found vertex sequences (sort vertices)
    vs.cleaned = lapply(vs, function(seq) {
        ## get types and names of vertices
        types = igraph::get.vertex.attribute(network, color.attr, index = seq)
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
#' @param motif.collaborating A motif describing the collaboration of authors on artifacts
#' @param motif.communicating A motif describing communication among authors only
#' @param motif.collaborating.and.communicating A motif describing authors collaborating
#'                                              and communicating at the same time
#' @param remove.duplicates If \code{remove.duplicates == TRUE}, any duplicate matched motifs are
#'                          removed. This logical value basically resembles the idea of respecting
#'                          the order within a matched motif or not.
#'
#' @return A named list,
#'         the item 'authors': the total number of authors in the network,
#'         the item 'artifacts': the total number of artifacts in the network,
#'         the item 'complete': the total number of author pairs,
#'         the item 'collaborating': the number of author pairs that collaborate
#'                                   (i.e., amount of matched instances of \code{motif.collaborating}),
#'         the item 'collaborating.raw': the matched instances of \code{motif.collaborating}),
#'         the item 'communicating': the number of author pairs that communicate
#'                                   (i.e., amount of matched instances of \code{motif.communicating}),
#'         the item 'communicating.raw': the matched instances of \code{motif.communicating}),
#'         the item 'collaborating.and.communicating': the number of author pairs that collaborate and communicate
#'                                   (i.e., amount of matched instances of \code{motif.collaborating.and.communicating})
#'         the item 'collaborating.and.communicating.raw': the matched instances of \code{motif.collaborating.and.communicating}),
#'
#'         the item 'p1': the fraction of author pairs communicating,
#'         the item 'p2': the fraction of collaborating author pairs that are also communicating
#'                        (i.e., the fraction of fulfilled coordination requirements),
#'         the item 'correct': a sanitiy check whether
#'                             collaborating.and.communicating.raw == intersect(collaborating.raw, communicating.raw)
#'
#' FIXME give the motifs as a named list that can be used for iteration!
#' FIXME change handling of raw data (store as attribute, make it optional, ...)
motifs.count = function(network, motif.collaborating, motif.communicating, motif.collaborating.and.communicating,
                        remove.duplicates = TRUE) {
    ## get isomorphisms, i.e., motifs in the network
    collaborating = motifs.search.in.network(network = network, motif = motif.collaborating,
                                             color.attr = "type", remove.duplicates = remove.duplicates)
    communicating = motifs.search.in.network(network = network, motif = motif.communicating,
                                             color.attr = "type", remove.duplicates = remove.duplicates)
    collaborating.and.communicating = motifs.search.in.network(network = network, motif = motif.collaborating.and.communicating,
                                                               color.attr = "type", remove.duplicates = remove.duplicates)

    ## compute intersections for verification of matched sets
    cleaned.collaborating = unique(motifs.remove.artifacts.from.matched.motifs(network, collaborating))
    cleaned.collaborating.and.communicating = unique(motifs.remove.artifacts.from.matched.motifs(network, collaborating.and.communicating))
    correct = setequal(cleaned.collaborating.and.communicating, intersect(cleaned.collaborating, communicating))

    ## get list of authors
    authors.length = length(igraph::V(network)[ type == TYPE.AUTHOR ])
    artifacts.length = length(igraph::V(network)[ type == TYPE.ARTIFACT ])
    authors.pairs = choose(authors.length, 2)

    # return an object for later inspection
    return(list(
        "authors" = authors.length, # total number of authors
        "artifacts" = artifacts.length, # total number of artifacts
        "complete" = authors.pairs, # total number of author pairs
        "collaborating" = length(collaborating), # number of author pairs that collaborate
        "collaborating.raw" = collaborating,
        "communicating" = length(communicating), # number of author pairs that communicate
        "communicating.raw" = communicating,
        "collaborating.and.communicating" = length(collaborating.and.communicating), # number of author pairs that collaborate and communicate
        "collaborating.and.communicating.raw" = collaborating.and.communicating,
        "p1" = length(communicating) / authors.pairs, # fraction p1
        "p2" = length(collaborating.and.communicating) / length(collaborating), # fraction p2
        "correct" = correct # sanity check
    ))
}

#' Retrieve the motif count for a predefined list of motifs.
#'
#' @param network The network to use for the search
#'
#' @return A named list,
#'         the item 'triangle': the motif data for the triangle motifs,
#'         the item 'square': the motif data for the square motifs.
#'
#'         The items' data is preserved as returned from \code{motifs.count}.
motifs.count.all = function(network) {
    # get motif counts
    triangle = motifs.count(network, MOTIFS.TRIANGLE.NEGATIVE, MOTIFS.LINE, MOTIFS.TRIANGLE.POSITIVE)
    square = motifs.count(network, MOTIFS.SQUARE.NEGATIVE, MOTIFS.LINE, MOTIFS.SQUARE.POSITIVE)

    # return raw data
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
        ## get types of nodes
        types = igraph::get.vertex.attribute(network, "type", index = seq)
        ## remove artifact nodes
        seq = seq[ types != TYPE.ARTIFACT ]
        return(seq)
    })

    return(vs.cleaned)
}
