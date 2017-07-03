## Claus Hunsen, 2015, 2017
## hunsen@fim.uni-passau.de


## libraries
requireNamespace("plyr") # for rbind.fill.matrix
requireNamespace("igraph") # graphs, baby!


## / / / / / / / / / / / / / /
## Line motif
## (i.e., two authors connected)
##

MOTIFS.LINE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", type = c(TYPE.AUTHOR, TYPE.AUTHOR)) +
    igraph::edges("D1","D2", type = c(TYPE.EDGES.INTRA))


## / / / / / / / / / / / / / /
## Triangle motif
##

## positive triangle motif
MOTIFS.TRIANGLE.POSITIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT)) +
    igraph::edges("D1","A" , "D2","A", "D1","D2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA))

## negative triangle motif
MOTIFS.TRIANGLE.NEGATIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A", type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT)) +
    igraph::edges("D1","A" , "D2","A", type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER))


## / / / / / / / / / / / / / /
## Square motif
##

## positive square motif
MOTIFS.SQUARE.POSITIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A1", "A2",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT, TYPE.ARTIFACT)) +
    igraph::edges("D1","A1" , "D2","A2", "A1","A2", "D1","D2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA))

## negative square motif
MOTIFS.SQUARE.NEGATIVE = igraph::make_empty_graph(directed = FALSE) +
    igraph::vertices("D1", "D2", "A1", "A2",
             type = c(TYPE.AUTHOR, TYPE.AUTHOR, TYPE.ARTIFACT, TYPE.ARTIFACT)) +
    igraph::edges("D1","A1" , "D2","A2", "A1","A2",
          type = c(TYPE.EDGES.INTER, TYPE.EDGES.INTER, TYPE.EDGES.INTRA))



#' Title
#'
#' @param network
#' @param motif
#' @param color.attr
#' @param remove.duplicates
#'
#' @return
#' @export
#'
#' @examples
motifs.search.in.network = function(network, motif, color.attr = "type", remove.duplicates = TRUE) {

    ## find motif in network
    vs = igraph::subgraph_isomorphisms(target = network, pattern = motif, method = "vf2",
                                       vertex.color1 = igraph::get.vertex.attribute(network, color.attr),
                                       vertex.color2 = igraph::get.vertex.attribute(motif, color.attr))

    ## normalize found vertex sequences (sort vertices)
    vs.cleaned = lapply(vs, function(seq) {
        ## get types and names of vertices
        types = igraph::get.vertex.attribute(network, "type", index = seq)
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


#' Title
#'
#' @param net
#' @param motif.collaborating
#' @param motif.mailing
#' @param motif.collaborating.and.mailing
#' @param remove.duplicates
#'
#' @return
#'
#' @examples
motifs.count = function(network, motif.collaborating, motif.mailing, motif.collaborating.and.mailing,
                        remove.duplicates = TRUE) {

    ## get isomorphisms
    collaborating = motifs.search.in.network(network = network, motif = motif.collaborating, color.attr = "type", remove.duplicates = remove.duplicates)
    mailing = motifs.search.in.network(network = network, motif = motif.mailing, color.attr = "type", remove.duplicates = remove.duplicates)
    collaborating.and.mailing = motifs.search.in.network(network = network, motif = motif.collaborating.and.mailing, color.attr = "type", remove.duplicates = remove.duplicates)

    ## compute intersections for verification of matched sets
    cleaned.collaborating = unique(motifs.remove.artifacts.from.matched.motifs(network, collaborating))
    cleaned.collaborating.and.mailing = unique(motifs.remove.artifacts.from.matched.motifs(network, collaborating.and.mailing))
    correct = setequal(cleaned.collaborating.and.mailing, intersect(cleaned.collaborating, mailing))

    ## get list of developers
    devs.length = length(igraph::V(network)[ type == TYPE.AUTHOR ])
    artifacts.length = length(igraph::V(network)[ type == TYPE.ARTIFACT ])
    dev.pairs = choose(devs.length, 2)

    # return an object for later inspection
    return(list(
        "developers" = devs.length, # total number of developers
        "artifacts" = artifacts.length, # total number of artifacts
        "complete" = dev.pairs, # total number of developer pairs
        "collaborating" = length(collaborating), # number of developer pairs that collaborate
        "collaborating.raw" = collaborating,
        "mailing" = length(mailing), # number of developer pairs that exchange emails
        "mailing.raw" = mailing,
        "collaborating.and.mailing" = length(collaborating.and.mailing), # number of developer pairs that collaborate and exchange emails
        "collaborating.and.mailing.raw" = collaborating.and.mailing,
        "correct" = correct, # sanity check
        "p1" = length(mailing) / dev.pairs, # fraction p1
        "p2" = length(collaborating.and.mailing) / length(collaborating) # fraction p2
    ))

}


#' Title
#'
#' @param network
#'
#' @return
#' @export
#'
#' @examples
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



#### internal


#' Title
#'
#' @param network
#' @param vs
#'
#' @return
#' @export
#'
#' @examples
motifs.remove.artifacts.from.matched.motifs = function(network, vs) {
    vs.cleaned = lapply(vs, function(seq) {
        ## get types of nodes
        types = igraph::get.vertex.attribute(network, "type", index = seq)
        ## remove artifact nodes
        seq = seq[ types != TYPE.ARTIFACT ]
        return(seq)
    })
    return(vs.cleaned)
}


