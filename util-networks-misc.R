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
## Copyright 2016-2017 by Sofie Kemper <kemperso@fim.uni-passau.de>
## Copyright 2016-2017 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2016-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020, 2023 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2017 by Angelika Schmid <schmidang@fim.uni-passau.de>
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019-2020 by Anselm Fehnker <anselm@muenster.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks
requireNamespace("Matrix") # for sparse matrices

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Get active authors  -----------------------------------------------------

#' Get all author names that are active in at least one of the networks.
#'
#' @param networks the list of networks from which the author names are wanted
#' @param globally decides if all author names are in one list or in separate lists for each network [default: TRUE]
#'
#' @return the list of author names as a list of vectors
get.author.names.from.networks = function(networks, globally = TRUE) {
    
    ## for each network, get a list of authors that are in this network
    active.authors.list = lapply(networks, function(network) {
        active.authors = igraph::V(network)$name
        active.authors = sort(active.authors)
        return(active.authors)
    })

    if (globally) {
        ## flatten the list of lists to one list of authors
        active.authors = unlist(active.authors.list, recursive = FALSE)
        
        ## remove distracting named list members
        names(active.authors) = NULL

        ## remove duplicates and order alphabetically ascending
        active.authors = active.authors[!duplicated(active.authors)]
        active.authors = sort(active.authors)
        ## return as a list
        return(list(active.authors))
    } else {
        return(active.authors.list)
    }
}

#' Get all author names that are active in at least one of the data sources during the data ranges.
#'
#' @param data.ranges the list of the data ranges
#' @param data.sources the data sources from which the author names should be retrieved,
#'                    can be either \code{"commits"}, \code{"mails"}, or \code{"issues"},
#'                    or any combination of them [default: c("commits", "mails", "issues")]
#' @param globally decides if all author names are in one list or in separate for each network [default: TRUE]
#'
#' @return the list of author names as a list of vectors
get.author.names.from.data = function(data.ranges, data.sources = c("commits", "mails", "issues"), globally = TRUE) {

    data.sources = match.arg.or.default(data.sources, several.ok = TRUE)

    ## for each range, get the authors who have been active on at least one data source in this range
    active.authors.list = lapply(data.ranges, function(range.data) {
        
        active.authors = range.data$get.authors.by.data.source(data.sources)
    
        active.authors.names = sort(active.authors[["author.name"]])

        return(active.authors.names)

    })
    
    if (globally) {
        ## flatten the list of lists to one list of authors
        active.authors = unlist(active.authors.list, recursive = FALSE)

        ## remove distracting named list members
        names(active.authors) = NULL

        ## remove duplicates and order alphabetically ascending
        active.authors = active.authors[!duplicated(active.authors)]
        active.authors = sort(active.authors)
        ## return as a list
        return(list(active.authors))
    } else {
        return(active.authors.list)
    }
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Adjacency matrices ----------------------------------------------------

#' Get a sparse expanded adjacency matrix (in triplet format) for a given network.
#'
#' The adjacency matrix is expanded as it may contain rows and columns for authors which are not part of the network
#' but given in the \code{authors} parameter. However, this also means that authors present in the network
#' but not given in the \code{authors} parameter are not contained in the expanded adjacency matrix.
#'
#' @param network the given network
#' @param authors all authors that are wanted in the adjacency matrix
#' @param weighted decides if the adjacency matrix shall be weighted [default: FALSE]
#'
#' @return the sparse adjacency matrix of the network
get.expanded.adjacency = function(network, authors, weighted = FALSE) {

    ## create an empty sparse matrix using the triplet form with the right size.
    ## x = 0 indicates that the matrix should contain numeric values (i.e., it is a 'dgTMatrix';
    ## without setting x = 0 it would be a binary 'ngTMatrix')
    matrix = Matrix::sparseMatrix(i = c(), j = c(), x = 0, dims = c(length(authors), length(authors)), repr = "T")

    ## add row and column names
    rownames(matrix) = authors
    colnames(matrix) = authors

    if (igraph::vcount(network) > 0) {

        if (weighted) {
            ## get the weighted adjacency matrix for the current network
            matrix.data = igraph::get.adjacency(network, attr = "weight")
        } else {
            ## get the unweighted sparse adjacency matrix for the current network
            matrix.data = igraph::get.adjacency(network)
        }
        
        network.authors.num = nrow(matrix.data)
        ## order the adjacency matrix and filter out authors that were not in authors list
        if (nrow(matrix.data) > 1) { # for a 1x1 matrix ordering does not work
            matrix.data = matrix.data[order((rownames(matrix.data)[rownames(matrix.data) %in% authors])), 
                            order((rownames(matrix.data)[rownames(matrix.data) %in% authors]))]
        }

        if(network.authors.num > nrow(matrix.data)) { 
            # write a warning with the number of authors from the network that we ignore
            warning.string = sprintf("The network had %d authors that will not be displayed in the matrix!",
                                        network.authors.num - nrow(matrix.data))
            warning(warning.string)
        }

        ## save the activity data per author
        if (nrow(matrix.data) > 0) {
            matrix[rownames(matrix.data), colnames(matrix.data)] = matrix.data
        }

        if (!weighted) {
            matrix[matrix > 0] = 1
        }

    }

    return(matrix)
}

#' Calculates a sparse adjacency matrix for each network in the list.
#' All adjacency matrices are expanded in such a way that the use the same set
#' of authors derived from all networks in the list.
#'
#' @param networks list of networks
#' @param weighted decides if the adjacency matrix shall be weighted [default: FALSE]
#'
#' @return the list of adjacency matrices
get.expanded.adjacency.matrices = function(networks, weighted = FALSE){

    adjacency.matrices = parallel::mclapply(networks, function(network) {
        active.authors = igraph::V(network)$name
        active.authors = sort(active.authors)
        return (get.expanded.adjacency(network = network, authors = active.authors, weighted = weighted))
    })

    return(adjacency.matrices)
}

#' Gets a list of networks, converts them to sparse adjacency matrices, and sums up the adjacency matrices cumulatively.
#' This means that the first entry of the returned list is just the adjacency matrix from the first network,
#' the second entry is the sum of the first and the second entry, and so on.
#'
#' @param networks list of networks
#' @param weighted decides if the adjacency matrix shall be weighted [default: FALSE]
#'
#' @return the list of cumulated adjacency matrices
get.expanded.adjacency.cumulated = function(networks, weighted = FALSE) {
    ## get expanded adjacency matrices first
    matrices = get.expanded.adjacency.matrices(networks, weighted)

    ## pair-wise sum of matrices: m.cumul(n) = m.cumul(m - 1) + m
    ## (intermediate results consecutively stored in matrices.cumulated)
    matrices.cumulated = list(matrices[[1]]) # first one is complete already

    if (length(matrices) > 1) {
        for (m in 2:(length(matrices))){

            matrices.cumulated[[m]] = matrices.cumulated[[m - 1]] + matrices[[m]]
            rownames(matrices.cumulated[[m]]) = rownames(matrices.cumulated[[m - 1]])
            colnames(matrices.cumulated[[m]]) = colnames(matrices.cumulated[[m - 1]])
            
            if (!weighted) {

                ## search for a non-zero entry and set them to an arbitray number (e.g., 42)
                ## to force that all non-zero entries are correctly set to 1 afterwards
                if(length(matrices.cumulated[[m]]@i) > 0)

                row = matrices.cumulated[[m]]@i[1]
                col = matrices.cumulated[[m]]@j[1]

                matrices.cumulated[[m]][row][col] = 42
                matrices.cumulated[[m]]@x = rep(1, length(matrices.cumulated[[m]]@i))
            }
        }
    }

    return(matrices.cumulated)
}

#' Converts a list of adjacency matrices to an array.
#'
#' @param adjacency.list the list of adjacency matrices
#'
#' @return the converted array
convert.adjacency.matrix.list.to.array = function(adjacency.list){

    ## create a 3-dimensional array representing the adjacency matrices (SIENA data format) as result
    array = array(data = 0, dim = c(nrow(adjacency.list[[1]]), nrow(adjacency.list[[1]]), length(adjacency.list)))
    rownames(array) = rownames(adjacency.list[[1]])
    colnames(array) = colnames(adjacency.list[[1]])

    ## copy the activity values from the adjacency matrices in the list to the corresponding array slices
    for (i in seq_along(adjacency.list)) {
        adjacency = adjacency.list[[i]]
        activity.indices = Matrix::which(adjacency != 0, arr.ind = TRUE)

        for (j in seq_len(nrow(activity.indices))) {
            array[as.vector(activity.indices[j, 1]), as.vector(activity.indices[j, 2]), i] =
                adjacency[as.vector(activity.indices[j, 1]), as.vector(activity.indices[j, 2])]
        }
    }

    return(array)
}
