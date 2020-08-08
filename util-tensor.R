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
## Copyright 2020 by Anselm Fehnker <anselm@muenster.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ForthOrderTensor class ------------------------------------------------

#' The class \code{ForthOrderTensor} creates an (author x relation x author x relation)
#' tensor from a list of networks. The tensor as well as the dimensions and lists
#' of the authors and relations are stored.
#'
ForthOrderTensor = R6::R6Class("ForthOrderTensor",

   ## * private ----------------------------------------------------------

   private = list(
     ## * * data ---------------------------------------------------------

     dim = NULL,
     relations = NULL,
     authors = NULL,
     tensor = NULL,

     ## * * tensor creation ----------------------------------------------

     #' Creates a forth-order tensor from a list of networks using their
     #' adjacency matrices.
     #'
     #' @param networks the list of networks
     #'
     #' @return the created tensor
     build.tensor.from.networks = function(networks, weighted = FALSE) {

       ## get adjacency matrices from networks
       adjacency.matrices = parallel::mclapply(networks, get.expanded.adjacency, private$authors, weighted)

       ## create an array with the size of the forth-order tensor that only contains zeros
       array <-array(0, dim = private$dim)

       ## transfer entries from adjacency matrices to array
       for (l in 1:length(adjacency.matrices)) {

         matrix = as(adjacency.matrices[[l]], "dgTMatrix")

         for (entry in 1:length(matrix@x)) {
           array[matrix@i[entry]+1, l, matrix@j[entry]+1, l] = matrix@x[entry]
         }
       }

       ## convert array to tensor
       tensor <- rTensor::as.tensor(array)

       return(tensor)
     }
   ),

   ## * * public ----------------------------------------------------------

   public = list(

     #' Constructor of the class. Constructs a new forth-order tensor instance
     #' based on the given list of networks.
     #'
     #' @param networks the given list of networks
     #' @param weighted bool if the tensor shall be weighted
     initialize = function(networks, weighted = FALSE) {

       private$relations = names(networks)
       private$authors = get.author.names.from.networks(networks)
       private$dim = c(length(private$authors), length(private$relations), length(private$authors), length(private$relations))
       private$tensor = private$build.tensor.from.networks(networks, weighted)

     },

     #' Get the list of authors of the tensor.
     #'
     #' @return the list of authors
     get.authors = function() {
       return(private$authors)
     },

     #' Get the list of relations of the tensor.
     #'
     #' @return the list of relations
     get.relations = function() {
       return(private$relations)
     },

     #' Get the tensor data saved in the object.
     #'
     #' @return the tensor data
     get.tensor = function() {
       return(private$tensor)
     }

   )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Get author networks -----------------------------------------------------

#' Get a list of author networks for each relation.
#' If a relation is not available for the current project, it is not added to the list.
#'
#' @param network.builder the network builder for the project
#' @param relations the relations of the wanted networks
#'
#' @return the list of networks
get.author.networks = function(network.builder, relations) {

  networks = list()

  networks = lapply(relations, function(rel) {

    ## retrieve network for relation
    network.builder$update.network.conf(updated.values = list(author.relation = rel))
    retrieved.network = network.builder$get.author.network()

    ## check if network is not empty
    if(igraph::vcount(retrieved.network) > 0){
      logging::loginfo("Added %s data to list", rel)
      return(retrieved.network)
    } else {
      logging::logwarn("There is no %s data available for the current project", rel)
      return(NA)
    }
  })

  ## add names of the relations
  names(networks) = relations

  ## removes empty networks
  networks = networks[!is.na(networks)]

  return(networks)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Get active authors  -----------------------------------------------------

#' Get all author names that are active in at least one of the networks.
#'
#' @param networks the list of networks
#'
#' @return the list of author names
get.author.names.from.networks = function(networks) {

  ## for each network, get a list of authors that are in this network
  active.authors.list = lapply(networks, function(network) {
    active.authors = igraph::V(network)$name
    return (active.authors)
  })

  ## flatten the list of lists to one list of authors
  active.authors = unlist(active.authors.list, recursive = FALSE)

  ## remove distracting named list members
  names(active.authors) = NULL

  ## remove duplicates
  active.authors = active.authors[!duplicated(active.authors)]

  ## order alphabetically ascending
  active.authors = active.authors[order(active.authors)]

  return (active.authors)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Get adjacency matrix ----------------------------------------------------

#' Get a sparse adjacency matrix for a network.
#'
#' @param network the given network
#' @param authors all authors that are wanted in the adjacency matrix
#' @param weighted bool if the adjacency matrix shall be weighted
#'
#' @return the list of author names
get.expanded.adjacency = function(network, authors, weighted = FALSE) {

  ## create an empty sparse matrix with the right size
  matrix = Matrix::sparseMatrix(i = c(), j = c(), dims = c(length(authors), length(authors)), giveCsparse = FALSE)
  matrix = as(matrix, "dgTMatrix")

  ## add row and column names
  rownames(matrix) = authors
  colnames(matrix) = authors

  if(igraph::vcount(network) > 0) {

    if(weighted) {
      ## get the weighted adjacency matrix for the current network
      matrix.data = igraph::get.adjacency(network, attr = "weight")
    } else {
      ## get the unweighted adjacency matrix for the current network
      matrix.data = igraph::get.adjacency(network)
    }

    ## order the adjacency matrix
    if(nrow(matrix.data)>1) { # for a 1x1 matrix ordering doesn't work
      matrix.data = matrix.data[order(rownames(matrix.data)), order(colnames(matrix.data))]
    }

    ## save the activity data per developer
    if(nrow(matrix.data)>0) {
      matrix[rownames(matrix.data), colnames(matrix.data)] = matrix.data
    }

    if(!weighted) {
      matrix[matrix > 0] <- 1
    }

  }

  return(matrix)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Calculate centrality ----------------------------------------------------

#' Calculate EDCPTD centrality for a given forth-order tensor.
#'
#' @param forth.order.tensor the given tensor
#'
#' @return data frame with EDCPTD score for every author
calculate.EDCPTD.centrality = function(forth.order.tensor) {

  ## create data frame for results
  results = data.frame(names = forth.order.tensor$get.authors(), EDCPTD.score = 0)

  ## decompose tensor
  decomposition <-rTensor::cp(forth.order.tensor$get.tensor(), num_components = 1, max_iter = 50, tol = 1e-05)

  ## calculate EDCPTD centrality
  for (y in 1:length(forth.order.tensor$get.relations())) {
    results[["EDCPTD.score"]] = (results[["EDCPTD.score"]]
    + abs(decomposition[["U"]][[1]][,1] * decomposition[["U"]][[2]][,1][y])
    + abs(decomposition[["U"]][[3]][,1] * decomposition[["U"]][[4]][,1][y]))/2
  }

  return(results)
}
