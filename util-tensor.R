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
## Copyright 2019-2020 by Anselm Fehnker <anselm@muenster.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks
requireNamespace("rTensor") # tensors


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## FourthOrderTensor class ------------------------------------------------

#' The class \code{FourthOrderTensor} creates an (author x relation x author x relation)
#' tensor from a list of networks. The tensor as well as the dimensions and lists
#' of the authors and relations are stored.
#'
FourthOrderTensor = R6::R6Class("FourthOrderTensor",

    ## * private ----------------------------------------------------------

    private = list(
        ## * * data -------------------------------------------------------

        dim = NULL,
        relations = NULL,
        authors = NULL,
        tensor = NULL,

        ## * * tensor creation --------------------------------------------

        #' Creates a fourth-order tensor from a list of networks using their
        #' adjacency matrices.
        #'
        #' @param networks the list of networks
        #' @param weighted decides if the tensor shall be weighted [default: FALSE]
        #'
        #' @return the created tensor
        build.tensor.from.networks = function(networks, weighted = FALSE) {

            ## get adjacency matrices from networks
            adjacency.matrices = get.expanded.adjacency.matrices(networks, weighted)
            ## create an array with the size of the fourth-order tensor that only contains zeros
            array = array(0, dim = private$dim)

            ## transfer entries from adjacency matrices to array
            for (l in seq_along(adjacency.matrices)) {

                matrix = as(adjacency.matrices[[l]], "dgTMatrix")

                for (entry in seq_along(matrix@x)) {
                    ## Transfer the entries from the adjacency matrix to the tensor.
                    ## Due to the property that the indexes saved in a sparse matrix start with 0,
                    ## while the indexes of an array start with 1, the indexes need to be shifted.
                    array[matrix@i[entry] + 1, l, matrix@j[entry] + 1, l] = matrix@x[entry]
                }
            }

            ## convert array to tensor
            tensor = rTensor::as.tensor(array)

            return(tensor)
        }
    ),

    ## * * public ----------------------------------------------------------

    public = list(

        #' Constructor of the class. Constructs a new fourth-order tensor instance
        #' based on the given list of networks.
        #'
        #' @param networks the given list of networks
        #' @param weighted decides if the tensor shall be weighted [default: FALSE]
        initialize = function(networks, weighted = FALSE) {

            private$relations = names(networks)
            private$authors = get.author.names.from.networks(networks)[[1]]
            private$dim = c(length(private$authors), length(private$relations), length(private$authors), length(private$relations))
            private$tensor = private$build.tensor.from.networks(networks, weighted)

        },

        #' Get the dimension of the tensor.
        #'
        #' @return the dimension
        get.dim = function() {
          return(private$dim)
        },

        #' Get the list of relations of the tensor.
        #'
        #' @return the list of relations
        get.relations = function() {
          return(private$relations)
        },

        #' Get the list of authors of the tensor.
        #'
        #' @return the list of authors
        get.authors = function() {
            return(private$authors)
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
get.author.networks.for.multiple.relations = function(network.builder, relations) {

    networks = list()

    networks = lapply(relations, function(rel) {

      ## retrieve network for relation
      network.builder$update.network.conf(updated.values = list(author.relation = rel))
      retrieved.network = network.builder$get.author.network()

      ## check if network is not empty
      if (igraph::vcount(retrieved.network) > 0){
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
## Calculate centrality ----------------------------------------------------

#' Calculate EDCPTD centrality for a given fourth-order tensor.
#' EDCPTD centrality is based on the work "Identifying Key Nodes in Multilayer Networks based on Tensor Decomposition"
#' by Dingjie Wang, Haitao Wang, and Xiufen Zou, Chaos 27, 063108 (2017) [1].
#' [1] https://doi.org/10.1063/1.4985185
#'
#' @param fourth.order.tensor the given tensor
#'
#' @return data frame with EDCPTD score for every author
calculate.EDCPTD.centrality = function(fourth.order.tensor) {

    ## create data frame for results
    results = data.frame(names = fourth.order.tensor$get.authors(), EDCPTD.score = 0)

    ## decompose tensor. 'num_components = 1' needed for EDCPTD centrality.
    ## 'max_iter' and 'tol' chosen from default in documentation.
    decomposition = rTensor::cp(fourth.order.tensor$get.tensor(), num_components = 1, max_iter = 25, tol = 1e-05)

    ## calculate EDCPTD centrality
    for (y in seq_along(fourth.order.tensor$get.relations())) {
        results[["EDCPTD.score"]] = (results[["EDCPTD.score"]]
                                     + abs(decomposition[["U"]][[1]][,1] * decomposition[["U"]][[2]][,1][y])
                                     + abs(decomposition[["U"]][[3]][,1] * decomposition[["U"]][[4]][,1][y])) / 2
    }

    return(results)
}
