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
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("patrick")
requireNamespace("igraph")

#' Construct the 'cross product' of two patrick::cases objects.
#' Each case of the first object is combined with each case of the second,
#' test names are joined with a comma.
#' The variable names in the two cases objects are assumed to be disjoint.
#'
#' Calls to this function can be nested to generate cross products of more cases.
#'
#' Example:
#'
#' joined.cases = cases.cross.product(patrick::cases(
#'      "sliding window: FALSE" = list(test.sliding.window = FALSE),
#'      "sliding window: TRUE" = list(test.sliding.window = TRUE)
#' ), patrick::cases(
#'      "PaStA: FALSE" = list(test.pasta = FALSE),
#'      "PaStA: TRUE" = list(test.pasta = TRUE)
#' ))
#'
#' yields the following tibble:
#'
#' .test_name test.sliding.window test.pasta
#' 1 sliding window: FALSE, PaStA: FALSE               FALSE      FALSE
#' 2  sliding window: FALSE, PaStA: TRUE               FALSE       TRUE
#' 3  sliding window: TRUE, PaStA: FALSE                TRUE      FALSE
#' 4   sliding window: TRUE, PaStA: TRUE                TRUE       TRUE
#'
#' Which can be used like this:
#' patrick::with_parameters_test_that( {...}, joined.cases)
#'
#' @param cases.1 a patrick::cases object (or compatible tibble).
#' @param cases.2 a patrick::cases object (or compatible tibble).
#'
#' @returns the cross product of the two patrick::cases objects, as a tibble.
cases.cross.product = function (cases.1, cases.2) {
    ## patrick::cases are tibbles with a column named '.test_name' and columns for each variable.

    ## Creating an empty tibble with matching types is not trivial.
    ## Therefore, the tibble is created with the first row of data.
    result = NULL
    is.first = TRUE

    for (id.1 in seq_len(nrow(cases.1))){
        row.1 = cases.1[id.1,]
        for (id.2 in seq_len(nrow(cases.2))){
            row.2 = cases.2[id.2,]

            test.name.1 = row.1[[".test_name"]]
            test.name.2 = row.2[[".test_name"]]
            ## The new test name consists of both previous names, joined with a comma.
            test.name.combined = paste(test.name.1, test.name.2, sep = ", ")

            ## Select everything from 'row.1' and everything but '.test_name' from 'row.2'.
            row.combined = cbind(row.1, subset(row.2, select = -.test_name))
            ## Set the new combined test name
            row.combined[[".test_name"]] = test.name.combined

            ## If this is the first row, we need to create the tibble.
            ## Otherwise, we add a row.
            if (is.first) {
                result = row.combined
                is.first = FALSE
            } else {
                result = tibble::add_case(result, row.combined)
            }
        }
    }

    return(result)
}

#' Remove the row names of each data frame in a list of dataframes.
#' Useful for comparing data where row names are allowed to change.
#'
#' Note that removing row names causes them to be set to increasing numbers starting at one,
#' so this function may have to be called on both lists involved in a comparison.
#'
#' @param data a list of dataframes
#'
#' @return the list of dataframes, but without row names
remove.row.names.from.data = function(data) {
    return(
        lapply(data, function (df) {
            row.names(df) = NULL
            return(df)
        })
    )
}

#' Remove the row names of all dataframes in a list of lists of dataframes, for example a list of range data objects.
#' @seealso remove.row.names.from.data
#'
#' @param list.of.lists.of.dfs a list of lists of dataframes
#'
#' @return the list of lists of dataframes, but without row names for the data frames
remove.row.names.from.inner.list.of.dfs = function(list.of.lists.of.dfs) {
    return(lapply(list.of.lists.of.dfs, remove.row.names.from.data))
}

#' Assert that two networks are equal. Used for testing purposes.
#'
#' @param network.expected the expected network
#' @param network.actual the actual network
assert.networks.equal = function(network.expected, network.actual) {
    ## TODO  as soon as the bug in igraph is fixed switch to the expect_true function below
    # expect_true(igraph::identical_graphs(network.expected, network.actual))
    expected.edges = igraph::as_data_frame(network.expected, what = "edges")
    expected.vertices = igraph::as_data_frame(network.expected, what = "vertices")

    actual.edges = igraph::as_data_frame(network.actual, what = "edges")
    actual.vertices = igraph::as_data_frame(network.actual, what = "vertices")

    expect_identical(expected.edges, actual.edges, info = "network edges")
    expect_identical(expected.vertices, actual.vertices, info = "network vertices")
}

#' Assert that two sparse matrices are equal. Used for testing purposes.
#' 
#' @param matrix.expected the expected matrix
#' @param matrix.actual the actual matrix
assert.sparse.matrices.equal = function(matrix.expected, matrix.actual) {
    # check if colnames and rownames are equal
    expect_equal(colnames(matrix.expected), colnames(matrix.actual))
    expect_equal(rownames(matrix.expected), rownames(matrix.actual))
    # check if matrices have the same size
    expected.size = length(matrix.expected)
    expect_equal(expected.size, length(matrix.actual))
    # check if contents are the same
    for(i in 1 : expected.size) {
        expect_equal(matrix.expected[i], matrix.actual[i])
    }
}