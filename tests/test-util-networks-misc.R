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
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2022 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2019 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2021 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## All Rights Reserved.


context("Tests for the file 'util-networks-misc.R'")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("getting all authors of a list of networks, list length 0", {

    ## Act
    result = get.author.names.from.networks(networks = list(), globally = FALSE)
    
    ## Assert
    expected = list()
    ## does currently not work like this if globally is TRUE, 
    ## since the unlist function returns null if input is empty list
    ## TODO clarify if this is intended or should be fixed
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of networks, list length 1", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    ## Act
    result = get.author.names.from.networks(networks = list(network))

    ## Assert
    expected = list("Dieter", "Heinz", "Klaus")
    
    expect_equal(expected, result)

})

test_that("getting all authors of a list of networks, list length 1, not global", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    ## Act
    result = get.author.names.from.networks(networks = list(network), globally = FALSE)

    ## Assert
    expected = list(list("Dieter", "Heinz", "Klaus"))
    
    expect_equal(expected, result)

})

test_that("getting all authors of a list of networks, list length 2", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    first.network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    second.vertices = data.frame(
        name = c("Detlef", "Dieter"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    second.edges = data.frame(
        from = "Detlef",
        to = "Dieter"
        )
    second.network = igraph::graph.data.frame(second.edges, directed = FALSE, vertices = second.vertices)
    ## Act
    result = get.author.names.from.networks(networks = list(first.network, second.network))

    ## Assert
    expected = list("Detlef", "Dieter", "Heinz", "Klaus")
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of networks, list length 2, not global", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    first.network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    second.vertices = data.frame(
        name = c("Detlef", "Dieter"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    second.edges = data.frame(
        from = "Detlef",
        to = "Dieter"
        )
    second.network = igraph::graph.data.frame(second.edges, directed = FALSE, vertices = second.vertices)
    ## Act
    result = get.author.names.from.networks(networks = list(first.network, second.network), globally = FALSE)

    ## Assert
    expected = list(list("Dieter", "Heinz", "Klaus"), list("Detlef", "Dieter"))
    
    expect_equal(expected, result)
})