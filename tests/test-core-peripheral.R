## This file is part of codeface-extraction-r, which is free software: you
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
## All Rights Reserved.


context("Tests for the file 'util-core-peripheral.R'")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

## Prepare global setting
proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
proj.data = ProjectData$new(proj.conf)

net.conf = NetworkConf$new()
net.builder = NetworkBuilder$new(proj.data, net.conf)

network = net.builder$get.author.network()

test_that("Vertex-degree classification using 'restrict.classification.to.authors'", {

    ## Act
    result = get.author.class.network.degree(network,
                                             restrict.classification.to.authors = c("Olaf", "Björn", "Darth Sidious"))

    ## Assert
    expected.core = data.frame(author.name = c("Olaf", "Björn"), vertex.degree = c(4, 2))
    expected.peripheral = data.frame(author.name = c("Darth Sidious"), vertex.degree = c(NA))
    expected = list(core = expected.core, peripheral = expected.peripheral)

    row.names(result$core) = NULL
    row.names(result$peripheral) = NULL
    expect_equal(expected, result)
})

test_that("Eigenvector classification", {

    ## Act
    set.seed(0)
    result = get.author.class.network.eigen(network)

    ## Assert
    expected.core = data.frame(author.name = c("Olaf", "Thomas", "Björn", "udo", "Fritz fritz@example.org"),
                               eigen.centrality = c(1.0, 0.7116, 0.7116, 0.25, 0.25))
    expected.peripheral = data.frame(author.name = c("georg", "Hans"), eigen.centrality = c(0.25, 0.25))
    expected = list(core = expected.core, peripheral = expected.peripheral)

    row.names(result$core) = NULL
    row.names(result$peripheral) = NULL
    expect_equal(expected, result, tolerance = 0.0001)
})

# TODO: Add a test for hierarchy classification

test_that("Commit-count classification using 'result.limit'" , {

    ## Act
    result = get.author.class.commit.count(proj.data, result.limit = 3)

    ## Assert
    expected.core = data.frame(author.name = c("Karl", "Olaf", "Thomas"), commit.count = c(2, 2, 2))
    expected = list(core = expected.core, peripheral = expected.core[0, ])

    row.names(result$core) = NULL
    row.names(result$peripheral) = NULL
    expect_equal(expected, result)
})

test_that("LOC-count classification" , {

    ## Act
    result = get.author.class.loc.count(proj.data)

    ## Assert
    expected.core = data.frame(author.name = c("Björn", "Karl", "Olaf", "Thomas"), loc.count = c(2, 2, 2, 2))
    expected = list(core = expected.core, peripheral = expected.core[0, ])

    row.names(result$core) = NULL
    row.names(result$peripheral) = NULL
    expect_equal(expected, result)
})

test_that("get.author.class", {

    ## Arrange
    prepared.authors = data.frame(author.name = c("AAA", "BBB", "CCC", "DDD", "EEE"), centrality = c(1, 1, 1, 1, 1))

    ## Act
    result = get.author.class(prepared.authors, "centrality")

    ## Assert
    expected = list(core = prepared.authors[1:4, ], peripheral = prepared.authors[5, ])
    expect_equal(result, expected)

    ## Arrange
    prepared.authors = data.frame(author.name = c("AAA", "BBB", "CCC"), centrality = c(0.5, 0.29, 0.21))

    ## Act
    result = get.author.class(prepared.authors, "centrality")

    ## Assert
    expected = list(core = prepared.authors, peripheral = prepared.authors[0, ])
    expect_equal(result, expected)

    ## Arrange
    prepared.authors = data.frame(author.name = c("AAA", "BBB", "CCC"), centrality = c(0, 0, 0))

    ## Act
    result = get.author.class(prepared.authors, "centrality")

    ## Assert
    expected = list(core = prepared.authors[0, ], peripheral = prepared.authors)
    expect_equal(result, expected)
})
