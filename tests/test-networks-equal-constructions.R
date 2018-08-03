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
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## All Rights Reserved.


context("Test equality when constructing the same network in different ways.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

#' Compare the edges and vertices of the corresponding networks constructed in different ways.
#'
#' @param split.author.networks.one the first list of split author networks
#' @param split.author.networks.two the second list of split author networks
#' @param split.bipartite.networks.one the first list of split bipartite networks
#' @param split.bipartite.networks.two the second list of split bipartite networks
compare.edge.and.vertex.lists = function(split.author.networks.one = NULL, split.author.networks.two = NULL,
                                          split.bipartite.networks.one = NULL, split.bipartite.networks.two = NULL) {

    for (i in seq_along(split.author.networks.one)) {
        author.edges.one = igraph::get.data.frame(split.author.networks.one[[i]], what = "edges")
        ordering = order(author.edges.one[["from"]], author.edges.one[["to"]],
                         author.edges.one[["date"]])
        author.edges.one = author.edges.one[ordering, ]
        rownames(author.edges.one) = seq_len(nrow(author.edges.one))
        author.edges.two = igraph::get.data.frame(split.author.networks.two[[i]], what = "edges")
        ordering = order(author.edges.two[["from"]], author.edges.two[["to"]],
                         author.edges.two[["date"]])
        author.edges.two = author.edges.two[ordering, ]
        rownames(author.edges.two) = seq_len(nrow(author.edges.two))
        author.vertices.one = igraph::get.data.frame(split.author.networks.one[[i]], what = "vertices")
        ordering = order(author.vertices.one[["name"]])
        author.vertices.one = author.vertices.one[ordering, ]
        rownames(author.vertices.one) = seq_len(nrow(author.vertices.one))
        author.vertices.two = igraph::get.data.frame(split.author.networks.two[[i]], what = "vertices")
        ordering = order(author.vertices.two[["name"]])
        author.vertices.two = author.vertices.two[ordering, ]
        rownames(author.vertices.two) = seq_len(nrow(author.vertices.two))

        expect_identical(author.edges.one, author.edges.two)
        expect_identical(author.vertices.one, author.vertices.two)

        bipartite.edges.one = igraph::get.data.frame(split.bipartite.networks.one[[i]], what = "edges")
        ordering = order(bipartite.edges.one[["from"]], bipartite.edges.one[["to"]],
                         bipartite.edges.one[["date"]])
        bipartite.edges.one = bipartite.edges.one[ordering, ]
        rownames(bipartite.edges.one) = seq_len(nrow(bipartite.edges.one))
        bipartite.edges.two = igraph::get.data.frame(split.bipartite.networks.two[[i]], what = "edges")
        ordering = order(bipartite.edges.two[["from"]], bipartite.edges.two[["to"]],
                         bipartite.edges.two[["date"]])
        bipartite.edges.two = bipartite.edges.two[ordering, ]
        rownames(bipartite.edges.two) = seq_len(nrow(bipartite.edges.two))
        bipartite.vertices.one = igraph::get.data.frame(split.bipartite.networks.one[[i]], what = "vertices")
        ordering = order(bipartite.vertices.one[["name"]])
        bipartite.vertices.one = bipartite.vertices.one[ordering, ]
        rownames(bipartite.vertices.one) = seq_len(nrow(bipartite.vertices.one))
        bipartite.vertices.two = igraph::get.data.frame(split.bipartite.networks.two[[i]], what = "vertices")
        ordering = order(bipartite.vertices.two[["name"]])
        bipartite.vertices.two = bipartite.vertices.two[ordering, ]
        rownames(bipartite.vertices.two) = seq_len(nrow(bipartite.vertices.two))

        expect_identical(bipartite.edges.one, bipartite.edges.two)
        expect_identical(bipartite.vertices.one, bipartite.vertices.two)
    }
}

test_that("Compare the bipartite and author network constructed in two ways with author/artifact relation 'cochange'", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    splitting.period = "3 min"

    ## network generation 1
    author.network = network.builder$get.author.network()
    bipartite.network = network.builder$get.bipartite.network()

    ## split the networks
    split.networks = split.networks.time.based(networks = list(author.network, bipartite.network),
                                               time.period = splitting.period, sliding.window = FALSE)

    ## separate the author and bipartite networks
    split.author.networks.one = split.networks[[1]]
    split.bipartite.networks.one = split.networks[[2]]

    ## network generation 2
    multi.network = network.builder$get.multi.network()

    ## split the network
    multi.network.split = split.network.time.based(network = multi.network, time.period = splitting.period)

    split.author.networks.two = list()
    split.bipartite.networks.two = list()

    ## extract the author and bipartite networks from the splitted multi networks
    for (i in seq_along(multi.network.split)) {
        author.net = extract.author.network.from.network(multi.network.split[[i]], remove.isolates = TRUE)
        bipartite.net = extract.bipartite.network.from.network(multi.network.split[[i]])

        split.author.networks.two[[i]] = author.net
        split.bipartite.networks.two[[i]] = bipartite.net
    }

    ## compare the edges and the vertices of all the author and bipartite networks that were previously
    ## created with different approaches
    compare.edge.and.vertex.lists(split.author.networks.one, split.author.networks.two,
                                   split.bipartite.networks.one, split.bipartite.networks.two)
})

test_that("Compare the bipartite and author network constructed in two ways with author relation 'mail' and artifact relation
          'cochange'", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "cochange"))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    splitting.period = "3 min"

    ## network generation 1
    author.network = network.builder$get.author.network()
    bipartite.network = network.builder$get.bipartite.network()

    ## split the networks
    split.networks = split.networks.time.based(networks = list(author.network, bipartite.network),
                                               time.period = splitting.period, sliding.window = FALSE)

    ## separate the author and bipartite networks
    split.author.networks.one = split.networks[[1]]
    split.bipartite.networks.one = split.networks[[2]]

    ## network generation 2
    multi.network = network.builder$get.multi.network()

    ## split the network
    multi.network.split = split.network.time.based(network = multi.network, time.period = splitting.period)

    split.author.networks.two = list()
    split.bipartite.networks.two = list()


    ## extract the author and bipartite networks from the splitted multi networks
    for (i in seq_along(multi.network.split)) {
        author.net = extract.author.network.from.network(multi.network.split[[i]], remove.isolates = TRUE)
        bipartite.net = extract.bipartite.network.from.network(multi.network.split[[i]])

        split.author.networks.two[[i]] = author.net
        split.bipartite.networks.two[[i]] = bipartite.net
    }

    ## compare the edges and the vertices of all the author and bipartite networks that were previously
    ## created with different approaches
    compare.edge.and.vertex.lists(split.author.networks.one, split.author.networks.two,
                                   split.bipartite.networks.one, split.bipartite.networks.two)
})

test_that("Compare the bipartite and author network constructed in two ways with author and artifact relation 'mail'", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    splitting.period = "3 year"

    ## network generation 1
    author.network = network.builder$get.author.network()
    bipartite.network = network.builder$get.bipartite.network()

    ## split the networks
    split.networks = split.networks.time.based(networks = list(author.network, bipartite.network),
                                               time.period = splitting.period, sliding.window = FALSE)

    ## separate the author and bipartite networks
    split.author.networks.one = split.networks[[1]]
    split.bipartite.networks.one = split.networks[[2]]

    ## network generation 2
    multi.network = network.builder$get.multi.network()

    ## split the network
    multi.network.split = split.network.time.based(network = multi.network, time.period = splitting.period)

    split.author.networks.two = list()
    split.bipartite.networks.two = list()


    ## extract the author and bipartite networks from the splitted multi networks
    for (i in seq_along(multi.network.split)) {
        author.net = extract.author.network.from.network(multi.network.split[[i]], remove.isolates = TRUE)
        bipartite.net = extract.bipartite.network.from.network(multi.network.split[[i]])

        split.author.networks.two[[i]] = author.net
        split.bipartite.networks.two[[i]] = bipartite.net
    }

    ## compare the edges and the vertices of all the author and bipartite networks that were previously
    ## created with different approaches
    compare.edge.and.vertex.lists(split.author.networks.one, split.author.networks.two,
                                   split.bipartite.networks.one, split.bipartite.networks.two)
})
