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
    for (i in 1:length(multi.network.split)) {
        author.net = extract.author.network.from.network(multi.network.split[[i]], remove.isolates = TRUE)
        bipartite.net = extract.bipartite.network.from.network(multi.network.split[[i]])

        split.author.networks.two[[i]] = author.net
        split.bipartite.networks.two[[i]] = bipartite.net
    }

    ## compare the edges and the vertices of all the author and bipartite networks that were previously
    ## created with different approaches
    for (i in 1:length(split.author.networks.one)) {
        author.edges.one = igraph::get.data.frame(split.author.networks.one[[i]], what = "edges")
        author.edges.two = igraph::get.data.frame(split.author.networks.two[[i]], what = "edges")
        author.vertices.one = igraph::get.data.frame(split.author.networks.one[[i]], what = "vertices")
        author.vertices.two = igraph::get.data.frame(split.author.networks.two[[i]], what = "vertices")

        expect_identical(author.edges.one, author.edges.two)
        expect_identical(author.vertices.one, author.vertices.two)

        bipartite.edges.one = igraph::get.data.frame(split.bipartite.networks.one[[i]], what = "edges")
        bipartite.edges.two = igraph::get.data.frame(split.bipartite.networks.two[[i]], what = "edges")
        bipartite.vertices.one = igraph::get.data.frame(split.bipartite.networks.one[[i]], what = "vertices")
        bipartite.vertices.two = igraph::get.data.frame(split.bipartite.networks.two[[i]], what = "vertices")

        expect_identical(bipartite.edges.one, bipartite.edges.two)
        expect_identical(bipartite.vertices.one, bipartite.vertices.two)
    }
})
