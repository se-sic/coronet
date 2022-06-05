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
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
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
#' @param split.networks.one the first list of split networks
#' @param split.networks.two the second list of split networks
compare.edge.and.vertex.lists = function(split.networks.one, split.networks.two) {

    for (i in seq_along(split.networks.one)) {
        edges.one = igraph::get.data.frame(split.networks.one[[i]], what = "edges")
        ordering = order(edges.one[["from"]], edges.one[["to"]],
                         edges.one[["date"]])
        edges.one = edges.one[ordering, ]
        rownames(edges.one) = seq_len(nrow(edges.one))
        edges.two = igraph::get.data.frame(split.networks.two[[i]], what = "edges")
        ordering = order(edges.two[["from"]], edges.two[["to"]],
                         edges.two[["date"]])
        edges.two = edges.two[ordering, ]
        rownames(edges.two) = seq_len(nrow(edges.two))
        vertices.one = igraph::get.data.frame(split.networks.one[[i]], what = "vertices")
        ordering = order(vertices.one[["name"]])
        vertices.one = vertices.one[ordering, ]
        rownames(vertices.one) = seq_len(nrow(vertices.one))
        vertices.two = igraph::get.data.frame(split.networks.two[[i]], what = "vertices")
        ordering = order(vertices.two[["name"]])
        vertices.two = vertices.two[ordering, ]
        rownames(vertices.two) = seq_len(nrow(vertices.two))

        expect_identical(edges.one, edges.two)
        expect_identical(vertices.one, vertices.two)
    }
}

patrick::with_parameters_test_that("Compare the bipartite and author network constructed in two ways", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = test.author.relation, artifact.relation = test.artifact.relation))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    splitting.period = test.splitting.period

    ## network generation 1
    author.network = network.builder$get.author.network()
    bipartite.network = network.builder$get.bipartite.network()

    ## split the networks
    split.networks = split.networks.time.based(networks = list(author.network, bipartite.network),
                                               time.period = splitting.period, sliding.window = test.sliding.window)

    ## separate the author and bipartite networks
    split.author.networks.one = split.networks[[1]]
    split.bipartite.networks.one = split.networks[[2]]

    ## network generation 2
    multi.network = network.builder$get.multi.network()

    ## split the network
    multi.network.split = split.network.time.based(network = multi.network, time.period = splitting.period,
                                                   sliding.window = test.sliding.window)

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
    compare.edge.and.vertex.lists(split.author.networks.one, split.author.networks.two)
    compare.edge.and.vertex.lists(split.bipartite.networks.one, split.bipartite.networks.two)
}, cases.cross.product(
    patrick::cases(
        "with author relation 'cochange' and artifact relation 'cochange'" =
            list(test.author.relation = 'cochange', test.artifact.relation = 'cochange',
                 test.splitting.period = '3 min'),
        "with author relation 'mail' and artifact relation 'cochange'" =
            list(test.author.relation = 'mail', test.artifact.relation = 'cochange',
                 test.splitting.period = '3 min'),
        "with author relation 'mail' and artifact relation 'mail'" =
            list(test.author.relation = 'mail', test.artifact.relation = 'mail',
                 test.splitting.period = '1 year')
    ),
    patrick::cases(
        "sliding window: FALSE" = list(test.sliding.window = FALSE),
        "sliding window: TRUE" = list(test.sliding.window = TRUE)
    )
))

## Vertex attribute order
test_that("Compare networks after adding vertex attributes in different order", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    author.network = network.builder$get.author.network()
    networks = split.network.time.based(author.network, number.windows = 2)

    ## add commit count or email attribute
    networks.commit.count = add.vertex.attribute.commit.count.author(networks, proj.data, aggregation.level = "range")
    networks.email = add.vertex.attribute.author.email(networks, proj.data)

    ## add the other attribute
    networks.both.1 = add.vertex.attribute.author.email(networks.commit.count, proj.data)
    networks.both.2 = add.vertex.attribute.commit.count.author(networks.email, proj.data, aggregation.level = "range")

    ## Order of attributes is now different, while the content is the same.
    ## The resulting networks are therefore not equal.
    expect_false(compare(networks.both.1, networks.both.2)$equal)
})

## Vertex attribute added twice
test_that("Compare networks after adding vertex attribute once or twice", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    author.network = network.builder$get.author.network()
    networks = split.network.time.based(author.network, number.windows = 2)

    ## add email attribute
    networks.email = add.vertex.attribute.author.email(networks, proj.data)

    ## add email attribute again
    networks.email.twice = add.vertex.attribute.author.email(networks.email, proj.data)

    ## the attribute should only be contained once, so the resulting graphs should be equal
    compare.edge.and.vertex.lists(networks.email, networks.email.twice)

})

## Edge attribute order
test_that("Compare networks after adding edge attributes in different order", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))
    net.conf$clear.edge.attributes()

    ## construct two networks with the edge attributes specified in different orders
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.builder$update.network.conf(updated.values = list(edge.attributes = c("author.name", "author.email")))
    author.network = network.builder$get.author.network()
    networks.1 = split.network.time.based(author.network, number.windows = 2)

    network.builder$update.network.conf(updated.values = list(edge.attributes = c("author.email", "author.name")))
    author.network = network.builder$get.author.network()
    networks.2 = split.network.time.based(author.network, number.windows = 2)

    ## edge attributes should be sorted, so the resulting networks should be the same
    compare.edge.and.vertex.lists(networks.1, networks.2)
})
