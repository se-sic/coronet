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
## Copyright 2024-2025 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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

    ## helper function to order edges
    order.edges = function(edges) {
        ordering = order(edges[["from"]], edges[["to"]],
                         sapply(edges[["date"]], function(date) sum(unlist(date), rm.na = TRUE)))
        edges = edges[ordering, ]
        rownames(edges) = seq_len(nrow(edges))
        return(edges)
    }

    # helper function to order vertices
    order.vertices = function(vertices) {
        ordering = order(vertices[["name"]])
        vertices = vertices[ordering, ]
        rownames(vertices) = seq_len(nrow(vertices))
        return(vertices)
    }

    for (i in seq_along(split.networks.one)) {
        edges.one = order.edges(igraph::as_data_frame(split.networks.one[[i]], what = "edges"))
        edges.two = order.edges(igraph::as_data_frame(split.networks.two[[i]], what = "edges"))
        expect_identical(edges.one, edges.two)

        vertices.one = order.vertices(igraph::as_data_frame(split.networks.one[[i]], what = "vertices"))
        vertices.two = order.vertices(igraph::as_data_frame(split.networks.two[[i]], what = "vertices"))
        expect_identical(vertices.one, vertices.two)
    }
}

## Test that splitting a multi network, then extracting networks and
## extracting networks first, then using split.networks.time.based results in
## the same networks.
## Note that this is only the case if both the author and the bipartite network are included,
## as otherwise, the multi network might cover a different time period and therefore split differently.
## Including the artifact network is optional, as every edge in the artifact network
## will have a corresponding edge in the author network.
patrick::with_parameters_test_that("Compare the bipartite and author network constructed in two ways", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = test.author.relation,
                                                 artifact.relation = test.artifact.relation,
                                                 author.all.authors = TRUE))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## splitting by time.period would require different periods for different network relations
    ## to produce reasonable results in reasonable time
    number.windows = 5

    ## network generation 1
    author.network = network.builder$get.author.network()
    bipartite.network = network.builder$get.bipartite.network()

    ## split the networks
    split.networks = split.networks.time.based(networks = list(author.network, bipartite.network),
                                               number.windows = number.windows,
                                               sliding.window = test.sliding.window,
                                               remove.isolates = FALSE)

    ## separate the author and bipartite networks
    split.author.networks.one = split.networks[[1]]
    split.bipartite.networks.one = split.networks[[2]]

    ## network generation 2
    multi.network = network.builder$get.multi.network()

    ## split the network
    multi.network.split = split.network.time.based(network = multi.network, number.windows = number.windows,
                                                   sliding.window = test.sliding.window,
                                                   remove.isolates = FALSE)

    split.author.networks.two = list()
    split.bipartite.networks.two = list()

    ## extract the author and bipartite networks from the split multi networks
    for (i in seq_along(multi.network.split)) {
        author.net = extract.author.network.from.network(multi.network.split[[i]], remove.isolates = FALSE)
        bipartite.net = extract.bipartite.network.from.network(multi.network.split[[i]], remove.isolates = FALSE)

        split.author.networks.two[[i]] = author.net
        split.bipartite.networks.two[[i]] = bipartite.net
    }

    ## compare the edges and the vertices of all the author and bipartite networks that were previously
    ## created with different approaches
    compare.edge.and.vertex.lists(split.author.networks.one, split.author.networks.two)
    compare.edge.and.vertex.lists(split.bipartite.networks.one, split.bipartite.networks.two)
}, cases.cross.product(
    cases.cross.product(
        patrick::cases(
            "with author relation 'cochange'" = list(test.author.relation = 'cochange'),
            "with author relation 'mail'" = list(test.author.relation = 'mail'),
            "with author relation 'issue'" = list(test.author.relation = 'issue')
        ),
        patrick::cases(
            "artifact relation 'cochange'" = list(test.artifact.relation = 'cochange'),
            "artifact relation 'mail'" = list(test.artifact.relation = 'mail'),
            "artifact relation 'issue'" = list(test.artifact.relation = 'issue')
        )
    ),
    patrick::cases(
        "sliding window: FALSE" = list(test.sliding.window = FALSE),
        "sliding window: TRUE" = list(test.sliding.window = TRUE)
    )
))

patrick::with_parameters_test_that("Compare the author, artifact and bipartite network constructed in two ways", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = test.author.relation,
                                                 artifact.relation = test.artifact.relation,
                                                 author.all.authors = TRUE))
    net.conf$clear.edge.attributes()

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## splitting by time.period would require different periods for different network relations
    ## to produce reasonable results in reasonable time
    number.windows = 5

    ## network generation 1
    bipartite.network = network.builder$get.bipartite.network()
    artifact.network = network.builder$get.artifact.network()
    author.network = network.builder$get.author.network()

    ## split the networks
    split.networks = split.networks.time.based(networks = list(bipartite.network, artifact.network,
                                                               author.network),
                                               number.windows = number.windows,
                                               sliding.window = test.sliding.window,
                                               remove.isolates = FALSE)

    ## separate the bipartite and artifact networks
    split.bipartite.networks.one = split.networks[[1]]
    split.artifact.networks.one = split.networks[[2]]
    split.author.networks.one = split.networks[[3]]

    ## network generation 2
    multi.network = network.builder$get.multi.network()

    ## split the network
    multi.network.split = split.network.time.based(network = multi.network, number.windows = number.windows,
                                                   sliding.window = test.sliding.window,
                                                   remove.isolates = FALSE)

    split.bipartite.networks.two = list()
    split.artifact.networks.two = list()
    split.author.networks.two = list()


    ## extract the bipartite and artifact networks from the split multi networks
    for (i in seq_along(multi.network.split)) {
        bipartite.net = extract.bipartite.network.from.network(multi.network.split[[i]], remove.isolates = FALSE)
        artifact.net = extract.artifact.network.from.network(multi.network.split[[i]], remove.isolates = FALSE)
        author.net = extract.author.network.from.network(multi.network.split[[i]], remove.isolates = FALSE)

        split.bipartite.networks.two[[i]] = bipartite.net
        split.artifact.networks.two[[i]] = artifact.net
        split.author.networks.two[[i]] = author.net
    }

    ## compare the edges and the vertices of all the bipartite and artifact networks that were previously
    ## created with different approaches
    compare.edge.and.vertex.lists(split.bipartite.networks.one, split.bipartite.networks.two)
    compare.edge.and.vertex.lists(split.artifact.networks.one, split.artifact.networks.two)
    compare.edge.and.vertex.lists(split.author.networks.one, split.author.networks.two)
}, cases.cross.product(
    cases.cross.product(
        patrick::cases(
            "with author relation 'cochange'" = list(test.author.relation = 'cochange'),
            "with author relation 'mail'" = list(test.author.relation = 'mail'),
            "with author relation 'issue'" = list(test.author.relation = 'issue')
        ),
        patrick::cases(
            "artifact relation 'cochange'" = list(test.artifact.relation = 'cochange'),
            "artifact relation 'mail'" = list(test.artifact.relation = 'mail'),
            "artifact relation 'issue'" = list(test.artifact.relation = 'issue')
        )
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
    networks.commit.count = add.vertex.attribute.author.commit.count(networks, proj.data, aggregation.level = "range")
    networks.email = add.vertex.attribute.author.email(networks, proj.data)

    ## add the other attribute
    networks.both.1 = add.vertex.attribute.author.email(networks.commit.count, proj.data)
    networks.both.2 = add.vertex.attribute.author.commit.count(networks.email, proj.data, aggregation.level = "range")

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
