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
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## All Rights Reserved.


context("Basic network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Simplification ----------------------------------------------------------

test_that("Simplify network with more than one relation", {

    ##
    ## Simplify networks with two relations, but only one loop edge
    ##

    ## create network configuration
    net.conf = NetworkConf$new()
    # net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "cochange"))

    ## create author network (relation = "mail") and artifact network and join them
    author.net =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", "C", type = TYPE.AUTHOR, kind = TYPE.AUTHOR) +
        igraph::edges("A", "A", type = TYPE.EDGES.INTRA, relation = "mail")
    artifact.net =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("Base_Feature", type = TYPE.ARTIFACT, kind = "feature")
    base.net = igraph::disjoint_union(author.net, artifact.net)

    ## create bipartite relations between authors and artifacts (relation = "cochange")
    bip.relations = list(
        cochange =
            list(
                Base_Feature = data.frame(
                    data.vertices = c("A", "A"),
                    date = get.date.from.string(c("2010-01-01", "2010-01-02")),
                    artifact = "feature"
                )
            )
    )

    ## add bipartite relations to base network
    g = add.edges.for.bipartite.relation(base.net, bip.relations, net.conf)

    ## simplify the network without any errors (hopefully)
    expect_error(simplify.network(g), NA) # expect that no error occurs
    expect_identical(igraph::V(simplify.network(g))$name, c("A", "B", "C", "Base_Feature")) # vertices
    expect_identical(igraph::ecount(simplify.network(g)), 1) # edges
    expect_true(igraph::are.connected(simplify.network(g), "A", "Base_Feature")) # specific edge

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Merge -------------------------------------------------------------------

test_that("Merge networks", {

    ##
    ## Merge two empty networks (i.e., no vertices at all)
    ##

    network.empty = create.empty.network(directed = FALSE)
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## Merge two empty networks (i.e., no vertices at all, but with vertex attributes)
    ##

    network.empty = create.empty.network(directed = FALSE)
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Extraction of sub-networks ----------------------------------------------

test_that("Extraction of sub-networks", {

    base.net = get.sample.network()
    base.net = igraph::delete_graph_attr(base.net, "layout")

    ##
    ## extraction of author networks
    ##

    ## construct original author network
    author.net.built = extract.author.network.from.network(base.net, remove.isolates = FALSE)

    ## construct expected author network (by removing artifact vertices and adjacent edges)
    author.net.expected = igraph::delete.vertices(base.net, igraph::V(base.net)[7:12])

    expect_true(igraph::identical_graphs(author.net.built, author.net.expected), info = "author-network extraction")

    ##
    ## extraction of bipartite networks
    ##

    ## construct original bipartite network
    bip.net.built = extract.bipartite.network.from.network(base.net)

    ## construct expected bipartite network (by removing unipartite edges and isolate vertices)
    bip.net.expected = igraph::delete.edges(base.net, igraph::E(base.net)[1:9])
    bip.net.expected = igraph::delete.vertices(bip.net.expected, "A2")

    expect_true(igraph::identical_graphs(bip.net.built, bip.net.expected), info = "bipartite-network extraction")

    ##
    ## extraction of artifact networks
    ##

    ## construct original artifact network
    art.net.built = extract.artifact.network.from.network(base.net, remove.isolates = FALSE)

    ## construct expected artifact network (by removing author vertices and adjacent edges)
    art.net.expected = igraph::delete.vertices(base.net, igraph::V(base.net)[1:6])

    expect_true(igraph::identical_graphs(art.net.built, art.net.expected), info = "artifact-network extraction")

    ##
    ## edge cases for functions
    ##

    ## extract bipartite from empty network (no error)
    expect_error(extract.bipartite.network.from.network(create.empty.network()), NA,
                 info = "extract bipartite network from empty network (no vertices, no edges)")


    ## extract bipartite from network without edge attribute 'type' (error!)
    expect_error(extract.bipartite.network.from.network(create.empty.network() + igraph::vertices(1)),
                 info = "extract bipartite network from network without edge attribute 'type'")

    ## extract bipartite from edgeless network (no error)
    edgeless.net = base.net - igraph::edges(seq_len(igraph::ecount(base.net)))
    expect_error(extract.bipartite.network.from.network(edgeless.net), NA,
                 info = "extract bipartite network from edgeless network")
    ## the extracted network should be empty then (but with all attributes!)
    expect_true(igraph::identical_graphs(
        extract.bipartite.network.from.network(edgeless.net),
        base.net - igraph::vertices(seq_len(igraph::vcount(base.net)))
    ), info = "extracted bipartite network is empty for edgeless base network")
})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Construction of networks without data -----------------------------------

test_that("Construction of networks without data", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", TRUE)
    proj.conf$update.value("commits.filter.untracked.files", TRUE)
    net.conf = NetworkConf$new()
    net.conf$clear.edge.attributes() # remove all but the mandatory edge attributes

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    proj.data$set.commits(NULL) # remove all commit data!
    proj.data$set.mails(NULL) # remove all mail data!
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ##
    ## single author relation
    ##

    network.builder$update.network.conf(updated.values = list(author.relation = "cochange"))

    ## test construction
    expect_error(network.builder$get.author.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.author.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## several author relations
    ##

    network.builder$update.network.conf(updated.values = list(author.relation = c("cochange", "mail")))

    ## test construction
    expect_error(network.builder$get.author.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.author.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## single artifact relation
    ##

    network.builder$update.network.conf(updated.values = list(artifact.relation = "cochange"))

    ## test construction
    expect_error(network.builder$get.artifact.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.artifact.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## several artifact relations
    ##

    network.builder$update.network.conf(updated.values = list(artifact.relation = c("cochange", "mail")))

    ## test construction
    expect_error(network.builder$get.artifact.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.artifact.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## single bipartite relation
    ##

    network.builder$update.network.conf(updated.values = list(artifact.relation = "cochange"))

    ## test construction
    expect_error(network.builder$get.bipartite.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.bipartite.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## several artifact relations
    ##

    network.builder$update.network.conf(updated.values = list(artifact.relation = c("cochange", "mail")))

    ## test construction
    expect_error(network.builder$get.bipartite.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.bipartite.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## single multi-network relation
    ##

    network.builder$update.network.conf(updated.values = list(author.relation = "cochange",
                                                              artifact.relation = "cochange"))

    ## test construction
    expect_error(network.builder$get.multi.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.multi.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

    ##
    ## several multi-network relations
    ##

    network.builder$update.network.conf(updated.values = list(author.relation = c("cochange", "mail"),
                                                              artifact.relation = c("cochange", "mail")))

    ## test construction
    expect_error(network.builder$get.multi.network(), NA) # expect that no error occurs

    ## test emptiness
    network.empty = network.builder$get.multi.network()
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Construction of networks with empty edge list ---------------------------

test_that("Construction of networks from empty edge list (with vertices)", {

    ## create data structures and network configuration as a basis
    edge.list = data.frame(from = c("D1", "D2"), to   = c("D2", "D1"))
    edge.list.as.sequence = as.vector(as.matrix(edge.list))
    vertices = data.frame(name = c("D1", "D2"))
    vertices.as.sequence = vertices[["name"]]
    directed = FALSE # directedness does not matter for this test, but should be consistent
    net.conf = NetworkConf$new()

    ## construct edgeless network
    net.edgeless = create.empty.network(directed = directed) + igraph::vertices(vertices.as.sequence)
    ## add attribute 'weight' which is always added by 'construct.network.from.edge.list'
    net.edgeless = igraph::set.edge.attribute(net.edgeless, "weight", value = 1)

    ##
    ## normal network
    ##

    net.constructed = construct.network.from.edge.list(vertices, edge.list, net.conf)
    net.expected = igraph::graph.empty(n = 0, directed = directed) +
        igraph::vertices(vertices.as.sequence) +
        igraph::edges(edge.list.as.sequence, weight = 1)

    ## check equality
    expect_true(igraph::identical_graphs(net.constructed, net.expected), label = "normal network construction")

    ##
    ## edgeless network: NULL
    ##

    net.constructed = construct.network.from.edge.list(vertices, NULL, net.conf)
    expect_true(igraph::identical_graphs(net.constructed, net.edgeless), label = "edgeless network: NULL")

    ##
    ## edgeless network: create.empty.edge.list()
    ##

    net.constructed = construct.network.from.edge.list(vertices, create.empty.edge.list(), net.conf)
    expect_true(igraph::identical_graphs(net.constructed, net.edgeless), label = "edgeless network: create.empty.edge.list()")

    ##
    ## edgeless network: empty data.frame
    ##

    net.constructed = construct.network.from.edge.list(vertices, data.frame(), net.conf)
    expect_true(igraph::identical_graphs(net.constructed, net.edgeless), label = "edgeless network: empty data.frame")

})

test_that("Construction of networks from empty edge list (without vertices)", {

    ## create data structures and network configuration as a basis
    edge.list = create.empty.edge.list()
    directed = FALSE # directedness does not matter for this test, but should be consistent
    net.conf = NetworkConf$new()

    ## construct edgeless network
    net.edgeless = create.empty.network(directed = directed)
    ## add attributes 'name' and 'weight' which is always added by 'construct.network.from.edge.list'
    net.edgeless = igraph::set.vertex.attribute(net.edgeless, "name", value = "name")
    net.edgeless = igraph::set.edge.attribute(net.edgeless, "weight", value = 1)

    ##
    ## vertices: NULL
    ##

    net.constructed = construct.network.from.edge.list(vertices = NULL, edge.list, net.conf, directed = directed)
    expect_true(igraph::identical_graphs(net.constructed, net.edgeless), label = "vertices: NULL")

    ##
    ## vertices: empty data.frame with columns
    ##

    vertices = create.empty.data.frame("name", "character")
    net.constructed = construct.network.from.edge.list(vertices, edge.list, net.conf)
    expect_true(igraph::identical_graphs(net.constructed, net.edgeless), label = "edgeless network: empty data.frame")

    ##
    ## vertices: empty data.frame
    ##

    net.constructed = construct.network.from.edge.list(data.frame(), edge.list, net.conf, directed = directed)
    expect_true(igraph::identical_graphs(net.constructed, net.edgeless), label = "vertices: empty data.frame")

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Edge attributes ---------------------------------------------------------

test_that("Addition of edge attributes regardless of empty data", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$clear.edge.attributes() # remove all but the mandatory edge attributes
    ## construct data object
    proj.data = ProjectData$new(project.conf = proj.conf)
    proj.data$set.commits(NULL)
    proj.data$set.mails(NULL)
    proj.data$set.issues(NULL)
    ## construct network builder
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## expected empty network with correct mandatory attributes
    network.expected = create.empty.network(directed = FALSE, add.attributes = TRUE)

    ##
    ## author networks
    ##

    ## cochange-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "cochange"))
    network.built = network.builder$get.author.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "mail"))
    network.built = network.builder$get.author.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "issue"))
    network.built = network.builder$get.author.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – issue")

    ##
    ## bipartite networks
    ##

    ## cochange-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = c("cochange")))
    network.built = network.builder$get.bipartite.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "mail"))
    network.built = network.builder$get.bipartite.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "issue"))
    network.built = network.builder$get.bipartite.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – issue")

    ##
    ## artifact networks
    ##

    ## cochange-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "cochange"))
    network.built = network.builder$get.artifact.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "mail"))
    network.built = network.builder$get.artifact.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "issue"))
    network.built = network.builder$get.artifact.network()
    ## 2) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – issue")


    ##
    ## multi networks
    ##

    ## cochange-based artifact network, cochange-based author network:
    ## 1) build network
    network.builder$update.network.conf(list(artifact.relation = "cochange", author.relation = "cochange"))
    network.built = network.builder$get.multi.network()
    ## 2) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – cochange/cochange"
    )

    ## mail-based artifact network, mail-based author network:
    ## 1) build network
    network.builder$update.network.conf(list(artifact.relation = "mail", author.relation = "mail"))
    network.built = network.builder$get.multi.network()
    ## 2) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – mail/mail"
    )

    ## issue-based artifact network, cochange-based author network:
    ## 1) build network
    network.builder$update.network.conf(list(artifact.relation = "issue", author.relation = "cochange"))
    network.built = network.builder$get.multi.network()
    ## 2) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – issue/cochange"
    )

})


test_that("Addition of edge attributes with data", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$clear.edge.attributes() # remove all but the mandatory edge attributes
    ## construct data object
    proj.data = ProjectData$new(project.conf = proj.conf)
    ## construct network builder
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## expected empty network with correct mandatory attributes
    network.expected = create.empty.network(directed = FALSE, add.attributes = TRUE)

    ##
    ## author networks
    ##

    ## cochange-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "cochange"))
    network.built = network.builder$get.author.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "mail"))
    network.built = network.builder$get.author.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "issue"))
    network.built = network.builder$get.author.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – issue")

    ##
    ## bipartite networks
    ##

    ## cochange-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = c("cochange")))
    network.built = network.builder$get.bipartite.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "mail"))
    network.built = network.builder$get.bipartite.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "issue"))
    network.built = network.builder$get.bipartite.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – issue")

    ##
    ## artifact networks
    ##

    ## cochange-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "cochange"))
    network.built = network.builder$get.artifact.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "mail"))
    network.built = network.builder$get.artifact.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "issue"))
    network.built = network.builder$get.artifact.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – issue")

    ##
    ## multi networks
    ##

    ## cochange-based artifact network, cochange-based author network:
    ## 1) build network
    network.builder$update.network.conf(list(artifact.relation = "cochange", author.relation = "cochange"))
    network.built = network.builder$get.multi.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – cochange/cochange"
    )

    ## mail-based artifact network, mail-based author network:
    ## 1) build network
    network.builder$update.network.conf(list(artifact.relation = "mail", author.relation = "mail"))
    network.built = network.builder$get.multi.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – mail/mail"
    )

    ## issue-based artifact network, cochange-based author network:
    ## 1) build network
    network.builder$update.network.conf(list(artifact.relation = "issue", author.relation = "cochange"))
    network.built = network.builder$get.multi.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete.vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – issue/cochange"
    )

})

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Extract data sources ----------------------------------------------------
test_that("Get the data sources from a network with two relations", {
    expected.relations = c("mails", "commits")
    network = get.sample.network()

    expect_identical(expected.relations, get.data.sources.from.relations(network), info = "data sources: mails, commits")
})

test_that("Get the data sources from a network with one relation", {
    expected.relations = c("mails")

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    ## construct data object
    proj.data = ProjectData$new(project.conf = proj.conf)

    ## construct network builder
    net.conf = NetworkConf$new()
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.builder$update.network.conf(updated.values = list(author.relation = "mail"))

    ## build network
    network = network.builder$get.author.network()

    expect_identical(expected.relations, get.data.sources.from.relations(network), info = "data sources: mails")
})
