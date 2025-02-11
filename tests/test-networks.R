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
## Copyright 2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.


context("Basic network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
CASESTUDY_EMPTY = "test_empty"
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
    expect_true(igraph::are_adjacent(simplify.network(g), "A", "Base_Feature")) # specific edge

})

test_that("Simplify basic multi-relational network", {

    ##
    ## Simplify networks with vertices connected by multi-relational edges
    ##

    ## create artifact network with vertices connected by "cochange" and "mail" edges
    network =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", type = TYPE.ARTIFACT, kind = "feature")
    for (i in 1:3) {
        network = igraph::add_edges(network, c("A", "B"), type = TYPE.EDGES.INTRA, relation = list("mail"))
        network = igraph::add_edges(network, c("A", "B"), type = TYPE.EDGES.INTRA, relation = list("cochange"))
    }

    network.expected = igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", type = TYPE.ARTIFACT, kind = "feature") +
        igraph::edges("A", "B", "A", "B", type = TYPE.EDGES.INTRA, relation = list(as.list(rep("mail", 3)),
                                                                                   as.list(rep("cochange", 3))))

    ## simplify network without simplifying multiple relations into single edges
    network.simplified = simplify.network(network, simplify.multiple.relations = FALSE)
    assert.networks.equal(network.simplified, network.expected)

    network.expected = igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", type = TYPE.ARTIFACT, kind = "feature") +
        igraph::edges("A", "B", type = TYPE.EDGES.INTRA, relation = list(list("mail", "cochange",
                                                                              "mail", "cochange",
                                                                              "mail", "cochange")))

    ## simplify network with simplifying multiple relations into single edges
    network.simplified = simplify.network(network, simplify.multiple.relations = TRUE)
    assert.networks.equal(network.simplified, network.expected)
})

test_that("Simplify author-network with relation = c('cochange', 'mail') using both algorithms", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = c("cochange", "mail"), simplify = TRUE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## vertex attributes
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas", "udo", "Fritz fritz@example.org", "georg", "Hans"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)


    ## ---------------------- simplify.multiple.relations == FALSE -------------------------- ##

    ## edge attributes
    data = data.frame(comb.1. = c("Björn", "Olaf", "Olaf", "Karl", # cochange
                                  "Björn", "Olaf"), # mail
                      comb.2. = c("Olaf", "Karl", "Thomas", "Thomas", # cochange
                                  "Olaf", "Thomas")) # mail
    data$date = list(as.list(get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45"))),
                     as.list(get.date.from.string(c("2016-07-12 16:05:41", "2016-07-12 16:06:10"))),
                     as.list(get.date.from.string(c("2016-07-12 16:05:41", "2016-07-12 16:06:32"))),
                     as.list(get.date.from.string(c("2016-07-12 16:06:10", "2016-07-12 16:06:32"))), # cochange
                     as.list(get.date.from.string(c("2016-07-12 15:58:40", "2016-07-12 15:58:50"))),
                     as.list(get.date.from.string(c("2016-07-12 16:04:40", "2016-07-12 16:05:37")))) # mail
    data$artifact.type = list(list("Feature", "Feature"), list("Feature", "Feature"),
                              list("Feature", "Feature"), list("Feature", "Feature"), # cochange
                              list("Mail", "Mail"), list("Mail", "Mail")) # mail
    data$hash = list(list("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                     list("3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61"),
                     list("3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                     list("1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"),
                     as.list(rep(NA, 2)), as.list(rep(NA, 2)))
    data$file = list(list("test.c", "test.c"), list("test2.c", "test3.c"), list("test2.c", "test2.c"), list("test3.c", "test2.c"),
                     as.list(rep(NA, 2)), as.list(rep(NA, 2)))
    data$artifact = list(list("A", "A"), list("Base_Feature", "Base_Feature"), list("Base_Feature", "Base_Feature"),
                         list("Base_Feature", "Base_Feature"), as.list(rep(NA, 2)), as.list(rep(NA, 2)))
    data$weight = rep(2, 6)
    data$type = rep(TYPE.EDGES.INTRA, 6)
    data$relation = list(list("cochange", "cochange"), list("cochange", "cochange"), list("cochange", "cochange"),
                       list("cochange", "cochange"), list("mail", "mail"), list("mail", "mail"))
    data$message.id = list(as.list(rep(NA, 2)), as.list(rep(NA, 2)), as.list(rep(NA, 2)), as.list(rep(NA, 2)),
                           list("<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>"),
                           list("<65a1sf31sagd684dfv31@mail.gmail.com>",
                                "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"))
    data$thread = list(as.list(rep(NA, 2)), as.list(rep(NA, 2)), as.list(rep(NA, 2)), as.list(rep(NA, 2)),
                       list("<thread-13#8>", "<thread-13#8>"), list("<thread-13#9>", "<thread-13#9>"))

    ## build expected network
    network.expected = igraph::graph_from_data_frame(data, vertices = authors,
                                                     directed = net.conf$get.value("author.directed"))

    ## build simplified network
    network.built = network.builder$get.author.network()

    assert.networks.equal(network.built, network.expected)


    ## ---------------------- simplify.multiple.relations == TRUE --------------------------- ##

    data = data.frame(comb.1. = c("Björn", "Olaf", "Olaf", "Karl"),
                      comb.2. = c("Olaf", "Karl", "Thomas", "Thomas"))

    data$date = list(as.list(get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",    # cochange
                                                    "2016-07-12 15:58:40", "2016-07-12 15:58:50"))), # mail
                     as.list(get.date.from.string(c("2016-07-12 16:05:41", "2016-07-12 16:06:10"))), # cochange
                     as.list(get.date.from.string(c("2016-07-12 16:05:41", "2016-07-12 16:06:32",    # cochange
                                                    "2016-07-12 16:04:40", "2016-07-12 16:05:37"))), # mail
                     as.list(get.date.from.string(c("2016-07-12 16:06:10", "2016-07-12 16:06:32")))) # cochange
    data$artifact.type = list(list("Feature", "Feature", "Mail", "Mail"),
                              list("Feature", "Feature"),
                              list("Feature", "Feature", "Mail", "Mail"),
                              list("Feature", "Feature"))

    data$hash = list(list("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338", NA, NA),
                     list("3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61"),
                     list("3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526", NA, NA),
                     list("1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"))
    data$file = list(list("test.c", "test.c", NA, NA), list("test2.c", "test3.c"),
                     list("test2.c", "test2.c", NA, NA), list("test3.c", "test2.c"))
    data$artifact = list(list("A", "A", NA, NA), list("Base_Feature", "Base_Feature"),
                         list("Base_Feature", "Base_Feature", NA, NA), list("Base_Feature", "Base_Feature"))
    data$weight = c(4, 2, 4, 2)
    data$type = rep(TYPE.EDGES.INTRA, 4)
    data$relation = list(list("cochange", "cochange", "mail", "mail"), list("cochange", "cochange"),
                         list("cochange", "cochange", "mail", "mail"), list("cochange", "cochange"))
    data$message.id = list(list(NA, NA, "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                        "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>"),
                           list(NA, NA),
                           list(NA, NA, "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                        "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"),
                           list(NA, NA))
    data$thread = list(list(NA, NA, "<thread-13#8>", "<thread-13#8>"),
                       list(NA, NA),
                       list(NA, NA, "<thread-13#9>", "<thread-13#9>"),
                       list(NA, NA))

    ## build expected network
    network.expected = igraph::graph_from_data_frame(data, vertices = authors,
                                                directed = net.conf$get.value("author.directed"))

    ## build simplified network
    network.builder$update.network.conf(updated.values = list(simplify.multiple.relations = TRUE))
    network.built = network.builder$get.author.network()

    assert.networks.equal(network.built, network.expected)

})

test_that("Simplify multiple basic multi-relational networks", {

    ##
    ## Simplify networks with vertices connected by multi-relational edges
    ##

    ## create artifact network with vertices connected by "cochange" and "mail edges"
    network.A =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", type = TYPE.ARTIFACT, kind = "feature")
    network.B =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("C", "D", type = TYPE.AUTHOR, kind = TYPE.AUTHOR)
    for (i in 1:3) {
        network.A = igraph::add_edges(network.A, c("A", "B"), type = TYPE.EDGES.INTRA, relation = list("mail"))
        network.A = igraph::add_edges(network.A, c("A", "B"), type = TYPE.EDGES.INTRA, relation = list("cochange"))
        network.B = igraph::add_edges(network.B, c("C", "D"), type = TYPE.EDGES.INTRA, relation = list("mail"))
        network.B = igraph::add_edges(network.B, c("C", "D"), type = TYPE.EDGES.INTRA, relation = list("cochange"))
    }

    ## add graph attributes
    network.A = igraph::set_graph_attr(network.A, "name", "network.A")
    network.B = igraph::set_graph_attr(network.B, "name", "network.B")
    networks = list(A = network.A, B = network.B)

    network.A.expected = igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", type = TYPE.ARTIFACT, kind = "feature") +
        igraph::edges("A", "B", "A", "B", type = TYPE.EDGES.INTRA)
    network.B.expected = igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("C", "D", type = TYPE.AUTHOR, kind = TYPE.AUTHOR) +
        igraph::edges("C", "D", "C", "D", type = TYPE.EDGES.INTRA)
    network.A.expected = igraph::set_edge_attr(network.A.expected, "relation", value = list(list("mail", "mail", "mail"),
                                                                                            list("cochange", "cochange", "cochange")))
    network.B.expected = igraph::set_edge_attr(network.B.expected, "relation", value = list(list("mail", "mail", "mail"),
                                                                                            list("cochange", "cochange", "cochange")))

    ## simplify networks without simplifying multiple relations into single edges
    networks.simplified = simplify.networks(networks, simplify.multiple.relations = FALSE)
    expect_true(length(networks.simplified) == 2)
    expect_identical(names(networks.simplified), names(networks))
    assert.networks.equal(networks.simplified[["A"]], network.A.expected)
    assert.networks.equal(networks.simplified[["B"]], network.B.expected)

    ## simplify networks with simplifying multiple relations into single edges
    networks.simplified = simplify.networks(networks, simplify.multiple.relations = TRUE)
    expect_true(length(networks.simplified) == 2)
    expect_identical(names(networks.simplified), names(networks))
    for (i in 1:2) {
        expect_identical(igraph::ecount(networks.simplified[[i]]), 1)
        expect_identical(igraph::E(networks.simplified[[i]])$type[[1]], "Unipartite")
        expect_identical(igraph::E(networks.simplified[[i]])$relation[[1]], list("mail", "cochange", "mail", "cochange", "mail", "cochange"))
    }

    ## verify graph attributes
    expect_identical(igraph::graph_attr(networks.simplified[["A"]], "name"), "network.A")
    expect_identical(igraph::graph_attr(networks.simplified[["B"]], "name"), "network.B")
})

test_that("Simplify network with multi-relational edges", {

    ## Note: Base network
    ## A -- (cochange)             --> D
    ## B -- (cochange)             --> E
    ## B -- (mail)                 --> E
    ## C -- (mail)                 --> F
    ## B -- (mail, cochange)       --> E
    ## B -- (mail, cochange, mail) --> E
    ## B -- (mail, mail)           --> E
    ## C -- (mail, mail)           --> F

    ## create network with vertices connected by multi-relational edges
    data = data.frame(comb.1. = c("A", "B", "B", "C", "B", "B", "B", "C"),
                      comb.2. = c("D", "E", "E", "F", "E", "E", "E", "F"))
    data$relation = list("cochange", "cochange",
                         "mail", "mail",
                         list("mail", "cochange"), list("mail", "cochange", "mail"),
                         list("mail", "mail"), list("mail", "mail"))

    ## build expected network
    network.built = igraph::graph_from_data_frame(data, vertices = c("A", "B", "C", "D", "E", "F"),
                                                  directed = FALSE)

    ## ---------------------- simplify.multiple.relations == FALSE -------------------------- ##

    network.built = simplify.network(network.built, simplify.multiple.relations = FALSE)

    ## Note: (mail) can be simplified with (mail, mail).
    ##       (mail) and (mail, mail) cannot be simplified with (mail, cochange).
    ##       (mail, cochange) can be simplified with (mail, cochange, mail).
    ## A -- (cochange)                             --> D
    ## B -- (cochange)                             --> E
    ## B -- (mail, mail, mail)                     --> E
    ## C -- (mail, mail, mail)                     --> F
    ## B -- (mail, cochange, mail, cochange, mail) --> E

    data = data.frame(comb.1. = c("A", "B", "B", "C", "B"),
                      comb.2. = c("D", "E", "E", "F", "E"))
    data$relation = list("cochange", "cochange",
                         list("mail", "mail", "mail"), list("mail", "mail", "mail"),
                         list("mail", "cochange", "mail", "cochange", "mail"))
    network.expected = igraph::graph_from_data_frame(data, vertices = c("A", "B", "C", "D", "E", "F"),
                                                     directed = FALSE)

    assert.networks.equal(network.built, network.expected)

})

test_that("Remove isolated vertices", {

    ## construct network
    edges = c("A", "A", "D", "C", "E", "C")
    network =
        igraph::make_empty_graph(n = 0, directed = TRUE) +
        igraph::vertices("A", "B", "C", "D", "E", "F") +
        igraph::edges(edges, relation = "cochange")

    ## remove isolate vertices
    network = delete.isolates(network)

    ## check correctness
    expect_identical(igraph::vertex_attr(network, "name"), c("A", "C", "D", "E"))

})

test_that("Remove isolated authors given a specific edge type", {

    ## construct network
    edges_inter = c("A", "A", "D", "C", "E", "C")
    edges_intra = c("F", "D", "A", "E", "D", "B")
    network =
        igraph::make_empty_graph(n = 0, directed = TRUE) +
        igraph::vertices("A", "B", "C", "D", "E", "F", type = TYPE.AUTHOR) +
        igraph::edges(edges_inter, relation = "cochange", type = TYPE.EDGES.INTER) +
        igraph::edges(edges_intra, relation = "cochange", type = TYPE.EDGES.INTRA)

    ## remove isolate vertices
    network.without.isolates.inter = delete.authors.without.specific.edges(network, specific.edge.type = TYPE.EDGES.INTER)
    network.without.isolates.intra = delete.authors.without.specific.edges(network, specific.edge.type = TYPE.EDGES.INTRA)

    ## check correctness
    expect_identical(igraph::vertex_attr(network.without.isolates.inter, "name"), c("A", "C", "D", "E"))
    expect_identical(igraph::vertex_attr(network.without.isolates.intra, "name"), c("A", "B", "D", "E", "F"))

})

test_that("Remove duplicate edges", {

    ##
    ## Remove duplicate edges from a network
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    net.conf = NetworkConf$new()
    net.conf$update.values(list("author.respect.temporal.order" = TRUE, author.directed = FALSE))
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.builder$update.network.conf(updated.values = list(author.relation = "mail"))

    ## construct data for expected network
    edges = data.frame(comb.1. = c("Björn", "Olaf", rep("Hans", 5)),
                       comb.2. = c("Olaf", "Thomas", rep("Hans", 5)),
                       date = get.date.from.string(c("2016-07-12 15:58:50", "2016-07-12 16:05:37",
                                                     "2010-07-12 12:05:41", "2010-07-12 12:05:42",
                                                     "2010-07-12 12:05:43", "2010-07-12 12:05:44",
                                                     "2010-07-12 12:05:45")),
                       artifact.type = rep("Mail", 7),
                       message.id = c("<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                      "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>",
                                      "<hans2@mail.gmail.com>", "<hans3@mail.gmail.com>",
                                      "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>",
                                      "<hans6@mail.gmail.com>"),
                       thread = c("<thread-13#8>", "<thread-13#9>", rep("<thread-42#6>", 5)),
                       weight = rep(1, 7),
                       type = rep(TYPE.EDGES.INTRA, 7),
                       relation = rep("mail", 7))
    vertices = data.frame(name = c("Björn", "udo", "Olaf", "Thomas", "Fritz fritz@example.org", "georg", "Hans"),
                          kind = rep("Author", 7),
                          type = rep("Author", 7))

    ## build expected network
    network.expected = igraph::graph_from_data_frame(edges, directed = FALSE, vertices = vertices)
    network.expected = convert.edge.attributes.to.list(network.expected)

    ## build network with unique edges
    network = network.builder$get.author.network()
    network.built = remove.duplicate.edges(network)

    assert.networks.equal(network.expected, network.built)

    ##
    ## Attempt to remove non-existent duplicate edges should not change anything
    ##

    assert.networks.equal(network.built, remove.duplicate.edges(network.built))

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
    author.net.expected = igraph::delete_vertices(base.net, igraph::V(base.net)[7:12])

    expect_true(igraph::identical_graphs(author.net.built, author.net.expected), info = "author-network extraction")

    ##
    ## extraction of bipartite networks
    ##

    ## construct original bipartite network
    bip.net.built = extract.bipartite.network.from.network(base.net, remove.isolates = TRUE)

    ## construct expected bipartite network (by removing unipartite edges and isolate vertices)
    bip.net.expected = igraph::delete_edges(base.net, igraph::E(base.net)[1:9])
    bip.net.expected = igraph::delete_vertices(bip.net.expected, "A2")

    expect_true(igraph::identical_graphs(bip.net.built, bip.net.expected), info = "bipartite-network extraction")

    ##
    ## extraction of artifact networks
    ##

    ## construct original artifact network
    art.net.built = extract.artifact.network.from.network(base.net, remove.isolates = FALSE)

    ## construct expected artifact network (by removing author vertices and adjacent edges)
    art.net.expected = igraph::delete_vertices(base.net, igraph::V(base.net)[1:6])

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
        extract.bipartite.network.from.network(edgeless.net, remove.isolates = TRUE),
        base.net - igraph::vertices(seq_len(igraph::vcount(base.net)))
    ), info = "extracted bipartite network is empty for edgeless base network")
})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Construction of networks without data -----------------------------------

test_that("Construction of networks without data", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY_EMPTY, ARTIFACT)
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
    net.edgeless = igraph::set_edge_attr(net.edgeless, "weight", value = 1)

    ##
    ## normal network
    ##

    net.constructed = construct.network.from.edge.list(vertices, edge.list, net.conf)
    net.expected = igraph::make_empty_graph(n = 0, directed = directed) +
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
    net.edgeless = igraph::set_vertex_attr(net.edgeless, "name", value = "name")
    net.edgeless = igraph::set_edge_attr(net.edgeless, "weight", value = 1)

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
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY_EMPTY, ARTIFACT)
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
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "mail"))
    network.built = network.builder$get.author.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "author network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(author.relation = "issue"))
    network.built = network.builder$get.author.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
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
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "mail"))
    network.built = network.builder$get.bipartite.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "bipartite network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "issue"))
    network.built = network.builder$get.bipartite.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
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
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – cochange")

    ## mail-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "mail"))
    network.built = network.builder$get.artifact.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_true(igraph::identical_graphs(network.built, network.expected), info = "artifact network – mail")

    ## issue-based network:
    ## 1) build network
    network.builder$update.network.conf(updated.values = list(artifact.relation = "issue"))
    network.built = network.builder$get.artifact.network()
    ## 2) remove all vertices since we only care about the attributes
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
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
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
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
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
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
    network.built = igraph::delete_vertices(network.built, seq_len(igraph::vcount(network.built)))
    ## 3) check attributes against expected network
    expect_identical(
        igraph::as_data_frame(network.built, what = "both"),
        igraph::as_data_frame(network.expected, what = "both"),
        info = "multi network – issue/cochange"
    )

})


patrick::with_parameters_test_that("Convert edge attributes to list", {

    ## configure edge attributes
    edge.attributes = c("date", "message.id", "thread", "weight", "type", "relation")
    attribute.defaults = list(get.date.from.string("2020-01-01 00:00:00"), "abc", "def", 1, TYPE.EDGES.INTRA, "mail")

    ## construct network
    network =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", "C", type = TYPE.AUTHOR, kind = TYPE.AUTHOR) +
        igraph::edges("A", "B", "B", "C", "C", "A")

    ## assign edge attributes
    for (i in seq_along(edge.attributes)) {
        network = igraph::set_edge_attr(network, edge.attributes[i], value = attribute.defaults[[i]])
    }

    ## convert specified edge attributes to list
    if (is.null(remain.as.is)) {
        network.listified = convert.edge.attributes.to.list(network)

        ## set 'remain.as.is' to the default of 'convert.edge.attributes.to.list'
        ## for later use in the validation process
        remain.as.is = names(EDGE.ATTR.HANDLING)
    } else {
        network.listified = convert.edge.attributes.to.list(network, remain.as.is = remain.as.is)
    }

    ## check edge attributes
    for (attr in igraph::edge_attr_names(network)) {
        conversion.function = ifelse(attr %in% remain.as.is, identity, as.list)
        expect_equal(
            conversion.function(igraph::edge_attr(network, attr)),
            igraph::edge_attr(network.listified, attr),
            info = paste("edge attribute", attr, "values")
        )
    }

}, patrick::cases(
    "remain.as.is: weight" = list(remain.as.is = c("weight")),
    "remain.as.is: date" = list(remain.as.is = c("date")),
    "remain.as.is: weight, date" = list(remain.as.is = c("weight", "date")),
    "remain.as.is: default" = list(remain.as.is = NULL)
))



## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Extract data sources ----------------------------------------------------
test_that("Get the data sources from a network with two relations", {
    expected.data.sources = c("mails", "commits")
    network = get.sample.network()

    expect_identical(expected.data.sources, get.data.sources.from.relations(network), info = "data sources: mails, commits")
})

test_that("Get the data sources from a network with one relation", {
    expected.data.sources = c("mails")

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

    expect_identical(expected.data.sources, get.data.sources.from.relations(network), info = "data sources: mails")
})

test_that("Get the data sources from a network with multiple relations on a single edge", {
    expected.data.sources = c("commits", "mails")

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    ## construct data object
    proj.data = ProjectData$new(project.conf = proj.conf)

    ## construct network builder
    net.conf = NetworkConf$new()
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.builder$update.network.conf(updated.values = list(author.relation = c("mail", "cochange")))

    ## build network
    network = network.builder$get.author.network()
    network = simplify.network(network, simplify.multiple.relations = TRUE)

    expect_identical(expected.data.sources, get.data.sources.from.relations(network), info = "data sources: commits, mails")
})
