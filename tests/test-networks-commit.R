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
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>

## All Rights Reserved.


context("Network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


##
## Tests for author.all.authors and author.only.committers
##



patrick::with_parameters_test_that("Network construction with commit-interactions as relation", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
    proj.conf$update.value("commit.interactions", TRUE)
    proj.conf$update.value("commit.interactions.filter.global", FALSE)
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(commit.relation = "commit.interaction",
                                                 commit.directed = test.directed))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.commit.network()
    ## build the expected network
    vertices = data.frame(
        name = c("3a0ed78458b3976243db6829f63eba3eead26774",
                 "0a1a5c523d835459c42f33e863623138555e2526",
                 "1143db502761379c2bfcecc2007fc34282e7ee61",
                 "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "d01921773fae4bed8186b0aa411d6a2f7a6626e6"),
        kind = TYPE.COMMIT,
        type = TYPE.COMMIT
        )
    edges = data.frame(
        base.hash = c("3a0ed78458b3976243db6829f63eba3eead26774",
                      "0a1a5c523d835459c42f33e863623138555e2526",
                      "1143db502761379c2bfcecc2007fc34282e7ee61",
                      "0a1a5c523d835459c42f33e863623138555e2526"),
        hash = c("0a1a5c523d835459c42f33e863623138555e2526",
                 "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "d01921773fae4bed8186b0aa411d6a2f7a6626e6"),
        func = c("GLOBAL", "test2.c::test2", "GLOBAL", "test2.c::test2"),
        interacting.author = c("Thomas", "Karl", "Olaf", "Thomas"),
        file = c("GLOBAL", "test2.c", "GLOBAL", "test2.c"),
        base.author = c("Olaf", "Thomas", "Karl", "Thomas"),
        base.func = c("test2.c::test2", "test2.c::test2",
                      "test3.c::test_function", "test2.c::test2"),
        base.file = c("test2.c", "test2.c", "test3.c", "test2.c"),
        artifact.type = c("CommitInteraction", "CommitInteraction", "CommitInteraction", "CommitInteraction"),
        weight = c(1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("commit.interaction", "commit.interaction", "commit.interaction", "commit.interaction")
        )
    network = igraph::graph.data.frame(edges, directed = test.directed, vertices = vertices)
    expect_true(igraph::identical_graphs(network.built, network))

    network.new.attr = add.vertex.attribute.commit.network(network.built, proj.data,  "deleted.lines", "NO_DATA")
    expect_identical(igraph::V(network.new.attr)$deleted.lines, c("0", "0","0", "NO_DATA", "0", "NO_DATA"))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

patrick::with_parameters_test_that("Network construction with cochange as relation, file as artifact", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(commit.relation = "cochange",
                                                 commit.directed = test.directed))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.commit.network()
    ## build the expected network
    vertices = data.frame(
        name = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "3a0ed78458b3976243db6829f63eba3eead26774",
                 "0a1a5c523d835459c42f33e863623138555e2526",
                 "1143db502761379c2bfcecc2007fc34282e7ee61"),
        date = get.date.from.string(c("2016-07-12 15:58:59",
                                      "2016-07-12 16:00:45",
                                      "2016-07-12 16:05:41",
                                      "2016-07-12 16:06:32",
                                      "2016-07-12 16:06:10")),
        kind = TYPE.COMMIT,
        type = TYPE.COMMIT
        )
    edges = data.frame(
        from = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "3a0ed78458b3976243db6829f63eba3eead26774"),
        to = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:32")),
        artifact.type = c("File", "File"),
        artifact = c("test.c", "test2.c"),
        weight = c(1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("cochange", "cochange")
        )

    if (test.directed) {
        edges <- edges[, c(2, 1, 3, 4, 5, 6, 7, 8), ]
    }
    network = igraph::graph.data.frame(edges, directed = test.directed, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

patrick::with_parameters_test_that("Network construction with cochange as relation, function as artifact", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "function")
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(commit.relation = "cochange",
                                                 commit.directed = test.directed))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.commit.network()
    ## build the expected network
    vertices = data.frame(
        name = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "3a0ed78458b3976243db6829f63eba3eead26774",
                 "0a1a5c523d835459c42f33e863623138555e2526",
                 "1143db502761379c2bfcecc2007fc34282e7ee61"),
        date = get.date.from.string(c("2016-07-12 15:58:59",
                                      "2016-07-12 16:00:45",
                                      "2016-07-12 16:05:41",
                                      "2016-07-12 16:06:32",
                                      "2016-07-12 16:06:10")),
        kind = TYPE.COMMIT,
        type = TYPE.COMMIT
        )
    edges = data.frame(
        from = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338", "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774"),
        to = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
               "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:05:41", "2016-07-12 16:05:41",
                                      "2016-07-12 16:06:32", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
        artifact.type = c("Function", "Function", "Function", "Function", "Function", "Function"),
        artifact = c("File_Level", "File_Level", "File_Level", "File_Level", "File_Level", "File_Level"),
        weight = c(1, 1, 1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA,
                 TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("cochange", "cochange", "cochange", "cochange", "cochange", "cochange")
        )

    if (test.directed) {
        edges <- edges[, c(2, 1, 3, 4, 5, 6, 7, 8), ]
    }
    network = igraph::graph.data.frame(edges, directed = test.directed, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

patrick::with_parameters_test_that("Network construction with cochange as relation, feature as artifact", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "feature")
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(commit.relation = "cochange",
                                                 commit.directed = test.directed))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.commit.network()
    ## build the expected network
    vertices = data.frame(
        name = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "3a0ed78458b3976243db6829f63eba3eead26774",
                 "1143db502761379c2bfcecc2007fc34282e7ee61",
                 "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 15:58:59",
                                      "2016-07-12 16:00:45",
                                      "2016-07-12 16:05:41",
                                      "2016-07-12 16:06:10",
                                      "2016-07-12 16:06:32")),
        kind = TYPE.COMMIT,
        type = TYPE.COMMIT
        )
    edges = data.frame(
        from = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "3a0ed78458b3976243db6829f63eba3eead26774",
                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61"),
        to = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "1143db502761379c2bfcecc2007fc34282e7ee61",
               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
        artifact.type = c("Feature", "Feature", "Feature", "Feature"),
        artifact = c("A", "Base_Feature", "Base_Feature", "Base_Feature"),
        weight = c(1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("cochange", "cochange", "cochange", "cochange")
        )

    if (test.directed) {
        edges <- edges[, c(2, 1, 3, 4, 5, 6, 7, 8), ]
    }
    network = igraph::graph.data.frame(edges, directed = test.directed, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

test_that("Adding vertex attributes to a commit network", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "feature")
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(commit.relation = "cochange",
                                                 commit.directed = FALSE))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.commit.network()
    network.new.attr = add.vertex.attribute.commit.network(network.built, proj.data,  "author.name", "NO_AUTHOR")
    ## build the expected network
    vertices = data.frame(
        name = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "3a0ed78458b3976243db6829f63eba3eead26774",
                 "1143db502761379c2bfcecc2007fc34282e7ee61",
                 "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 15:58:59",
                                      "2016-07-12 16:00:45",
                                      "2016-07-12 16:05:41",
                                      "2016-07-12 16:06:10",
                                      "2016-07-12 16:06:32")),
        kind = TYPE.COMMIT,
        type = TYPE.COMMIT,
        author.name = c("Björn",
                        "Olaf",
                        "Olaf",
                        "Karl",
                        "Thomas")
        )
    edges = data.frame(
        from = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "3a0ed78458b3976243db6829f63eba3eead26774",
                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61"),
        to = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "1143db502761379c2bfcecc2007fc34282e7ee61",
               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
        artifact.type = c("Feature", "Feature", "Feature", "Feature"),
        artifact = c("A", "Base_Feature", "Base_Feature", "Base_Feature"),
        weight = c(1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("cochange", "cochange", "cochange", "cochange")
        )

    network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    expect_true(igraph::identical_graphs(network.new.attr, network))

    network.new.attr = add.vertex.attribute.commit.network(network.new.attr, proj.data,  "commit.id", "NO_ID")

    ## build the expected network
    vertices = data.frame(
        name = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "3a0ed78458b3976243db6829f63eba3eead26774",
                 "1143db502761379c2bfcecc2007fc34282e7ee61",
                 "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 15:58:59",
                                      "2016-07-12 16:00:45",
                                      "2016-07-12 16:05:41",
                                      "2016-07-12 16:06:10",
                                      "2016-07-12 16:06:32")),
        kind = TYPE.COMMIT,
        type = TYPE.COMMIT,
        author.name = c("Björn",
                        "Olaf",
                        "Olaf",
                        "Karl",
                        "Thomas"),
        commit.id = c("<commit-32712>", "<commit-32713>",
                      "<commit-32710>", "<commit-32714>", "<commit-32711>")
        )
    edges = data.frame(
        from = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "3a0ed78458b3976243db6829f63eba3eead26774",
                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61"),
        to = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "1143db502761379c2bfcecc2007fc34282e7ee61",
               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
        date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
        artifact.type = c("Feature", "Feature", "Feature", "Feature"),
        artifact = c("A", "Base_Feature", "Base_Feature", "Base_Feature"),
        weight = c(1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("cochange", "cochange", "cochange", "cochange")
        )

    network.two = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    expect_true(igraph::identical_graphs(network.new.attr, network.two))
})