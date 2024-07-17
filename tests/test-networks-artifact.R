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
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2023-2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.


context("Network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


test_that("Network construction of the undirected artifact-cochange network", {

    ## build expected network:
    ## 1) vertices
    vertices = data.frame(name = c("Base_Feature", "foo", "A"),
                          kind = "Feature",
                          type = TYPE.ARTIFACT)
    ## 2) edges
    edges = data.frame(
        from = "Base_Feature",
        to = "foo",
        date = get.date.from.string("2016-07-12 16:06:32"),
        artifact.type = "Feature",
        hash = "0a1a5c523d835459c42f33e863623138555e2526",
        file = "test2.c",
        author.name = "Thomas",
        weight = 1,
        type = TYPE.EDGES.INTRA,
        relation = "cochange"
    )
    ## 3) build expected network
    network.expected = igraph::graph_from_data_frame(edges, directed = FALSE, vertices = vertices)


    ##
    ## without untracked files
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", TRUE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.artifact.network()

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))


    ##
    ## with untracked files
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.artifact.network()

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))
})

patrick::with_parameters_test_that("Network construction of an issue-based artifact-network", {
    ## build expected network:
    ## 1) vertices
    vertices = data.frame(name = c("<issue-jira-ZEPPELIN-328>",
                                   "<issue-github-2>",
                                   "<issue-github-6>",
                                   "<issue-github-3>",
                                   "<issue-github-1>",
                                   "<issue-jira-ZEPPELIN-332>" ,
                                   "<issue-github-4>"),
                          kind = "Issue",
                          type = TYPE.ARTIFACT)
    ## 2) edges
    edges = data.frame(
        from = c("<issue-github-3>", "<issue-github-3>", "<issue-jira-ZEPPELIN-328>"),
        to = c("<issue-github-2>", "<issue-github-6>", "<issue-jira-ZEPPELIN-332>"),
        date = get.date.from.string(c("2016-08-07 15:30:00", "2016-08-07 15:37:02", "2017-05-21 12:00:00")),
        artifact.type = c("IssueEvent", "IssueEvent", "IssueEvent"),
        issue.id = c("<issue-github-3>", "<issue-github-3>", "<issue-jira-ZEPPELIN-328>"),
        event.name = c("add_link", "add_link", "add_link"),
        author.name = c("Thomas", "Karl", "Thomas"),
        weight = c(1, 1, 1),
        type = TYPE.EDGES.INTRA,
        relation = "issue"
    )

    ## 3) when constructing directed networks, we cannot deduplicate jira edges
    if (test.directed) {
        edges = rbind(edges, data.frame(
            from = "<issue-jira-ZEPPELIN-332>",
            to = "<issue-jira-ZEPPELIN-328>",
            date = get.date.from.string("2017-05-21 12:00:00"),
            artifact.type = "IssueEvent",
            issue.id = "<issue-jira-ZEPPELIN-332>",
            event.name = "add_link",
            author.name = "Thomas",
            weight = 1,
            type = TYPE.EDGES.INTRA,
            relation = "issue"
        ))
    }

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.from.source", c("jira", "github"))
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "issue", artifact.directed = test.directed))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build expected network
    network.expected = igraph::graph_from_data_frame(edges, directed = test.directed, vertices = vertices)

    ## build network
    network.built = network.builder$get.artifact.network()

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

patrick::with_parameters_test_that("Network construction of an empty 'comments-only' issue-based artifact-network", {

    ##
    ## 'issues.only.comments' (by default), this should not create any edges
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.from.source", c("jira", "github"))
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "issue", artifact.directed = test.directed))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.artifact.network()

    ## 1) vertices
    vertices = data.frame(name = c("<issue-jira-ZEPPELIN-328>",
                                   "<issue-github-2>",
                                   "<issue-github-1>",
                                   "<issue-github-3>",
                                   "<issue-github-4>",
                                   "<issue-jira-ZEPPELIN-332>",
                                   "<issue-github-6>"),
                          kind = "Issue",
                          type = TYPE.ARTIFACT)
    ## 2) edges
    edges = data.frame(
        from = character(), to = character(), date = get.date.from.string(character(0)), artifact.type = character(),
        issue.id = character(), event.name = character(), weight = numeric(), type = character(),
        relation = character()
    )

    ## build expected network
    network.expected = igraph::graph_from_data_frame(edges, directed = test.directed, vertices = vertices)

    ## test
    assert.networks.equal(network.built, network.expected)
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

patrick::with_parameters_test_that("Network construction with commit-interactions as relation, artifact type 'file'", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
    proj.conf$update.value("commit.interactions", TRUE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commit.interactions.filter.global", FALSE)
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "commit.interaction",
                                                 artifact.directed = test.directed))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.artifact.network()
    ## build the expected nbetwork
    vertices = data.frame(
        name = c("test2.c", "test3.c", "GLOBAL"),
        kind = "File",
        type = TYPE.ARTIFACT
        )
    edges = data.frame(
        from = c("GLOBAL", "test2.c", "GLOBAL", "test2.c"),
        to = c("test2.c", "test2.c", "test3.c", "test2.c"),
        func = c("GLOBAL", "test2.c::test2", "GLOBAL", "test2.c::test2"),
        hash = c("0a1a5c523d835459c42f33e863623138555e2526",
                 "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "d01921773fae4bed8186b0aa411d6a2f7a6626e6"),
        base.hash = c("3a0ed78458b3976243db6829f63eba3eead26774",
                      "0a1a5c523d835459c42f33e863623138555e2526",
                      "1143db502761379c2bfcecc2007fc34282e7ee61",
                      "0a1a5c523d835459c42f33e863623138555e2526"),
        base.func = c("test2.c::test2", "test2.c::test2",
                      "test3.c::test_function", "test2.c::test2"),
        base.author = c("Olaf", "Thomas", "Karl", "Thomas"),
        interacting.author = c("Thomas", "Karl", "Olaf", "Thomas"),
        artifact.type = c("File", "File", "File", "File"),
        weight = c(1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("commit.interaction", "commit.interaction", "commit.interaction", "commit.interaction")
        )
    network = igraph::graph_from_data_frame(edges, directed = test.directed, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))

patrick::with_parameters_test_that("Network construction with commit-interactions as relation, artifact type 'function'", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "function")
    proj.conf$update.value("commit.interactions", TRUE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commit.interactions.filter.global", FALSE)
    proj.data = ProjectData$new(project.conf = proj.conf)

    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "commit.interaction",
                                                 artifact.directed = test.directed))

    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.artifact.network()
    ## build the expected network
    vertices = data.frame(
        name = c("test2.c::test2", "test3.c::test_function", "GLOBAL"),
        kind = "Function",
        type = TYPE.ARTIFACT
        )
    edges = data.frame(
        from = c("GLOBAL", "test2.c::test2", "GLOBAL", "test2.c::test2"),
        to = c("test2.c::test2", "test2.c::test2",
               "test3.c::test_function", "test2.c::test2"),
        hash = c("0a1a5c523d835459c42f33e863623138555e2526",
                 "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                 "d01921773fae4bed8186b0aa411d6a2f7a6626e6"),
        file = c("GLOBAL", "test2.c", "GLOBAL", "test2.c"),
        base.hash = c("3a0ed78458b3976243db6829f63eba3eead26774",
                      "0a1a5c523d835459c42f33e863623138555e2526",
                      "1143db502761379c2bfcecc2007fc34282e7ee61",
                      "0a1a5c523d835459c42f33e863623138555e2526"),
        base.file = c("test2.c", "test2.c", "test3.c", "test2.c"),
        base.author = c("Olaf", "Thomas", "Karl", "Thomas"),
        interacting.author = c("Thomas", "Karl", "Olaf", "Thomas"),
        artifact.type = c("Function", "Function", "Function", "Function"),
        weight = c(1, 1, 1, 1),
        type = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA, TYPE.EDGES.INTRA),
        relation = c("commit.interaction", "commit.interaction", "commit.interaction", "commit.interaction")
        )
    network = igraph::graph_from_data_frame(edges, directed = test.directed, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network))
}, patrick::cases(
    "directed: FALSE" = list(test.directed = FALSE),
    "directed: TRUE" = list(test.directed = TRUE)
))
