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
## Copyright 2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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
    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)


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

test_that("Network construction of an issue-based artifact-network", {
    ## build expected network:
    ## 1) vertices
    vertices = data.frame(name = c("<issue-jira-ZEPPELIN-328>",
                                   "<issue-jira-ZEPPELIN-332>" ,
                                   "<issue-github-2>",
                                   "<issue-github-6>",
                                   "<issue-github-3>",
                                   "<issue-github-1>",
                                   "<issue-github-4>"),
                          kind = "Issue",
                          type = TYPE.ARTIFACT)
    ## 2) edges
    edges = data.frame(
        from = c("<issue-jira-ZEPPELIN-328>", "<issue-github-3>", "<issue-github-3>"),
        to = c("<issue-jira-ZEPPELIN-332>", "<issue-github-2>", "<issue-github-6>"),
        date = get.date.from.string(c("2013-06-01 06:54:00", "2016-08-07 15:30:00", "2016-08-07 15:37:02")),
        artifact.type = c("IssueEvent", "IssueEvent", "IssueEvent"),
        issue.id = c("<issue-jira-ZEPPELIN-328>", "<issue-github-3>", "<issue-github-3>"),
        event.name = c("add_link", "add_link", "add_link"),
        author.name = c("Thomas", "Thomas", "Karl"),
        weight = c(1, 1, 1),
        type = TYPE.EDGES.INTRA,
        relation = "issue"
    )

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "issue"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build expected network
    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    ## build network
    network.built = network.builder$get.artifact.network()

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of an empty 'comments-only' issue-based artifact-network", {

    ##
    ## 'issues.only.comments' (by default), this should not create any edges
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "issue"))

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
    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    ## test
    compare.networks(network.built, network.expected)
})