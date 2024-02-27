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
## Copyright 2017, 2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2023 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2023 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2018-2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2023-2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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


##
## Tests for author.all.authors and author.only.committers
##

test_that("Amount of authors (author.all.authors, author.only.committers).", {

    ## author sets
    authors.committing = c("Björn", "Olaf", "Thomas", "Karl")
    authors.mailing = c("Björn", "Olaf", "Thomas", "georg", "Hans", "udo", "Fritz fritz@example.org")
    authors.issue = c("Karl", "Olaf", "Thomas", "Björn", "Max")

    authors.all = union(union(authors.committing, authors.mailing), authors.issue)

    authors.all.aoc = intersect(authors.all, authors.committing)
    authors.committing.aoc = intersect(authors.committing, authors.committing)
    authors.mailing.aoc = intersect(authors.mailing, authors.committing)
    authors.issue.aoc = intersect(authors.issue, authors.committing)

    ## expected sets of vertices in the networks for artifact.relation = "cochange"
    expected = list(
        ## author.relation
        cochange = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.all.aoc, bipartite = authors.all.aoc, multi = authors.all.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.all, bipartite = authors.all, multi = authors.all
                )
            ),
            "FALSE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.committing.aoc, bipartite = authors.committing.aoc, multi = authors.committing.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.committing, bipartite = authors.committing, multi = authors.committing
                )
            )
        ),
        mail = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.all.aoc, bipartite = authors.all.aoc, multi = authors.all.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.all, bipartite = authors.all, multi = authors.all
                )
            ),
            "FALSE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.mailing.aoc, bipartite = authors.committing.aoc, multi = authors.mailing.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.mailing, bipartite = authors.committing, multi = authors.mailing
                )
            )
        ),
        issue = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.all.aoc, bipartite = authors.all.aoc, multi = authors.all.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.all, bipartite = authors.all, multi = authors.all
                )
            ),
            "FALSE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.issue.aoc, bipartite = authors.committing.aoc, multi = authors.issue.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.issue, bipartite = authors.committing, multi = authors.issue
                )
            )
        )
    )

    ## run all tests
    for (author.relation in c("cochange", "mail", "issue")) {
        for (author.all.authors in c(TRUE, FALSE)) {
            for (author.only.committers in c(TRUE, FALSE)) {

                ## configurations
                proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
                proj.conf$update.value("commits.filter.base.artifact", FALSE)
                proj.conf$update.value("commits.filter.untracked.files", TRUE)
                net.conf = NetworkConf$new()

                ## update network configuration
                net.conf$update.values(updated.values = list(
                    author.relation = author.relation, artifact.relation = "cochange",
                    author.all.authors = author.all.authors, author.only.committers = author.only.committers)
                )

                ## construct objects
                x.data = ProjectData$new(proj.conf)
                x = NetworkBuilder$new(x.data, net.conf)

                ## author network
                x$reset.environment()
                a = x$get.author.network()
                expect_true(
                    setequal(
                        igraph::V(a)[ type == TYPE.AUTHOR ]$name,
                        expected[[author.relation]][[as.character(author.all.authors)]][[as.character(author.only.committers)]][["author"]]
                    ),
                    info = sprintf("Author network (author.relation = %s, author.all.authors = %s, author.only.committers = %s",
                                   author.relation, author.all.authors, author.only.committers)
                )

                ## bipartite network
                x$reset.environment()
                b = x$get.bipartite.network()
                expect_true(
                    setequal(
                        igraph::V(b)[ type == TYPE.AUTHOR ]$name,
                        expected[[author.relation]][[as.character(author.all.authors)]][[as.character(author.only.committers)]][["bipartite"]]
                    ),
                    info = sprintf("Bipartite network (author.relation = %s, author.all.authors = %s, author.only.committers = %s",
                                   author.relation, author.all.authors, author.only.committers)
                )

                ## multi network
                x$reset.environment()
                m = x$get.multi.network()
                expect_true(
                    setequal(
                        igraph::V(m)[ type == TYPE.AUTHOR ]$name,
                        expected[[author.relation]][[as.character(author.all.authors)]][[as.character(author.only.committers)]][["multi"]]
                    ),
                    info = sprintf("Multi network (author.relation = %s, author.all.authors = %s, author.only.committers = %s",
                                   author.relation, author.all.authors, author.only.committers)
                )

            }
        }
    }

})

test_that("Network construction of the undirected author-cochange network", {

    ## build expected network:
    ## 1) vertices
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)

    ## 2) edges
    data = data.frame(comb.1. = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl"),
                      comb.2. = c("Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas", "Thomas"),
                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                    "2016-07-12 16:06:10", "2016-07-12 16:05:41", "2016-07-12 16:06:32",
                                                    "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                      artifact.type = "Feature",
                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                               "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c"),
                      artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature",
                                   "Base_Feature", "Base_Feature"),
                      weight = 1,
                      type = TYPE.EDGES.INTRA,
                      relation = "cochange"
    )
    ## 3) build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = authors)


    ##
    ## without untracked files
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))


    ##
    ## with untracked files
    ##

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", TRUE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the undirected but temorally ordered author-cochange network", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", author.directed = FALSE,
                                                 author.respect.temporal.order = TRUE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)

    ## edge attributes
    data = data.frame(comb.1. = c("Olaf", "Karl", "Thomas", "Thomas"),
                      comb.2. = c("Björn", "Olaf", "Olaf", "Karl"),
                      date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:10",
                                                    "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                      artifact.type = "Feature",
                      hash = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "1143db502761379c2bfcecc2007fc34282e7ee61",
                              "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test.c", "test3.c", "test2.c", "test2.c"),
                      artifact = c("A", "Base_Feature", "Base_Feature", "Base_Feature"),
                      weight = 1,
                      type = TYPE.EDGES.INTRA,
                      relation = "cochange"
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = authors)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the directed author-cochange network", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", author.directed = TRUE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)

    ## edge attributes
    data = data.frame(from = c("Olaf", "Karl", "Thomas", "Thomas"),
                      to = c("Björn", "Olaf", "Olaf", "Karl"),
                      date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32",
                                                    "2016-07-12 16:06:32")),
                      artifact.type = "Feature",
                      hash = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "1143db502761379c2bfcecc2007fc34282e7ee61",
                               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test.c", "test3.c", "test2.c", "test2.c"),
                      artifact = c("A", "Base_Feature", "Base_Feature", "Base_Feature"),
                      weight = 1,
                      type = TYPE.EDGES.INTRA,
                      relation = "cochange"
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = TRUE, vertices = authors)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the directed author-cochange network without respecting temporal order", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", author.directed = TRUE,
                                                 author.respect.temporal.order = FALSE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)

    ## edge attributes
    data = data.frame(comb.1. = c("Björn", "Olaf", "Olaf", "Karl", "Olaf", "Thomas", "Karl", "Thomas"),
                      comb.2. = c("Olaf", "Björn", "Karl", "Olaf", "Thomas", "Olaf", "Thomas", "Karl"),
                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                    "2016-07-12 16:06:10", "2016-07-12 16:05:41", "2016-07-12 16:06:32",
                                                    "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                      artifact.type = "Feature",
                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                               "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c"),
                      artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature",
                                   "Base_Feature", "Base_Feature"),
                      weight = 1,
                      type = TYPE.EDGES.INTRA,
                      relation = "cochange"
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = TRUE, vertices = authors)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the undirected simplified author-cochange network", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", simplify = TRUE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)

    ## make test independent of igraph version
    date.attr = igraph::get.edge.attribute(network.built, "date")
    date.conversion.function = ifelse(all(sapply(date.attr, lubridate::is.POSIXct)),
                                      get.date.from.unix.timestamp, identity)

    ## edge attributes
    data = data.frame(
        from = c("Björn", "Olaf", "Olaf", "Karl"),
        to = c("Olaf", "Karl", "Thomas", "Thomas"),
        date = I(list(date.conversion.function(c(1468339139, 1468339245)),
                      date.conversion.function(c(1468339541, 1468339570)),
                      date.conversion.function(c(1468339541, 1468339592)),
                      date.conversion.function(c(1468339570, 1468339592)))),
        artifact.type = I(list(c("Feature", "Feature"), c("Feature", "Feature"), c("Feature", "Feature"),
                               c("Feature", "Feature"))),
        hash = I(list(
            c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
            c("3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61"),
            c("3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
            c("1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"))),
        file = I(list(c("test.c", "test.c"), c("test2.c", "test3.c"), c("test2.c", "test2.c"), c("test3.c", "test2.c"))),
        artifact = I(list(c("A", "A"), c("Base_Feature", "Base_Feature"), c("Base_Feature", "Base_Feature"),
                          c("Base_Feature", "Base_Feature"))),
        weight = 2,
        type = TYPE.EDGES.INTRA,
        relation = "cochange"
    )

    ## remove the 'AsIs' class from the edge attributes that have been inserted via `I(...)`
    data[["date"]] = unclass(data[["date"]])
    data[["artifact.type"]] = unclass(data[["artifact.type"]])
    data[["hash"]] = unclass(data[["hash"]])
    data[["file"]] = unclass(data[["file"]])
    data[["artifact"]] = unclass(data[["artifact"]])

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = authors)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Network construction of the undirected author-issue network with all issue data", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "issue"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    vertices = data.frame(name = c("Thomas", "Olaf", "Björn", "Karl", "udo", "Max"),
                          kind = TYPE.AUTHOR,
                          type = TYPE.AUTHOR)

    ## edge attributes
    edges = data.frame(from = c(rep("Thomas", 5), rep("Thomas", 4), rep("Olaf", 3), # <issue-github-1>
                                rep("Olaf", 4), rep("Thomas", 3), rep("Thomas", 3), # <issue-github-2>
                                rep("Olaf", 7), rep("Thomas", 7), rep("Thomas", 4), # <issue-github-3>
                                rep("Olaf", 3), # <issue-github-4>
                                rep("Thomas", 4), rep("Karl", 3), rep("Björn", 7), rep("Olaf", 3), rep("Thomas", 3), rep("Thomas", 7),
                                rep("Thomas", 3), rep("Björn", 6), rep("Olaf", 2), rep("Olaf", 6), # <issue-github-6>
                                rep("Thomas", 11), rep("Thomas", 8), rep("Olaf", 11), # <issue-jira-ZEPPELIN-328>
                                rep("Björn", 6), rep("Thomas", 5), rep("Thomas", 5) # <issue-jira-ZEPPELIN-332>
                                ),
                       to = c(rep("Olaf", 5), rep("Björn", 4), rep("Björn", 3), # <issue-github-1>
                              rep("Björn", 4), rep("Björn", 3), rep("Olaf", 3), # <issue-github-2>
                              rep("Karl", 7), rep("Karl", 7), rep("Olaf", 4), # <issue-github-3>
                              rep("Björn", 3), # <issue-github-4>
                              rep("udo", 4), rep("udo", 3), rep("udo", 7), rep("udo", 3), rep("Karl", 3), rep("Björn", 7),
                              rep("Olaf", 3), rep("Karl", 6), rep("Karl", 2), rep("Björn", 6), # <issue-github-6>
                              rep("Björn", 11), rep("Olaf", 8), rep("Björn", 11), # <issue-jira-ZEPPELIN-328>
                              rep("Max", 6), rep("Björn", 5), rep("Max", 5) # <issue-jira-ZEPPELIN-332>
                                ),
                       date = get.date.from.string(c( "2016-07-12 15:59:25", "2016-07-12 15:59:25", "2016-07-12 15:59:59", # <issue-github-1>
                                                      "2016-07-12 16:01:01", "2016-07-14 13:37:00", "2016-07-12 15:59:25",
                                                      "2016-07-12 15:59:25", "2016-07-12 15:59:59", "2016-07-12 16:06:01",
                                                      "2016-07-12 16:01:01", "2016-07-14 13:37:00", "2016-07-12 16:06:01",
                                                      "2016-07-12 14:59:25", "2016-07-12 14:59:25", "2016-07-12 16:04:59", # <issue-github-2>
                                                      "2016-07-12 16:04:59", "2016-07-12 14:59:25", "2016-07-12 14:59:25",
                                                      "2016-08-07 15:30:00", "2016-07-12 16:04:59", "2016-07-12 16:04:59",
                                                      "2016-08-07 15:30:00",
                                                      "2016-07-12 15:59:25", "2016-07-12 15:59:59", "2016-08-07 15:37:02", # <issue-github-3>
                                                      "2016-08-07 15:37:02", "2016-08-31 16:45:09", "2016-07-12 15:59:25",
                                                      "2016-07-12 16:06:30", "2016-07-12 15:59:25", "2016-07-12 15:59:59",
                                                      "2016-08-07 15:37:02", "2016-08-07 15:37:02", "2016-08-31 16:45:09",
                                                      "2016-08-07 15:30:00", "2016-10-05 16:45:09", "2016-07-12 15:59:25",
                                                      "2016-07-12 16:06:30", "2016-08-07 15:30:00", "2016-10-05 16:45:09",
                                                      "2016-07-12 16:02:02", "2016-07-12 16:02:02", "2016-07-12 16:02:02", # <issue-github-4>
                                                      "2016-07-12 15:30:02", "2016-07-12 15:30:02", "2016-07-12 16:03:59", # <issue-github-6>
                                                      "2016-10-13 15:30:02", "2016-07-12 15:30:02", "2016-07-12 15:30:02",
                                                      "2016-08-07 15:37:02", "2016-07-12 15:30:02", "2016-07-12 15:30:02",
                                                      "2016-08-31 15:30:02", "2016-10-05 15:30:02", "2016-12-07 15:30:02",
                                                      "2016-12-07 15:30:02", "2017-05-23 12:32:39", "2016-07-12 15:30:02",
                                                      "2016-07-12 15:30:02", "2017-05-23 12:31:34", "2016-07-12 16:03:59",
                                                      "2016-10-13 15:30:02", "2016-08-07 15:37:02", "2016-07-12 16:03:59",
                                                      "2016-10-13 15:30:02", "2016-08-31 15:30:02", "2016-10-05 15:30:02",
                                                      "2016-12-07 15:30:02", "2016-12-07 15:30:02", "2017-05-23 12:32:39",
                                                      "2016-07-12 16:03:59", "2016-10-13 15:30:02", "2017-05-23 12:31:34",
                                                      "2016-08-07 15:37:02", "2016-08-31 15:30:02", "2016-10-05 15:30:02",
                                                      "2016-12-07 15:30:02", "2016-12-07 15:30:02", "2017-05-23 12:32:39",
                                                      "2016-08-07 15:37:02", "2017-05-23 12:31:34", "2016-08-31 15:30:02",
                                                      "2016-10-05 15:30:02", "2016-12-07 15:30:02", "2016-12-07 15:30:02",
                                                      "2017-05-23 12:32:39", "2017-05-23 12:31:34",
                                                      "2013-04-21 23:52:09", "2013-04-21 23:52:09", "2017-05-21 12:00:00", # <issue-jira-ZEPPELIN-328>
                                                      "2017-05-21 12:00:00", "2013-05-05 21:46:30", "2013-05-05 21:49:21",
                                                      "2013-05-05 21:49:34", "2013-05-06 01:04:34", "2013-05-25 03:48:41",
                                                      "2013-05-25 04:08:07", "2013-06-01 06:53:06", "2013-04-21 23:52:09",
                                                      "2013-04-21 23:52:09", "2017-05-21 12:00:00", "2017-05-21 12:00:00",
                                                      "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                                      "2013-06-01 06:50:26", "2013-05-05 21:46:30", "2013-05-05 21:49:21",
                                                      "2013-05-05 21:49:34", "2013-05-06 01:04:34", "2013-05-25 03:48:41",
                                                      "2013-05-25 04:08:07", "2013-06-01 06:53:06", "2013-05-25 03:25:06",
                                                      "2013-05-25 06:06:53", "2013-05-25 06:22:23", "2013-06-01 06:50:26",
                                                      "2016-07-12 16:01:30", "2016-07-12 16:02:30", "2016-07-15 19:55:39", # <issue-jira-ZEPPELIN-332>
                                                      "2016-07-15 20:07:47", "2016-07-27 20:12:08", "2016-07-28 06:27:52",
                                                      "2016-07-12 16:01:30", "2016-07-12 16:02:30", "2016-07-15 19:55:39",
                                                      "2017-05-21 12:00:00", "2017-05-21 12:00:00", "2016-07-15 20:07:47",
                                                      "2016-07-27 20:12:08", "2016-07-28 06:27:52", "2017-05-21 12:00:00",
                                                      "2017-05-21 12:00:00"
                                                      )),
                       artifact.type = "IssueEvent",
                       issue.id = c(rep("<issue-github-1>", 12), rep("<issue-github-2>", 10), rep("<issue-github-3>", 18),
                                    rep("<issue-github-4>", 3), rep("<issue-github-6>", 44), rep("<issue-jira-ZEPPELIN-328>", 30),
                                    rep("<issue-jira-ZEPPELIN-332>", 16)),
                       event.name = c("created", "commented", "state_updated", "commented", "state_updated", "created", # <issue-github-1>
                                      "commented", "state_updated", "commented", "commented", "state_updated", "commented",
                                      "created", "commented", "merged", "state_updated", "created", "commented", "referenced_by", # <issue-github-2>
                                      "merged", "state_updated", "referenced_by",
                                      "created", "commented", "add_link", "add_link", "referenced", "assigned", "state_updated", "created", # <issue-github-3>
                                      "commented", "add_link", "add_link", "referenced", "add_link", "referenced", "assigned", "state_updated", "add_link",
                                      "referenced",
                                      "commit_added", "created", "commented", # <issue-github-4>
                                      "mentioned", "subscribed", "commented", "add_link", "mentioned", "subscribed", "referenced_by", # <issue-github-6>
                                      "mentioned", "subscribed", "mentioned", "subscribed", "mentioned", "subscribed", "commented",
                                      "mentioned", "subscribed", "labeled", "commented", "add_link", "referenced_by", "commented", "add_link", "mentioned",
                                      "subscribed", "mentioned", "subscribed", "commented", "commented", "add_link", "labeled", "referenced_by",
                                      "mentioned", "subscribed", "mentioned", "subscribed", "commented", "referenced_by", "labeled",
                                      "mentioned", "subscribed", "mentioned", "subscribed", "commented", "labeled",
                                      "created", "commented", "referenced_by", "add_link", "commented", "commented", "commented", "commented", # <issue-jira-ZEPPELIN-328>
                                      "commented", "commented", "resolution_updated", "created", "commented", "referenced_by", "add_link", "commented",
                                      "commented", "commented", "commented", "commented", "commented", "commented", "commented", "commented",
                                      "commented", "resolution_updated", "commented", "commented", "commented", "commented",
                                      "created", "commented", "commented", "commented", "commented", "commented", "created", # <issue-jira-ZEPPELIN-332>
                                    "commented", "commented", "referenced_by", "add_link", "commented", "commented", "commented", "referenced_by", "add_link"
                                      ),
                       weight = 1,
                       type = TYPE.EDGES.INTRA,
                       relation = "issue"
            )

    ## build expected network
    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the undirected author-issue network with just comment events", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "issue"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    vertices = data.frame(name = c("Thomas", "Olaf", "Björn", "Karl", "Max"),
                          kind = TYPE.AUTHOR,
                          type = TYPE.AUTHOR)

    ## edge attributes
    edges = data.frame(from = c(rep("Thomas", 2), rep("Thomas", 2), rep("Olaf", 2), # <issue-github-1>
                                rep("Thomas", 2), # <issue-github-6>
                                rep("Thomas", 7), rep("Thomas", 5), rep("Björn", 10), # <issue-jira-ZEPPELIN-328>
                                rep("Björn", 5) # <issue-jira-ZEPPELIN-332>
                                ),
                       to = c(rep("Olaf", 2), rep("Björn", 2), rep("Björn", 2), # <issue-github-1>
                              rep("Björn", 2), # <issue-github-6>
                              rep("Björn", 7), rep("Olaf", 5), rep("Olaf", 10), # <issue-jira-ZEPPELIN-328>
                              rep("Max", 5) # <issue-jira-ZEPPELIN-332>
                              ),
                       date = get.date.from.string(c( "2016-07-12 15:59:25", "2016-07-12 16:01:01", "2016-07-12 15:59:25", # <issue-github-1>
                                                      "2016-07-12 16:06:01", "2016-07-12 16:01:01", "2016-07-12 16:06:01",
                                                      "2016-07-12 16:03:59", "2017-05-23 12:32:39", # <issue-github-6>
                                                      "2013-04-21 23:52:09", "2013-05-05 21:46:30", "2013-05-05 21:49:21", # <issue-jira-ZEPPELIN-328>
                                                      "2013-05-05 21:49:34", "2013-05-06 01:04:34", "2013-05-25 03:48:41",
                                                      "2013-05-25 04:08:07", "2013-04-21 23:52:09", "2013-05-25 03:25:06",
                                                      "2013-05-25 06:06:53", "2013-05-25 06:22:23", "2013-06-01 06:50:26",
                                                      "2013-05-05 21:46:30", "2013-05-05 21:49:21", "2013-05-05 21:49:34",
                                                      "2013-05-06 01:04:34", "2013-05-25 03:48:41", "2013-05-25 04:08:07",
                                                      "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                                      "2013-06-01 06:50:26",
                                                      "2016-07-12 16:02:30", "2016-07-15 19:55:39", "2016-07-15 20:07:47", # <issue-jira-ZEPPELIN-332>
                                                      "2016-07-27 20:12:08", "2016-07-28 06:27:52"
                                                      )),
                       artifact.type = "IssueEvent",
                       issue.id = c( rep("<issue-github-1>", 6), rep("<issue-github-6>", 2),
                                     rep("<issue-jira-ZEPPELIN-328>", 22), rep("<issue-jira-ZEPPELIN-332>", 5) ),
                       event.name = "commented",
                       weight = 1,
                       type = TYPE.EDGES.INTRA,
                       relation = "issue")

    ## build expected network
    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Network construction with only untracked files (no edges expected)", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange"))
    net.conf$clear.edge.attributes()

    ## construct objects and keep just commits on untracked files
    proj.data = ProjectData$new(project.conf = proj.conf)
    commit.data = subset(proj.data$get.commits.unfiltered(), artifact == UNTRACKED.FILE.EMPTY.ARTIFACT)
    proj.data$set.commits(commit.data)

    ## build network
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.author.network()

    ## build expected network (two vertices, no edges)
    vertices = list(name = c("Karl", "Thomas"), kind = TYPE.AUTHOR, type = TYPE.AUTHOR)
    network.expected = create.empty.network(directed = FALSE, add.attributes = TRUE)
    network.expected = igraph::add.vertices(network.expected, nv = max(lengths(vertices)), attr = vertices)

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))
})
