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
## Copyright 2017 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
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
                proj.conf$update.value("artifact.filter.base", FALSE)
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

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = c("Björn", "Olaf", "Karl", "Thomas")

    ## edge attributes
    data = data.frame(comb.1. = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl"),
                      comb.2. = c("Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas", "Thomas"),
                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41", "2016-07-12 16:06:10",
                                                    "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                               "1143db502761379c2bfcecc2007fc34282e7ee61", "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                               "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c"),
                      artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature", "Feature", "Feature"),
                      artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature"),
                      weight = c(1, 1, 1, 1, 1, 1, 1, 1),
                      type = TYPE.EDGES.INTRA,
                      relation = c("cochange", "cochange", "cochange", "cochange", "cochange", "cochange", "cochange", "cochange")
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = authors)
    network.expected = igraph::set.vertex.attribute(network.expected, "id", value = igraph::get.vertex.attribute(network.expected, "name"))
    igraph::V(network.expected)$type = TYPE.AUTHOR

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the directed author-cochange network", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", author.directed = TRUE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = c("Björn", "Olaf", "Karl", "Thomas")

    ## edge attributes
    data = data.frame(from = c("Olaf", "Karl", "Thomas", "Thomas"),
                      to = c("Björn", "Olaf", "Olaf", "Karl"),
                      date = get.date.from.string(c("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32",
                                                    "2016-07-12 16:06:32")),
                      hash = c("5a5ec9675e98187e1e92561e1888aa6f04faa338", "1143db502761379c2bfcecc2007fc34282e7ee61",
                               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test.c", "test3.c", "test2.c", "test2.c"),
                      artifact.type = c("Feature", "Feature", "Feature", "Feature"),
                      artifact = c("A", "Base_Feature", "Base_Feature", "Base_Feature"),
                      weight = c(1, 1, 1, 1),
                      type = TYPE.EDGES.INTRA,
                      relation = c("cochange", "cochange", "cochange", "cochange")
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = TRUE, vertices = authors)
    network.expected = igraph::set.vertex.attribute(network.expected, "id", value = igraph::get.vertex.attribute(network.expected, "name"))
    igraph::V(network.expected)$type = TYPE.AUTHOR

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the undirected simplified author-cochange network", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", simplify = TRUE))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = c("Björn", "Olaf", "Karl", "Thomas")

    ## edge attributes
    data = data.frame(from = c("Björn", "Olaf", "Olaf", "Karl"),
                      to = c("Olaf", "Karl", "Thomas", "Thomas"),
                      date = I(list(c(1468339139, 1468339245), c(1468339541, 1468339570), c(1468339541, 1468339592), c(1468339570, 1468339592))),
                      hash = I(list(c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338"), c("3a0ed78458b3976243db6829f63eba3eead26774",
                               "1143db502761379c2bfcecc2007fc34282e7ee61"), c("3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                               c("1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526"))),
                      file = I(list(c("test.c", "test.c"), c("test2.c", "test3.c"), c("test2.c", "test2.c"), c("test3.c", "test2.c"))),
                      artifact.type = I(list(c("Feature", "Feature"), c("Feature", "Feature"), c("Feature", "Feature"), c("Feature", "Feature"))),
                      artifact = I(list(c("A", "A"), c("Base_Feature", "Base_Feature"), c("Base_Feature", "Base_Feature"), c("Base_Feature", "Base_Feature"))),
                      weight = c(2, 2, 2, 2),
                      type = TYPE.EDGES.INTRA,
                      relation = c("cochange", "cochange", "cochange", "cochange")
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = authors)
    network.expected = igraph::set.vertex.attribute(network.expected, "id", value = igraph::get.vertex.attribute(network.expected, "name"))
    igraph::V(network.expected)$type = TYPE.AUTHOR

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Network construction of the undirected author-issue network with all issue data", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "issue"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    vertices = data.frame(name = c("Karl", "Olaf", "Thomas", "udo", "Björn", "Max"),
                          id = c("Karl", "Olaf", "Thomas", "udo", "Björn", "Max"),
                          type = TYPE.AUTHOR)

    edges = data.frame(from = c("Karl", "Karl", "Karl", "Karl", "Karl", "Karl", "Karl", "Karl", "Karl", "Karl", "Karl",
                                "Olaf", "Olaf", "Olaf", "Thomas", "Thomas", "Thomas", "Thomas", "udo", "udo", "udo",
                                "udo", "udo", "udo", "udo", "Olaf", "Olaf", "Olaf", "Thomas", "Thomas", "Thomas",
                                "Thomas", "Thomas", "Thomas", "Thomas", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf",
                                "Olaf", "Olaf", "Olaf", "Olaf", "Thomas", "Thomas", "Thomas", "Thomas", "Thomas", "Thomas",
                                "Thomas", "Thomas", "Thomas", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf",
                                "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Björn", "Björn", "Björn",
                                "Björn", "Björn", "Björn"),
                       to = c( "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Thomas", "Thomas", "Thomas", "Thomas",
                               "Thomas", "Thomas", "Thomas", "Thomas", "udo", "udo", "udo", "udo", "Björn", "Björn", "Björn",
                               "Björn", "Björn", "Björn", "Björn", "udo", "udo", "udo", "Björn", "Björn", "Björn", "Björn",
                               "Björn", "Björn", "Björn", "Thomas", "Thomas", "Thomas", "Björn", "Björn", "Björn", "Björn",
                               "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn",
                               "Thomas", "Thomas", "Thomas", "Thomas", "Thomas", "Thomas", "Björn", "Björn", "Björn", "Björn",
                               "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Max", "Max", "Max", "Max", "Max",
                               "Max" ),
                       date = get.date.from.string(c( "2013-04-21 23:52:09", "2013-05-05 23:28:57", "2013-05-05 23:28:57", "2013-05-25 20:02:08",
                                                      "2013-05-25 20:02:08", "2013-06-01 22:37:03", "2013-04-21 23:52:09", "2013-05-05 23:28:57",
                                                      "2013-05-05 23:28:57", "2013-06-01 22:37:03", "2016-07-19 10:47:25", "2013-05-25 20:02:08",
                                                      "2013-05-25 20:02:08", "2016-07-19 10:47:25", "2016-04-17 02:07:37", "2016-04-17 02:07:37",
                                                      "2016-07-14 02:03:14", "2016-07-15 08:37:57", "2016-04-17 02:07:37", "2016-04-17 02:07:37",
                                                      "2016-07-14 17:42:52", "2016-07-15 08:37:57", "2016-07-15 08:37:57", "2016-07-27 22:25:25",
                                                      "2016-07-27 22:25:25", "2016-04-17 02:07:37", "2016-04-17 02:07:37", "2016-07-27 22:25:25",
                                                      "2016-07-14 02:03:14", "2016-07-14 17:42:52", "2016-07-15 08:37:57", "2016-07-15 08:37:57",
                                                      "2016-07-15 08:37:57", "2016-07-27 22:25:25", "2016-07-27 22:25:25", "2016-07-14 02:03:14",
                                                      "2016-07-15 08:37:57", "2016-07-27 22:25:25", "2016-07-14 17:42:52", "2016-07-15 08:37:57",
                                                      "2016-07-15 08:37:57", "2016-07-27 22:25:25", "2016-07-27 22:25:25", "2016-07-27 22:25:25",
                                                      "2016-07-12 15:59:25", "2016-07-12 15:59:25", "2016-07-12 15:59:25", "2016-07-12 16:03:23",
                                                      "2016-07-12 16:05:47", "2016-08-31 18:21:48", "2016-10-13 15:33:56", "2016-12-06 14:03:42",
                                                      "2016-12-07 15:53:02", "2016-07-12 15:59:25", "2016-07-12 15:59:25", "2016-10-05 01:07:46",
                                                      "2016-12-07 15:37:02", "2016-12-07 15:37:02", "2016-12-07 15:37:21", "2016-07-12 15:59:25",
                                                      "2016-07-12 16:03:23", "2016-07-12 16:05:47", "2016-08-31 18:21:48", "2016-10-05 01:07:46",
                                                      "2016-10-13 15:33:56", "2016-12-06 14:03:42", "2016-12-07 15:37:02", "2016-12-07 15:37:02",
                                                      "2016-12-07 15:37:21", "2016-12-07 15:53:02", "2016-12-07 15:53:02", "2017-02-20 22:25:41",
                                                      "2017-03-02 17:30:10", "2017-05-23 12:32:21", "2017-05-23 12:32:21", "2017-05-23 12:32:39" )),
                       issue.id = c( "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>",
                                     "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>", "<issue-2>",
                                     "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>",
                                     "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>",
                                     "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>",
                                     "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>",
                                     "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>",
                                     "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>",
                                     "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>",
                                     "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>",
                                     "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>",
                                     "<issue-51>", "<issue-51>", "<issue-57>", "<issue-57>", "<issue-57>", "<issue-57>",
                                     "<issue-57>", "<issue-57>" ),
                       event.name = c( "created", "commented", "referenced", "merged", "closed", "head_ref_deleted", "created",
                                       "commented", "referenced", "head_ref_deleted", "referenced", "merged", "closed",
                                       "referenced", "mentioned", "subscribed", "commented", "commented", "mentioned",
                                       "subscribed", "commented", "mentioned", "subscribed", "mentioned", "subscribed",
                                       "mentioned", "subscribed", "commented", "commented", "commented", "mentioned",
                                       "subscribed", "commented", "mentioned", "subscribed", "commented", "commented",
                                       "commented", "commented", "mentioned", "subscribed", "mentioned", "subscribed",
                                       "commented", "mentioned", "subscribed", "created", "renamed", "commented", "commented",
                                       "commented", "commented", "commented", "mentioned", "subscribed", "commented", "merged",
                                       "closed", "commented", "created", "renamed", "commented", "commented", "commented",
                                       "commented", "commented", "merged", "closed", "commented", "commented", "created",
                                       "commented", "commented", "merged", "closed", "commented" ),
                       weight = c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1 ),
                       type = TYPE.EDGES.INTRA,
                       relation = c("issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue", "issue",
                                     "issue", "issue", "issue", "issue")
            )

    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Network construction of the undirected author-issue network with just comment events", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "issue"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    vertices = data.frame(name = c("Karl", "Thomas", "Björn", "Olaf", "Max"),
                          id = c("Karl", "Thomas", "Björn", "Olaf", "Max"),
                          type = TYPE.AUTHOR)

    edges = data.frame(from = c( "Thomas", "Thomas", "Thomas", "Thomas", "Thomas", "Thomas", "Björn", "Björn", "Björn",
                                 "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn" ),
                       to = c( "Björn", "Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf",
                               "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Max", "Max", "Max" ),
                       date = get.date.from.string(c( "2016-07-14 02:03:14", "2016-07-14 17:42:52", "2016-07-15 08:37:57",
                                                      "2016-07-14 02:03:14", "2016-07-15 08:37:57", "2016-07-27 22:25:25",
                                                      "2016-07-14 17:42:52", "2016-07-27 22:25:25", "2016-07-12 16:05:47",
                                                      "2016-08-31 18:21:48", "2016-10-05 01:07:46", "2016-10-13 15:33:56",
                                                      "2016-12-06 14:03:42", "2016-12-07 15:37:21", "2016-12-07 15:53:02",
                                                      "2017-02-20 22:25:41", "2017-03-02 17:30:10", "2017-05-23 12:32:39" )),
                       issue.id = c( "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>", "<issue-48>",
                                     "<issue-48>", "<issue-48>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>",
                                     "<issue-51>", "<issue-51>", "<issue-51>", "<issue-57>", "<issue-57>", "<issue-57>" ),
                       event.name = c( "commented", "commented", "commented", "commented", "commented", "commented",
                                       "commented", "commented", "commented", "commented", "commented", "commented",
                                       "commented", "commented", "commented", "commented", "commented", "commented" ),
                       weight = c(1),
                       type = TYPE.EDGES.INTRA,
                       relation = c("issue"))

    network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})
