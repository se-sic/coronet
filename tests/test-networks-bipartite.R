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
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2018-2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
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

test_that("Construction of the bipartite network for the feature artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.bipartite.network()

    ## construct expected network:
    ## 1) construct expected vertices
    authors = data.frame(
        name = c("Björn", "Karl", "Olaf", "Thomas"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
    )
    artifacts = data.frame(
        name = c("A", "Base_Feature", "foo"),
        kind = "Feature",
        type = TYPE.ARTIFACT
    )
    vertices = plyr::rbind.fill(authors, artifacts)
    ## 2) construct expected edge attributes
    network.expected.data = data.frame(
        from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas", "Thomas"),
        to   = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo"),
        date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                      "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
        artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature"),
        hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                 "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
        file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c"),
        artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo"),
        weight = 1,
        type = TYPE.EDGES.INTER,
        relation = "cochange"
    )
    ## 3) construct expected network
    network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                directed = net.conf$get.value("author.directed"))

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the bipartite network for the file artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Karl", "Olaf", "Thomas"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("test.c", "test3.c", "test2.c"),
                  kind = "File",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas"),
                  to   = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                  artifact.type = c("File", "File", "File", "File", "File"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                           "0a1a5c523d835459c42f33e863623138555e2526"),
                  file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  artifact = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                          directed = net.conf$get.value("author.directed"))

              expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the bipartite network for the function artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "function")
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Karl", "Olaf", "Thomas"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("File_Level", "test3.c::test_function"),
                  kind = "Function",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas"),
                  to   = c("File_Level", "test3.c::test_function", "File_Level", "File_Level", "File_Level"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                  artifact.type = c("Function", "Function", "Function", "Function", "Function"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                           "0a1a5c523d835459c42f33e863623138555e2526"),
                  file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  artifact = c("File_Level", "test3.c::test_function", "File_Level", "File_Level", "File_Level"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, directed = net.conf$get.value("author.directed"), vertices = vertices)

              expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Construction of the bipartite network for the featureexpression artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "featureexpression")
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Olaf"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("defined(A)", "Base_Feature"),
                  kind = "FeatureExpression",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Olaf", "Olaf"),
                  to   = c("defined(A)", "defined(A)", "Base_Feature"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41")),
                  artifact.type = c("FeatureExpression", "FeatureExpression", "FeatureExpression"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                           "3a0ed78458b3976243db6829f63eba3eead26774"),
                  file = c("test.c", "test.c", "test2.c"),
                  artifact = c("defined(A)", "defined(A)", "Base_Feature"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                          directed = net.conf$get.value("author.directed"))

              expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Construction of the bipartite network for the feature artifact with author.relation = 'cochange' and artifact.
          relation = 'issue'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "issue"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name =c("Björn", "Karl", "Max", "Olaf", "Thomas"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                           "<issue-github-1>", "<issue-github-6>", "<issue-github-3>", "<issue-github-4>"),
                  kind = "Issue",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)

              ## 2) construct expected edge attributes (issues ordered by 'author.name')
              network.expected.data = data.frame(
                  from = c("Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Karl", "Max",
                           "Max", "Max", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Thomas", "Thomas", "Thomas"),
                  to   = c("<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                           "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                           "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>",
                           "<issue-github-3>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                           "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                           "<issue-github-1>", "<issue-github-4>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>"),
                  date = get.date.from.string(c("2013-05-05 21:46:30", "2013-05-05 21:49:21", "2013-05-05 21:49:34", # Björn
                                                "2013-05-06 01:04:34", "2013-05-25 03:48:41", "2013-05-25 04:08:07",
                                                "2016-07-12 14:59:25", "2016-07-12 16:02:30", "2016-07-12 16:06:01",
                                                "2016-07-15 19:55:39", "2017-05-23 12:32:39",
                                                "2016-07-12 15:59:59", #Karl
                                                "2016-07-15 20:07:47", "2016-07-27 20:12:08", "2016-07-28 06:27:52", # Max
                                                "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                                "2013-06-01 06:50:26", "2016-07-12 16:01:01", "2016-07-12 16:02:02", # Olaf
                                                "2013-04-21 23:52:09", "2016-07-12 15:59:25", "2016-07-12 16:03:59")), # Thomas
                  artifact.type = "IssueEvent",
                  issue.id = c("<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                               "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                               "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>",
                               "<issue-github-3>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                               "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                               "<issue-github-1>", "<issue-github-4>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>"),
                  event.name = "commented",
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "issue"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, directed = net.conf$get.value("author.directed"), vertices = vertices)

              expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Construction of the directed bipartite network for the feature artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Karl", "Olaf", "Thomas"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("A", "Base_Feature", "foo"),
                  kind = "Feature",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas", "Thomas"),
                  to   = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                  artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                           "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                  file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c"),
                  artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                          directed = net.conf$get.value("author.directed"))

              expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Construction of the directed bipartite network for the file artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Karl", "Olaf", "Thomas"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("test.c", "test3.c", "test2.c"),
                  kind = "File",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas"),
                  to   = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                  artifact.type = c("File", "File", "File", "File", "File"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                           "0a1a5c523d835459c42f33e863623138555e2526"),
                  file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  artifact = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                          directed = net.conf$get.value("author.directed"))

              expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the directed bipartite network for the function artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "function")
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Karl", "Olaf", "Thomas"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("File_Level", "test3.c::test_function"),
                  kind = "Function",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas"),
                  to   = c("File_Level", "test3.c::test_function", "File_Level", "File_Level", "File_Level"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                  artifact.type = c("Function", "Function", "Function", "Function", "Function"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                           "0a1a5c523d835459c42f33e863623138555e2526"),
                  file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c"),
                  artifact = c("File_Level", "test3.c::test_function", "File_Level", "File_Level", "File_Level"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                          directed = net.conf$get.value("author.directed"))

              expect_true(igraph::identical_graphs(network.built, network.expected))
})

test_that("Construction of the directed bipartite network for the featureexpression artifact with author.relation = 'cochange' and artifact.
          relation = 'cochange'.", {

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "featureexpression")
              proj.conf$update.value("commits.filter.base.artifact", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              ## construct expected network:
              ## 1) construct expected vertices
              authors = data.frame(
                  name = c("Björn", "Olaf"),
                  kind = TYPE.AUTHOR,
                  type = TYPE.AUTHOR
              )
              artifacts = data.frame(
                  name = c("defined(A)", "Base_Feature"),
                  kind = "FeatureExpression",
                  type = TYPE.ARTIFACT
              )
              vertices = plyr::rbind.fill(authors, artifacts)
              ## 2) construct expected edge attributes
              network.expected.data = data.frame(
                  from = c("Björn", "Olaf", "Olaf"),
                  to   = c("defined(A)", "defined(A)", "Base_Feature"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41")),
                  artifact.type = c("FeatureExpression", "FeatureExpression", "FeatureExpression"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                           "3a0ed78458b3976243db6829f63eba3eead26774"),
                  file = c("test.c", "test.c", "test2.c"),
                  artifact = c("defined(A)", "defined(A)", "Base_Feature"),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = "cochange"
              )
              ## 3) construct expected network
              network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                          directed = net.conf$get.value("author.directed"))

              expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Network construction with only untracked files (no edges and artifacts expected)", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange"))
    net.conf$clear.edge.attributes()

    ## construct objects and keep just commits on untracked files
    proj.data = ProjectData$new(project.conf = proj.conf)
    commit.data = subset(proj.data$get.commits(), artifact == UNTRACKED.FILE.EMPTY.ARTIFACT)
    proj.data$set.commits(commit.data)

    ## build network
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)
    network.built = network.builder$get.bipartite.network()

    ## build expected network (two vertices, no edges):
    ## 1) construct vertices
    vertices = data.frame(name = c("Karl", "Thomas"), kind = TYPE.AUTHOR, type = TYPE.AUTHOR)
    ## 2) add edge first to obtain edge attributes
    edges = network.expected.data = data.frame(
                          from = c("Karl"),
                          to   = c("Thomas"),
                          date = get.date.from.unix.timestamp(0),
                          artifact.type = "",
                          weight = 0,
                          type = TYPE.EDGES.INTRA,
                          relation = "cochange"
                      )
    ## 3) construct network
    network.expected = construct.network.from.edge.list(vertices = vertices, edge.list = edges, network.conf = net.conf,
                                                        directed = net.conf$get.value("author.directed"))
    ## 4) remove edge again
    network.expected = igraph::delete.edges(network.expected, 1)

    ## test
    expect_true(igraph::identical_graphs(network.built, network.expected))
})

