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
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
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

test_that("Construction of the multi network for the feature artifact with author.relation = 'cochange' and artifact.
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
              network.built = network.builder$get.multi.network()

              ## build expected network
              vertices = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas",
                                             "Base_Feature", "foo", "A"),
                                    kind = c(rep(TYPE.AUTHOR, 4), rep("Feature", 3)),
                                    type = c(rep(TYPE.AUTHOR, 4), rep(TYPE.ARTIFACT, 3))
                                    )
              row.names(vertices) = c("Björn", "Olaf", "Karl", "Thomas",
                                      "Base_Feature", "foo", "A")

              edges = data.frame(
                  from = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl",
                           "Base_Feature", "Base_Feature", "Björn", "Olaf", "Olaf", "Karl", "Thomas",
                           "Thomas"),
                  to = c("Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas", "Thomas", "foo",
                         "foo", "A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "foo"),
                  date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                "2016-07-12 16:06:10", "2016-07-12 16:05:41", "2016-07-12 16:06:32",
                                                "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32",
                                                "2016-07-12 16:06:32", "2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                "2016-07-12 16:05:41", "2016-07-12 16:06:10", "2016-07-12 16:06:32",
                                                "2016-07-12 16:06:32")),
                  artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature", "Feature",
                                    "Feature", "Feature", "Feature", "Feature", "Feature", "Feature", "Feature",
                                    "Feature", "Feature"),
                  hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                           "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                           "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526",
                           "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526",
                           "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                           "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                           "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                  file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c",
                           "test2.c", "test2.c", "test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c"),
                  artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature",
                               "Base_Feature", "Base_Feature", "foo", "A", "A", "Base_Feature", "Base_Feature", "Base_Feature",
                               "foo"),
                  weight = 1,
                  type = c(rep(TYPE.EDGES.INTRA, 10), rep(TYPE.EDGES.INTER, 6)),
                  relation = "cochange"
              )

              network.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

              expected.edges = igraph::as_data_frame(network.expected, what = "edges")
              expected.vertices = igraph::as_data_frame(network.expected, what = "vertices")

              built.edges = igraph::as_data_frame(network.built, what = "edges")
              built.vertices = igraph::as_data_frame(network.built, what = "vertices")

              expect_identical(expected.edges, built.edges, info = "Multi network edges")
              expect_identical(expected.vertices, built.vertices, info = "Multi network vertices")
              ## TODO  as soon as the bug in igraph is fixed switch to the expect_true function below
              # expect_true(igraph::identical_graphs(network.expected, network.built))
          })
