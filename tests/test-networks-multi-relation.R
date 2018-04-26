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

test_that("Network construction of the undirected author network with relation = c('cochange', 'mail')", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = c("cochange", "mail")))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.author.network()

    ## vertex attributes
    authors = data.frame(name = c("Björn", "Olaf", "Karl", "Thomas", "udo", "Fritz fritz@example.org", "georg", "Hans"),
                         kind = TYPE.AUTHOR,
                         type = TYPE.AUTHOR)

    ## edge attributes
    data = data.frame(comb.1. = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl", # cochange
                                  "Björn", "Björn", "Olaf",  "Olaf"), # mail
                      comb.2. = c("Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas", "Thomas", # cochange
                                  "Olaf", "Olaf", "Thomas", "Thomas"), # mail
                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41", # cochange
                                                    "2016-07-12 16:06:10", "2016-07-12 16:05:41", "2016-07-12 16:06:32",
                                                    "2016-07-12 16:06:10",   "2016-07-12 16:06:32",
                                                    "2016-07-12 15:58:40", "2016-07-12 15:58:50", "2016-07-12 16:04:40", # mail
                                                    "2016-07-12 16:05:37")),
                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                               "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526",
                               rep(NA, 4)),
                      file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c",
                               rep(NA, 4)),
                      artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature", "Feature", "Feature", #cochange
                                        rep("Mail", 4)), #mail
                      artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature",
                                   rep(NA, 4)),
                      weight = 1,
                      type = TYPE.EDGES.INTRA,
                      relation = c("cochange", "cochange", "cochange", "cochange", "cochange", "cochange", "cochange", "cochange",
                                   rep("mail", 4)),
                      message.id = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                     "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                     "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                     "<65a1sf31sagd684dfv31@mail.gmail.com>", "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"),
                      thread = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                 "<thread-8>", "<thread-8>", "<thread-9>", "<thread-9>")
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = authors)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the bipartite network for the feature artifact with author.relation = c('cochange', 'issue') and artifact.
          relation = c('issue', 'mail').", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = c("cochange", "issue"), artifact.relation = c("issue", "mail")))

     ## construct objects
     proj.data = ProjectData$new(project.conf = proj.conf)
     network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

     ## build network
     network.built = network.builder$get.bipartite.network()

     ## construct expected network:
     ## 1) construct expected vertices
     authors1 = data.frame( # author -- issue
        name = c("Björn", "Karl", "Max", "Olaf", "Thomas"),
        type = TYPE.AUTHOR,
        artifact.type = NA,
        kind = TYPE.AUTHOR
     )
     authors2 = data.frame( # author --mail
         name = c("Fritz fritz@example.org", "georg", "Hans", "udo"),
         type = TYPE.AUTHOR,
         artifact.type = NA,
         kind = TYPE.AUTHOR
     )
    issues = data.frame(
         name = c("<issue-51>", "<issue-48>", "<issue-57>", "<issue-2>"),
         type = TYPE.ARTIFACT,
         artifact.type = "issue",
         kind = "issue"
    )
    threads = data.frame(
            name = c("<thread-1>", "<thread-2>", "<thread-8>",
                     "<thread-4>", "<thread-5>", "<thread-6>", "<thread-7>", "<thread-9>", "<thread-3>"),
            type = TYPE.ARTIFACT,
            artifact.type = "thread",
            kind = "thread"
    )
    vertices = plyr::rbind.fill(authors1, issues, authors2, threads)
    ## 2) construct expected edge attributes
    network.expected.data = data.frame(
                  from = c("Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Karl", "Max", # issue
                           "Olaf", "Olaf", "Olaf", "Thomas", "Thomas",
                           "Björn", "Björn", "Björn",  "Fritz fritz@example.org", "georg", "Hans", "Hans", "Hans", # mail
                           "Hans", "Hans", "Hans", "Hans", "Olaf", "Olaf", "Thomas", "udo"),
                  to   = c("<issue-51>", "<issue-48>", "<issue-51>", "<issue-51>", "<issue-51>", "<issue-51>",
                           "<issue-57>", "<issue-57>", "<issue-2>", "<issue-57>", "<issue-48>", "<issue-51>",
                           "<issue-51>", "<issue-48>", "<issue-48>", "<thread-1>",
                           "<thread-2>", "<thread-8>", "<thread-4>", "<thread-5>", "<thread-6>", "<thread-6>",
                           "<thread-6>", "<thread-6>", "<thread-6>", "<thread-6>", "<thread-7>", "<thread-8>",
                           "<thread-9>", "<thread-9>", "<thread-3>"),
                  date = get.date.from.string(c("2016-07-12 16:05:47", "2016-07-14 17:42:52", "2016-08-31 18:21:48", # issue
                                                "2016-10-13 15:33:56", "2016-12-06 14:03:42", "2016-12-07 15:53:02",
                                                "2017-02-20 22:25:41", "2017-03-02 17:30:10", "2013-05-05 23:28:57",
                                                "2017-05-23 12:32:39", "2016-07-27 22:25:25", "2016-10-05 01:07:46",
                                                "2016-12-07 15:37:21", "2016-07-14 02:03:14", "2016-07-15 08:37:57",
                                                "2004-10-09 18:38:13", "2005-02-09 18:49:49", "2016-07-12 15:58:40", # mail
                                                "2010-07-12 11:05:35", "2010-07-12 12:05:34", "2010-07-12 12:05:40",
                                                "2010-07-12 12:05:41", "2010-07-12 12:05:42", "2010-07-12 12:05:43",
                                                "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2010-07-12 12:05:46",
                                                "2016-07-12 15:58:50", "2016-07-12 16:05:37", "2016-07-12 16:04:40",
                                                "2010-07-12 10:05:36")),
                  artifact.type = c(rep("Issue", 15), rep("Mail", 16)),
                  issue.id = c("<issue-51>", "<issue-48>", "<issue-51>", "<issue-51>", "<issue-51>",
                               "<issue-51>", "<issue-57>", "<issue-57>", "<issue-2>", "<issue-57>",
                               "<issue-48>", "<issue-51>", "<issue-51>", "<issue-48>", "<issue-48>",
                               rep(NA,16)),
                  event.name = c("commented", "commented", "commented", "commented", "commented",
                                 "commented", "commented", "commented", "commented", "commented",
                                 "commented", "commented", "commented", "commented", "commented",
                                 rep(NA, 16)),
                  type = TYPE.EDGES.INTER,
                  weight = 1,
                  relation = c(rep("issue", 15), rep("mail", 16)),
                  message.id = c(rep(NA, 15),
                                 "<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>", "<1107974989.17910.6.camel@jmcmullan>",
                                 "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                 "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>", "<dfhglkjdgjkhnwrd@mail.gmail.com>",
                                 "<hans1@mail.gmail.com>", "<hans2@mail.gmail.com>","<hans3@mail.gmail.com>",
                                 "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>", "<hans6@mail.gmail.com>",
                                 "<hans7@mail.gmail.com>", "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                 "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>", "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                 "<asddghdswqeasdasd@mail.gmail.com>"),
                  thread = c(rep(NA, 15),
                             "<thread-1>", "<thread-2>", "<thread-8>", "<thread-4>", "<thread-5>", "<thread-6>",
                             "<thread-6>", "<thread-6>", "<thread-6>", "<thread-6>", "<thread-6>", "<thread-7>",
                             "<thread-8>", "<thread-9>", "<thread-9>", "<thread-3>")
    )
    ## 3) construct expected network
    network.expected = igraph::graph.data.frame(network.expected.data, directed = net.conf$get.value("author.directed"), vertices = vertices)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the multi network for the feature artifact with author.relation = c('cochange', 'mail') and artifact.
          relation = c('cochange', 'issue').", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = c("cochange", "mail"), artifact.relation = c("cochange", "issue")))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.multi.network()

    ## build expected network
    vertices = data.frame(
        name = c("Björn", "Olaf", "Karl", "Thomas", "udo", "Fritz fritz@example.org", "georg", "Hans",
                 "Base_Feature", "foo", "A", "<issue-2>", "<issue-48>", "<issue-51>", "<issue-57>"),
        kind = c(rep(TYPE.AUTHOR, 8), rep("feature", 3), rep("issue", 4)),
        type = c(rep(TYPE.AUTHOR, 8), rep(TYPE.ARTIFACT, 7)),
        artifact.type = c(rep(NA, 8), rep("feature", 3), rep("issue", 4))
    )
    row.names(vertices) = c("Björn", "Olaf", "Karl", "Thomas", "udo", "Fritz fritz@example.org", "georg", "Hans",
                            "Base_Feature", "foo", "A", "<issue-2>", "<issue-48>", "<issue-51>", "<issue-57>")

    edges = data.frame(from = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl", # author cochange
                                "Björn", "Björn", "Olaf", "Olaf", # author mail
                                "Base_Feature", "Base_Feature",   # artifact cochange
                                "Björn", "Olaf", "Olaf", "Karl", "Thomas", "Thomas", # bipartite cochange
                                rep("Björn",8), rep("Olaf",3), "Karl", "Thomas", "Thomas"), # bipartite issue
                       to = c("Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas", "Thomas", # author cochange
                              "Olaf", "Olaf", "Thomas", "Thomas", # author mail
                              "foo", "foo", # artifact cochange
                              "A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "foo", # bipartite cochange
                              "<issue-51>", "<issue-48>", rep("<issue-51>", 4), rep("<issue-57>", 2), "<issue-48>",
                              rep("<issue-51>", 2), "<issue-2>", rep("<issue-48>", 2)), # bipartite issue
                       date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41", # author cochange
                                                     "2016-07-12 16:06:10", "2016-07-12 16:05:41", "2016-07-12 16:06:32",
                                                     "2016-07-12 16:06:10", "2016-07-12 16:06:32",
                                                     "2016-07-12 15:58:40", "2016-07-12 15:58:50", "2016-07-12 16:04:40",
                                                     "2016-07-12 16:05:37",
                                                     "2016-07-12 16:06:32", "2016-07-12 16:06:32",                        # artifact cochange
                                                     "2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41", # bipartite cochange
                                                     "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32",
                                                     "2016-07-12 16:05:47", "2016-07-14 17:42:52", "2016-08-31 18:21:48", # bipartite issue
                                                     "2016-10-13 15:33:56", "2016-12-06 14:03:42", "2016-12-07 15:53:02",
                                                     "2017-02-20 22:25:41", "2017-03-02 17:30:10", "2016-07-27 22:25:25",
                                                     "2016-10-05 01:07:46", "2016-12-07 15:37:21", "2013-05-05 23:28:57",
                                                     "2016-07-14 02:03:14", "2016-07-15 08:37:57")),
                        hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338", # author cochange
                                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                 "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                                 "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526",
                                 NA, NA, NA, NA,                                                                         # author mail
                                 "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526", # artifact cochange
                                 "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338", # bipartite cochange
                                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                 "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526",
                                 rep(NA, 14)),                                                                           # bipartite issue
                        file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c", # author cochange
                                 NA, NA, NA, NA,
                                 "test2.c", "test2.c",                                                                 # artifact cochange
                                 "test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c",                       # bipartite cochange
                                 rep(NA, 14)),
                        artifact.type = c(rep("Feature", 8),
                                          rep("Mail", 4),
                                          rep("Feature", 2),
                                          rep("Feature", 6),
                                          rep("Issue", 14)),
                        artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", # author cochange
                                     "Base_Feature",
                                     rep(NA, 4),
                                     "Base_Feature", "foo", # bipartite cochange
                                     "A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "foo", # bipartite cochange
                                     rep(NA, 14)),
                        weight = 1,
                        type = c(rep(TYPE.EDGES.INTRA, 14), rep(TYPE.EDGES.INTER, 20)),
                        relation = c(rep("cochange", 8),
                                     rep("mail", 4),
                                     rep("cochange", 2),
                                     rep("cochange", 6),
                                     rep("issue", 14)),
                        message.id = c(rep(NA, 8),
                                       "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                       "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                       "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                       "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>",
                                       rep(NA, 22)),
                        thread = c(rep(NA, 8),
                                   "<thread-8>", "<thread-8>", "<thread-9>", "<thread-9>",
                                   rep(NA, 22)),
                        issue.id = c(rep(NA, 20),
                                     "<issue-51>", "<issue-48>","<issue-51>", "<issue-51>", "<issue-51>",
                                     "<issue-51>", "<issue-57>", "<issue-57>", "<issue-48>", "<issue-51>", "<issue-51>",
                                     "<issue-2>",  "<issue-48>", "<issue-48>"),
                        event.name = c(rep(NA, 20), rep("commented", 14))
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
