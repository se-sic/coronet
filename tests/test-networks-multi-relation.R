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
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018-2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
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
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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
                      artifact.type = c(rep("Feature", 8), # cochange
                                        rep("Mail", 4)), # mail
                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                               "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                               "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526",
                               rep(NA, 4)),
                      file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c",
                               rep(NA, 4)),
                      artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature",
                                   rep(NA, 4)),
                      weight = 1,
                      type = TYPE.EDGES.INTRA,
                      relation = c(rep("cochange", 8),
                                   rep("mail", 4)),
                      message.id = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                     "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                     "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                     "<65a1sf31sagd684dfv31@mail.gmail.com>", "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"),
                      thread = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                 "<thread-13#8>", "<thread-13#8>", "<thread-13#9>", "<thread-13#9>")
    )

    ## build expected network
    network.expected = igraph::graph.data.frame(data, vertices = authors,
                                                directed = net.conf$get.value("author.directed"))

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the bipartite network for the feature artifact with author.relation = c('cochange', 'issue') and artifact.
          relation = c('issue', 'mail').", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
     )
     authors2 = data.frame( # author --mail
         name = c("Fritz fritz@example.org", "georg", "Hans", "udo"),
         kind = TYPE.AUTHOR,
         type = TYPE.AUTHOR
     )
    issues = data.frame(
         name = c("<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                  "<issue-github-1>", "<issue-github-6>", "<issue-github-3>", "<issue-github-4>"),
         kind = "Issue",
         type = TYPE.ARTIFACT
    )
    threads = data.frame(
            name = c("<thread-13#1>", "<thread-42#2>", "<thread-13#8>",
                     "<thread-42#4>", "<thread-42#5>", "<thread-42#6>", "<thread-42#7>", "<thread-13#9>", "<thread-13#3>"),
            kind = "MailThread",
            type = TYPE.ARTIFACT
    )
    vertices = plyr::rbind.fill(authors1, issues, authors2, threads)
    ## 2) construct expected edge attributes (data sorted by 'author.name')
    network.expected.data = data.frame(
                  from = c("Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Karl", "Max", # issue
                           "Max", "Max", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Thomas", "Thomas", "Thomas",
                           "Björn", "Björn", "Björn",  "Fritz fritz@example.org", "georg", "Hans", "Hans", "Hans", # mail
                           "Hans", "Hans", "Hans", "Hans", "Olaf", "Olaf", "Thomas", "udo"),
                  to   = c("<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", # issue
                           "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                           "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>",
                           "<issue-github-3>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                           "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                           "<issue-github-1>", "<issue-github-4>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>",
                           "<thread-13#1>", "<thread-42#2>", "<thread-13#8>", "<thread-42#4>", "<thread-42#5>", "<thread-42#6>", "<thread-42#6>", # mail
                           "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#7>", "<thread-13#8>", "<thread-13#9>",
                           "<thread-13#9>", "<thread-13#3>"),
                  date = get.date.from.string(c("2013-05-05 21:46:30", "2013-05-05 21:49:21", "2013-05-05 21:49:34", # issue
                                                "2013-05-06 01:04:34", "2013-05-25 03:48:41", "2013-05-25 04:08:07",
                                                "2016-07-12 14:59:25", "2016-07-12 16:02:30", "2016-07-12 16:06:01",
                                                "2016-07-15 19:55:39", "2017-05-23 12:32:39", "2016-07-12 15:59:59",
                                                "2016-07-15 20:07:47", "2016-07-27 20:12:08", "2016-07-28 06:27:52",
                                                "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                                "2013-06-01 06:50:26", "2016-07-12 16:01:01", "2016-07-12 16:02:02",
                                                "2013-04-21 23:52:09", "2016-07-12 15:59:25", "2016-07-12 16:03:59",
                                                "2004-10-09 18:38:13", "2005-02-09 18:49:49", "2016-07-12 15:58:40", # mail
                                                "2010-07-12 11:05:35", "2010-07-12 12:05:34", "2010-07-12 12:05:40",
                                                "2010-07-12 12:05:41", "2010-07-12 12:05:42", "2010-07-12 12:05:43",
                                                "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2010-07-12 12:05:46",
                                                "2016-07-12 15:58:50", "2016-07-12 16:05:37", "2016-07-12 16:04:40",
                                                "2010-07-12 10:05:36")),
                  artifact.type = c(rep("IssueEvent", 24), rep("Mail", 16)),
                  message.id = c(rep(NA, 24),
                                 "<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>", "<1107974989.17910.6.camel@jmcmullan>",
                                 "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                 "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>", "<dfhglkjdgjkhnwrd@mail.gmail.com>",
                                 "<hans1@mail.gmail.com>", "<hans2@mail.gmail.com>", "<hans3@mail.gmail.com>",
                                 "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>", "<hans6@mail.gmail.com>",
                                 "<hans7@mail.gmail.com>", "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                 "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>", "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                 "<asddghdswqeasdasd@mail.gmail.com>"),
                  thread = c(rep(NA, 24),
                             "<thread-13#1>", "<thread-42#2>", "<thread-13#8>", "<thread-42#4>", "<thread-42#5>", "<thread-42#6>",
                             "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#7>",
                             "<thread-13#8>", "<thread-13#9>", "<thread-13#9>", "<thread-13#3>"),
                  issue.id = c("<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", # issue
                               "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                               "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>",
                               "<issue-github-3>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                               "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                               "<issue-github-1>", "<issue-github-4>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>",
                               rep(NA,16)),
                  event.name = c(rep("commented", 24),
                                 rep(NA, 16)),
                  weight = 1,
                  type = TYPE.EDGES.INTER,
                  relation = c(rep("issue", 24), rep("mail", 16))
    )

    ## 3) build expected network
    network.expected = igraph::graph.data.frame(network.expected.data, vertices = vertices,
                                                directed = net.conf$get.value("author.directed"))

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the multi network for the feature artifact with author.relation = c('cochange', 'mail') and artifact.
          relation = c('cochange', 'issue').", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = c("cochange", "mail"), artifact.relation = c("cochange", "issue")))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.multi.network()

    ## construct expected network:
     ## 1) construct expected vertices
    vertices = data.frame(
        name = c("Björn", "Olaf", "Karl", "Thomas", "udo", "Fritz fritz@example.org", "georg", "Hans",
                 "Base_Feature", "foo", "A", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-github-1>", "<issue-github-3>",
                 "<issue-github-4>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>"),
        kind = c(rep(TYPE.AUTHOR, 8), rep("Feature", 3), rep("Issue", 7)),
        type = c(rep(TYPE.AUTHOR, 8), rep(TYPE.ARTIFACT, 10))
    )
    row.names(vertices) = c("Björn", "Olaf", "Karl", "Thomas", "udo", "Fritz fritz@example.org", "georg", "Hans",
                            "Base_Feature", "foo", "A", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-github-1>", "<issue-github-3>",
                            "<issue-github-4>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>")

    ## 2) construct expected edge attributes (data sorted by 'author.name')
    edges = data.frame(from = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl", # author cochange
                                "Björn", "Björn", "Olaf", "Olaf", # author mail
                                "Base_Feature", "Base_Feature",   # artifact cochange
                                "Björn", "Olaf", "Olaf", "Karl", "Thomas", "Thomas", # bipartite cochange
                                "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", # bipartite issue
                                "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Thomas", "Thomas", "Thomas"),
                       to = c("Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas", "Thomas", # author cochange
                              "Olaf", "Olaf", "Thomas", "Thomas", # author mail
                              "foo", "foo", # artifact cochange
                              "A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "foo", # bipartite cochange
                              "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", # bipartite issue
                              "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>",
                              "<issue-github-6>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                              "<issue-github-1>", "<issue-github-4>", "<issue-github-3>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>"),
                       date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41", # author cochange
                                                     "2016-07-12 16:06:10", "2016-07-12 16:05:41", "2016-07-12 16:06:32",
                                                     "2016-07-12 16:06:10", "2016-07-12 16:06:32",
                                                     "2016-07-12 15:58:40", "2016-07-12 15:58:50", "2016-07-12 16:04:40",
                                                     "2016-07-12 16:05:37",
                                                     "2016-07-12 16:06:32", "2016-07-12 16:06:32",                        # artifact cochange
                                                     "2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41", # bipartite cochange
                                                     "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32",
                                                     "2013-05-05 21:46:30", "2013-05-05 21:49:21", "2013-05-05 21:49:34", # bipartite issue
                                                     "2013-05-06 01:04:34", "2013-05-25 03:48:41", "2013-05-25 04:08:07", "2016-07-12 14:59:25",
                                                     "2016-07-12 16:02:30", "2016-07-12 16:06:01", "2016-07-15 19:55:39", "2017-05-23 12:32:39",
                                                     "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                                     "2013-06-01 06:50:26", "2016-07-12 16:01:01", "2016-07-12 16:02:02",
                                                     "2016-07-12 15:59:59", "2013-04-21 23:52:09", "2016-07-12 15:59:25",
                                                     "2016-07-12 16:03:59")),
                        artifact.type = c(rep("Feature", 8), rep("Mail", 4), rep("Feature", 2), rep("Feature", 6),
                                          rep("IssueEvent", 21)),
                        hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338", # author cochange
                                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                 "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526",
                                 "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526",
                                 NA, NA, NA, NA,                                                                         # author mail
                                 "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526", # artifact cochange
                                 "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338", # bipartite cochange
                                 "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                 "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526",
                                 rep(NA, 21)),                                                                           # bipartite issue
                        file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c", "test3.c", "test2.c", # author cochange
                                 NA, NA, NA, NA,
                                 "test2.c", "test2.c",                                                                 # artifact cochange
                                 "test.c", "test.c", "test2.c", "test3.c", "test2.c", "test2.c",                       # bipartite cochange
                                 rep(NA, 21)),
                        artifact = c("A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", "Base_Feature", # author cochange
                                     "Base_Feature",
                                     rep(NA, 4),
                                     "Base_Feature", "foo", # bipartite cochange
                                     "A", "A", "Base_Feature", "Base_Feature", "Base_Feature", "foo", # bipartite cochange
                                     rep(NA, 21)),
                        weight = 1,
                        type = c(rep(TYPE.EDGES.INTRA, 14), rep(TYPE.EDGES.INTER, 27)),
                        relation = c(rep("cochange", 8), rep("mail", 4), rep("cochange", 2), rep("cochange", 6),
                                     rep("issue", 21)),
                        message.id = c(rep(NA, 8),
                                       "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                       "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                       "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                       "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>",
                                       rep(NA, 29)),
                        thread = c(rep(NA, 8),
                                   "<thread-13#8>", "<thread-13#8>", "<thread-13#9>", "<thread-13#9>",
                                   rep(NA, 29)),
                        issue.id = c(rep(NA, 20),
                                     "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", # bipartite issue
                                     "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>",
                                     "<issue-github-6>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                                     "<issue-github-1>", "<issue-github-4>", "<issue-github-3>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>"),
                        event.name = c(rep(NA, 20), rep("commented", 21))
     )

    ## 3) build expected network
    network.expected = igraph::graph.data.frame(edges, vertices = vertices,
                                                directed = net.conf$get.value("author.directed"))

    compare.networks(network.expected, network.built)
})

test_that("Construction of the multi-artifact bipartite network with artifact relations 'cochange' and 'issue'", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf.cochange = NetworkConf$new()
    net.conf.cochange$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))
    net.conf.issue = NetworkConf$new()
    net.conf.issue$update.values(updated.values = list(author.relation = "issue", artifact.relation = "issue"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder.cochange = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.cochange)
    network.builder.issue = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.issue)

    ## build a multi-artifact network by merging two different artifact networks
    net.cochange = network.builder.cochange$get.bipartite.network()
    net.issue = network.builder.issue$get.bipartite.network()

    net.combined = merge.networks(list(net.cochange, net.issue))

    ## build expected network
    vertices = data.frame(name = c("Björn", "Karl", "Olaf", "Thomas", "A", "Base_Feature",
                                   "foo", "Max", "<issue-jira-ZEPPELIN-328>",
                                   "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                                   "<issue-github-1>", "<issue-github-6>", "<issue-github-3>",
                                   "<issue-github-4>"),
                          kind = c(rep(TYPE.AUTHOR, 4), rep("Feature", 3), TYPE.AUTHOR, rep("Issue", 7)),
                          type = c(rep(TYPE.AUTHOR, 4), rep(TYPE.ARTIFACT, 3), TYPE.AUTHOR, rep(TYPE.ARTIFACT, 7))
    )
    row.names(vertices) = c("Björn", "Karl", "Olaf", "Thomas", "A", "Base_Feature",
                            "foo", "Max", "<issue-jira-ZEPPELIN-328>",
                            "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                            "<issue-github-1>", "<issue-github-6>", "<issue-github-3>",
                            "<issue-github-4>")

    edges = data.frame(
        from = c("Björn",  "Karl",   "Olaf",   "Olaf",   "Thomas", "Thomas", "Björn",
                 "Björn",  "Björn",  "Björn",  "Björn",  "Björn",  "Björn",  "Björn",
                 "Björn",  "Björn",  "Björn",  "Karl",   "Max",    "Max",    "Max",
                 "Olaf",   "Olaf",   "Olaf",   "Olaf",   "Olaf",   "Olaf",   "Thomas",
                 "Thomas", "Thomas"),
        to = c("A",                         "Base_Feature",              "A",
               "Base_Feature",             "Base_Feature",             "foo",
               "<issue-jira-ZEPPELIN-328>","<issue-jira-ZEPPELIN-328>","<issue-jira-ZEPPELIN-328>",
               "<issue-jira-ZEPPELIN-328>","<issue-jira-ZEPPELIN-328>","<issue-jira-ZEPPELIN-328>",
               "<issue-github-2>",         "<issue-jira-ZEPPELIN-332>","<issue-github-1>",
               "<issue-jira-ZEPPELIN-332>","<issue-github-6>",         "<issue-github-3>",
               "<issue-jira-ZEPPELIN-332>","<issue-jira-ZEPPELIN-332>","<issue-jira-ZEPPELIN-332>",
               "<issue-jira-ZEPPELIN-328>","<issue-jira-ZEPPELIN-328>","<issue-jira-ZEPPELIN-328>",
               "<issue-jira-ZEPPELIN-328>","<issue-github-1>",         "<issue-github-4>",
               "<issue-jira-ZEPPELIN-328>","<issue-github-1>",         "<issue-github-6>"),
        date = get.date.from.string(c("2016-07-12 15:58:59 UTC", "2016-07-12 16:06:10 UTC",
                                      "2016-07-12 16:00:45 UTC", "2016-07-12 16:05:41 UTC",
                                      "2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC",
                                      "2013-05-05 21:46:30 UTC", "2013-05-05 21:49:21 UTC",
                                      "2013-05-05 21:49:34 UTC", "2013-05-06 01:04:34 UTC",
                                      "2013-05-25 03:48:41 UTC", "2013-05-25 04:08:07 UTC",
                                      "2016-07-12 14:59:25 UTC", "2016-07-12 16:02:30 UTC",
                                      "2016-07-12 16:06:01 UTC", "2016-07-15 19:55:39 UTC",
                                      "2017-05-23 12:32:39 UTC", "2016-07-12 15:59:59 UTC",
                                      "2016-07-15 20:07:47 UTC", "2016-07-27 20:12:08 UTC",
                                      "2016-07-28 06:27:52 UTC", "2013-05-25 03:25:06 UTC",
                                      "2013-05-25 06:06:53 UTC", "2013-05-25 06:22:23 UTC",
                                      "2013-06-01 06:50:26 UTC", "2016-07-12 16:01:01 UTC",
                                      "2016-07-12 16:02:02 UTC", "2013-04-21 23:52:09 UTC",
                                      "2016-07-12 15:59:25 UTC", "2016-07-12 16:03:59 UTC")),
        artifact.type = c(rep("Feature", 6), rep("IssueEvent", 24)),
        hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                 "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                 "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526",
                 rep(NA, 24)),
        file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c", rep(NA, 24)),
        artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo", rep(NA, 24)),
        weight = c(rep(1, 30)),
        type = c(rep("Bipartite", 30)),
        relation = c(rep("cochange", 6), rep("issue", 24)),
        issue.id = c(NA,                          NA,                          NA,
                     NA,                          NA,                          NA,
                     "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                     "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                     "<issue-github-2>",          "<issue-jira-ZEPPELIN-332>", "<issue-github-1>",
                     "<issue-jira-ZEPPELIN-332>", "<issue-github-6>",          "<issue-github-3>",
                     "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                     "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                     "<issue-jira-ZEPPELIN-328>", "<issue-github-1>",          "<issue-github-4>",
                     "<issue-jira-ZEPPELIN-328>", "<issue-github-1>",          "<issue-github-6>"),
        event.name = c(rep(NA, 6), rep("commented", 24))
    )

    net.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    compare.networks(net.expected, net.combined)

})

test_that("Construction of the multi-artifact bipartite network with artifact relations 'cochange' and 'mail'", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf.cochange = NetworkConf$new()
    net.conf.cochange$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))
    net.conf.mail = NetworkConf$new()
    net.conf.mail$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder.cochange = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.cochange)
    network.builder.mail = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.mail)

    ## build a multi-artifact network by merging two different artifact networks
    net.cochange = network.builder.cochange$get.bipartite.network()
    net.mail = network.builder.mail$get.bipartite.network()

    net.combined = merge.networks(list(net.cochange, net.mail))

    ## build expected network
    vertices = data.frame(name = c("Björn",                  "Karl",                   "Olaf",
                                   "Thomas",                 "A",                      "Base_Feature",
                                   "foo",                    "Fritz fritz@example.org","georg",
                                   "Hans",                   "udo",                    "<thread-13#1>",
                                   "<thread-42#2>",             "<thread-13#8>",             "<thread-42#4>",
                                   "<thread-42#5>",             "<thread-42#6>",             "<thread-42#7>",
                                   "<thread-13#9>",             "<thread-13#3>"),
                          kind = c(rep(TYPE.AUTHOR, 4), rep("Feature", 3), rep(TYPE.AUTHOR, 4), rep("MailThread", 9)),
                          type = c(rep(TYPE.AUTHOR, 4), rep(TYPE.ARTIFACT, 3), rep(TYPE.AUTHOR, 4), rep(TYPE.ARTIFACT, 9))
    )
    row.names(vertices) = c("Björn",                  "Karl",                   "Olaf",
                            "Thomas",                 "A",                      "Base_Feature",
                            "foo",                    "Fritz fritz@example.org","georg",
                            "Hans",                   "udo",                    "<thread-13#1>",
                            "<thread-42#2>",             "<thread-13#8>",             "<thread-42#4>",
                            "<thread-42#5>",             "<thread-42#6>",             "<thread-42#7>",
                            "<thread-13#9>",             "<thread-13#3>")

    edges = data.frame(
           from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas", "Thomas", "Björn", "Björn",
                    "Björn", "Fritz fritz@example.org", "georg", "Hans", "Hans", "Hans",
                    "Hans", "Hans", "Hans", "Hans", "Olaf", "Olaf", "Thomas", "udo"),
           to = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo", "<thread-13#1>",
                  "<thread-42#2>", "<thread-13#8>", "<thread-42#4>", "<thread-42#5>", "<thread-42#6>",
                  "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>",
                  "<thread-42#7>", "<thread-13#8>", "<thread-13#9>", "<thread-13#9>", "<thread-13#3>"),
           date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                         "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:32",
                                         "2004-10-09 18:38:13", "2005-02-09 18:49:49", "2016-07-12 15:58:40",
                                         "2010-07-12 11:05:35", "2010-07-12 12:05:34", "2010-07-12 12:05:40",
                                         "2010-07-12 12:05:41", "2010-07-12 12:05:42", "2010-07-12 12:05:43",
                                         "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2010-07-12 12:05:46",
                                         "2016-07-12 15:58:50", "2016-07-12 16:05:37", "2016-07-12 16:04:40",
                                         "2010-07-12 10:05:36")),
           artifact.type = c(rep("Feature", 6), rep("Mail", 16)),
           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                    "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                    "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526",
                    rep(NA, 16)),
           file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c", rep(NA, 16)),
           artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo", rep(NA, 16)),
           weight = rep(1,22),
           type = rep("Bipartite", 22),
           relation = c(rep("cochange", 6), rep("mail", 16)),
           message.id = c(rep(NA, 6), "<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>",
                          "<1107974989.17910.6.camel@jmcmullan>", "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                          "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>", "<dfhglkjdgjkhnwrd@mail.gmail.com>",
                          "<hans1@mail.gmail.com>", "<hans2@mail.gmail.com>", "<hans3@mail.gmail.com>",
                          "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>", "<hans6@mail.gmail.com>",
                          "<hans7@mail.gmail.com>", "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                          "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>",
                          "<65a1sf31sagd684dfv31@mail.gmail.com>", "<asddghdswqeasdasd@mail.gmail.com>"
                          ),
           thread = c(rep(NA, 6), "<thread-13#1>", "<thread-42#2>", "<thread-13#8>", "<thread-42#4>",
                      "<thread-42#5>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>",
                      "<thread-42#6>", "<thread-42#6>", "<thread-42#7>", "<thread-13#8>", "<thread-13#9>",
                      "<thread-13#9>", "<thread-13#3>")
    )

    net.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    compare.networks(net.expected, net.combined)

})

test_that("Construction of the multi-artifact bipartite network with artifact relations 'issue' and 'mail'", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf.issue = NetworkConf$new()
    net.conf.issue$update.values(updated.values = list(author.relation = "issue", artifact.relation = "issue"))
    net.conf.mail = NetworkConf$new()
    net.conf.mail$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder.issue = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.issue)
    network.builder.mail = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.mail)

    ## build a multi-artifact network by merging two different artifact networks
    net.issue = network.builder.issue$get.bipartite.network()
    net.mail = network.builder.mail$get.bipartite.network()

    net.combined = merge.networks(list(net.issue, net.mail))

    ## build expected network
    vertices = data.frame(name = c("Björn", "Karl", "Max", "Olaf", "Thomas",
                                   "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                                   "<issue-github-1>", "<issue-github-6>", "<issue-github-3>", "<issue-github-4>",
                                   "Fritz fritz@example.org", "georg", "Hans", "udo", "<thread-13#1>",
                                   "<thread-42#2>", "<thread-13#8>", "<thread-42#4>", "<thread-42#5>", "<thread-42#6>",
                                   "<thread-42#7>", "<thread-13#9>", "<thread-13#3>"),
                          kind = c(rep("Author", 5), rep("Issue", 7), rep("Author", 4), rep("MailThread", 9)),
                          type = c(rep("Author", 5), rep("Artifact", 7), rep("Author", 4), rep("Artifact", 9))
                          )
    row.names(vertices) = c("Björn", "Karl", "Max", "Olaf", "Thomas",
                            "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                            "<issue-github-1>", "<issue-github-6>", "<issue-github-3>", "<issue-github-4>",
                            "Fritz fritz@example.org", "georg", "Hans", "udo", "<thread-13#1>",
                            "<thread-42#2>", "<thread-13#8>", "<thread-42#4>", "<thread-42#5>", "<thread-42#6>",
                            "<thread-42#7>", "<thread-13#9>", "<thread-13#3>")

    edges = data.frame(
           from = c("Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn",
                    "Björn", "Björn", "Karl", "Max", "Max", "Max", "Olaf", "Olaf", "Olaf", "Olaf",
                    "Olaf", "Olaf", "Thomas", "Thomas", "Thomas", "Björn", "Björn", "Björn",
                    "Fritz fritz@example.org", "georg", "Hans", "Hans", "Hans", "Hans", "Hans",
                    "Hans", "Hans", "Olaf", "Olaf", "Thomas", "udo"),
           to = c("<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>",
                  "<issue-github-6>", "<issue-github-3>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                  "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>",
                  "<issue-github-4>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>",
                  "<issue-github-6>", "<thread-13#1>", "<thread-42#2>", "<thread-13#8>", "<thread-42#4>",
                  "<thread-42#5>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>",
                  "<thread-42#6>", "<thread-42#7>", "<thread-13#8>", "<thread-13#9>", "<thread-13#9>", "<thread-13#3>"),
           date = get.date.from.string(c("2013-05-05 21:46:30", "2013-05-05 21:49:21", "2013-05-05 21:49:34",
                                         "2013-05-06 01:04:34", "2013-05-25 03:48:41", "2013-05-25 04:08:07",
                                         "2016-07-12 14:59:25", "2016-07-12 16:02:30", "2016-07-12 16:06:01",
                                         "2016-07-15 19:55:39", "2017-05-23 12:32:39", "2016-07-12 15:59:59",
                                         "2016-07-15 20:07:47", "2016-07-27 20:12:08", "2016-07-28 06:27:52",
                                         "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                         "2013-06-01 06:50:26", "2016-07-12 16:01:01", "2016-07-12 16:02:02",
                                         "2013-04-21 23:52:09", "2016-07-12 15:59:25", "2016-07-12 16:03:59",
                                         "2004-10-09 18:38:13", "2005-02-09 18:49:49", "2016-07-12 15:58:40",
                                         "2010-07-12 11:05:35", "2010-07-12 12:05:34", "2010-07-12 12:05:40",
                                         "2010-07-12 12:05:41", "2010-07-12 12:05:42", "2010-07-12 12:05:43",
                                         "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2010-07-12 12:05:46",
                                         "2016-07-12 15:58:50", "2016-07-12 16:05:37", "2016-07-12 16:04:40",
                                         "2010-07-12 10:05:36")),
           artifact.type = c(rep("IssueEvent", 24), rep("Mail", 16)),
           issue.id = c("<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                        "<issue-github-2>", "<issue-jira-ZEPPELIN-332>", "<issue-github-1>",
                        "<issue-jira-ZEPPELIN-332>", "<issue-github-6>", "<issue-github-3>",
                        "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-4>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-github-1>", "<issue-github-6>", rep(NA, 16)),
           event.name = c(rep("commented", 24), rep(NA, 16)),
           weight = rep(1, 40),
           type = rep("Bipartite", 40),
           relation = c(rep("issue", 24), rep("mail", 16)),
           message.id = c(rep(NA, 24),
                          "<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>", "<1107974989.17910.6.camel@jmcmullan>",
                          "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                          "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>", "<dfhglkjdgjkhnwrd@mail.gmail.com>",
                          "<hans1@mail.gmail.com>", "<hans2@mail.gmail.com>", "<hans3@mail.gmail.com>",
                          "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>", "<hans6@mail.gmail.com>",
                          "<hans7@mail.gmail.com>", "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                          "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>",
                          "<65a1sf31sagd684dfv31@mail.gmail.com>", "<asddghdswqeasdasd@mail.gmail.com>"
                          ),
           thread = c(rep(NA, 24), "<thread-13#1>", "<thread-42#2>", "<thread-13#8>", "<thread-42#4>", "<thread-42#5>",
                      "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>",
                      "<thread-42#7>", "<thread-13#8>", "<thread-13#9>", "<thread-13#9>", "<thread-13#3>")
    )

    net.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    compare.networks(net.expected, net.combined)

})

test_that("Construction of the multi-artifact bipartite network with artifact relations 'cochange', 'issue', and 'mail'", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf.cochange = NetworkConf$new()
    net.conf.cochange$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))
    net.conf.issue = NetworkConf$new()
    net.conf.issue$update.values(updated.values = list(author.relation = "issue", artifact.relation = "issue"))
    net.conf.mail = NetworkConf$new()
    net.conf.mail$update.values(updated.values = list(author.relation = "mail", artifact.relation = "mail"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder.cochange = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.cochange)
    network.builder.issue = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.issue)
    network.builder.mail = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf.mail)

    ## build a multi-artifact network by merging two different artifact networks
    net.cochange = network.builder.cochange$get.bipartite.network()
    net.issue = network.builder.issue$get.bipartite.network()
    net.mail = network.builder.mail$get.bipartite.network()

    net.combined = merge.networks(list(net.cochange, net.issue, net.mail))

    ## build expected network
    vertices = data.frame(name = c("Björn", "Karl", "Olaf", "Thomas", "A", "Base_Feature", "foo",
                                   "Max", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>",
                                   "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-github-6>",
                                   "<issue-github-3>", "<issue-github-4>", "Fritz fritz@example.org",
                                   "georg", "Hans", "udo", "<thread-13#1>", "<thread-42#2>", "<thread-13#8>",
                                   "<thread-42#4>", "<thread-42#5>", "<thread-42#6>", "<thread-42#7>", "<thread-13#9>",
                                   "<thread-13#3>"),
                          kind = c(rep("Author", 4), rep("Feature", 3), "Author",
                                   rep("Issue", 7), rep("Author", 4), rep("MailThread", 9)),
                          type = c(rep("Author", 4), rep("Artifact", 3), "Author",
                                   rep("Artifact", 7), rep("Author", 4), rep("Artifact", 9)))
    row.names(vertices) = c("Björn", "Karl", "Olaf", "Thomas", "A", "Base_Feature", "foo",
                            "Max", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>",
                            "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-github-6>",
                            "<issue-github-3>", "<issue-github-4>", "Fritz fritz@example.org",
                            "georg", "Hans", "udo", "<thread-13#1>", "<thread-42#2>", "<thread-13#8>",
                            "<thread-42#4>", "<thread-42#5>", "<thread-42#6>", "<thread-42#7>", "<thread-13#9>",
                            "<thread-13#3>")

    edges = data.frame(
           from = c("Björn", "Karl", "Olaf", "Olaf", "Thomas", "Thomas", "Björn", "Björn",
                    "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn", "Björn",
                    "Björn", "Karl", "Max", "Max", "Max", "Olaf", "Olaf", "Olaf", "Olaf",
                    "Olaf", "Olaf", "Thomas", "Thomas", "Thomas", "Björn", "Björn", "Björn",
                    "Fritz fritz@example.org", "georg", "Hans", "Hans", "Hans", "Hans", "Hans",
                    "Hans", "Hans", "Olaf", "Olaf", "Thomas", "udo"),
           to = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo", "<issue-jira-ZEPPELIN-328>",
                  "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-github-2>",
                  "<issue-jira-ZEPPELIN-332>", "<issue-github-1>", "<issue-jira-ZEPPELIN-332>",
                  "<issue-github-6>", "<issue-github-3>", "<issue-jira-ZEPPELIN-332>",
                  "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-github-1>", "<issue-github-4>", "<issue-jira-ZEPPELIN-328>",
                  "<issue-github-1>", "<issue-github-6>", "<thread-13#1>", "<thread-42#2>",
                  "<thread-13#8>", "<thread-42#4>", "<thread-42#5>", "<thread-42#6>", "<thread-42#6>",
                  "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#7>",
                  "<thread-13#8>", "<thread-13#9>", "<thread-13#9>", "<thread-13#3>"),
           date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                         "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:32",
                                         "2013-05-05 21:46:30", "2013-05-05 21:49:21", "2013-05-05 21:49:34",
                                         "2013-05-06 01:04:34", "2013-05-25 03:48:41", "2013-05-25 04:08:07",
                                         "2016-07-12 14:59:25", "2016-07-12 16:02:30", "2016-07-12 16:06:01",
                                         "2016-07-15 19:55:39", "2017-05-23 12:32:39", "2016-07-12 15:59:59",
                                         "2016-07-15 20:07:47", "2016-07-27 20:12:08", "2016-07-28 06:27:52",
                                         "2013-05-25 03:25:06", "2013-05-25 06:06:53", "2013-05-25 06:22:23",
                                         "2013-06-01 06:50:26", "2016-07-12 16:01:01", "2016-07-12 16:02:02",
                                         "2013-04-21 23:52:09", "2016-07-12 15:59:25", "2016-07-12 16:03:59",
                                         "2004-10-09 18:38:13", "2005-02-09 18:49:49", "2016-07-12 15:58:40",
                                         "2010-07-12 11:05:35", "2010-07-12 12:05:34", "2010-07-12 12:05:40",
                                         "2010-07-12 12:05:41", "2010-07-12 12:05:42", "2010-07-12 12:05:43",
                                         "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2010-07-12 12:05:46",
                                         "2016-07-12 15:58:50", "2016-07-12 16:05:37", "2016-07-12 16:04:40",
                                         "2010-07-12 10:05:36")),
           artifact.type = c(rep("Feature", 6), rep("IssueEvent", 24), rep("Mail", 16)),
           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                    "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                    "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526",
                    rep(NA, 40)),
           file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c", rep(NA, 40)),
           artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo", rep(NA, 40)),
           weight = rep(1, 46),
           type = rep("Bipartite", 46),
           relation = c(rep("cochange", 6), rep("issue", 24), rep("mail", 16)),
           issue.id = c(rep(NA, 6), "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-github-2>", "<issue-jira-ZEPPELIN-332>",
                        "<issue-github-1>", "<issue-jira-ZEPPELIN-332>", "<issue-github-6>",
                        "<issue-github-3>", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                        "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>",
                        "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>",
                        "<issue-github-4>", "<issue-jira-ZEPPELIN-328>", "<issue-github-1>",
                        "<issue-github-6>", rep(NA, 16)),
           event.name = c(rep(NA, 6), rep("commented", 24), rep(NA, 16)),
           message.id = c(rep(NA, 30), "<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>",
                          "<1107974989.17910.6.camel@jmcmullan>", "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                          "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>", "<dfhglkjdgjkhnwrd@mail.gmail.com>",
                          "<hans1@mail.gmail.com>", "<hans2@mail.gmail.com>", "<hans3@mail.gmail.com>",
                          "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>", "<hans6@mail.gmail.com>",
                          "<hans7@mail.gmail.com>", "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                          "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>",
                          "<65a1sf31sagd684dfv31@mail.gmail.com>", "<asddghdswqeasdasd@mail.gmail.com>"),
           thread = c(rep(NA, 30), "<thread-13#1>", "<thread-42#2>", "<thread-13#8>", "<thread-42#4>",
                      "<thread-42#5>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>", "<thread-42#6>",
                      "<thread-42#6>", "<thread-42#6>", "<thread-42#7>", "<thread-13#8>", "<thread-13#9>",
                      "<thread-13#9>", "<thread-13#3>")
    )

    net.expected = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    compare.networks(net.expected, net.combined)

})
