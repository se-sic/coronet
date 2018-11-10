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
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## All Rights Reserved.


context("Reading functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("Read the raw commit data with the feature artifact.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    commit.data.read = read.commits(proj.conf$get.value("datapath"), proj.conf$get.value("artifact"))

    ## build the expected data.frame
    commit.data.expected = data.frame(commit.id = sprintf("<commit-%s>", c(32712, 32712, 32713, 32713, 32710, 32710, 32714, 32711, 32711)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                        "2016-07-12 16:00:45", "2016-07-12 16:05:41", "2016-07-12 16:05:41",
                                                        "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      author.name = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Thomas", "Thomas"),
                                      author.email = c("bjoern@example.org", "bjoern@example.org", "olaf@example.org",
                                                     "olaf@example.org", "olaf@example.org", "olaf@example.org", "karl@example.org",
                                                     "thomas@example.org", "thomas@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 15:58:59", "2016-07-20 10:00:44",
                                                                              "2016-07-20 10:00:44", "2016-07-12 17:05:55", "2016-07-12 17:05:55",
                                                                              "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      committer.name = c("Björn", "Björn", "Björn", "Björn", "Thomas", "Thomas", "Karl", "Thomas", "Thomas"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org", "bjoern@example.org", "bjoern@example.org",
                                                          "thomas@example.org", "thomas@example.org", "karl@example.org", "thomas@example.org",
                                                          "thomas@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                             "5a5ec9675e98187e1e92561e1888aa6f04faa338", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                             "3a0ed78458b3976243db6829f63eba3eead26774", "3a0ed78458b3976243db6829f63eba3eead26774",
                                             "1143db502761379c2bfcecc2007fc34282e7ee61", "0a1a5c523d835459c42f33e863623138555e2526",
                                             "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                                      added.lines = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                                      deleted.lines = as.integer(c(1, 1, 0, 0, 0, 0, 0, 0, 0)),
                                      diff.size = as.integer(c(2, 2, 1, 1, 1, 1, 1, 1, 1)),
                                      file = c("test.c", "test.c", "test.c", "test.c", "test2.c", "test2.c", "test3.c", "test2.c", "test2.c"),
                                      artifact = c("A", "defined(A)", "A", "defined(A)", "Base_Feature", "Base_Feature", "Base_Feature",
                                                 "Base_Feature", "foo"),
                                      artifact.type = c("Feature", "FeatureExpression", "Feature", "FeatureExpression", "Feature",
                                                      "FeatureExpression", "Feature", "Feature", "Feature"),
                                      artifact.diff.size = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1)))

    ## check the results
    expect_identical(commit.data.read, commit.data.expected, info = "Raw commit data.")

    ## check order of commits (ordered by date?)
    dates = order(commit.data.read[["date"]])
    dates.expected = seq_len(nrow(commit.data.expected))
    expect_identical(dates, dates.expected, info = "Ordering by date.")
})


test_that("Read the raw commit data with the file artifact.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")

    ## read the actual data
    commit.data.read = read.commits(proj.conf$get.value("datapath"), proj.conf$get.value("artifact"))

    ## build the expected data.frame
    commit.data.expected = data.frame(commit.id = sprintf("<commit-%s>", c(32716, 32717, 32718, 32719, 32715)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                                    "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                                      author.name = c("Björn", "Olaf", "Olaf", "Karl", "Thomas"),
                                      author.email = c("bjoern@example.org", "olaf@example.org", "olaf@example.org",
                                                     "karl@example.org", "thomas@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-20 10:00:44", "2016-07-12 17:05:55",
                                                                              "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                                      committer.name = c("Björn", "Björn", "Thomas", "Karl", "Thomas"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org", "thomas@example.org",
                                                        "karl@example.org", "thomas@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                             "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                             "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files = as.integer(c(1, 1, 1, 1, 1)),
                                      added.lines = as.integer(c(1, 1, 1, 1, 1)),
                                      deleted.lines = as.integer(c(1, 0, 0, 0, 0)),
                                      diff.size = as.integer(c(2, 1, 1, 1, 1)),
                                      file = c("test.c", "test.c", "test2.c", "test3.c", "test2.c"),
                                      artifact = c("test.c", "test.c", "test2.c", "test3.c", "test2.c"),
                                      artifact.type = c("File", "File", "File", "File", "File"),
                                      artifact.diff.size = c(1, 1, 1, 1, 1))

    ## check the results
    expect_identical(commit.data.read, commit.data.expected, info = "Raw commit data.")

    ## check order of commits (ordered by date?)
    dates = order(commit.data.read[["date"]])
    dates.expected = seq_len(nrow(commit.data.expected))
    expect_identical(dates, dates.expected, info = "Ordering by date.")
})


test_that("Read the synchronicity data.", {
    ## configuration object
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    synchronicity.data.read = read.synchronicity(proj.conf$get.value("datapath.synchronicity"), proj.conf$get.value("artifact"),
                                                 proj.conf$get.value("synchronicity.time.window"))

    ## build the expected data.frame
    synchronicity.data.expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                                             synchronous = c(TRUE, TRUE, FALSE, FALSE))

    ## check the results
    expect_identical(synchronicity.data.read, synchronicity.data.expected, info = "Synchronicity data.")
})

test_that("Read the mail data.", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    mail.data.read = read.mails(proj.conf$get.value("datapath"))

    ## build the expected data.frame
    ## NOTE: the empty date is set to a real date here in order to build the data.frame without problems
    mail.data.expected = data.frame(author.name = c("Björn", "Björn", "udo", "Fritz fritz@example.org", "georg", "Hans",
                                                  "Hans", "Hans", "Hans", "Hans", "Hans", "Hans", "Thomas", "Björn", "Olaf",
                                                  "Thomas", "Olaf"),
                                    author.email = c("bjoern@example.org", "bjoern@example.org", "udo@example.org",
                                                   "asd@sample.org", "heinz@example.org", "hans1@example.org", "hans1@example.org",
                                                   "hans1@example.org", "hans1@example.org", "hans1@example.org", "hans1@example.org",
                                                   "hans1@example.org", "thomas@example.org", "bjoern@example.org", "olaf@example.org",
                                                   "thomas@example.org", "olaf@example.org"),
                                    message.id = c("<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>", "<1107974989.17910.6.camel@jmcmullan>",
                                                 "<asddghdswqeasdasd@mail.gmail.com>", "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>",
                                                 "<dfhglkjdgjkhnwrd@mail.gmail.com>", "<hans1@mail.gmail.com>", "<hans2@mail.gmail.com>",
                                                 "<hans3@mail.gmail.com>", "<hans4@mail.gmail.com>", "<hans5@mail.gmail.com>",
                                                 "<hans6@mail.gmail.com>", "<hans7@mail.gmail.com>", "<saf54sd4gfasf46asf46@mail.gmail.com>",
                                                 "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                                 "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                                 "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                                 "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"),
                                    date = get.date.from.string(c("2004-10-09 18:38:13", "2005-02-09 18:49:49", "2010-07-12 10:05:36",
                                                           "2010-07-12 11:05:35", "2010-07-12 12:05:34", "2010-07-12 12:05:40",
                                                           "2010-07-12 12:05:41", "2010-07-12 12:05:42", "2010-07-12 12:05:43",
                                                           "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2010-07-12 12:05:46",
                                                           "2016-07-12 15:58:40", "2016-07-12 15:58:40", "2016-07-12 15:58:50",
                                                           "2016-07-12 16:04:40", "2016-07-12 16:05:37")),
                                    date.offset = as.integer(c(200, -500, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 0, 0, -400, 100, 200)),
                                    subject = c("Re: Fw: busybox 202 with tab", "Doubled date", "Only mail address", "name is mail address",
                                              "name is mail address", "name is mail address", "name is mail address", "name is mail address",
                                              "name is mail address", "name is mail address", "name is mail address", "name is mail address",
                                              "=?KOI8-R?Q?=EF=D4=D7=C5=D4:_Some_patches?= 2", "Re: busybox 1",
                                              "=?KOI8-R?Q?=EF=D4=D7=C5=D4:_Some_patches?= tab", "Re: Fw: busybox 2 tab",
                                              "Re: Fw: busybox 10"),
                                    thread = sprintf("<thread-%s>", c(1, 2, 3, 4, 5, 6, 6, 6, 6, 6, 6, 7, 8, 8, 8, 9, 9)),
                                    artifact.type = "Mail"
                                    )
    ## delete the line with the empty date
    mail.data.expected = mail.data.expected[-13, ]

    ## check the results
    expect_identical(mail.data.read, mail.data.expected, info = "Mail data.")

    ## check order of commits (ordered by date?)
    dates = order(mail.data.read[["date"]])
    dates.expected = seq_len(nrow(mail.data.expected))
    expect_identical(dates, dates.expected, info = "Ordering by date.")
})

test_that("Read the author data.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    author.data.read = read.authors(proj.conf$get.value("datapath"))

    ## build the expected data.frame
    author.data.expected = data.frame(
        author.id = as.integer(c(4936, 4937, 4938, 4939, 4940, 4941, 4942, 4943, 4944)),
        author.name = c("Thomas", "Olaf", "Björn", "udo", "Fritz fritz@example.org", "georg", "Hans", "Karl", "Max"),
        author.email = c("thomas@example.org", "olaf@example.org", "bjoern@example.org", "udo@example.org",
                         "asd@sample.org", "heinz@example.org", "hans1@example.org", "karl@example.org", "max@example.org")
    )

    ## check the results
    expect_identical(author.data.read, author.data.expected, info = "Author data.")
})

test_that("Read and parse the pasta data.", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    pasta.data.read = read.pasta(proj.conf$get.value("datapath.pasta"))

    ## build the expected data.frame
    pasta.data.expected = data.frame(message.id = c("<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>", "<asddghdswqeasdasd@mail.gmail.com>",
                                                   "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>", "<hans1@mail.gmail.com>",
                                                   "<hans2@mail.gmail.com>", "<hans3@mail.gmail.com>", "<saf54sd4gfasf46asf46@mail.gmail.com>",
                                                   "<saf54sd4gfasf46asf46@mail.gmail.com>"),
                                     commit.hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    NA, "1143db502761379c2bfcecc2007fc34282e7ee61",
                                                    "1143db502761379c2bfcecc2007fc34282e7ee61", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                                    "0a1a5c523d835459c42f33e863623138555e2526", "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0"),
                                     revision.set.id = c("<revision-set-1>", "<revision-set-2>", "<revision-set-3>",
                                                         "<revision-set-4>", "<revision-set-4>", "<revision-set-4>",
                                                         "<revision-set-5>", "<revision-set-5>"))

    ## check the results
    expect_identical(pasta.data.read, pasta.data.expected, info = "PaStA data.")
})

test_that("Read and parse the issue data.", {
    ## FIXME @Roger1995: update issues.list with a more recent content!

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    issue.data.read = read.issues(proj.conf$get.value("datapath.issues"))

    ## build the expected data.frame
    issue.data.expected = data.frame(issue.id = sprintf("<issue-%s>", rep(c("ZEPPELIN-328", "ZEPPELIN-332", 3, 6), c(13, 6, 7, 10))),
                                     issue.title = rep(c("[ZEPPELIN-328] Interpreter page should clarify the % magic syntax for interpreter group.name",
                                                         "[ZEPPELIN-332] CNFE when running SQL query against Cassandra temp table",
                                                         "Error in construct.networks.from.list for openssl function networks",
                                                         "Distinguish directedness of networks and edge-construction algorithm"), c(13, 6, 7, 10)),
                                     issue.type = rep(c("['issue', 'bug']", "['issue', 'bug']", "['issue', 'bug']", "['issue', 'bug', 'enhancement']"),  c(13, 6, 7, 10)),
                                     issue.state = rep(c("closed", "open", "closed", "open"), c(13, 6, 7, 10)),
                                     issue.resolution = rep(c("['fixed']", "['unresolved']", "[]", "[]"), c(13, 6, 7, 10)),
                                     creation.date = get.date.from.string(rep(c("2015-09-29 21:44:21", "2015-10-01 14:21:10", "2017-02-21 12:02:34", "2017-03-08 14:27:05"),
                                                                              c(13, 6, 7, 10))),
                                     closing.date = get.date.from.string(rep(c("2015-10-15 06:23:06", NA, "2017-02-27 13:51:09", NA), c(13, 6, 7, 10))),
                                     issue.components = rep(c("['GUI', 'Interpreters']", "['Interpreters']", "[]", "[]"), c(13, 6, 7, 10)),
                                     event.type = c("created","commented","commented","commented","commented","commented","commented","commented","commented","commented","commented","commented",
                                                    "resolution_updated","created","commented","commented","commented","commented","commented","created","assigned","commented","state_updated",
                                                    "add_link","referenced","referenced","mentioned","subscribed","commented","mentioned","subscribed","add_link","mentioned","subscribed","labeled","commented"),
                                     author.name = c("Thomas","Thomas","Björn","Björn","Björn","Björn","Olaf","Björn","Björn","Olaf","Olaf","Olaf","Björn","Björn","Björn","Björn","Max","Max","Max","Karl",
                                                     "Olaf","Karl","Olaf","Karl","Karl","Thomas","udo","udo","Thomas","Björn","Björn","Thomas","Björn","Björn","Olaf","Björn"),
                                     author.email = c("thomas@example.org","thomas@example.org","bjoern@example.org","bjoern@example.org","bjoern@example.org","bjoern@example.org","olaf@example.org",
                                                      "bjoern@example.org","bjoern@example.org","olaf@example.org","olaf@example.org","olaf@example.org","bjoern@example.org","bjoern@example.org",
                                                      "bjoern@example.org","bjoern@example.org","max@example.org","max@example.org","max@example.org","karl@example.org","olaf@example.org","karl@example.org",
                                                      "olaf@example.org","karl@example.org","karl@example.org","thomas@example.org","udo@example.org","udo@example.org","thomas@example.org","bjoern@example.org",
                                                      "bjoern@example.org","thomas@example.org","bjoern@example.org","bjoern@example.org","olaf@example.org","bjoern@example.org"),
                                     event.date = get.date.from.string(c("2015-09-29 21:44:21","2015-09-29 21:44:21","2015-09-29 21:46:30","2015-09-29 21:49:21","2015-09-29 21:49:34","2015-09-30 01:04:34"
                                                                         ,"2015-09-30 03:25:06","2015-09-30 03:48:41","2015-09-30 04:08:07","2015-09-30 06:06:53","2015-09-30 06:22:23","2015-09-30 06:50:26"
                                                                         ,"2015-10-01 06:23:06","2015-10-01 14:21:10","2015-10-01 14:21:10","2015-10-01 19:55:39","2015-10-01 20:07:47","2015-10-01 20:12:08"
                                                                         ,"2015-10-03 06:27:52","2017-02-21 12:02:34","2017-02-21 12:02:34","2017-02-21 12:16:49","2017-02-27 13:51:09","2017-02-27 13:51:09"
                                                                         ,"2017-02-27 16:45:09","2017-02-27 16:45:09","2017-07-27 15:30:02","2017-07-27 15:30:02","2017-07-27 15:30:02","2017-07-27 15:30:02"
                                                                         ,"2017-07-27 15:30:02","2017-07-27 15:30:02","2017-07-27 15:30:02","2017-07-27 15:30:02","2017-08-03 14:30:20","2018-08-06 16:30:37")),
                                     event.info.1 = c("open","open","open","open","open","open","open","open","open","open","open","open","fixed","open","open","open","open","open","open","open","","open"
                                                      ,"closed","930af63a030fb92e48eddff01f53284c3eeba80e","","","Thomas","Thomas","open","Thomas","Thomas","fb52357f05958007b867da06f4077abdc04fa0d8","udo","udo","decided","open"),
                                     event.info.2 = c("['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']",
                                                      "['unresolved']","['unresolved']","unresolved","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","['unresolved']","[]","","[]","open",
                                                      "commit","","","thomas@example.org","thomas@example.org","[]","thomas@example.org","thomas@example.org","commit","udo@example.org","udo@example.org","","[]"),
                                     artifact.type = "IssueEvent"
                                     )
    ## calculate event IDs
    issue.data.expected[["event.id"]] = sapply(
        paste(issue.data.expected[["issue.id"]], issue.data.expected[["author.name"]], issue.data.expected[["event.date"]], sep = "_"),
        function(event) { digest::digest(event, algo="sha1", serialize = FALSE) }
    )
    ## set row names as integers
    attr(issue.data.expected, "row.names") = as.integer(seq(from = 1, to = 36, by = 1))

    ## check the results
    expect_identical(issue.data.read, issue.data.expected, info = "Issue data.")

})
