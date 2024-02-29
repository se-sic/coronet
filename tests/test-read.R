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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2018-2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2020-2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021 by Mirabdulla Yusifli <s8miyusi@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2022-2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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
    commit.data.expected = data.frame(commit.id = format.commit.ids(c(32712, 32712, 32713, 32713, 32710, 32710, 32714, 32715, 32716,
                                                                           32711, 32711)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                        "2016-07-12 16:00:45", "2016-07-12 16:05:41", "2016-07-12 16:05:41",
                                                        "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                        "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      author.name = c("Björn", "Björn", "Olaf", "Olaf", "Olaf", "Olaf", "Karl", "Karl", "Thomas",
                                                      "Thomas", "Thomas"),
                                      author.email = c("bjoern@example.org", "bjoern@example.org", "olaf@example.org",
                                                     "olaf@example.org", "olaf@example.org", "olaf@example.org", "karl@example.org",
                                                     "karl@example.org", "thomas@example.org", "thomas@example.org", "thomas@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 15:58:59", "2016-07-20 10:00:44",
                                                                              "2016-07-20 10:00:44", "2016-07-12 17:05:55", "2016-07-12 17:05:55",
                                                                              "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                              "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      committer.name = c("Björn", "Björn", "Björn", "Björn", "Thomas", "Thomas", "Karl", "Karl", "Thomas",
                                                         "Thomas", "Thomas"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org", "bjoern@example.org", "bjoern@example.org",
                                                          "thomas@example.org", "thomas@example.org", "karl@example.org", "karl@example.org",
                                                          "thomas@example.org", "thomas@example.org", "thomas@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                             "5a5ec9675e98187e1e92561e1888aa6f04faa338", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                             "3a0ed78458b3976243db6829f63eba3eead26774", "3a0ed78458b3976243db6829f63eba3eead26774",
                                             "1143db502761379c2bfcecc2007fc34282e7ee61", "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                             "d01921773fae4bed8186b0aa411d6a2f7a6626e6", "0a1a5c523d835459c42f33e863623138555e2526",
                                             "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                                      added.lines = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                                      deleted.lines = as.integer(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
                                      diff.size = as.integer(c(2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                                      file = c("test.c", "test.c", "test.c", "test.c", "test2.c", "test2.c", "test3.c", UNTRACKED.FILE,
                                               UNTRACKED.FILE, "test2.c", "test2.c"),
                                      artifact = c("A", "defined(A)", "A", "defined(A)", "Base_Feature", "Base_Feature", "Base_Feature",
                                                   UNTRACKED.FILE.EMPTY.ARTIFACT, UNTRACKED.FILE.EMPTY.ARTIFACT, "Base_Feature", "foo"),
                                      artifact.type = c("Feature", "FeatureExpression", "Feature", "FeatureExpression", "Feature",
                                                      "FeatureExpression", "Feature", UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE,
                                                      UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE, "Feature", "Feature"),
                                      artifact.diff.size = as.integer(c(1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1)))

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
    commit.data.expected = data.frame(commit.id = format.commit.ids(c(32716, 32717, 32718, 32719, 32720, 32721, 32715)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                                    "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                    "2016-07-12 16:06:32")),
                                      author.name = c("Björn", "Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas"),
                                      author.email = c("bjoern@example.org", "olaf@example.org", "olaf@example.org",
                                                     "karl@example.org", "karl@example.org", "thomas@example.org", "thomas@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-20 10:00:44", "2016-07-12 17:05:55",
                                                                              "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                              "2016-07-12 16:06:32")),
                                      committer.name = c("Björn", "Björn", "Thomas", "Karl", "Karl", "Thomas", "Thomas"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org", "thomas@example.org",
                                                        "karl@example.org", "karl@example.org", "thomas@example.org", "thomas@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                             "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                             "418d1dc4929ad1df251d2aeb833dd45757b04a6f", "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                             "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files = as.integer(c(1, 1, 1, 1, 1, 1, 1)),
                                      added.lines = as.integer(c(1, 1, 1, 1, 1, 1, 1)),
                                      deleted.lines = as.integer(c(1, 0, 0, 0, 0, 0, 0)),
                                      diff.size = as.integer(c(2, 1, 1, 1, 1, 1, 1)),
                                      file = c("test.c", "test.c", "test2.c", "test3.c", UNTRACKED.FILE, UNTRACKED.FILE, "test2.c"),
                                      artifact = c("test.c", "test.c", "test2.c", "test3.c", UNTRACKED.FILE, UNTRACKED.FILE, "test2.c"),
                                      artifact.type = c("File", "File", "File", "File", UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE,
                                                        UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE, "File"),
                                      artifact.diff.size = as.integer(c(1, 1, 1, 1, 0, 0, 1)))

    ## check the results
    expect_identical(commit.data.read, commit.data.expected, info = "Raw commit data.")

    ## check order of commits (ordered by date?)
    dates = order(commit.data.read[["date"]])
    dates.expected = seq_len(nrow(commit.data.expected))
    expect_identical(dates, dates.expected, info = "Ordering by date.")
})

test_that("Read the commit message data.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")

    ## read the actual data
    commit.message.data.read = read.commit.messages(proj.conf$get.value("datapath"))

    ## build the expected data.frame
    commit.data.expected = data.frame(commit.id = format.commit.ids(c(32712, 32713, 32710, 32714, 32715, 32716, 32711)),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                               "418d1dc4929ad1df251d2aeb833dd45757b04a6f", "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                               "0a1a5c523d835459c42f33e863623138555e2526"),
                                      title = c("Add stuff", "Add some more stuff", "I added important things", "I wish it would work now", "Wish", "...", ""),
                                      message = c("", "", "the things are\nnothing", "", "intensifies", "still\ndoesn't\nwork\nas expected", "" ))

    ## check the results
    expect_identical(commit.message.data.read, commit.data.expected, info = "Commit message data.")
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
                                             synchronicity = c(TRUE, TRUE, FALSE, FALSE))

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
                                    thread = sprintf("<thread-%s>", c("13#1", "42#2", "13#3", "42#4", "42#5", "42#6", "42#6", "42#6",
                                                                      "42#6", "42#6", "42#6", "42#7", "13#8", "13#8", "13#8", "13#9", "13#9")),
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
        author.id = as.character(c(4936, 4937, 4938, 4939, 4940, 4941, 4942, 4943, 4944)),
        author.name = c("Thomas", "Olaf", "Björn", "udo", "Fritz fritz@example.org", "georg", "Hans", "Karl", "Max"),
        author.email = c("thomas@example.org", "olaf@example.org", "bjoern@example.org", "udo@example.org",
                         "asd@sample.org", "heinz@example.org", "hans1@example.org", "karl@example.org", "max@example.org"),
        is.bot = c(TRUE, NA, FALSE, NA, NA, NA, NA, NA, NA)
    )

    ## check the results
    expect_identical(author.data.read, author.data.expected, info = "Author data.")
})

test_that("Read and parse the gender data.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    gender.data.read = read.gender(proj.conf$get.value("datapath.gender"))

    ## build the expected data.frame
    gender.data.expected = data.frame(author.name = c("Björn", "Fritz fritz@example.org", "georg", "Hans", "Karl", "Max", "Olaf", "Thomas", "udo"),
                                      gender = c("male", NA, "male", "male", "male", "male", "female", "male", "female"))

    ## check the results
    expect_identical(gender.data.read, gender.data.expected, info = "Gender data.")
})

test_that("Read the raw bot data.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    bot.data.read = read.bot.info(proj.conf$get.value("datapath"))

    ## build the expected data.frame
    bot.data.expected = data.frame(
        author.name = c("Thomas", "Björn", "udo", "CLAassistant"),
        author.email = c("thomas@example.org", "bjoern@example.org", "udo@example.org", "nomail"),
        is.bot = c(TRUE, FALSE, NA, TRUE)
    )

    ## check the results
    expect_identical(bot.data.read, bot.data.expected, info = "Bot data.")
})

test_that("Read custom event timestamps.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("custom.event.timestamps.file", "custom-events.list")

    ## read the actual data
    timestamps = read.custom.event.timestamps(proj.conf$get.value("datapath"), proj.conf$get.value("custom.event.timestamps.file"))

    timestamps.expected = list(
        "Test event 1" = "2016-07-12 15:00:00",
        "Test event 2" = "2016-07-12 16:00:00",
        "Test event 3" = "2016-07-12 16:05:00",
        "Test event 4" = "2016-08-08",
        "Test event 5" = "2016-10-05 09:00:00"
    )

    expect_identical(timestamps, timestamps.expected, "Custom timestamps.")
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
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data from all sources
    proj.conf$update.value("issues.from.source", c("jira", "github"))
    issue.data.read = read.issues(proj.conf$get.value("datapath.issues"), proj.conf$get.value("issues.from.source"))

    ## read the actual data from jira
    proj.conf$update.value("issues.from.source", "jira")
    issue.data.read.jira = read.issues(proj.conf$get.value("datapath.issues"), proj.conf$get.value("issues.from.source"))

    ## read the actual data from github
    proj.conf$update.value("issues.from.source", "github")
    issue.data.read.github = read.issues(proj.conf$get.value("datapath.issues"), proj.conf$get.value("issues.from.source"))

    ## build the expected data.frame
    issue.data.expected = data.frame(issue.id = c(rep("<issue-jira-ZEPPELIN-328>", 15), rep("<issue-jira-ZEPPELIN-332>", 8),
                                                  rep("<issue-github-3>", 9), rep("<issue-github-6>", 11),
                                                  rep("<issue-github-1>", 6), rep("<issue-github-2>", 5), rep("<issue-github-4>", 3)),
                                     issue.title = c(rep("[ZEPPELIN-328] Interpreter page should clarify the % magic syntax for interpreter group.name", 15),
                                                     rep("[ZEPPELIN-332] CNFE when running SQL query against Cassandra temp table", 8),
                                                     rep("Error in construct.networks.from.list for openssl function networks", 9),
                                                     rep("Distinguish directedness of networks and edge-construction algorithm", 11),
                                                     rep("Example pull request 1", 6),
                                                     rep("Example pull request 2", 5),
                                                     rep("Example pull request 4", 3)),
                                     issue.type = I(c(rep(list(list("issue" , "bug")), 15), rep(list(list("issue" , "bug")), 8),
                                                      rep(list(list("issue" , "bug")), 9), rep(list(list("issue", "bug", "enhancement")), 11),
                                                    rep(list(list("pull request")), 6), rep(list(list("pull request")), 5), rep(list(list("pull request", "enhancement")), 3))),
                                     issue.state = c(rep("closed", 15), rep("open", 8), rep("closed", 9), rep("open", 11),
                                                     rep("reopened", 6), rep("closed", 5), rep("open", 3)),
                                     issue.resolution = I(c(rep(list(list("fixed")), 15), rep(list(list("unresolved")), 8),
                                                            rep(list(list()), 9), rep(list(list()), 11),
                                                            rep(list(list()), 6), rep(list(list()), 5), rep(list(list()), 3))),
                                     creation.date = get.date.from.string(c(rep("2013-04-21 23:52:09", 15),
                                                                            rep("2016-07-12 16:01:30", 8),
                                                                            rep("2016-07-12 15:59:25", 9),
                                                                            rep("2016-07-12 14:30:13", 11),
                                                                            rep("2016-07-14 13:37:00", 6),
                                                                            rep("2016-07-12 14:59:25", 5),
                                                                            rep("2016-07-12 16:02:02", 3))),
                                     closing.date = get.date.from.string(c(rep("2013-05-25 20:02:08", 15), rep(NA, 8),
                                                                           rep("2016-07-12 16:06:30", 9), rep(NA, 11),
                                                                           rep(NA, 6),
                                                                           rep("2016-07-12 16:04:59", 5),
                                                                           rep(NA, 3))),
                                     issue.components = I(c(rep(list(list("GUI" , "Interpreters")), 15), rep(list(list("Interpreters")), 8),
                                                            rep(list(list()), 9), rep(list(list()), 11),
                                                            rep(list(list()), 6), rep(list(list()), 5), rep(list(list()), 3))),
                                     event.name = c("created", "commented", "commented", "commented", "commented", "commented",
                                                    "commented", "commented", "commented", "commented", "commented", "commented",
                                                    "resolution_updated", "referenced_by", "add_link", "referenced_by", "add_link", "created",
                                                    "commented", "commented", "commented", "commented", "commented", "created", "assigned", "commented",
                                                    "state_updated", "add_link", "referenced", "referenced", "add_link", "add_link", "mentioned", "subscribed",
                                                    "commented", "mentioned", "subscribed", "add_link", "mentioned", "subscribed", "labeled", "commented",
                                                    "referenced_by", "created", "commented", "state_updated", "commented", "commented", "state_updated",
                                                    "created", "commented", "merged", "state_updated", "referenced_by", "commit_added", "created", "commented"),
                                     author.name = c("Thomas", "Thomas", "Björn", "Björn", "Björn", "Björn", "Olaf", "Björn", "Björn", "Olaf", "Olaf", "Olaf",
                                                     "Björn", "Thomas", "Thomas", "Thomas", "Thomas", "Björn", "Björn", "Björn", "Max", "Max", "Max", "Karl",
                                                     "Olaf", "Karl", "Olaf", "Karl", "Karl", "Thomas", "Karl", "Thomas", "udo", "udo", "Thomas", "Björn", "Björn",
                                                     "Thomas", "Björn", "Björn", "Olaf", "Björn", "Karl", "Thomas", "Thomas", "Thomas", "Olaf", "Björn", "Olaf",
                                                     "Björn", "Björn", "Olaf", "Olaf", "Thomas", "Björn", "Olaf", "Olaf"),
                                     author.email = c("thomas@example.org", "thomas@example.org", "bjoern@example.org",
                                                      "bjoern@example.org", "bjoern@example.org", "bjoern@example.org",
                                                      "olaf@example.org", "bjoern@example.org", "bjoern@example.org",
                                                      "olaf@example.org", "olaf@example.org", "olaf@example.org",
                                                      "bjoern@example.org", "thomas@example.org", "thomas@example.org",
                                                      "thomas@example.org", "thomas@example.org", "bjoern@example.org",
                                                      "bjoern@example.org", "bjoern@example.org", "max@example.org", "max@example.org",
                                                      "max@example.org", "karl@example.org", "olaf@example.org",
                                                      "karl@example.org", "olaf@example.org", "karl@example.org", "karl@example.org",
                                                      "thomas@example.org", "karl@example.org", "thomas@example.org", "udo@example.org",
                                                      "udo@example.org", "thomas@example.org", "bjoern@example.org",
                                                      "bjoern@example.org", "thomas@example.org", "bjoern@example.org", "bjoern@example.org",
                                                      "olaf@example.org", "bjoern@example.org", "karl@example.org",
                                                      "thomas@example.org", "thomas@example.org", "thomas@example.org",
                                                      "olaf@example.org", "bjoern@example.org", "olaf@example.org",
                                                      "bjoern@example.org", "bjoern@example.org", "olaf@example.org",
                                                      "olaf@example.org", "thomas@example.org", "bjoern@example.org",
                                                      "olaf@example.org", "olaf@example.org"),
                                     date = get.date.from.string(c("2013-04-21 23:52:09", "2013-04-21 23:52:09",
                                                                   "2013-05-05 21:46:30", "2013-05-05 21:49:21",
                                                                   "2013-05-05 21:49:34", "2013-05-06 01:04:34",
                                                                   "2013-05-25 03:25:06", "2013-05-25 03:48:41",
                                                                   "2013-05-25 04:08:07", "2013-05-25 06:06:53",
                                                                   "2013-05-25 06:22:23", "2013-06-01 06:50:26",
                                                                   "2013-06-01 06:53:06", "2017-05-21 12:00:00",
                                                                   "2017-05-21 12:00:00", "2017-05-21 12:00:00",
                                                                   "2017-05-21 12:00:00", "2016-07-12 16:01:30",
                                                                   "2016-07-12 16:02:30", "2016-07-15 19:55:39",
                                                                   "2016-07-15 20:07:47", "2016-07-27 20:12:08",
                                                                   "2016-07-28 06:27:52", "2016-07-12 15:59:25",
                                                                   "2016-07-12 15:59:25", "2016-07-12 15:59:59",
                                                                   "2016-07-12 16:06:30", "2016-08-07 15:37:02",
                                                                   "2016-08-31 16:45:09", "2016-10-05 16:45:09",
                                                                   "2016-08-07 15:37:02", "2016-08-07 15:30:00",
                                                                   "2016-07-12 15:30:02", "2016-07-12 15:30:02",
                                                                   "2016-07-12 16:03:59", "2016-08-31 15:30:02",
                                                                   "2016-10-05 15:30:02", "2016-10-13 15:30:02",
                                                                   "2016-12-07 15:30:02", "2016-12-07 15:30:02",
                                                                   "2017-05-23 12:31:34", "2017-05-23 12:32:39",
                                                                   "2016-08-07 15:37:02", "2016-07-12 15:59:25",
                                                                   "2016-07-12 15:59:25", "2016-07-12 15:59:59",
                                                                   "2016-07-12 16:01:01", "2016-07-12 16:06:01",
                                                                   "2016-07-14 13:37:00", "2016-07-12 14:59:25",
                                                                   "2016-07-12 14:59:25", "2016-07-12 16:04:59",
                                                                   "2016-07-12 16:04:59", "2016-08-07 15:30:00",
                                                                   "2016-07-12 16:02:02", "2016-07-12 16:02:02",
                                                                   "2016-07-12 16:02:02")),
                                     event.info.1 = c("open", "open", "open", "open", "open", "open", "open", "open", "open",
                                                      "open", "open", "open", "fixed", "<issue-jira-ZEPPELIN-332>", "<issue-jira-ZEPPELIN-332>",
                                                      "<issue-jira-ZEPPELIN-328>", "<issue-jira-ZEPPELIN-328>", "open", "open", "open", "open", "open", "open", "open",
                                                      "", "open", "closed", "930af63a030fb92e48eddff01f53284c3eeba80e", "", "", "<issue-github-6>", 
                                                      "<issue-github-2>", "Thomas", "Thomas", "open", "Thomas", "Thomas", "fb52357f05958007b867da06f4077abdc04fa0d8",
                                                      "udo", "udo", "decided", "open", "<issue-github-3>", "open", "open", "closed", "closed", "closed", "open",
                                                      "open", "open", "", "closed", "<issue-github-3>",
                                                      "72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "open", "open"),
                                     event.info.2 = NA, # is assigned later
                                     event.id = NA, # is assigned later
                                     issue.source = c(rep("jira", 23), rep("github", 20), rep("github", 14)),
                                     artifact.type = "IssueEvent"
                                     )

    issue.data.expected[["event.info.2"]] = I(list(
                                                list("unresolved"), list("unresolved"), list("unresolved"), list("unresolved"),
                                                list("unresolved"), list("unresolved"), list("unresolved"), list("unresolved"),
                                                list("unresolved"), list("unresolved"), list("unresolved"), list("unresolved"),
                                                "unresolved", "issue", "issue", "issue", "issue", list("unresolved"), list("unresolved"),
                                                list("unresolved"), list("unresolved"), list("unresolved"), list("unresolved"), list(), "", list(),
                                                "open", "commit", "", "", "issue", "issue", "thomas@example.org", "thomas@example.org", list(),
                                                "thomas@example.org", "thomas@example.org", "commit", "udo@example.org", "udo@example.org",
                                                "", list(), "issue", list(), list(), "open", list(), list(), "closed",
                                                list(), list(), "", "open", "issue",
                                                "2016-07-12 15:58:59", list(), list()
                                            ))

    ## calculate event IDs
    issue.data.expected[["event.id"]] = sapply(
        paste(issue.data.expected[["issue.id"]], issue.data.expected[["author.name"]], issue.data.expected[["date"]], sep = "_"),
        function(event) { digest::digest(event, algo="sha1", serialize = FALSE) }
    )

    # split the expected data to only jira and only github data
    issue.data.expected.jira = subset(issue.data.expected, issue.data.expected[["issue.source"]] == "jira")
    issue.data.expected.github = subset(issue.data.expected, issue.data.expected[["issue.source"]] == "github")

    ## set row names as integers
    attr(issue.data.expected, "row.names") = as.integer(seq(from = 1, to = nrow(issue.data.expected), by = 1))
    attr(issue.data.expected.jira, "row.names") = as.integer(seq(from = 1, to = nrow(issue.data.expected.jira), by = 1))
    attr(issue.data.expected.github, "row.names") = as.integer(seq(from = 1, to = nrow(issue.data.expected.github), by = 1))

    ## sort by date
    issue.data.expected = issue.data.expected[order(issue.data.expected[["date"]], decreasing = FALSE), ]
    issue.data.expected.jira = issue.data.expected.jira[order(issue.data.expected.jira[["date"]], decreasing = FALSE), ]
    issue.data.expected.github = issue.data.expected.github[order(issue.data.expected.github[["date"]], decreasing = FALSE), ]

    ## check the results
    expect_identical(issue.data.read, issue.data.expected, info = "Issue data.")
    expect_identical(issue.data.read.jira, issue.data.expected.jira, info = "Issue data jira.")
    expect_identical(issue.data.read.github, issue.data.expected.github, info = "Issue data github.")
})

