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
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2021 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2020-2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021 by Mirabdulla Yusifli <s8miyusi@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.


context("Tests for ProjectData functionalities.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
CASESTUDY_EMPTY = "test_empty"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("Compare two ProjectData objects on empty data", {

    ## initialize a ProjectData object with the ProjectConf and clone it into another one
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY_EMPTY, ARTIFACT)
    proj.conf$update.value("pasta", TRUE)
    proj.conf$update.value("gender", TRUE)
    proj.data.one = ProjectData$new(project.conf = proj.conf)
    proj.data.two = proj.data.one$clone(deep = TRUE)

    expect_true(proj.data.one$equals(proj.data.two), info = "Two identical ProjectData objects (clone).")

    ## Always change one data source in the one object, test for equality, change it in the
    ## second object, as well, and test for equality. It is always equal because when no data has been read, it is an
    ## empty table, the same as the data that is read. The only exception is the author data as this must not be empty

    proj.data.two$get.pasta()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (pasta).")
    proj.data.one$get.pasta()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (pasta).")

    proj.data.one$get.commits.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commits).")
    proj.data.two$get.commits.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commits).")

    proj.data.two$get.mails.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (mails).")
    proj.data.one$get.mails.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (mails).")

    proj.data.one$get.issues()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (issues.filtered).")
    proj.data.two$get.issues()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (issues.filtered).")

    proj.data.two$get.authors()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (authors).")
    proj.data.one$get.authors()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (authors).")

    proj.data.two$get.gender()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (gender).")
    proj.data.one$get.gender()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (gender).")

    proj.data.one$set.project.conf.entry("synchronicity", TRUE)
    proj.data.one$set.project.conf.entry("synchronicity.time.window", 5)
    proj.data.one$get.synchronicity()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (synchronicity).")
    proj.data.two$set.project.conf.entry("synchronicity", TRUE)
    proj.data.two$set.project.conf.entry("synchronicity.time.window", 5)
    proj.data.two$get.synchronicity()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (synchronicity).")

    proj.data.one$set.project.conf.entry("commit.messages", "message")
    proj.data.one$get.commit.messages()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (commit.messages).")
    proj.data.two$set.project.conf.entry("commit.messages", "message")
    proj.data.two$get.commit.messages()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commit.messages).")

    proj.data.one$set.project.conf.entry("commit.interactions", TRUE)
    proj.data.one$get.commit.interactions()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (commit.interactions).")
    proj.data.two$set.project.conf.entry("commit.interactions", TRUE)
    proj.data.two$get.commit.interactions()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commit.interactions).")
})

test_that("Compare two ProjectData objects on non-empty data", {

    ## initialize a ProjectData object with the ProjectConf and clone it into another one
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("pasta", TRUE)
    proj.conf$update.value("gender", TRUE)

    proj.data.one = ProjectData$new(project.conf = proj.conf)
    proj.data.two = proj.data.one$clone(deep = TRUE)

    expect_true(proj.data.one$equals(proj.data.two), info = "Two identical ProjectData objects (clone).")

    ## always change one data source in the one object, test for equality, change it in the
    ## second object, as well, and test for equality.

    ## test 'equals' on pasta
    proj.data.two$get.pasta()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (pasta).")

    proj.data.one$get.pasta()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (pasta).")

    ## test 'equals' on commits
    proj.data.one$get.commits.unfiltered()

    ## prevent 'equals' from triggering a read when calling the getter
    proj.data.two$update.project.conf(list("commits.locked" = TRUE))
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (commits).")
    proj.data.two$update.project.conf(list("commits.locked" = FALSE))

    proj.data.two$get.commits.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commits).")

    ## test 'equals' on mails
    proj.data.two$get.mails.unfiltered()

    ## prevent 'equals' from triggering a read when calling the getter
    proj.data.one$update.project.conf(list("mails.locked" = TRUE))
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (mails).")
    proj.data.one$update.project.conf(list("mails.locked" = FALSE))

    proj.data.one$get.mails.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (mails).")

    ## test 'equals' on (filtered) issues
    proj.data.one$get.issues()

    ## prevent 'equals' from triggering a read when calling the getter
    proj.data.two$update.project.conf(list("issues.locked" = TRUE))
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (issues.filtered).")
    proj.data.two$update.project.conf(list("issues.locked" = FALSE))

    proj.data.two$get.issues()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (issues.filtered).")

    ## test 'equals' on gender
    proj.data.two$get.gender()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (gender).")

    proj.data.one$get.gender()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (gender).")

    ## test 'equals' on authors
    proj.data.two$get.authors()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (authors).")

    proj.data.one$get.authors()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (authors).")

    ## test 'equals' on synchronicity
    proj.data.one$set.project.conf.entry("synchronicity", TRUE)
    proj.data.one$set.project.conf.entry("synchronicity.time.window", 5)
    proj.data.one$get.synchronicity()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (synchronicity).")

    proj.data.two$set.project.conf.entry("synchronicity", TRUE)
    proj.data.two$set.project.conf.entry("synchronicity.time.window", 5)
    proj.data.two$get.synchronicity()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (synchronicity).")

    ## test 'equals' on commit messages
    proj.data.one$set.project.conf.entry("commit.messages", "title")
    proj.data.one$get.commit.messages()
    expect_false(proj.data.one$equals(proj.data.two), "Two non-identical ProjectData objects (commit.messages).")

    proj.data.two$set.project.conf.entry("commit.messages", "title")
    proj.data.two$get.commit.messages()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commit.messages).")

    ## test 'equals' on commits with commit message titles
    ## At this point commit message data is already present and the getter (the setter respectively) merges the
    ## titles to the commits
    proj.data.one$get.commits.unfiltered()
    proj.data.two$get.commits.unfiltered()
    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects (commits with title).")

    ## test 'equals' on commits with commit messages and titles
    ## reset the commits of the first data object and this time load commit messages and compare this to the titles
    proj.data.one$set.project.conf.entry("commit.messages", "message")
    proj.data.one$get.commits.unfiltered()
    expect_false(proj.data.one$equals(proj.data.two),
                 "Two non-identical ProjectData objects (commits with messages / titles).")
})

test_that("Compare two RangeData objects", {

    ## initialize a ProjectData object with the ProjectConf
    ## cut it on the base of commits and clone the resulting RangeData object
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("commits")
    range.data.two = range.data.one$clone(deep = TRUE)

    ## compare the two equal RangeData objects
    expect_true(range.data.one$equals(range.data.two))

    ## cut the ProjectData object on base of issues in order to get another
    ## RangeData object to check for inequality
    range.data.three = proj.data.base$get.data.cut.to.same.date("issues")

    expect_false(range.data.one$equals(range.data.three))

    ## check whether a ProjectData object can be compared to a RangeData object
    expect_false(range.data.one$equals(proj.data.base))
    expect_false(proj.data.base$equals(range.data.one))

    ## create a RangeData object with the same data sources as proj.data.base
    ## and check for inequality
    timestamps = proj.data.base$get.data.timestamps(outermost = TRUE)
    range.data.four = split.data.time.based(proj.data.base, bins =
                                                c(timestamps[["start"]][[1]], timestamps[["end"]][[1]]))[[1]]

    expect_false(proj.data.base$equals(range.data.four))

})

test_that("Filter patchstack mails", {

    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("mails.filter.patchstack.mails", TRUE)

    ## create the project data
    proj.data = ProjectData$new(proj.conf)

    ## retrieve the mails while filtering patchstack mails
    mails.filtered = proj.data$get.mails()

    ## create new project with filtering disabled
    proj.conf$update.value("mails.filter.patchstack.mails", FALSE)
    proj.data = ProjectData$new(proj.conf)

    ## retrieve the mails without filtering patchstack mails
    mails.unfiltered = proj.data$get.mails()

    ## get message ids
    mails.filtered.mids = mails.filtered[["message.id"]]
    mails.unfiltered.mids = mails.unfiltered[["message.id"]]

    expect_equal(setdiff(mails.unfiltered.mids, mails.filtered.mids), c("<hans2@mail.gmail.com>",
                                                                        "<hans3@mail.gmail.com>",
                                                                        "<hans4@mail.gmail.com>",
                                                                        "<hans5@mail.gmail.com>",
                                                                        "<hans6@mail.gmail.com>"))
})

test_that("Filter patchstack mails with PaStA enabled", {
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("mails.filter.patchstack.mails", TRUE)
    proj.conf$update.value("pasta", TRUE)

    proj.data = ProjectData$new(proj.conf)

    ## retrieve filtered PaStA data by calling 'get.pasta' which calls the filtering functionality internally
    filtered.pasta = proj.data$get.pasta()

    ## ensure that the remaining mails have not been touched
    expect_true("<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>" %in% filtered.pasta[["message.id"]])
    expect_true("<asddghdswqeasdasd@mail.gmail.com>" %in% filtered.pasta[["message.id"]])
    expect_true("<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>" %in% filtered.pasta[["message.id"]])
    expect_equal(2, sum(filtered.pasta[["message.id"]] == "<saf54sd4gfasf46asf46@mail.gmail.com>"))

    ## ensure that the three PaStA entries relating to the filtered patchstack mails have been merged to a single new
    ## PaStA entry which has assigned the message ID of the first patchstack mail
    expect_true("<hans1@mail.gmail.com>" %in% filtered.pasta[["message.id"]])

    ## ensure that there are no other entries than the ones that have been verified to exist above
    expect_equal(6, nrow(filtered.pasta))
})


test_that("Merge commit messages to commit data", {
    ## initialize a ProjectData object with the ProjectConf
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(project.conf = proj.conf)

    commits = proj.data$get.commits.unfiltered()

    commit.data.expected = data.frame(commit.id = format.commit.ids(c(32712, 32713, 32710, 32714, 32715, 32716,
                                                                           32711, 32711)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                                    "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                    "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      author.name = c("Björn", "Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas"),
                                      author.email = c("bjoern@example.org", "olaf@example.org", "olaf@example.org", "karl@example.org",
                                                       "karl@example.org", "thomas@example.org", "thomas@example.org", "thomas@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-20 10:00:44", "2016-07-12 17:05:55",
                                                                              "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                              "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      committer.name = c("Björn", "Björn", "Thomas", "Karl", "Karl", "Thomas",  "Thomas", "Thomas"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org", "thomas@example.org", "karl@example.org",
                                                          "karl@example.org", "thomas@example.org", "thomas@example.org", "thomas@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                               "418d1dc4929ad1df251d2aeb833dd45757b04a6f", "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1)),
                                      added.lines = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1)),
                                      deleted.lines = as.integer(c(1, 0, 0, 0, 0, 0, 0, 0)),
                                      diff.size = as.integer(c(2, 1, 1, 1, 1, 1, 1, 1)),
                                      file = c("test.c", "test.c", "test2.c", "test3.c", UNTRACKED.FILE,
                                               UNTRACKED.FILE, "test2.c", "test2.c"),
                                      artifact = c("A", "A", "Base_Feature", "Base_Feature",
                                                   UNTRACKED.FILE.EMPTY.ARTIFACT, UNTRACKED.FILE.EMPTY.ARTIFACT, "Base_Feature", "foo"),
                                      artifact.type = c("Feature", "Feature", "Feature","Feature", UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE,
                                                        UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE, "Feature", "Feature"),
                                      artifact.diff.size = as.integer(c(1, 1, 1, 1, 0, 0, 1, 1)),
                                      title = c("Add stuff", "Add some more stuff", "I added important things", "I wish it would work now", "Wish", "...", "", ""),
                                      message = c("", "", "the things are\nnothing", "", "intensifies", "still\ndoesn't\nwork\nas expected", "", ""))

    expect_identical(commits, commit.data.expected, info = "Add commit messages with title")
})

test_that("Merge commit message titles to commit data", {
    ## initialize a ProjectData object with the ProjectConf
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commit.messages", "title")
    proj.data = ProjectData$new(project.conf = proj.conf)

    commits = proj.data$get.commits.unfiltered()

    commit.data.expected = data.frame(commit.id = format.commit.ids(c(32712, 32713, 32710, 32714, 32715, 32716,
                                                                           32711, 32711)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:05:41",
                                                                    "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                    "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      author.name = c("Björn", "Olaf", "Olaf", "Karl", "Karl", "Thomas", "Thomas", "Thomas"),
                                      author.email = c("bjoern@example.org", "olaf@example.org", "olaf@example.org", "karl@example.org",
                                                       "karl@example.org", "thomas@example.org", "thomas@example.org", "thomas@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-20 10:00:44", "2016-07-12 17:05:55",
                                                                              "2016-07-12 16:06:10", "2016-07-12 16:06:20", "2016-07-12 16:06:30",
                                                                              "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                      committer.name = c("Björn", "Björn", "Thomas", "Karl", "Karl", "Thomas",  "Thomas", "Thomas"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org", "thomas@example.org", "karl@example.org",
                                                          "karl@example.org", "thomas@example.org", "thomas@example.org", "thomas@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                               "3a0ed78458b3976243db6829f63eba3eead26774", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                               "418d1dc4929ad1df251d2aeb833dd45757b04a6f", "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                               "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1)),
                                      added.lines = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1)),
                                      deleted.lines = as.integer(c(1, 0, 0, 0, 0, 0, 0, 0)),
                                      diff.size = as.integer(c(2, 1, 1, 1, 1, 1, 1, 1)),
                                      file = c("test.c", "test.c", "test2.c", "test3.c", UNTRACKED.FILE,
                                               UNTRACKED.FILE, "test2.c", "test2.c"),
                                      artifact = c("A", "A", "Base_Feature", "Base_Feature",
                                                   UNTRACKED.FILE.EMPTY.ARTIFACT, UNTRACKED.FILE.EMPTY.ARTIFACT, "Base_Feature", "foo"),
                                      artifact.type = c("Feature", "Feature", "Feature","Feature", UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE,
                                                        UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE, "Feature", "Feature"),
                                      artifact.diff.size = as.integer(c(1, 1, 1, 1, 0, 0, 1, 1)),
                                      title = c("Add stuff", "Add some more stuff", "I added important things", "I wish it would work now", "Wish", "...", "", ""))

    expect_identical(commits, commit.data.expected, info = "Add only commit title")
})


test_that("Filter bots from commit data", {
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("filter.bots", TRUE)
    ## disable all other filterings
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)

    proj.data = ProjectData$new(proj.conf)

    filtered.commits = proj.data$get.commits()

    expect_true(all(filtered.commits[["author.name"]] != "Thomas"))
    ## there are now 5 rows in the filtered commit data, since 3 rows have been removed during filtering
    expect_equal(nrow(filtered.commits), 5)
})

test_that("Filter bots from issue data", {
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("filter.bots", TRUE)
    ## disable all other filterings
    proj.conf$update.value("issues.only.comments", FALSE)

    proj.data = ProjectData$new(proj.conf)

    filtered.issues = proj.data$get.issues()

    expect_true(all(filtered.issues[["author.name"]] != "Thomas"))
    ## there are now 43 issue events remaining, since 10 issue events have been removed during filtering
    expect_equal(nrow(filtered.issues), 43)
})

test_that("Filter bots from mail data", {
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("filter.bots", TRUE)
    ## disable all other filterings
    proj.conf$update.value("mails.filter.patchstack.mails", FALSE)

    proj.data = ProjectData$new(proj.conf)

    filtered.mails = proj.data$get.mails()

    expect_true(all(filtered.mails[["author.name"]] != "Thomas"))
    ## there are now 15 mails remaining, since 2 mails have been removed during filtering
    expect_equal(nrow(filtered.mails), 15)
})

test_that("Re-read custom events after config change", {
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data = ProjectData$new(proj.conf)

    expect_identical(proj.data$get.custom.event.timestamps(), list(), "Unconfigured timestamps.")

    proj.data$set.project.conf.entry("custom.event.timestamps.file", "revisions.list")
    timestamps.revisions = proj.data$get.custom.event.timestamps()
    timestamps.revisions.expected = list(
        v1 = "2016-07-12 15:58:08",
        v2 = "2016-07-12 16:00:45",
        v3 = "2016-07-12 16:06:32"
    )
    expect_identical(timestamps.revisions, timestamps.revisions.expected, "Custom timestamps.")

    proj.data$set.project.conf.entry("custom.event.timestamps.file", "custom-events.list")
    timestamps.events = proj.data$get.custom.event.timestamps()
    timestamps.events.expected = list(
        "Test event 1" = "2016-07-12 15:00:00",
        "Test event 2" = "2016-07-12 16:00:00",
        "Test event 3" = "2016-07-12 16:05:00",
        "Test event 4" = "2016-08-08",
        "Test event 5" = "2016-10-05 09:00:00"
    )

    expect_identical(timestamps.events, timestamps.events.expected, "Custom timestamps.")

    proj.data$set.project.conf.entry("custom.event.timestamps.file", "nonexistent.file")
    expect_identical(proj.data$get.custom.event.timestamps(), list(), "Empty timestamps from invalid file")

    proj.data$set.project.conf.entry("custom.event.timestamps.file", "custom-events.list")
    timestamps.events = proj.data$get.custom.event.timestamps()
    timestamps.events.expected = list(
        "Test event 1" = "2016-07-12 15:00:00",
        "Test event 2" = "2016-07-12 16:00:00",
        "Test event 3" = "2016-07-12 16:05:00",
        "Test event 4" = "2016-08-08",
        "Test event 5" = "2016-10-05 09:00:00"
    )
    expect_identical(timestamps.events, timestamps.events.expected, "Custom timestamps.")

    proj.data$set.project.conf.entry("custom.event.timestamps.locked", TRUE)
    proj.data$clear.custom.event.timestamps()
    expect_identical(proj.data$get.custom.event.timestamps(), list(), "cleared and locked timestamps")
})

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Data paths of RangeData -------------------------------------------------

##
## Tests that check RangeData objects for the right data path
##

test_that("Cut data and check for right data path", {
    ## Build the ProjectData object and cut its data to same dates
    project.configuration = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    project.data = ProjectData$new(project.configuration)
    project.data = project.data$get.data.cut.to.same.date(data.sources = c("mails", "commits"))

    expected = "./codeface-data/results/testing/test_feature/feature"
    result = project.data$get.data.path()
    expect_identical(result, expected, info = "RangeData data path.")

    commit.data = project.data$get.commits.unfiltered()

    ## Update project configuration, for example, add pasta data, and retrieve mail data afterwards
    project.data$update.project.conf(updated.values = list("pasta" = TRUE))
    pasta = project.data$get.pasta()
    mails = project.data$get.mails.unfiltered()

    ## reset environment
    project.data$reset.environment()

    ## check whether mail data is still the same after resetting the environment
    expect_identical(mails, project.data$get.mails.unfiltered())
})

test_that("Create RangeData objects from Codeface ranges and check data path", {
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    data = construct.data(proj.conf, callgraphs = TRUE)

    range.paths = run.lapply(data, "get.data.path")
    range.paths = unlist(range.paths, use.names = FALSE)

    expected.paths = c("./codeface-data/results/testing/test_feature/feature/001--v1-v2",
                       "./codeface-data/results/testing/test_feature/feature/002--v2-v3")

    expect_identical(range.paths, expected.paths, "RangeData data paths")
})

test_that("Compare two ProjectData Objects with commit.interactions", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
    proj.conf$update.value("commit.interactions", TRUE)
    proj.conf$update.value("commits.filter.untracked.files", FALSE)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("filter.commit.interactions", FALSE)

    proj.data.one = ProjectData$new(project.conf = proj.conf)
    proj.data.two = proj.data.one$clone(deep = TRUE)

    ## test if the project data is equal and the commit interactions are as well
    expect_equal(proj.data.one$get.commit.interactions(), proj.data.two$get.commit.interactions())
    expect_true(proj.data.one$equals(proj.data.two))

    ## change commit interactions of one project data and assert that equality check fails
    proj.data.two$set.commit.interactions(create.empty.commit.interaction.list())
    expect_false(proj.data.one$equals(proj.data.two))

    ## change commit data in one to test if commit-interactions are correctly updated
    ## call get.commit.interactions() once to restore read interactions
    proj.data.two$get.commit.interactions()

    ## change commits in one project data
    commit.data = proj.data.one$get.commits()
    commit.data[["hash"]][[5]] = 1
    proj.data.one$set.commits(commit.data)

    ## use isTRUE to compress result of all.equal into a single boolean
    expect_false(isTRUE(all.equal(proj.data.one$get.commit.interactions(),
                                  proj.data.two$get.commit.interactions())))

    ## The data frame should still have 4 entries:
    expect_true(nrow(proj.data.one$get.commit.interactions()) == 4)
    ## after cleanup is called, the data frame should only have 3 entries:
    proj.data.one$cleanup.commit.interactions()
    expect_true(nrow(proj.data.one$get.commit.interactions()) == 3)

    ## set commit list of one project data to empty and test that last
    ## two rows of result data frame are empty
    proj.data.two$set.commits(create.empty.commits.list())

    ## create empty data frame of correct size
    commit.interactions.data.expected = data.frame(matrix(nrow = 4, ncol = 8))
    ## assure that the correct type is used
    for(i in seq_len(8)) {
        commit.interactions.data.expected[[i]] = as.character(commit.interactions.data.expected[[i]])
    }
    ## set everything except for authors as expected
    colnames(commit.interactions.data.expected) = c("commit.hash", "base.hash", "func", "file",
                                                    "base.func", "base.file", "base.author",
                                                    "interacting.author")
    commit.interactions.data.expected[["commit.hash"]] =
                                                        c("0a1a5c523d835459c42f33e863623138555e2526",
                                                        "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                                        "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                        "d01921773fae4bed8186b0aa411d6a2f7a6626e6")
    commit.interactions.data.expected[["base.hash"]] =
                                                      c("3a0ed78458b3976243db6829f63eba3eead26774",
                                                        "0a1a5c523d835459c42f33e863623138555e2526",
                                                        "1143db502761379c2bfcecc2007fc34282e7ee61",
                                                        "0a1a5c523d835459c42f33e863623138555e2526")
    commit.interactions.data.expected[["func"]] = c("GLOBAL", "test2.c::test2", "GLOBAL", "test2.c::test2")
    commit.interactions.data.expected[["file"]] = c("GLOBAL", "test2.c", "GLOBAL", "test2.c")
    commit.interactions.data.expected[["base.func"]] = c("test2.c::test2", "test2.c::test2",
                                                         "test3.c::test_function", "test2.c::test2")
    commit.interactions.data.expected[["base.file"]] = c("test2.c", "test2.c", "test3.c", "test2.c")

    expect_equal(proj.data.two$get.commit.interactions(), commit.interactions.data.expected)

    ## reactivate filtering of commit interactions
    proj.data.two$set.project.conf.entry("filter.commit.interactions", TRUE)
    expect_true(nrow(proj.data.two$get.commit.interactions()) == 2)
})
