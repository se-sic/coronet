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
## Copyright 2018-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2020 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## All Rights Reserved.


context("Tests for ProjectData functionalities.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("Compare two ProjectData objects", {

    ## initialize a ProjectData object with the ProjectConf and clone it into another one
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("pasta", TRUE)
    proj.data.one = ProjectData$new(project.conf = proj.conf)
    proj.data.two = proj.data.one$clone()

    expect_true(proj.data.one$equals(proj.data.two), info = "Two identical ProjectData objects.")

    ## Always change one data source in the one object, test for inequality, change it in the
    ## second object, as well, and test for equality.

    ## change the second data object

    proj.data.two$get.pasta()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.one$get.pasta()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")

    proj.data.one$get.commits()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.two$get.commits()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")

    proj.data.two$get.mails()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.one$get.mails()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")

    proj.data.one$get.issues()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.two$get.issues()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")

    proj.data.two$get.authors()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.one$get.authors()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")

    proj.data.one$get.synchronicity()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.two$get.synchronicity()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")
})

test_that("Compare two RangeData objects", {

    ## initialize a ProjectData object with the ProjectConf
    ## cut it on the base of commits and clone the resulting RangeData object
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("commits")
    range.data.two = range.data.one$clone()

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

    commits = proj.data$get.commits()

    commit.data.expected = data.frame(commit.id = sprintf("<commit-%s>", c(32712, 32713, 32710, 32714, 32715, 32716,
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

    # throw away the row names as they are permuted when merging and
    # we do not care for their order in the test
    rownames(commits) = NULL
    rownames(commit.data.expected) = NULL

    expect_identical(commits, commit.data.expected)
})

test_that("Merge commit message titles to commit data", {
    ## initialize a ProjectData object with the ProjectConf
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commit.messages", "title")
    proj.data = ProjectData$new(project.conf = proj.conf)

    commits = proj.data$get.commits()

    commit.data.expected = data.frame(commit.id = sprintf("<commit-%s>", c(32712, 32713, 32710, 32714, 32715, 32716,
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

    # throw away the row names as they are permuted when merging and
    # we do not care for their order in the test
    rownames(commits) = NULL
    rownames(commit.data.expected) = NULL

    expect_identical(commits, commit.data.expected)
})
