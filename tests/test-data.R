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

    ##initialize a ProjectData object with the ProjectConf and clone it into another one
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("pasta", TRUE)
    proj.data.one = ProjectData$new(project.conf = proj.conf)
    proj.data.two = proj.data.one$clone()

    expect_true(proj.data.one$equals(proj.data.two), info = "Two identical ProjectData objects.")

    ## Always change one data source in the one object, test for inequality, change it in the
    ## second object, as well, and test for equality.

    ##change the second data object

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

    ## ensure that PaStA data relating to Hans' mail 2 and 3 do not exist anymore since they have also been filtered
    ## during patchstack mail filtering
    expect_false("<hans2@mail.gmail.com>" %in% filtered.pasta[["message.id"]])
    expect_false("<hans3@mail.gmail.com>" %in% filtered.pasta[["message.id"]])

    ## ensure that all three PaStA entries that existed previously do still exist but have been associated to the
    ## very first mail of the patchstack
    expect_equal(3, sum(filtered.pasta[["message.id"]] == "<hans1@mail.gmail.com>"))
})
