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
    proj.data.one = ProjectData$new(project.conf = proj.conf)
    proj.data.two = proj.data.one$clone()

    expect_true(proj.data.one$equals(proj.data.two), info = "Two identical ProjectData objects.")

    ## Always change one data source in the one object, test for inequality, change it in the
    ## second object, as well, and test for equality.

    ##change the second data object
    proj.data.one$get.commits()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.two$get.commits()

    expect_true(proj.data.one$equals(proj.data.two), "Two identical ProjectData objects.")

    proj.data.two$get.pasta()

    expect_false(proj.data.one$equals(proj.data.two), "Two not identical ProjectData objects.")

    proj.data.one$get.pasta()

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
