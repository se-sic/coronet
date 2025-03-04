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
## Copyright 2025 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.


context("Tests for the file 'util-core-peripheral.R'")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

## Prepare global setting
proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

test_that("Commit message preprocessing steps: Lowercase transformation", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.messages(proj.data, preprocessing = "lowercase")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          preprocessed.message = c("add stuff",
                                                   "add some more stuff",
                                                   "i added important things the things are\nnothing",
                                                   "i wish it would work now",
                                                   "wish intensifies",
                                                   "... still\ndoesn't\nwork\nas expected",
                                                   ""))

    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: Punctuation removal", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.messages(proj.data, preprocessing = "punctuation")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          preprocessed.message = c("Add stuff",
                                                   "Add some more stuff",
                                                   "I added important things the things are\nnothing",
                                                   "I wish it would work now",
                                                   "Wish intensifies",
                                                   "still\ndoesnt\nwork\nas expected",
                                                   ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: Stopword removal", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.messages(proj.data, preprocessing = "stopwords")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          preprocessed.message = c("Add stuff",
                                                   "Add   stuff",
                                                   "I added important things  things \nnothing",
                                                   "I wish   work now",
                                                   "Wish intensifies",
                                                   "... still\n\nwork\n expected",
                                                   ""))

    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: Whitespace removal", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.messages(proj.data, preprocessing = "whitespaces")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          preprocessed.message = c("Add stuff",
                                                   "Add some more stuff",
                                                   "I added important things the things are nothing",
                                                   "I wish it would work now",
                                                   "Wish intensifies",
                                                   "... still doesn't work as expected",
                                                   ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: All preprocesing", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.messages(proj.data)

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          preprocessed.message = c("add stuff",
                                                   "add stuff",
                                                   "added important things things nothing",
                                                   "wish work now",
                                                   "wish intensifies",
                                                   "still doesnt work expected",
                                                   ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: limited commit number", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.messages(proj.data, commit.hashes = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                                                    "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                                    "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                                                    "0a1a5c523d835459c42f33e863623138555e2526"))

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          preprocessed.message = c("add stuff",
                                                   "add stuff",
                                                   "still doesnt work expected",
                                                   ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message stemming: only lowercase preprocessing", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.stemmed.commit.messages(proj.data, preprocessing = "lowercase")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          stemmed.message = c("add stuff",
                                              "add some more stuff",
                                              "i ad import thing the thing are noth",
                                              "i wish it would work now",
                                              "wish intensifi",
                                              "... still doesn't work as expect",
                                              ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message stemming: All preprocesing", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.stemmed.commit.messages(proj.data)

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          stemmed.message = c("add stuff",
                                              "add stuff",
                                              "ad import thing thing noth",
                                              "wish work now",
                                              "wish intensifi",
                                              "still doesnt work expect",
                                              ""))
    ## Assert
    expect_equal(expected, result)
})
