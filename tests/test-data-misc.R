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
## Copyright 2026 by Thomas Bock <bockthom@cmu.edu>
## All Rights Reserved.


context("Tests for the file 'util-data-misc.R'")

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
    result = get.preprocessed.commit.messages(proj.data, preprocessing = "lowercase")

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

test_that("Commit message preprocessing steps: Lowercase transformation, only title", {
    proj.conf$update.value("commit.messages", "title")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.commit.messages(proj.data, preprocessing = "lowercase")

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
                                                   "i added important things",
                                                   "i wish it would work now",
                                                   "wish",
                                                   "...",
                                                   ""))

    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: Punctuation removal", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.commit.messages(proj.data, preprocessing = "punctuation")

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
    result = get.preprocessed.commit.messages(proj.data, preprocessing = "stopwords")

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
    result = get.preprocessed.commit.messages(proj.data, preprocessing = "whitespaces")

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
    result = get.preprocessed.commit.messages(proj.data)

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
                                                   "still work expected",
                                                   ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: All preprocesing but nonexisting commit", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.commit.messages(proj.data, commit.hashes = c("1234567890123456789012345678901234567890"))

    ## Act
    expected = create.empty.data.frame(c("hash", "preprocessed.message"))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message preprocessing steps: limited commit number", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.preprocessed.commit.messages(proj.data, commit.hashes = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
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
                                                   "still work expected",
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
                                              "still work expect",
                                              ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message tokenization", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.tokenized.commit.messages(proj.data)

    ## Act
    expected = list(c("Add", "stuff"),
                    c("Add", "some", "more", "stuff"),
                    c("I", "added", "important", "things", "the", "things", "are", "nothing"),
                    c("I", "wish", "it", "would", "work", "now"),
                    c("Wish", "intensifies"),
                    c("...", "still", "doesn't", "work", "as", "expected"),
                    character(0))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message tokenization, only 2 commits", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.tokenized.commit.messages(proj.data, c("3a0ed78458b3976243db6829f63eba3eead26774",
                                                         "1143db502761379c2bfcecc2007fc34282e7ee61"))

    ## Act
    expected = list(c("I", "added", "important", "things", "the", "things", "are", "nothing"),
                    c("I", "wish", "it", "would", "work", "now"))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message tokenization, only title", {
    proj.conf$update.value("commit.messages", "title")
    proj.data = ProjectData$new(proj.conf)
    result =  get.tokenized.commit.messages(proj.data)

    ## Act
    expected = list(c("Add", "stuff"),
                    c("Add", "some", "more", "stuff"),
                    c("I", "added", "important", "things"),
                    c("I", "wish", "it", "would", "work", "now"),
                    c("Wish"),
                    c("..."),
                    character(0))

    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message lemmatization: only lowercase preprocessing", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.lemmatized.commit.messages(proj.data, preprocessing = "lowercase")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          lemmatized.message = c("add stuff",
                                              "add some much stuff",
                                              "i add important thing the thing be nothing",
                                              "i wish it would work now",
                                              "wish intensify",
                                              "... still doesn't work as expect",
                                              ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message lemmatization: All preprocesing", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.lemmatized.commit.messages(proj.data)

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          lemmatized.message = c("add stuff",
                                              "add stuff",
                                              "add important thing thing nothing",
                                              "wish work now",
                                              "wish intensify",
                                              "still work expect",
                                              ""))
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: any match, single string", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data, strings = "add")

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774"),
                          message = c("Add stuff ",
                                      "Add some more stuff ",
                                      "I added important things the things are\nnothing"))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: any match, single string, only 2 commits", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data,
                                             strings = "add",
                                             commit.hashes = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                                               "5a5ec9675e98187e1e92561e1888aa6f04faa338"))

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                          message = c("Add stuff ",
                                      "Add some more stuff "))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: any match, multiple strings", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data, strings = c("add", "intensifies"))

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f"),
                          message = c("Add stuff ",
                                      "Add some more stuff ",
                                      "I added important things the things are\nnothing",
                                      "Wish intensifies"))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: any match, multiple strings, case sensitive", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data, strings = c("Add", "Intensifies"), ignore.case = FALSE)

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                          message = c("Add stuff ",
                                      "Add some more stuff "))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: any match, multiple strings, only title", {
    proj.conf$update.value("commit.messages", "title")
    proj.data = ProjectData$new(proj.conf)
    result = get.commit.messages.by.strings(proj.data, strings = c("add", "intensifies"))

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774"),
                          message = c("Add stuff",
                                      "Add some more stuff",
                                      "I added important things"))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: all match, multiple strings, no result", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data, strings = c("add", "intensifies"), match = all)

    ## Act
    expected = create.empty.data.frame(c("hash", "message"), c("character", "character"))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: all match, multiple strings", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data, strings = c("add", "stuff"), match = all)

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                          message = c("Add stuff ",
                                      "Add some more stuff "))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message keyword search: at least 2 match, multiple strings", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result =  get.commit.messages.by.strings(proj.data, strings = c("add", "stuff", "I "),
    match = function(x) {
        return (sum(unlist(x)) >= 2)
    })

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774"),
                          message = c("Add stuff ",
                                      "Add some more stuff ",
                                      "I added important things the things are\nnothing"))
    rownames(result) = NULL
    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message token counts", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.commit.message.counts(proj.data)

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                   "3a0ed78458b3976243db6829f63eba3eead26774",
                                   "1143db502761379c2bfcecc2007fc34282e7ee61",
                                   "418d1dc4929ad1df251d2aeb833dd45757b04a6f",
                                   "d01921773fae4bed8186b0aa411d6a2f7a6626e6",
                                   "0a1a5c523d835459c42f33e863623138555e2526"),
                          count = c(2, 4, 8, 6, 2, 6, 0))

    ## Assert
    expect_equal(expected, result)
})

test_that("Commit message token counts, omly 2 commits", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    result = get.commit.message.counts(proj.data, c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                                    "5a5ec9675e98187e1e92561e1888aa6f04faa338"))

    ## Act
    expected = data.frame(hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                   "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                          count = c(2, 4))

    ## Assert
    expect_equal(expected, result)
})

test_that("Extract commit-message tags: test data contains no tags", {
    proj.conf$update.value("commit.messages", "message")
    proj.data = ProjectData$new(proj.conf)
    commit.message.data = proj.data$get.commit.messages()
    result = extract.commit.message.tags(commit.message.data, hash.col = "hash", body.col = "message")

    ## Act
    expected = data.frame(commit.hash = character(0), tag = character(0),
                          name = character(0), email = character(0))

    ## Assert
    expect_equal(expected, as.data.frame(result))

    ## Canonicalization should also handle the empty case gracefully
    result.clean = canonicalize.commit.message.tags(result)

    ## Act
    expected.clean = data.frame(commit.hash = character(0), tag = character(0),
                                name = character(0), email = character(0),
                                tag.clean = character(0))

    ## Assert
    expect_equal(expected.clean, as.data.frame(result.clean))

    ## Statistics on the canonicalized tags should also be empty, but well-typed
    result.clean[["tag"]] = result.clean[["tag.clean"]]
    stats = get.commit.message.tag.statistics(result.clean)

    ## Act
    expected.stats = data.frame(tag = character(0), occurrences = integer(0),
                                distinct.commits = integer(0), with.email = integer(0),
                                without.email = integer(0), distinct.people = integer(0))

    ## Assert
    expect_equal(expected.stats, as.data.frame(stats))
})

test_that("Extract commit-message tags: multiple tags across two commits", {
    commits = data.frame(
        hash = c("hash1", "hash2"),
        body = c(paste("Fix bug in parser",
                       "",
                       "Signed-off-by: John Doe <john@example.com>",
                       "Reviewed-by: Bob <bob@example.com>",
                       sep = "\n"),
                 paste("Add new feature",
                       "",
                       "Signed-of-by: John Doe <john@example.com>",
                       "Cc: max@example.com",
                       "Acked-by: Someone Else",
                       sep = "\n"))
    )
    result = extract.commit.message.tags(commits, hash.col = "hash", body.col = "body")

    ## Act
    expected = data.frame(
        commit.hash = c("hash1", "hash1", "hash2", "hash2", "hash2"),
        tag         = c("Signed-off-by", "Reviewed-by", "Signed-of-by", "Cc", "Acked-by"),
        name        = c("John Doe", "Bob", "John Doe", NA_character_, "Someone Else"),
        email       = c("john@example.com", "bob@example.com", "john@example.com",
                        "max@example.com", NA_character_)
    )

    ## Assert
    expect_equal(expected, as.data.frame(result))

    ## Canonicalization: Signed-off-by and Signed-of-by are similar enough to merge
    ## (all other pairwise similarities should be below the default threshold of 0.7)
    result.clean = canonicalize.commit.message.tags(result)

    ## Act
    expected.clean = expected
    expected.clean[["tag.clean"]] = c("Signed-off-by", "Reviewed-by", "Signed-off-by", "Cc", "Acked-by")

    ## Assert
    expect_equal(expected.clean, as.data.frame(result.clean))

    ## Statistics, computed using the canonical tag spelling
    result.clean[["tag"]] = result.clean[["tag.clean"]]
    stats = get.commit.message.tag.statistics(result.clean)

    ## Act
    expected.stats = data.frame(
        tag              = c("Signed-off-by", "Reviewed-by", "Cc", "Acked-by"),
        occurrences      = c(2L, 1L, 1L, 1L),
        distinct.commits = c(2L, 1L, 1L, 1L),
        with.email       = c(2L, 1L, 1L, 0L),
        without.email    = c(0L, 0L, 0L, 1L),
        distinct.people  = c(1L, 1L, 1L, 1L)
    )

    ## Assert
    expect_equal(expected.stats, as.data.frame(stats))
})
