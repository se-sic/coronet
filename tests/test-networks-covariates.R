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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Klara Schlüter <schluete@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## All Rights Reserved.


##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"
AGGREGATION.LEVELS = c("range", "cumulative", "all.ranges", "project.cumulative", "project.all.ranges", "complete")

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Global information ------------------------------------------------------

mybins = c("2016-07-12 15:00:00", "2016-07-12 16:00:00", "2016-07-12 16:05:00", "2016-08-31 18:00:00")
myranges = construct.ranges(mybins, sliding.window = FALSE)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility functions -------------------------------------------------------

#' Load test data and generate test networks
#'
#' @return Tuple containing project data and list of networks
get.network.covariates.test.networks = function(network.type = c("author", "artifact")) {

    network.type.function = paste("get", match.arg(network.type), "network", sep = ".")

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))

    ## retrieve project data and network builder
    project.data = ProjectData$new(proj.conf)
    project.data$set.issues(NULL)

    ## split data
    input.data = split.data.time.based(project.data, bins = mybins)
    input.data.networks = lapply(input.data, function(d) NetworkBuilder$new(d, net.conf)[[network.type.function]]())

    return(list("networks" = input.data.networks, "project.data" = project.data))
}

#' Helper for the first activitity tests: Gets the first activity per person and data source for possible
#' aggregation levels as a nested list.
#'
#' @return A list (elements represent the levels) of lists (elements represent the networks after splitting) of lists
#'         (elements represent the vertices which represent persons) of lists (elements represent the different data
#'         sources) of dates as PoSIXct.
get.expected.first.activity = function() {
    expected.attributes = list(
        range = network.covariates.test.build.expected(
            list(
                list(
                    mails = "2016-07-12 15:58:40 UTC",
                    commits = "2016-07-12 15:58:59 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = NA,
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 16:05:37 UTC",
                    commits = "2016-07-12 16:05:41 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:10 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:32 UTC",
                    issues = NA
                )
            )
        ),
        cumulative = network.covariates.test.build.expected(
            list(
                list(
                    mails = "2016-07-12 15:58:40 UTC",
                    commits = "2016-07-12 15:58:59 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:10 UTC",
                    issues = NA
                ),
                list(
                    mails = "2016-07-12 16:04:40 UTC",
                    commits = "2016-07-12 16:06:32 UTC",
                    issues = NA
                )
            )
        ),
        all.ranges = network.covariates.test.build.expected(
            list(
                list(
                    mails = "2016-07-12 15:58:40 UTC",
                    commits = "2016-07-12 15:58:59 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:10 UTC",
                    issues = NA
                ),
                list(
                    mails = "2016-07-12 16:04:40 UTC",
                    commits = "2016-07-12 16:06:32 UTC",
                    issues = NA
                )
            )
        ),
        project.cumulative = network.covariates.test.build.expected(
            list(
                list(
                    mails = "2004-10-09 18:38:13 UTC",
                    commits = "2016-07-12 15:58:59 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:10 UTC",
                    issues = NA
                ),
                list(
                    mails = "2016-07-12 16:04:40 UTC",
                    commits = "2016-07-12 16:06:32 UTC",
                    issues = NA
                )
            )
        ),
        project.all.ranges = network.covariates.test.build.expected(
            list(
                list(
                    mails = "2004-10-09 18:38:13 UTC",
                    commits = "2016-07-12 15:58:59 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:10 UTC",
                    issues = NA
                ),
                list(
                    mails = "2016-07-12 16:04:40 UTC",
                    commits = "2016-07-12 16:06:32 UTC",
                    issues = NA
                )
            )
        ),
        complete = network.covariates.test.build.expected(
            list(
                list(
                    mails = "2004-10-09 18:38:13 UTC",
                    commits = "2016-07-12 15:58:59 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                )
            ),
            list(
                list(
                    mails = "2016-07-12 15:58:50 UTC",
                    commits = "2016-07-12 16:00:45 UTC",
                    issues = NA
                ),
                list(
                    mails = NA,
                    commits = "2016-07-12 16:06:10 UTC",
                    issues = NA
                ),
                list(
                    mails = "2016-07-12 16:04:40 UTC",
                    commits = "2016-07-12 16:06:32 UTC",
                    issues = NA
                )
            )
        )
    )

    ## convert date strings to POSIXct
    expected.attributes = lapply(expected.attributes, function(level) {
        lapply(level, function(network) {
            lapply(network, function(person) {
                lapply(person, function(date.per.datasource) {
                    return(get.date.from.string(date.per.datasource))
                })
            })
        })
    })

    return(expected.attributes)
}

#' Get splitted test data
#'
#' @return splitted test data for each level
get.network.covariates.test.networks.data = function(network.type = c("author", "artifact")) {
    networks.and.data = get.network.covariates.test.networks()

    ## split data by networks
    results = lapply(AGGREGATION.LEVELS, function(level)
        split.data.by.networks(networks.and.data[["networks"]], networks.and.data[["project.data"]], level)
    )
    names(results) = AGGREGATION.LEVELS

    return(results)
}

#' Sample computation callback
#'
#' @param range The range identifier
#' @param range.data The current range data
#' @param current.network The current network
#'
#' @return A list containing the value 1 for each author except "Olaf"
test.compute.attr = function(range, range.data, current.network) {
    authors = range.data$get.authors()[["author.name"]]

    ## Olaf should get default value
    authors = authors[-which(authors == "Olaf")]

    attributes = lapply(authors, function(name) 1)
    names(attributes) = authors
    return(attributes)
}

#' Build list with appropriate range names
#'
#' @param x Value for first range
#' @param y Value for second range
#' @param z Value for third range
#'
#' @return The list of x, y, z with range names
network.covariates.test.build.expected = function(x, y, z) {
    arguments = list(x, y, z)
    names(arguments) = myranges

    return(arguments)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unit tests for author networks ------------------------------------------

#' Test the add.vertex.attribute method
test_that("Test add.vertex.attribute", {

    ## Test setup

    test.networks = get.network.covariates.test.networks.data()
    expected.attributes = network.covariates.test.build.expected(c(1), c(42), c(42, 1, 1))

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute(test.networks[[level]], "test.attr", 42,
                                                  test.compute.attr)

        actual.attributes = lapply(networks.with.attr, function(net) igraph::V(net)$test.attr)
        expect_identical(expected.attributes, actual.attributes)
    })
})

#' Test the split.and.add.vertex.attribute method
test_that("Test split.and.add.vertex.attribute", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = network.covariates.test.build.expected(c(1), c(42), c(42, 1, 1))

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = split.and.add.vertex.attribute(networks.and.data[["networks"]],
                                                            networks.and.data[["project.data"]],
                                                            "test.attr", level, 42, test.compute.attr)

        actual.attributes = lapply(networks.with.attr, function(net) igraph::V(net)$test.attr)
        expect_identical(expected.attributes, actual.attributes)
    })
})

#' Test the add.vertex.attribute.commit.count.author method
test_that("Test add.vertex.attribute.commit.count.author", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = network.covariates.test.build.expected(c(1L), c(1L), c(1L, 1L, 1L)),
        cumulative = network.covariates.test.build.expected(c(1L), c(1L), c(2L, 1L,  1L)),
        all.ranges = network.covariates.test.build.expected(c(1L), c(2L), c(2L, 1L,  1L)),
        project.cumulative = network.covariates.test.build.expected(c(1L), c(1L), c(2L, 1L,  1L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L), c(2L), c(2L, 1L, 1L)),
        complete = network.covariates.test.build.expected(c(1L), c(2L), c(2L, 1L, 1L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.commit.count.author(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "commit.count")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.commit.count.committer.and.author method
test_that("Test add.vertex.attribute.commit.count.committer.and.author", {

    ## Test setup
    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = network.covariates.test.build.expected(c(1L), c(0L), c(0L, 1L, 1L)),
        cumulative = network.covariates.test.build.expected(c(1L), c(0L), c(0L, 1L,  1L)),
        all.ranges = network.covariates.test.build.expected(c(1L), c(0L), c(0L, 1L,  1L)),
        project.cumulative = network.covariates.test.build.expected(c(1L), c(0L), c(0L, 1L, 1L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L), c(0L), c(0L, 1L, 1L)),
        complete = network.covariates.test.build.expected(c(1L), c(0L), c(0L, 1L, 1L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.commit.count.committer.and.author(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "commit.count.committer.and.author")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.commit.count.committer.or.author method
test_that("Test add.vertex.attribute.commit.count.committer.or.author", {

    ## Test setup
    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = network.covariates.test.build.expected(c(1L), c(1L), c(1L, 1L, 2L)),
        cumulative = network.covariates.test.build.expected(c(1L), c(1L), c(2L, 1L,  2L)),
        all.ranges = network.covariates.test.build.expected(c(2L), c(2L), c(2L, 1L,  2L)),
        project.cumulative = network.covariates.test.build.expected(c(1L), c(1L), c(2L, 1L,  2L)),
        project.all.ranges = network.covariates.test.build.expected(c(2L), c(2L), c(2L, 1L,  2L)),
        complete = network.covariates.test.build.expected(c(2L), c(2L), c(2L, 1L,  2L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.commit.count.committer.or.author(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "commit.count.committer.or.author")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.email method
test_that("Test add.vertex.attribute.author.email", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = network.covariates.test.build.expected(
        c("bjoern@example.org"),
        c("olaf@example.org"),
        c("olaf@example.org", "karl@example.org", "thomas@example.org")
    )

    ## Test

    networks.with.attr = add.vertex.attribute.author.email(
        networks.and.data[["networks"]], networks.and.data[["project.data"]]
    )

    actual.attributes = lapply(networks.with.attr,  igraph::get.vertex.attribute, name = "author.email")

    expect_identical(expected.attributes, actual.attributes)
})

#' Test the add.vertex.attribute.artifact.count method
test_that("Test add.vertex.attribute.artifact.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = network.covariates.test.build.expected(c(1L), c(1L), c(1L, 1L, 2L)),
        cumulative = network.covariates.test.build.expected(c(1L), c(1L), c(2L, 1L, 2L)),
        all.ranges = network.covariates.test.build.expected(c(1L), c(2L), c(2L, 1L, 2L)),
        project.cumulative = network.covariates.test.build.expected(c(1L), c(1L), c(2L, 1L, 2L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L), c(2L), c(2L, 1L, 2L)),
        complete = network.covariates.test.build.expected(c(1L), c(2L), c(2L, 1L, 2L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.artifact.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "artifact.count")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.first.activity method with computation over all types.
test_that("Test add.vertex.attribute.first.activity with multiple types and computation over all types", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            list(list(all.activities = "2016-07-12 15:58:40 UTC")),
            list(list(all.activities = "2016-07-12 16:00:45 UTC")),
            list(list(all.activities = "2016-07-12 16:05:37 UTC"),
                 list(all.activities = "2016-07-12 16:06:10 UTC"),
                 list(all.activities = "2016-07-12 16:06:32 UTC")
            )
        ),
        cumulative = network.covariates.test.build.expected(
            list(list(all.activities = "2016-07-12 15:58:40 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC"),
                 list(all.activities = "2016-07-12 16:06:10 UTC"),
                 list(all.activities = "2016-07-12 16:04:40 UTC")
            )
        ),
        all.ranges = network.covariates.test.build.expected(
            list(list(all.activities = "2016-07-12 15:58:40 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC"),
                 list(all.activities = "2016-07-12 16:06:10 UTC"),
                 list(all.activities = "2016-07-12 16:04:40 UTC")
            )
        ),
        project.cumulative = network.covariates.test.build.expected(
            list(list(all.activities = "2004-10-09 18:38:13 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC"),
                 list(all.activities = "2016-07-12 16:06:10 UTC"),
                 list(all.activities = "2016-07-12 16:04:40 UTC")
            )
        ),
        project.all.ranges = network.covariates.test.build.expected(
            list(list(all.activities = "2004-10-09 18:38:13 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC"),
                 list(all.activities = "2016-07-12 16:06:10 UTC"),
                 list(all.activities = "2016-07-12 16:04:40 UTC")
            )
        ),
        complete = network.covariates.test.build.expected(
            list(list(all.activities = "2004-10-09 18:38:13 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC")),
            list(list(all.activities = "2016-07-12 15:58:50 UTC"),
                 list(all.activities = "2016-07-12 16:06:10 UTC"),
                 list(all.activities = "2016-07-12 16:04:40 UTC")
            )
        )
    )

    ## convert date strings to POSIXct
    expected.attributes = lapply(expected.attributes, function(level) {
        lapply(level, function(network) {
            lapply(network, function(person) {
                lapply(person, function(date.per.datasource) {
                    return(get.date.from.string(date.per.datasource))
                })
            })
        })
    })

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {

        networks.with.attributes = add.vertex.attribute.first.activity(
            list.of.networks = networks.and.data[["networks"]], project.data = networks.and.data[["project.data"]],
            activity.types = c("mails", "commits", "issues"), name = "first.activity", aggregation.level = level,
            default.value = NA, take.first.over.all.activity.types = TRUE
        )
        actual.attributes = lapply(networks.with.attributes, igraph::get.vertex.attribute, name = "first.activity")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.first.activity method with multiple activity types and computation per type.
test_that("Test add.vertex.attribute.first.activity with multiple types and computation per type", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()
    expected.attributes = get.expected.first.activity()

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {

        networks.with.attributes = add.vertex.attribute.first.activity(
            list.of.networks = networks.and.data[["networks"]], project.data = networks.and.data[["project.data"]],
            activity.types = c("mails", "commits", "issues"), name = "first.activity", aggregation.level = level,
            default.value = NA, take.first.over.all.activity.types = FALSE
        )
        actual.attributes = lapply(networks.with.attributes, igraph::get.vertex.attribute, name = "first.activity")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.first.activity method with one activity type and computation per type.
test_that("Test add.vertex.attribute.first.activity with one type and computation per type", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()
    expected.attributes = get.expected.first.activity()
    expected.attributes = lapply(expected.attributes, function(level) {
        lapply(level, function(network) {
            lapply(network, function(person) {
                return(person["commits"])
            })
        })
    })


    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {

        networks.with.attributes = add.vertex.attribute.first.activity(
            list.of.networks = networks.and.data[["networks"]], project.data = networks.and.data[["project.data"]],
            activity.types = c("commits"), name = "first.activity", aggregation.level = level,
            default.value = NA, take.first.over.all.activity.types = FALSE
        )
        actual.attributes = lapply(networks.with.attributes, igraph::get.vertex.attribute, name = "first.activity")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.active.ranges method
test_that("Test add.vertex.attribute.active.ranges", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = network.covariates.test.build.expected(
        list(myranges[1]), list(myranges[2:3]), list(myranges[2:3], myranges[3], myranges[3])
    )

    ## Test

    networks.with.attr = add.vertex.attribute.active.ranges(
        networks.and.data[["networks"]], networks.and.data[["project.data"]]
    )

    actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "active.ranges")

    expect_identical(expected.attributes, actual.attributes)
})

#' Test the add.vertex.attribute.author.role.simple method
test_that("Test add.vertex.attribute.author.role.simple", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            )
        ),
        cumulative = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("peripheral"), c("core", "core", "peripheral")
            )
        ),
        all.ranges = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            )
        ),
        project.cumulative = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("peripheral"), c("core", "core", "peripheral")
            )
        ),
        project.all.ranges = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            )
        ),
        complete = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            )
        )
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        lapply(c("commit.count", "loc.count"), function(type) {
            networks.with.attr = add.vertex.attribute.author.role.simple(
                networks.and.data[["networks"]], networks.and.data[["project.data"]],
                type = type, aggregation.level = level
            )

            actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "author.role")

            expect_identical(expected.attributes[[level]][[type]], actual.attributes)
        })
    })
})

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unit tests for artifact networks ----------------------------------------

#' Test the add.vertex.attribute.artifact.editor.count method
test_that("Test add.vertex.attribute.artifact.editor.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes = network.covariates.test.build.expected(list(1L), list(1L), list(3L, 1L))

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c(1L), c(1L), c(3L, 1L)),
        cumulative = network.covariates.test.build.expected(
            c(1L), c(2L), c(3L, 1L)),
        all.ranges = network.covariates.test.build.expected(
            c(2L), c(2L), c(3L, 1L)),
        project.cumulative = network.covariates.test.build.expected(
            c(1L), c(2L), c(3L, 1L)),
        project.all.ranges = network.covariates.test.build.expected(
            c(2L), c(2L), c(3L, 1L)),
        complete = network.covariates.test.build.expected(
            c(2L), c(2L), c(3L, 1L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.artifact.editor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "editor.count")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.artifact.first.occurrence method
test_that("Test add.vertex.attribute.artifact.first.occurrence", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:05:41 UTC", "2016-07-12 16:06:32 UTC")
        ),
        cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 15:58:59 UTC"),
            c("2016-07-12 16:05:41 UTC", "2016-07-12 16:06:32 UTC")
        ),
        all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 15:58:59 UTC"),
            c("2016-07-12 16:05:41 UTC", "2016-07-12 16:06:32 UTC")
        ),
        project.cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 15:58:59 UTC"),
            c("2016-07-12 16:05:41 UTC", "2016-07-12 16:06:32 UTC")
        ),
        project.all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 15:58:59 UTC"),
            c("2016-07-12 16:05:41 UTC", "2016-07-12 16:06:32 UTC")
        ),
        complete = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 15:58:59 UTC"),
            c("2016-07-12 16:05:41 UTC", "2016-07-12 16:06:32 UTC")
        )
    )

    ## convert date strings to POSIXct
    expected.attributes = lapply(expected.attributes, function(times) {
        lapply(times, function(date.vector) {
            get.date.from.string(date.vector)
        })
    })

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.artifact.first.occurrence(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "first.occurrence")

        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.artifact.change.count method
test_that("Test add.vertex.attribute.artifact.change.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c(1L), c(1L), c(3L, 1L)),
        cumulative = network.covariates.test.build.expected(
            c(1L), c(2L), c(3L, 1L)),
        all.ranges = network.covariates.test.build.expected(
            c(2L), c(2L), c(3L, 1L)),
        project.cumulative = network.covariates.test.build.expected(
            c(1L), c(2L), c(3L, 1L)),
        project.all.ranges = network.covariates.test.build.expected(
            c(2L), c(2L), c(3L, 1L)),
        complete = network.covariates.test.build.expected(
            c(2L), c(2L), c(3L, 1L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.artifact.change.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "change.count")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})
