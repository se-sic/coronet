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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2021 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018-2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018-2019 by Klara Schlüter <schluete@fim.uni-passau.de>
## Copyright 2018-2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021-2022 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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
mybins.since.2010 = c("2010-07-12 12:00:00", "2016-07-12 15:00:00", "2016-07-12 16:00:00", "2016-07-12 16:05:00", "2016-08-31 18:00:00")
myranges.since.2010 = construct.ranges(mybins.since.2010, sliding.window = FALSE)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility functions -------------------------------------------------------

#' Load test data and generate test networks
#'
#' @param network.type which network to get (\code{"author"} or \code{"artifact"}). [default: "author"]
#' @param issues whether to retain issue data. If \code{FALSE}, issue data is deleted. [default: FALSE]
#' @param author.relation passed to the network config. [default: "cochange"]
#' @param bins the bins which control splitting. [default: mybins]
#'
#' @return Tuple containing project data and list of networks
get.network.covariates.test.networks = function(network.type = c("author", "artifact"), issues = FALSE,
                                                author.relation = c("cochange", "issue", "mail"),
                                                artifact.relation = c("cochange", "issue", "mail"),
                                                bins = mybins,
                                                issues.only.comments = FALSE) {

    author.relation = match.arg(author.relation)
    artifact.relation = match.arg(artifact.relation)
    network.type.function = switch(match.arg(network.type),
                                   "author" = "get.author.network",
                                   "artifact" = "get.artifact.network")

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.value("commits.filter.untracked.files", TRUE)
    proj.conf$update.value("issues.only.comments", issues.only.comments)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = author.relation, artifact.relation = artifact.relation,
                                simplify = FALSE))

    ## retrieve project data
    project.data = ProjectData$new(proj.conf)
    if (!issues) {
        project.data$set.issues(NULL)
    }

    ## split data
    input.data = split.data.time.based(project.data, bins = bins)
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

#' Helper for tests of the function add.vertex.attribute.author.active.ranges: Returns the expected active ranges per range,
#' author and data source as a nested list.
#'
#' @return A list with elements that represent the range (the test data is split to build one network per range), each
#'         containing a list with elements that represent authors, the networks vertices, each containing a list of active
#'         ranges per data source.
get.expected.active.ranges = function() {

    ## expected active ranges per person and data source
    bjoern = list("mails" = list(myranges[1]), "commits" = list(myranges[1]), "issues" = list())
    olaf = list("mails" = list(myranges[1], myranges[3]), "commits" = list(myranges[2], myranges[3]), "issues" = list())
    karl = list("mails" = list(), "commits" = list(myranges[3]) , "issues" = list())
    thomas = list("mails" = list(myranges[2]), "commits" = list(myranges[3]), "issues" = list())

    ## list of expected vertex attributes per network range containing active ranges for the corresponding persons.
    expected.attributes = network.covariates.test.build.expected(
        list(bjoern), list(olaf), list(olaf, karl, thomas)
    )
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

#' Build list with appropriate range names
#'
#' @param x Value for first range
#' @param y Value for second range
#' @param z Value for third range
#'
#' @return The list of x, y, z with range names
network.covariates.test.build.expected.since.2010 = function(w, x, y, z) {
    arguments = list(w, x, y, z)
    names(arguments) = myranges.since.2010

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

#' Test the add.vertex.attribute.author.commit.count method
test_that("Test add.vertex.attribute.author.commit.count", {
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
        networks.with.attr = add.vertex.attribute.author.commit.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "commit.count")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.commit.count.committer.and.author method
test_that("Test add.vertex.attribute.author.commit.count.committer.and.author", {

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
        networks.with.attr = add.vertex.attribute.author.commit.count.committer.and.author(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "commit.count.committer.and.author")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.commit.count.committer.or.author method
test_that("Test add.vertex.attribute.author.commit.count.committer.or.author", {

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
        networks.with.attr = add.vertex.attribute.author.commit.count.committer.or.author(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "commit.count.committer.or.author")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.mail.count method
test_that("Test add.vertex.attribute.author.mail.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks(author.relation = "mail", bins = mybins.since.2010)

    expected.attributes = list(
        range = network.covariates.test.build.expected.since.2010(c(1L, 7L), c(1L, 1L), c(1L), c(1L)),
        cumulative = network.covariates.test.build.expected.since.2010(c(1L, 7L), c(1L, 1L), c(1L), c(2L)),
        all.ranges = network.covariates.test.build.expected.since.2010(c(1L, 7L), c(1L, 2L), c(1L), c(2L)),
        project.cumulative = network.covariates.test.build.expected.since.2010(c(1L, 7L), c(3L, 1L), c(1L), c(2L)),
        project.all.ranges = network.covariates.test.build.expected.since.2010(c(1L, 7L), c(3L, 2L), c(1L), c(2L)),
        complete = network.covariates.test.build.expected.since.2010(c(1L, 7L), c(3L, 2L), c(1L), c(2L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.mail.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "mail.count")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.mail.thread.count method
test_that("Test add.vertex.attribute.author.mail.thread.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks(author.relation = "mail", bins = mybins.since.2010)

    expected.attributes = list(
        range = network.covariates.test.build.expected.since.2010(c(1L, 2L), c(1L, 1L), c(1L), c(1L)),
        cumulative = network.covariates.test.build.expected.since.2010(c(1L, 2L), c(1L, 1L), c(1L), c(2L)),
        all.ranges = network.covariates.test.build.expected.since.2010(c(1L, 2L), c(1L, 2L), c(1L), c(2L)),
        project.cumulative = network.covariates.test.build.expected.since.2010(c(1L, 2L), c(3L, 1L), c(1L), c(2L)),
        project.all.ranges = network.covariates.test.build.expected.since.2010(c(1L, 2L), c(3L, 2L), c(1L), c(2L)),
        complete = network.covariates.test.build.expected.since.2010(c(1L, 2L), c(3L, 2L), c(1L), c(2L))
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.mail.thread.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "mail.thread.count")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Helper function for add.vertex.attribute.issue.*.count tests
sum.expected.attributes = function(expected.attributes.issues.only, expected.attributes.prs.only) {
    result = lapply(names(expected.attributes.issues.only), function(n) {
        issue.attr = expected.attributes.issues.only[[n]]
        pr.attr = expected.attributes.prs.only[[n]]
        sum.attr = lapply(names(issue.attr), function (n2) {
            issue.attr[[n2]] + pr.attr[[n2]]
        })
        names(sum.attr) = names(issue.attr)
        return(sum.attr)
    })
    names(result) = names(expected.attributes.issues.only)
    return(result)
}

#' Test the add.vertex.attribute.author.issue.count method
test_that("Test add.vertex.attribute.author.issue.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks(issues=TRUE, author.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(0L, 1L, 1L, 1L), c(0L, 1L, 1L), c(2L, 1L, 1L, 2L, 1L)),
        cumulative = network.covariates.test.build.expected(c(0L, 1L, 1L, 1L), c(1L, 1L, 1L), c(2L, 1L, 2L, 2L, 1L)),
        all.ranges = network.covariates.test.build.expected(c(2L, 2L, 1L, 1L), c(1L, 2L, 2L), c(2L, 1L, 2L, 2L, 1L)),
        project.cumulative = network.covariates.test.build.expected(c(2L, 1L, 2L, 1L), c(2L, 2L, 3L), c(3L, 2L, 4L, 2L, 1L)),
        project.all.ranges = network.covariates.test.build.expected(c(4L, 2L, 2L, 1L), c(2L, 3L, 4L), c(3L, 2L, 4L, 2L, 1L)),
        complete = network.covariates.test.build.expected(c(4L, 2L, 3L, 1L), c(3L, 3L, 4L), c(3L, 3L, 4L, 2L, 1L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(3L, 1L, 0L), c(1L, 1L, 1L, 0L, 0L)),
        cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(3L, 1L, 1L), c(2L, 3L, 2L, 0L, 0L)),
        all.ranges = network.covariates.test.build.expected(c(2L, 0L, 3L, 0L), c(3L, 2L, 2L), c(2L, 3L, 2L, 0L, 0L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(3L, 2L, 1L), c(3L, 3L, 2L, 0L, 0L)),
        project.all.ranges = network.covariates.test.build.expected(c(2L, 0L, 3L, 0L), c(3L, 3L, 2L), c(3L, 3L, 2L, 0L, 0L)),
        complete = network.covariates.test.build.expected(c(2L, 0L, 3L, 0L), c(3L, 3L, 2L), c(3L, 3L, 2L, 0L, 0L))
    )

    expected.attributes.both = sum.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            issue.type = "pull.requests", name = "pull.request.count"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pull.request.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})


#' Test the add.vertex.attribute.author.issues.commented.count method
test_that("Test add.vertex.attribute.author.issues.commented.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks(issues = TRUE, author.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 0L, 0L, 1L)),
        cumulative = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 1L, 1L, 1L)),
        all.ranges = network.covariates.test.build.expected(c(1L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 1L, 1L, 1L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 1L, 1L, 0L), c(1L, 2L, 2L), c(2L, 1L, 2L, 1L, 1L)),
        project.all.ranges = network.covariates.test.build.expected(c(2L, 1L, 1L, 0L), c(1L, 2L, 2L), c(2L, 1L, 2L, 1L, 1L)),
        complete = network.covariates.test.build.expected(c(2L, 1L, 1L, 0L), c(1L, 3L, 2L), c(3L, 1L, 2L, 1L, 1L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(2L, 0L, 0L), c(1L, 0L, 0L, 0L, 0L)),
        cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(2L, 0L, 1L), c(1L, 2L, 1L, 0L, 0L)),
        all.ranges = network.covariates.test.build.expected(c(1L, 0L, 2L, 0L), c(2L, 1L, 1L), c(1L, 2L, 1L, 0L, 0L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(2L, 1L, 1L), c(2L, 2L, 1L, 0L, 0L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L, 0L, 2L, 0L), c(2L, 2L, 1L), c(2L, 2L, 1L, 0L, 0L)),
        complete = network.covariates.test.build.expected(c(1L, 0L, 2L, 0L), c(2L, 2L, 1L), c(2L, 2L, 1L, 0L, 0L))
    )

    expected.attributes.both = sum.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issues.commented.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issues.commented.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issues.commented.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            issue.type = "pull.requests", name = "pull.requests.commented.count"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pull.requests.commented.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issues.commented.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issues.commented.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.issue.creation.count method
test_that("Test add.vertex.attribute.author.issue.creation.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks(issues = TRUE, author.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 0L), c(0L, 0L, 0L, 0L, 0L)),
        cumulative = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 0L), c(1L, 0L, 0L, 1L, 0L)),
        all.ranges = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 0L), c(1L, 0L, 0L, 1L, 0L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 1L, 1L, 0L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 1L, 1L, 0L)),
        complete = network.covariates.test.build.expected(c(1L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 1L, 1L, 0L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(1L, 0L, 0L), c(0L, 0L, 0L, 0L, 0L)),
        cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(1L, 0L, 1L), c(0L, 1L, 1L, 0L, 0L)),
        all.ranges = network.covariates.test.build.expected(c(1L, 0L, 1L, 0L), c(1L, 0L, 1L), c(0L, 1L, 1L, 0L, 0L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(1L, 1L, 1L), c(1L, 1L, 1L, 0L, 0L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L, 0L, 1L, 0L), c(1L, 1L, 1L), c(1L, 1L, 1L, 0L, 0L)),
        complete = network.covariates.test.build.expected(c(1L, 0L, 1L, 0L), c(1L, 1L, 1L), c(1L, 1L, 1L, 0L, 0L))
    )

    expected.attributes.both = sum.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.creation.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.creation.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.creation.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            issue.type = "pull.requests", name = "pull.request.creation.count"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pull.request.creation.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.creation.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.creation.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.issue.comment.count method
test_that("Test add.vertex.attribute.author.issue.comment.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks(issues = TRUE, author.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 1L), c(1L, 0L, 0L, 0L, 3L)),
        cumulative = network.covariates.test.build.expected(c(0L, 1L, 0L, 0L), c(0L, 1L, 1L), c(2L, 0L, 1L, 1L, 3L)),
        all.ranges = network.covariates.test.build.expected(c(1L, 1L, 0L, 0L), c(0L, 2L, 1L), c(2L, 0L, 1L, 1L, 3L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 1L, 4L, 0L), c(4L, 7L, 2L), c(8L, 4L, 2L, 1L, 3L)),
        project.all.ranges = network.covariates.test.build.expected(c(2L, 1L, 4L, 0L), c(4L, 8L, 2L), c(8L, 4L, 2L, 1L, 3L)),
        complete = network.covariates.test.build.expected(c(2L, 1L, 4L, 0L), c(4L, 9L, 2L), c(9L, 4L, 2L, 1L, 3L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(2L, 0L, 0L), c(1L, 0L, 0L, 0L, 0L)),
        cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(2L, 0L, 1L), c(1L, 2L, 1L, 0L, 0L)),
        all.ranges = network.covariates.test.build.expected(c(1L, 0L, 2L, 0L), c(2L, 1L, 1L), c(1L, 2L, 1L, 0L, 0L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 0L, 0L, 0L), c(2L, 1L, 1L), c(2L, 2L, 1L, 0L, 0L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L, 0L, 2L, 0L), c(2L, 2L, 1L), c(2L, 2L, 1L, 0L, 0L)),
        complete = network.covariates.test.build.expected(c(1L, 0L, 2L, 0L), c(2L, 2L, 1L), c(2L, 2L, 1L, 0L, 0L))
    )

    expected.attributes.both = sum.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.comment.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.comment.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.comment.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            issue.type = "pull.requests", name = "pull.request.comment.count"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pull.request.comment.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.author.issue.comment.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, issue.type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.comment.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
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

#' Test the add.vertex.attribute.author.artifact.count method
test_that("Test add.vertex.attribute.author.artifact.count", {

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
        networks.with.attr = add.vertex.attribute.author.artifact.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "artifact.count")

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.first.activity method with computation over all types.
test_that("Test add.vertex.attribute.author.first.activity with multiple types and computation over all types", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    ## lock issues in order to prevent them from being read because that alters the first activity dates
    networks.and.data$project.data$set.project.conf.entry("issues.locked", TRUE)

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

        networks.with.attributes = add.vertex.attribute.author.first.activity(
            list.of.networks = networks.and.data[["networks"]], project.data = networks.and.data[["project.data"]],
            activity.types = c("mails", "commits", "issues"), name = "first.activity", aggregation.level = level,
            default.value = NA, combine.activity.types = TRUE
        )
        actual.attributes = lapply(networks.with.attributes, igraph::get.vertex.attribute, name = "first.activity")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.first.activity method with multiple activity types and computation per type.
test_that("Test add.vertex.attribute.author.first.activity with multiple types and computation per type", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()
    expected.attributes = get.expected.first.activity()

    ## lock issues in order to prevent them from being read because that alters the first activity dates
    networks.and.data$project.data$set.project.conf.entry("issues.locked", TRUE)

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {

        networks.with.attributes = add.vertex.attribute.author.first.activity(
            list.of.networks = networks.and.data[["networks"]], project.data = networks.and.data[["project.data"]],
            activity.types = c("mails", "commits", "issues"), name = "first.activity", aggregation.level = level,
            default.value = NA, combine.activity.types = FALSE
        )
        actual.attributes = lapply(networks.with.attributes, igraph::get.vertex.attribute, name = "first.activity")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.first.activity method with one activity type and computation per type.
test_that("Test add.vertex.attribute.author.first.activity with one type and computation per type", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = get.expected.first.activity()
    expected.attributes = lapply(expected.attributes, function(level) {
        lapply(level, function(network) {
            lapply(network, function(person) {
                return(person["mails"])
            })
        })
    })


    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {

        networks.with.attributes = add.vertex.attribute.author.first.activity(
            list.of.networks = networks.and.data[["networks"]], project.data = networks.and.data[["project.data"]],
            activity.types = c("mails"), name = "first.activity", aggregation.level = level,
            default.value = NA, combine.activity.types = FALSE
        )
        actual.attributes = lapply(networks.with.attributes, igraph::get.vertex.attribute, name = "first.activity")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.active.ranges method with computation over all types
test_that("Test add.vertex.attribute.author.active.ranges with computation over all types", {

    ## Test setup
    networks.and.data = get.network.covariates.test.networks()

    ## lock issues in order to prevent them from being read because that alters the first activity dates
    networks.and.data$project.data$set.project.conf.entry("issues.locked", TRUE)

    ## Test
    networks.with.attr = add.vertex.attribute.author.active.ranges(
        networks.and.data[["networks"]], networks.and.data[["project.data"]],
        combine.activity.types = TRUE
    )
    actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "active.ranges")

    # adjust prepared expected attributes to the current use case
    expected.attributes = lapply(get.expected.active.ranges(), function(active.ranges) {
        active.ranges = lapply(active.ranges, function(person) {
            unlisted.person = list("all.activity.types" = as.list(unique(unlist(person))))
            return(unlisted.person)
        })
        return(active.ranges)
    })
    expect_identical(expected.attributes, actual.attributes)
})

#' Test default values for the add.vertex.attribute.author.active.ranges method
test_that("Test default values of add.vertex.attribute.author.active.ranges", {

    ## Test setup
    networks.and.data = get.network.covariates.test.networks()

    ## lock issues in order to prevent them from being read because that alters the first activity dates
    networks.and.data$project.data$set.project.conf.entry("issues.locked", TRUE)

    ## Test
    test.networks = networks.and.data[["networks"]]
    test.data = networks.and.data[["project.data"]]
    test.activity.types = c("mails", "issues")
    test.default.value =  "test.default.value"
    networks.with.attr = add.vertex.attribute.author.active.ranges(test.networks, test.data,
        activity.types = test.activity.types, default.value = test.default.value)
    actual.attributes = lapply(networks.with.attr, igraph:: get.vertex.attribute, name = "active.ranges")

    # adjust prepared expected attributes to the current use case
    expected.attributes = lapply(get.expected.active.ranges(), function(active.ranges) {
        active.ranges = lapply(active.ranges, function(person) {
            person.cut.activity.types = person[test.activity.types]
            person.replaced.expected.default = lapply(person.cut.activity.types, function(activity.type) {
                if (length(activity.type) == 0) {
                    activity.type = test.default.value
                }
                return(activity.type)
            })
            return(person.replaced.expected.default)
        })
        return(active.ranges)
    })
    expect_identical(expected.attributes, actual.attributes)
})

#' Test the add.vertex.attribute.author.role.simple method
test_that("Test add.vertex.attribute.author.role.simple", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(
        range = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "core")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "core")
            )
        ),
        cumulative = list(
            commit.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
            ),
            loc.count = network.covariates.test.build.expected(
                c("core"), c("core"), c("core", "core", "peripheral")
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
                c("core"), c("core"), c("core", "core", "peripheral")
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

            expect_identical(expected.attributes[[level]][[type]], actual.attributes,
                             info = sprintf("level = '%s', type = '%s'", level, type))
        })
    })
})

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unit tests for artifact networks ----------------------------------------

#' Test the add.vertex.attribute.artifact.editor.count method
test_that("Test add.vertex.attribute.artifact.editor.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes.author = list(
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
    expected.attributes.committer = list(
        range = network.covariates.test.build.expected(
            c(1L), c(1L), c(2L, 1L)),
        cumulative = network.covariates.test.build.expected(
            c(1L), c(1L), c(2L, 1L)),
        all.ranges = network.covariates.test.build.expected(
            c(1L), c(1L), c(2L, 1L)),
        project.cumulative = network.covariates.test.build.expected(
            c(1L), c(1L), c(2L, 1L)),
        project.all.ranges = network.covariates.test.build.expected(
            c(1L), c(1L), c(2L, 1L)),
        complete = network.covariates.test.build.expected(
            c(1L), c(1L), c(2L, 1L))
    )
    expected.attributes.both = list(
        range = network.covariates.test.build.expected(
            c(1L), c(2L), c(3L, 1L)),
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
        networks.with.attr.author = add.vertex.attribute.artifact.editor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )
        networks.with.attr.committer = add.vertex.attribute.artifact.editor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level, editor.definition = "committer"
        )
        networks.with.attr.both = add.vertex.attribute.artifact.editor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level, editor.definition = c("author", "committer")
        )

        actual.attributes.author = lapply(networks.with.attr.author, igraph::get.vertex.attribute, name = "editor.count")
        actual.attributes.committer = lapply(networks.with.attr.committer, igraph::get.vertex.attribute, name = "editor.count")
        actual.attributes.both = lapply(networks.with.attr.both, igraph::get.vertex.attribute, name = "editor.count")

        expect_equal(expected.attributes.author[[level]], actual.attributes.author)
        expect_equal(expected.attributes.committer[[level]], actual.attributes.committer)
        expect_equal(expected.attributes.both[[level]], actual.attributes.both)
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

#' Test the add.vertex.attribute.artifact.last.edited method
test_that("Test add.vertex.attribute.artifact.last.edited", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC")
        ),
        cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC")
        ),
        all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 16:00:45 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC")
        ),
        project.cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:59 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC")
        ),
        project.all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 16:00:45 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC")
        ),
        complete = network.covariates.test.build.expected(
            c("2016-07-12 16:00:45 UTC"), c("2016-07-12 16:00:45 UTC"),
            c("2016-07-12 16:06:32 UTC", "2016-07-12 16:06:32 UTC")
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
        networks.with.attr = add.vertex.attribute.artifact.last.edited(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "last.edited")

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

## Unit tests for mail artifact networks

#' mail thread contributor count
test_that("Test add.vertex.attribute.mail.thread.contributor.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "mail")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c(2), c(1), c(1)
        ),
        cumulative = network.covariates.test.build.expected(
            c(2), c(1), c(2)
        ),
        all.ranges = network.covariates.test.build.expected(
            c(2), c(2), c(2)
        ),
        project.cumulative = network.covariates.test.build.expected(
            c(2), c(1), c(2)
        ),
        project.all.ranges = network.covariates.test.build.expected(
            c(2), c(2), c(2)
        ),
        complete = network.covariates.test.build.expected(
            c(2), c(2), c(2)
        )
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.mail.thread.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "thread.contributor.count")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' mail thread message count
test_that("Test add.vertex.attribute.mail.thread.message.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "mail")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c(2), c(1), c(1)
        ),
        cumulative = network.covariates.test.build.expected(
            c(2), c(1), c(2)
        ),
        all.ranges = network.covariates.test.build.expected(
            c(2), c(2), c(2)
        ),
        project.cumulative = network.covariates.test.build.expected(
            c(2), c(1), c(2)
        ),
        project.all.ranges = network.covariates.test.build.expected(
            c(2), c(2), c(2)
        ),
        complete = network.covariates.test.build.expected(
            c(2), c(2), c(2)
        )
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.mail.thread.message.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "thread.message.count")

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' mail thread start date
test_that("Test add.vertex.attribute.mail.thread.start.date", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "mail")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c("2016-07-12 15:58:40 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:05:37 UTC")
        ),
        cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:40 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:04:40 UTC")
        ),
        all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 15:58:40 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:04:40 UTC")
        ),
        project.cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:40 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:04:40 UTC")
        ),
        project.all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 15:58:40 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:04:40 UTC")
        ),
        complete = network.covariates.test.build.expected(
            c("2016-07-12 15:58:40 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:04:40 UTC")
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
        networks.with.attr = add.vertex.attribute.mail.thread.start.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "thread.start.date")

        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' mail thread end date
test_that("Test add.vertex.attribute.mail.thread.end.date", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "mail")

    expected.attributes = list(
        range = network.covariates.test.build.expected(
            c("2016-07-12 15:58:50 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:05:37 UTC")
        ),
        cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:50 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:05:37 UTC")
        ),
        all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 15:58:50 UTC"), c("2016-07-12 16:05:37 UTC"), c("2016-07-12 16:05:37 UTC")
        ),
        project.cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:58:50 UTC"), c("2016-07-12 16:04:40 UTC"), c("2016-07-12 16:05:37 UTC")
        ),
        project.all.ranges = network.covariates.test.build.expected(
            c("2016-07-12 15:58:50 UTC"), c("2016-07-12 16:05:37 UTC"), c("2016-07-12 16:05:37 UTC")
        ),
        complete = network.covariates.test.build.expected(
            c("2016-07-12 15:58:50 UTC"), c("2016-07-12 16:05:37 UTC"), c("2016-07-12 16:05:37 UTC")
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
        networks.with.attr = add.vertex.attribute.mail.thread.end.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "thread.end.date")

        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_equal(expected.attributes[[level]], actual.attributes)
    })
})

#' mail thread originating mailing list
test_that("Test add.vertex.attribute.mail.thread.originating.mailing.list", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "mail", bins = mybins.since.2010)

    expected.attributes = network.covariates.test.build.expected.since.2010(
            c("42", "42", "42"), c("13"), c("13"), c("13")
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.mail.thread.originating.mailing.list(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute,
                                   name = "thread.originating.mailing.list")

        expect_equal(expected.attributes, actual.attributes)
    })
})

## Unit tests for issue artifact networks

#' Helper function for add.vertex.attribute.issue.* tests
#'
#' Merges expected data for issue networks, where attributes may be considered for issues only, PRs only, or both.
#' This function returns the data for both, given the data for issues only and PRs only, where \code{NA} is used for
#' vertices of the other type.
#' The data is given as lists of lists of vectors, and merged by replacing the \code{NA}-values of the first list
#' with the non-\code{NA} values of the second. Therefore, it only works if all vertices have a non-\code{NA} value in
#' exactly one of the two lists.
#'
#' @param expected.attributes.issues.only a list of lists of vectors, containing some value for each issue vertex and
#'                                        \code{NA} for PR vertices
#' @param expected.attributes.prs.only a list of lists of vectors, containing some value for each PR vertex and
#'                                     \code{NA} for issue vertices
#'
#' @return a list of lists of vectors, containing values for issue and PR vertices
merge.expected.attributes = function(expected.attributes.issues.only, expected.attributes.prs.only) {
    result = lapply(names(expected.attributes.issues.only), function(n) {
        issue.attr = expected.attributes.issues.only[[n]]
        pr.attr = expected.attributes.prs.only[[n]]
        sum.attr = lapply(names(issue.attr), function (n2) {
            a = issue.attr[[n2]]
            b = pr.attr[[n2]]
            ## assign the non-NA values of b to the previously-NA values of a.
            ## this only works properly if, at each index, exactly one of the vectors is NA.
            a[is.na(a)] = b[!is.na(b)]
            return(a)
        })
        names(sum.attr) = names(issue.attr)
        return(sum.attr)
    })
    names(result) = names(expected.attributes.issues.only)
    return(result)
}

#' issue contributor count
test_that("Test add.vertex.attribute.issue.contributor.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(1L, 2L, NA), c(NA, 1L, NA, 1L, NA), c(NA, 3L, 2L, NA, 2L)),
        cumulative = network.covariates.test.build.expected(c(1L, 2L, NA), c(NA, 1L, NA, 2L, NA), c(NA, 3L, 2L, NA, 4L)),
        all.ranges = network.covariates.test.build.expected(c(4L, 3L, NA), c(NA, 2L, NA, 4L, NA), c(NA, 3L, 2L, NA, 4L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 2L, NA), c(NA, 2L, NA, 2L, NA), c(NA, 3L, 3L, NA, 4L)),
        project.all.ranges = network.covariates.test.build.expected(c(4L, 3L, NA), c(NA, 3L, NA, 4L, NA), c(NA, 3L, 3L, NA, 4L)),
        complete = network.covariates.test.build.expected(c(5L, 3L, NA), c(NA, 3L, NA, 5L, NA), c(NA, 3L, 3L, NA, 5L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(NA, NA, 1L), c(1L, NA, 2L, NA, 1L), c(2L, NA, NA, 1L, NA)),
        cumulative = network.covariates.test.build.expected(c(NA, NA, 1L), c(2L, NA, 2L, NA, 1L), c(3L, NA, NA, 2L, NA)),
        all.ranges = network.covariates.test.build.expected(c(NA, NA, 3L), c(3L, NA, 2L, NA, 2L), c(3L, NA, NA, 2L, NA)),
        project.cumulative = network.covariates.test.build.expected(c(NA, NA, 1L), c(2L, NA, 2L, NA, 2L), c(3L, NA, NA, 3L, NA)),
        project.all.ranges = network.covariates.test.build.expected(c(NA, NA, 3L), c(3L, NA, 2L, NA, 3L), c(3L, NA, NA, 3L, NA)),
        complete = network.covariates.test.build.expected(c(NA, NA, 3L), c(3L, NA, 2L, NA, 3L), c(3L, NA, NA, 3L, NA))
    )

    expected.attributes.both = merge.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.contributor.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.contributor.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.contributor.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

test_that("Test add.vertex.attribute.issue.contributor.count with issues.only.comments", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue",
                                                             issues.only.comments = TRUE)

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(NA, 1L), c(NA, NA, 1L, 1L), c(NA, 2L)),
        cumulative = network.covariates.test.build.expected(c(NA, 1L), c(NA, NA, 1L, 1L), c(NA, 2L)),
        all.ranges = network.covariates.test.build.expected(c(NA, 1L), c(NA, NA, 2L, 1L), c(NA, 2L)),
        project.cumulative = network.covariates.test.build.expected(c(NA, 1L), c(NA, NA, 1L, 1L), c(NA, 2L)),
        project.all.ranges = network.covariates.test.build.expected(c(NA, 1L), c(NA, NA, 2L, 1L), c(NA, 2L)),
        complete = network.covariates.test.build.expected(c(NA, 1L), c(NA, NA, 2L, 2L), c(NA, 2L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(1L, NA), c(1L, 1L, NA, NA), c(1L, NA)),
        cumulative = network.covariates.test.build.expected(c(1L, NA), c(2L, 1L, NA, NA), c(3L, NA)),
        all.ranges = network.covariates.test.build.expected(c(3L, NA), c(3L, 1L, NA, NA), c(3L, NA)),
        project.cumulative = network.covariates.test.build.expected(c(1L, NA), c(2L, 1L, NA, NA), c(3L, NA)),
        project.all.ranges = network.covariates.test.build.expected(c(3L, NA), c(3L, 1L, NA, NA), c(3L, NA)),
        complete = network.covariates.test.build.expected(c(3L, NA), c(3L, 1L, NA, NA), c(3L, NA))
    )

    expected.attributes.both = merge.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.contributor.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.contributor.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.contributor.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

test_that("Test add.vertex.attribute.issue.contributor.count with issues.only.comments and use.unfiltered.data", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue",
                                                             issues.only.comments = TRUE)

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(NA, 2L), c(NA, NA, 1L, 1L), c(NA, 2L)),
        cumulative = network.covariates.test.build.expected(c(NA, 2L), c(NA, NA, 1L, 2L), c(NA, 2L)),
        all.ranges = network.covariates.test.build.expected(c(NA, 3L), c(NA, NA, 2L, 4L), c(NA, 2L)),
        project.cumulative = network.covariates.test.build.expected(c(NA, 2L), c(NA, NA, 2L, 2L), c(NA, 3L)),
        project.all.ranges = network.covariates.test.build.expected(c(NA, 3L), c(NA, NA, 3L, 4L), c(NA, 3L)),
        complete = network.covariates.test.build.expected(c(NA, 3L), c(NA, NA, 3L, 5L), c(NA, 3L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(1L, NA), c(1L, 2L, NA, NA), c(2L, NA)),
        cumulative = network.covariates.test.build.expected(c(1L, NA), c(2L, 2L, NA, NA), c(3L, NA)),
        all.ranges = network.covariates.test.build.expected(c(3L, NA), c(3L, 2L, NA, NA), c(3L, NA)),
        project.cumulative = network.covariates.test.build.expected(c(1L, NA), c(2L, 2L, NA, NA), c(3L, NA)),
        project.all.ranges = network.covariates.test.build.expected(c(3L, NA), c(3L, 2L, NA, NA), c(3L, NA)),
        complete = network.covariates.test.build.expected(c(3L, NA), c(3L, 2L, NA, NA), c(3L, NA))
    )

    expected.attributes.both = merge.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "issues", use.unfiltered.data = TRUE
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.contributor.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests", use.unfiltered.data = TRUE)

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.contributor.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.contributor.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "all", use.unfiltered.data = TRUE
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.contributor.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

#' issue event count
test_that("Test add.vertex.attribute.issue.event.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(1L, 3L, NA), c(NA, 2L, NA, 1L, NA), c(NA, 4L, 4L, NA, 2L)),
        cumulative = network.covariates.test.build.expected(c(1L, 3L, NA), c(NA, 2L, NA, 2L, NA), c(NA, 7L, 6L, NA, 4L)),
        all.ranges = network.covariates.test.build.expected(c(4L, 7L, NA), c(NA, 6L, NA, 4L, NA), c(NA, 7L, 6L, NA, 4L)),
        project.cumulative = network.covariates.test.build.expected(c(1L, 3L, NA), c(NA, 3L, NA, 2L, NA), c(NA, 7L, 7L, NA, 4L)),
        project.all.ranges = network.covariates.test.build.expected(c(4L, 7L, NA), c(NA, 7L, NA, 4L, NA), c(NA, 7L, 7L, NA, 4L)),
        complete = network.covariates.test.build.expected(c(8L, 8L, NA), c(NA, 7L, NA, 8L, NA), c(NA, 8L, 7L, NA, 8L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(NA, NA, 2L), c(1L, NA, 2L, NA, 1L), c(2L, NA, NA, 1L, NA)),
        cumulative = network.covariates.test.build.expected(c(NA, NA, 2L), c(3L, NA, 2L, NA, 1L), c(5L, NA, NA, 2L, NA)),
        all.ranges = network.covariates.test.build.expected(c(NA, NA, 5L), c(5L, NA, 2L, NA, 2L), c(5L, NA, NA, 2L, NA)),
        project.cumulative = network.covariates.test.build.expected(c(NA, NA, 2L), c(3L, NA, 2L, NA, 2L), c(5L, NA, NA, 3L, NA)),
        project.all.ranges = network.covariates.test.build.expected(c(NA, NA, 5L), c(5L, NA, 2L, NA, 3L), c(5L, NA, NA, 3L, NA)),
        complete = network.covariates.test.build.expected(c(NA, NA, 5L), c(5L, NA, 2L, NA, 3L), c(5L, NA, NA, 3L, NA))
    )

    expected.attributes.both = merge.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.event.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.event.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.event.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.event.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.event.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.event.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

#' issue comment count
test_that("Test add.vertex.attribute.issue.comment.count", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(c(0L, 1L, NA), c(NA, 1L, NA, 1L, NA), c(NA, 0L, 4L, NA, 0L)),
        cumulative = network.covariates.test.build.expected(c(0L, 1L, NA), c(NA, 1L, NA, 1L, NA), c(NA, 1L, 5L, NA, 1L)),
        all.ranges = network.covariates.test.build.expected(c(1L, 1L, NA), c(NA, 5L, NA, 1L, NA), c(NA, 1L, 5L, NA, 1L)),
        project.cumulative = network.covariates.test.build.expected(c(0L, 1L, NA), c(NA, 1L, NA, 1L, NA), c(NA, 1L, 5L, NA, 1L)),
        project.all.ranges = network.covariates.test.build.expected(c(1L, 1L, NA), c(NA, 5L, NA, 1L, NA), c(NA, 1L, 5L, NA, 1L)),
        complete = network.covariates.test.build.expected(c(2L, 1L, NA), c(NA, 5L, NA, 2L, NA), c(NA, 1L, 5L, NA, 2L))
    )

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(c(NA, NA, 1L), c(1L, NA, 1L, NA, 0L), c(1L, NA, NA, 0L, NA)),
        cumulative = network.covariates.test.build.expected(c(NA, NA, 1L), c(2L, NA, 1L, NA, 0L), c(3L, NA, NA, 0L, NA)),
        all.ranges = network.covariates.test.build.expected(c(NA, NA, 3L), c(3L, NA, 1L, NA, 0L), c(3L, NA, NA, 0L, NA)),
        project.cumulative = network.covariates.test.build.expected(c(NA, NA, 1L), c(2L, NA, 1L, NA, 1L), c(3L, NA, NA, 1L, NA)),
        project.all.ranges = network.covariates.test.build.expected(c(NA, NA, 3L), c(3L, NA, 1L, NA, 1L), c(3L, NA, NA, 1L, NA)),
        complete = network.covariates.test.build.expected(c(NA, NA, 3L), c(3L, NA, 1L, NA, 1L), c(3L, NA, NA, 1L, NA))
    )

    expected.attributes.both = merge.expected.attributes(expected.attributes.issues.only, expected.attributes.prs.only)

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.comment.event.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.comment.event.count")

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.comment.event.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.comment.event.count")

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.comment.event.count(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.comment.event.count")

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

#' issue opened date
test_that("Test add.vertex.attribute.issue.opened.date", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = network.covariates.test.build.expected(
        c("2016-07-12 14:30:13",
          "2016-07-12 15:59:25",
          NA),
        c(NA,
          "2016-07-12 16:01:30",
          NA,
          "2016-07-12 14:30:13",
          NA),
        c(NA,
          "2016-07-12 15:59:25",
          "2016-07-12 16:01:30",
          NA,
          "2016-07-12 14:30:13"))

    expected.attributes.prs.only = network.covariates.test.build.expected(
        c(NA,
          NA,
          "2016-07-14 13:37:00"),
        c("2016-07-14 13:37:00",
          NA,
          "2016-07-12 16:02:02",
          NA,
          "2016-07-12 14:59:25"),
        c("2016-07-14 13:37:00",
          NA,
          NA,
          "2016-07-12 14:59:25",
          NA))

    expected.attributes.both = network.covariates.test.build.expected(
        c("2016-07-12 14:30:13",
          "2016-07-12 15:59:25",
          "2016-07-14 13:37:00"),
        c("2016-07-14 13:37:00",
          "2016-07-12 16:01:30",
          "2016-07-12 16:02:02",
          "2016-07-12 14:30:13",
          "2016-07-12 14:59:25"),
        c("2016-07-14 13:37:00",
          "2016-07-12 15:59:25",
          "2016-07-12 16:01:30",
          "2016-07-12 14:59:25",
          "2016-07-12 14:30:13"))

    ## convert date strings to POSIXct
    expected.attributes.issues.only = lapply(expected.attributes.issues.only, function(date.vector) {get.date.from.string(date.vector)})

    expected.attributes.prs.only = lapply(expected.attributes.prs.only, function(date.vector) {get.date.from.string(date.vector)})

    expected.attributes.both = lapply(expected.attributes.both, function(date.vector) {get.date.from.string(date.vector)})

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.opened.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.opened.date")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.issues.only, actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.opened.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.opened.date")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.prs.only, actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.opened.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.opened.date")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.both, actual.attributes)
    })
})

#' issue closed date
test_that("Test add.vertex.attribute.issue.closed.date", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = network.covariates.test.build.expected(
        c(NA,
          "2016-07-12 16:06:30",
          NA),
        c(NA,
          NA,
          NA,
          NA,
          NA),
        c(NA,
          "2016-07-12 16:06:30",
          NA,
          NA,
          NA))

    expected.attributes.prs.only = network.covariates.test.build.expected(
        c(NA,
          NA,
          NA),
        c(NA,
          NA,
          NA,
          NA,
          "2016-07-12 16:04:59"),
        c(NA,
          NA,
          NA,
          "2016-07-12 16:04:59",
          NA))

    expected.attributes.both = network.covariates.test.build.expected(
        c(NA,
          "2016-07-12 16:06:30",
          NA),
        c(NA,
          NA,
          NA,
          NA,
          "2016-07-12 16:04:59"),
        c(NA,
          "2016-07-12 16:06:30",
          NA,
          "2016-07-12 16:04:59",
          NA))

    ## convert date strings to POSIXct
    expected.attributes.issues.only = lapply(expected.attributes.issues.only, function(date.vector) {get.date.from.string(date.vector)})

    expected.attributes.prs.only = lapply(expected.attributes.prs.only, function(date.vector) {get.date.from.string(date.vector)})

    expected.attributes.both = lapply(expected.attributes.both, function(date.vector) {get.date.from.string(date.vector)})

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.closed.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.closed.date")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.issues.only, actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.closed.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.closed.date")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.prs.only, actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.closed.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.closed.date")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.both, actual.attributes)
    })
})

#' issue last activity date
test_that("Test add.vertex.attribute.issue.last.activity.date", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = list(
        range = network.covariates.test.build.expected(
            c("2016-07-12 15:30:02", "2016-07-12 15:59:59", NA),
            c(NA                   , "2016-07-12 16:02:30", NA                   , "2016-07-12 16:03:59", NA),
            c(NA                   , "2016-08-31 16:45:09", "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02")),
        cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:30:02", "2016-07-12 15:59:59", NA),
            c(NA                   , "2016-07-12 16:02:30", NA                   , "2016-07-12 16:03:59", NA),
            c(NA                   , "2016-08-31 16:45:09", "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02")),
        all.ranges = network.covariates.test.build.expected(
            c("2016-08-31 15:30:02", "2016-08-31 16:45:09", NA),
            c(NA                   , "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02", NA),
            c(NA                   , "2016-08-31 16:45:09", "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02")),
        project.cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:30:02", "2016-07-12 15:59:59", NA),
            c(NA                   , "2016-07-12 16:02:30", NA                   , "2016-07-12 16:03:59", NA),
            c(NA                   , "2016-08-31 16:45:09", "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02")),
        project.all.ranges = network.covariates.test.build.expected(
            c("2016-08-31 15:30:02", "2016-08-31 16:45:09", NA),
            c(NA                   , "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02", NA),
            c(NA                   , "2016-08-31 16:45:09", "2016-07-28 06:27:52", NA                   , "2016-08-31 15:30:02")),
        complete = network.covariates.test.build.expected(
            c("2017-05-23 12:32:39", "2016-10-05 16:45:09", NA),
            c(NA                   , "2016-07-28 06:27:52", NA                   , "2017-05-23 12:32:39", NA),
            c(NA                   , "2016-10-05 16:45:09", "2016-07-28 06:27:52", NA                   , "2017-05-23 12:32:39")))

    expected.attributes.prs.only = list(
        range = network.covariates.test.build.expected(
            c(NA                   , NA                   , "2016-07-12 15:59:59"),
            c("2016-07-12 16:01:01", NA                   , "2016-07-12 16:02:02", NA                   , "2016-07-12 16:04:59"),
            c("2016-07-14 13:37:00", NA                   , NA                   , "2016-08-07 15:30:00", NA)),
        cumulative = network.covariates.test.build.expected(
            c(NA                   , NA                   , "2016-07-12 15:59:59"),
            c("2016-07-12 16:01:01", NA                   , "2016-07-12 16:02:02", NA                   , "2016-07-12 16:04:59"),
            c("2016-07-14 13:37:00", NA                   , NA                   , "2016-08-07 15:30:00", NA)),
        all.ranges = network.covariates.test.build.expected(
            c(NA                   , NA                   , "2016-07-14 13:37:00"),
            c("2016-07-14 13:37:00", NA                   , "2016-07-12 16:02:02", NA                   , "2016-08-07 15:30:00"),
            c("2016-07-14 13:37:00", NA                   , NA                   , "2016-08-07 15:30:00", NA)),
        project.cumulative = network.covariates.test.build.expected(
            c(NA                   , NA                   , "2016-07-12 15:59:59"),
            c("2016-07-12 16:01:01", NA                   , "2016-07-12 16:02:02", NA                   , "2016-07-12 16:04:59"),
            c("2016-07-14 13:37:00", NA                   , NA                   , "2016-08-07 15:30:00", NA)),
        project.all.ranges = network.covariates.test.build.expected(
            c(NA                   , NA                   , "2016-07-14 13:37:00"),
            c("2016-07-14 13:37:00", NA                   , "2016-07-12 16:02:02", NA                   , "2016-08-07 15:30:00"),
            c("2016-07-14 13:37:00", NA                   , NA                   , "2016-08-07 15:30:00", NA)),
        complete = network.covariates.test.build.expected(
            c(NA                   , NA                   , "2016-07-14 13:37:00"),
            c("2016-07-14 13:37:00", NA                   , "2016-07-12 16:02:02", NA                   , "2016-08-07 15:30:00"),
            c("2016-07-14 13:37:00", NA                   , NA                   , "2016-08-07 15:30:00", NA)))

    expected.attributes.both = list(
        range = network.covariates.test.build.expected(
            c("2016-07-12 15:30:02", "2016-07-12 15:59:59", "2016-07-12 15:59:59"),
            c("2016-07-12 16:01:01", "2016-07-12 16:02:30", "2016-07-12 16:02:02", "2016-07-12 16:03:59", "2016-07-12 16:04:59"),
            c("2016-07-14 13:37:00", "2016-08-31 16:45:09", "2016-07-28 06:27:52", "2016-08-07 15:30:00", "2016-08-31 15:30:02")),
        cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:30:02", "2016-07-12 15:59:59", "2016-07-12 15:59:59"),
            c("2016-07-12 16:01:01", "2016-07-12 16:02:30", "2016-07-12 16:02:02", "2016-07-12 16:03:59", "2016-07-12 16:04:59"),
            c("2016-07-14 13:37:00", "2016-08-31 16:45:09", "2016-07-28 06:27:52", "2016-08-07 15:30:00", "2016-08-31 15:30:02")),
        all.ranges = network.covariates.test.build.expected(
            c("2016-08-31 15:30:02", "2016-08-31 16:45:09", "2016-07-14 13:37:00"),
            c("2016-07-14 13:37:00", "2016-07-28 06:27:52", "2016-07-12 16:02:02", "2016-08-31 15:30:02", "2016-08-07 15:30:00"),
            c("2016-07-14 13:37:00", "2016-08-31 16:45:09", "2016-07-28 06:27:52", "2016-08-07 15:30:00", "2016-08-31 15:30:02")),
        project.cumulative = network.covariates.test.build.expected(
            c("2016-07-12 15:30:02", "2016-07-12 15:59:59", "2016-07-12 15:59:59"),
            c("2016-07-12 16:01:01", "2016-07-12 16:02:30", "2016-07-12 16:02:02", "2016-07-12 16:03:59", "2016-07-12 16:04:59"),
            c("2016-07-14 13:37:00", "2016-08-31 16:45:09", "2016-07-28 06:27:52", "2016-08-07 15:30:00", "2016-08-31 15:30:02")),
        project.all.ranges = network.covariates.test.build.expected(
            c("2016-08-31 15:30:02", "2016-08-31 16:45:09", "2016-07-14 13:37:00"),
            c("2016-07-14 13:37:00", "2016-07-28 06:27:52", "2016-07-12 16:02:02", "2016-08-31 15:30:02", "2016-08-07 15:30:00"),
            c("2016-07-14 13:37:00", "2016-08-31 16:45:09", "2016-07-28 06:27:52", "2016-08-07 15:30:00", "2016-08-31 15:30:02")),
        complete = network.covariates.test.build.expected(
            c("2017-05-23 12:32:39", "2016-10-05 16:45:09", "2016-07-14 13:37:00"),
            c("2016-07-14 13:37:00", "2016-07-28 06:27:52", "2016-07-12 16:02:02", "2017-05-23 12:32:39", "2016-08-07 15:30:00"),
            c("2016-07-14 13:37:00", "2016-10-05 16:45:09", "2016-07-28 06:27:52", "2016-08-07 15:30:00", "2017-05-23 12:32:39")))

    ## convert date strings to POSIXct
    expected.attributes.issues.only = lapply(expected.attributes.issues.only, function(times) {
        lapply(times, function(date.vector) {
            get.date.from.string(date.vector)
        })
    })
    expected.attributes.prs.only = lapply(expected.attributes.prs.only, function(times) {
        lapply(times, function(date.vector) {
            get.date.from.string(date.vector)
        })
    })
    expected.attributes.both = lapply(expected.attributes.both, function(times) {
        lapply(times, function(date.vector) {
            get.date.from.string(date.vector)
        })
    })

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.last.activity.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.last.activity")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.issues.only[[level]], actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.last.activity.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.last.activity")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.prs.only[[level]], actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.last.activity.date(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.last.activity")
        ## convert UNIX timestamps to POSIXct
        actual.attributes = lapply(actual.attributes, get.date.from.unix.timestamp)

        expect_identical(expected.attributes.both[[level]], actual.attributes)
    })
})

#' issue title
test_that("Test add.vertex.attribute.issue.title", {
    ## Test setup
    networks.and.data = get.network.covariates.test.networks("artifact", issues = TRUE, artifact.relation = "issue")

    expected.attributes.issues.only = network.covariates.test.build.expected(
            c("Distinguish directedness of networks and edge-construction algorithm",
              "Error in construct.networks.from.list for openssl function networks",
              NA),
            c(NA,
              "[ZEPPELIN-332] CNFE when running SQL query against Cassandra temp table",
              NA,
              "Distinguish directedness of networks and edge-construction algorithm",
              NA),
            c(NA,
              "Error in construct.networks.from.list for openssl function networks",
              "[ZEPPELIN-332] CNFE when running SQL query against Cassandra temp table",
              NA,
              "Distinguish directedness of networks and edge-construction algorithm"))

    expected.attributes.prs.only = network.covariates.test.build.expected(
        c(NA,
          NA,
          "Example pull request 1"),
        c("Example pull request 1",
          NA,
          "Example pull request 4",
          NA,
          "Example pull request 2"),
        c("Example pull request 1",
          NA,
          NA,
          "Example pull request 2",
          NA))

    expected.attributes.both = network.covariates.test.build.expected(
        c("Distinguish directedness of networks and edge-construction algorithm",
          "Error in construct.networks.from.list for openssl function networks",
          "Example pull request 1"),
        c("Example pull request 1",
          "[ZEPPELIN-332] CNFE when running SQL query against Cassandra temp table",
          "Example pull request 4",
          "Distinguish directedness of networks and edge-construction algorithm",
          "Example pull request 2"),
        c("Example pull request 1",
          "Error in construct.networks.from.list for openssl function networks",
          "[ZEPPELIN-332] CNFE when running SQL query against Cassandra temp table",
          "Example pull request 2",
          "Distinguish directedness of networks and edge-construction algorithm"))

    ## Test issues only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.title(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "issues"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.title")

        expect_identical(expected.attributes.issues.only, actual.attributes)
    })

    # Test PRs only

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.title(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level,
            type = "pull.requests")

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pr.title")

        expect_identical(expected.attributes.prs.only, actual.attributes)
    })

    # Test both

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.title(
            networks.and.data[["networks"]], networks.and.data[["project.data"]], aggregation.level = level, type = "all"
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.title")

        expect_identical(expected.attributes.both, actual.attributes)
    })
})

#' pull request state
test_that("Test add.vertex.attribute.pr.open.merged.or.closed", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "issue")

    expected.attributes = network.covariates.test.build.expected(
        c(NA, NA, "open"), c("open", NA, "open", NA, "merged"), c("open", NA, NA, "merged", NA)
    )

    ## Test

    networks.with.attr = add.vertex.attribute.pr.open.merged.or.closed(
        networks.and.data[["networks"]], networks.and.data[["project.data"]])

    actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "pull.request.state")

    expect_equal(expected.attributes, actual.attributes)
})

#' issue is pull request
test_that("Test add.vertex.attribute.issue.is.pull.request", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact", artifact.relation = "issue")

    expected.attributes = network.covariates.test.build.expected(
        c(FALSE, FALSE, TRUE), c(TRUE, FALSE, TRUE, FALSE, TRUE), c(TRUE, FALSE, FALSE, TRUE, FALSE)
    )

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.issue.is.pull.request(
            networks.and.data[["networks"]], networks.and.data[["project.data"]],
            aggregation.level = level
        )

        actual.attributes = lapply(networks.with.attr, igraph::get.vertex.attribute, name = "issue.is.pull.request")

        expect_equal(expected.attributes, actual.attributes)
    })
})

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unit tests for empty attribute data -------------------------------------

#' Test addition of attributes despite of empty data
test_that("Test addition of attributes despite of empty data", {

    bins = get.date.from.string(c("2010-01-01", "2010-02-01"))
    range = construct.ranges(bins)

    ## construct "empty" RangeData object:
    ## 1) get ProjectData object
    proj.data = get.network.covariates.test.networks()[["project.data"]]
    ## 2) split it with obscure ranges to get rid of data
    proj.data.empty = split.data.time.based(project.data = proj.data, bins = bins)[[1]]

    ## construct empty network
    networks = list(create.empty.network(add.attributes = TRUE))
    names(networks) = range

    ## add commit-count attribute
    net.commit.count = add.vertex.attribute.author.commit.count(networks, proj.data.empty, default = 0L)[[1]]
    expect_true("commit.count" %in% igraph::list.vertex.attributes(net.commit.count))

    ## add author-role attribute:
    ## 1) construct empty classification
    classification = list(get.author.class(data.frame(), "foo", classification.category = "count"))
    names(classification) = range
    ## 2) add attribute
    net.author.role = add.vertex.attribute.author.role(networks, classification, default = "unclassified")[[1]]
    expect_true("author.role" %in% igraph::list.vertex.attributes(net.author.role))

})

#' Test addition of attributes despite of non-captured vertices
test_that("Test addition of attributes despite of non-captured vertices", {

    bins = get.date.from.string(c("2010-01-01", "2010-02-01"))
    range = construct.ranges(bins)

    ## construct "empty" RangeData object:
    ## 1) get ProjectData object
    proj.data = get.network.covariates.test.networks()[["project.data"]]
    ## 2) split it with obscure ranges to get rid of data
    proj.data.empty = split.data.time.based(project.data = proj.data, bins = bins)[[1]]

    ## construct empty network with one additional vertex
    network = create.empty.network(add.attributes = TRUE) +
        igraph::vertices("<unknown-author>", type = TYPE.AUTHOR, kind = TYPE.AUTHOR)
    networks = list(network)
    names(networks) = range

    ## add commit-count attribute
    net.commit.count = add.vertex.attribute.author.commit.count.committer.and.author(networks, proj.data.empty, default = 0L)[[1]]

    ## check existence and proper value
    expect_true("commit.count.committer.and.author" %in% igraph::list.vertex.attributes(net.commit.count))
    expect_identical(igraph::get.vertex.attribute(net.commit.count, "commit.count.committer.and.author"), 0L)

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unit tests for helper functions------------------------------------------

#' Test get.first.activity.data with missing commits, mails, and issues.
#' Not all authors are expected to have activities in all data sources
test_that("Test get.first.activity.data with missing commits, mails, and issues", {

    ## initialize a ProjectData object with the ProjectConf
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)

    ## create a RangeData object with the same data sources as 'proj.data.base'.
    ## some data is already missing; this is used to test whether the function correctly sets missing data as 'NA'.
    timestamps = proj.data.base$get.data.timestamps(outermost = TRUE)
    range.data = split.data.time.based(proj.data.base,
                                       bins = c(timestamps[["start"]][[1]], timestamps[["end"]][[1]]))[[1]]

    first.activity.data = get.first.activity.data(range.data)

    expected.activity.data = list(
        "Björn" = list(commits = get.date.from.string("2016-07-12 15:58:59"),
                       mails = get.date.from.string("2004-10-09 18:38:13"),
                       issues = get.date.from.string("2013-05-05 21:46:30")),
        "Olaf" = list(commits = get.date.from.string("2016-07-12 16:00:45"),
                      mails = get.date.from.string("2016-07-12 15:58:50"),
                      issues = get.date.from.string("2013-05-25 03:25:06")),
        "Thomas" = list(commits = get.date.from.string("2016-07-12 16:06:32"),
                        mails = get.date.from.string("2016-07-12 16:04:40"),
                        issues = get.date.from.string("2013-04-21 23:52:09")),
        "Fritz fritz@example.org" = list(commits = get.date.from.string(NA),
                                         mails = get.date.from.string("2010-07-12 11:05:35"),
                                         issues = get.date.from.string(NA)),
        "georg" = list(commits = get.date.from.string(NA),
                       mails = get.date.from.string("2010-07-12 12:05:34"),
                       issues = get.date.from.string(NA)),
        "Hans" = list(commits = get.date.from.string(NA),
                      mails = get.date.from.string("2010-07-12 12:05:40"),
                      issues = get.date.from.string(NA)),
        "udo" = list(commits = get.date.from.string(NA),
                     mails = get.date.from.string("2010-07-12 10:05:36"),
                     issues = get.date.from.string(NA)),
        "Karl" = list(commits = get.date.from.string(NA),
                      mails = get.date.from.string(NA),
                      issues = get.date.from.string("2016-07-12 15:59:59")),
        "Max" = list(commits = get.date.from.string(NA),
                      mails = get.date.from.string(NA),
                      issues = get.date.from.string("2016-07-15 20:07:47"))
    )

    expect_equal(first.activity.data, expected.activity.data)
})

#' Test get.first.activity.data with missing commits and mails for all authors
test_that("Test get.first.activity.data with missing commits and mails for all authors", {

    ## initialize a ProjectData object with the ProjectConf
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data = ProjectData$new(project.conf = proj.conf)

    ## get the timestamps for splitting before discarding the data
    timestamps = proj.data$get.data.timestamps(outermost = TRUE)

    ## clear commit and mail data completely so that all authors have neither any commits nor mails left
    proj.data$set.commits(create.empty.commits.list())
    proj.data$set.mails(create.empty.mails.list())

    ## also lock the data sources in order to prevent reloading the data when calling the getters
    proj.conf$update.value("commits.locked", TRUE)
    proj.conf$update.value("mails.locked", TRUE)

    ## create a RangeData object with the same data sources as 'proj.data.base'.
    ## some data is already missing; this is used to test whether the function correctly sets missing data as 'NA'.
    range.data = split.data.time.based(proj.data,
                                       bins = c(timestamps[["start"]][[1]], timestamps[["end"]][[1]]),
                                       split.basis = c("issues"))[[1]]

    first.activity.data = get.first.activity.data(range.data)

    ## leave the authors without any data out
    expected.activity.data = list(
        "Björn" = list(commits = get.date.from.string(NA),
                       mails = get.date.from.string(NA),
                       issues = get.date.from.string("2013-05-05 21:46:30")),
        "Karl" = list(commits = get.date.from.string(NA),
                      mails = get.date.from.string(NA),
                      issues = get.date.from.string("2016-07-12 15:59:59")),
        "Max" = list(commits = get.date.from.string(NA),
                     mails = get.date.from.string(NA),
                     issues = get.date.from.string("2016-07-15 20:07:47")),
        "Olaf" = list(commits = get.date.from.string(NA),
                      mails = get.date.from.string(NA),
                      issues = get.date.from.string("2013-05-25 03:25:06")),
        "Thomas" = list(commits = get.date.from.string(NA),
                        mails = get.date.from.string(NA),
                        issues = get.date.from.string("2013-04-21 23:52:09")))

    expect_equal(first.activity.data, expected.activity.data)
})
