## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"
AGGREGATION.LEVELS = c("range", "cumulative", "project")

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility functions -------------------------------------------------------

#' Load test data and generate test networks
#' @return Tuple containing project data and list of networks
get.network.covariates.test.networks = function(network.type = c("author", "artifact")) {

    network.type.function = paste("get", match.arg(network.type), "network", sep = ".")

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))

    ## retrieve project data and network builder
    project.data = ProjectData$new(proj.conf)

    ## split data
    mybins = parse.date(c("2016-07-12 15:00:00", "2016-07-12 16:00:00", "2016-07-12 16:05:00", "2030-01-01 00:00:00"))
    input.data = split.data.time.based(project.data, bins = mybins)
    input.data.networks = lapply(input.data, function(d) NetworkBuilder$new(d, net.conf)[[network.type.function]]())

    return(list("networks" = input.data.networks, "project.data" = project.data))
}

#' Get splitted test data
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
#' @param range.data The current range data
#' @param current.network The current network
#' @return A list containing the value 1 for each author except "Olaf"
test.compute.attr = function(range.data, current.network) {
    authors = range.data$get.authors()[["author.name"]]

    ## Olaf should get default value
    authors = authors[-which(authors == "Olaf")]

    attributes = lapply(authors, function(name) 1)
    names(attributes) = authors
    return(attributes)
}

#' Build list with appropriate range names
#' @param a Value for first range
#' @param b Value for second range
#' @param c Value for third range
#' @return The list of a,b,c with range names
network.covariates.test.build.expected = function(a,b,c) {
    arguments = list(a,b,c)

    names(arguments) = c('2016-07-12 15:00:00-2016-07-12 16:00:00',
                         '2016-07-12 16:00:00-2016-07-12 16:05:00',
                         '2016-07-12 16:05:00-2030-01-01 00:00:00')

    return(arguments)
}

dateList = function(...) {
    return(parse.date(list(...)))
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unit tests for author networks ------------------------------------------

#' Test the add.vertex.attribute method
test_that("Test add.vertex.attribute", {
    ## Test setup

    test.networks = get.network.covariates.test.networks.data()
    expected.attributes = network.covariates.test.build.expected(list(1), list(42), list(42, 1, 1))

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

    expected.attributes = network.covariates.test.build.expected(list(1), list(42), list(42, 1, 1))

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

    expected.attributes = list(range = network.covariates.test.build.expected(list(1L), list(1L), list(1L, 1L, 1L)),
                               cumulative = network.covariates.test.build.expected(list(1L), list(1L), list(2L, 1L,  1L)),
                               project = network.covariates.test.build.expected(list(1L), list(2L), list(2L, 1L, 1L)))

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.commit.count.author(networks.and.data[["networks"]],
                                                               networks.and.data[["project.data"]],
                                                            aggregation.level = level)
        actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "commit.count"))

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})


#' Test the add.vertex.attribute.author.email method
test_that("Test add.vertex.attribute.author.email", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = network.covariates.test.build.expected(list("hunsen@fim.uni-passau.de"),
                                           list("olaf@example.org"),
                                           list("olaf@example.org", "karl@example.org", "thomas@example.org"))

    ## Test

    networks.with.attr = add.vertex.attribute.author.email(networks.and.data[["networks"]],
                                                           networks.and.data[["project.data"]])

    actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "author.email"))

    expect_identical(expected.attributes, actual.attributes)
})

#' Test the add.vertex.attribute.artifact.count method
test_that("Test add.vertex.attribute.artifact.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(range = network.covariates.test.build.expected(list(1L),
                                                                              list(1L),
                                                                              list(1L, 1L, 1L)),
                               cumulative = network.covariates.test.build.expected(list(1L),
                                                                                   list(1L),
                                                                                   list(2L, 1L, 1L)),
                               project = network.covariates.test.build.expected(list(1L),
                                                                                list(2L),
                                                                                list(2L, 1L, 1L)))

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.artifact.count(networks.and.data[["networks"]],
                                                               networks.and.data[["project.data"]],
                                                               aggregation.level = level)

        actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "artifact.count"))

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.first.activity method
test_that("Test add.vertex.attribute.first.activity", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(range = list(mail = network.covariates.test.build.expected(dateList("2016-07-12 15:58:40"),
                                                                                          dateList(NA),
                                                                                          dateList("2016-07-12 16:05:37",
                                                                                                   NA, NA)),
                                            commit = network.covariates.test.build.expected(dateList("2016-07-12 15:58:59"),
                                                                                            dateList("2016-07-12 16:00:45"),
                                                                                            dateList("2016-07-12 16:05:41",
                                                                                                     "2016-07-12 16:06:10",
                                                                                                     "2016-07-12 16:06:32")),

                                            issue = network.covariates.test.build.expected(dateList("2016-07-12 15:59:25"),
                                                                                           dateList(NA),
                                                                                           dateList("2016-07-27 22:25:25",
                                                                                                    NA, "2016-07-14 02:03:14"))),

                               cumulative = list(mail = network.covariates.test.build.expected(dateList("2004-10-09 18:38:13"),
                                                                                               dateList("2016-07-12 15:58:50"),
                                                                                               dateList("2016-07-12 15:58:50", NA, "2016-07-12 16:04:40")),
                                   commit = network.covariates.test.build.expected(dateList("2016-07-12 15:58:59"),
                                                                                   dateList("2016-07-12 16:00:45"),
                                                                                   dateList("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                                   issue = network.covariates.test.build.expected(dateList("2016-07-12 15:59:25"),
                                                                                  dateList("2013-05-25 20:02:08"),
                                                                                  dateList("2013-05-25 20:02:08", "2013-04-21 23:52:09", "2016-07-12 15:59:25"))),
                               project = list(mail = network.covariates.test.build.expected(dateList("2004-10-09 18:38:13"),
                                                                                            dateList("2016-07-12 15:58:50"),
                                                                                            dateList("2016-07-12 15:58:50", NA, "2016-07-12 16:04:40")),
                                   commit = network.covariates.test.build.expected(dateList("2016-07-12 15:58:59"),
                                                                                   dateList("2016-07-12 16:00:45"),
                                                                                   dateList("2016-07-12 16:00:45", "2016-07-12 16:06:10", "2016-07-12 16:06:32")),
                                   issue = network.covariates.test.build.expected(dateList("2016-07-12 15:59:25"),
                                                                                  dateList("2013-05-25 20:02:08"),
                                                                                  dateList("2013-05-25 20:02:08", "2013-04-21 23:52:09", "2016-07-12 15:59:25"))))


    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
         lapply(c("mail", "commit", "issue"), function(type) {

            networks.with.attr = add.vertex.attribute.first.activity(networks.and.data[["networks"]],
                                                                 networks.and.data[["project.data"]],
                                                                 aggregation.level = level,
                                                                 activity.type = type)

            actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "first.activity"))

            expect_equal(expected.attributes[[level]][[type]], actual.attributes)
        })
    })
})


#' Test the add.vertex.attribute.active.ranges method
test_that("Test add.vertex.attribute.active.ranges", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(range = network.covariates.test.build.expected(list(c("2016-07-12 15:00:00-2016-07-12 16:00:00")),
                                                                              list(c("2016-07-12 16:00:00-2016-07-12 16:05:00",
                                                                                     "2016-07-12 16:05:00-2030-01-01 00:00:00")),
                                                                              list(c("2016-07-12 16:00:00-2016-07-12 16:05:00",
                                                                                     "2016-07-12 16:05:00-2030-01-01 00:00:00"),
                                                                                   "2016-07-12 16:05:00-2030-01-01 00:00:00",
                                                                                   "2016-07-12 16:05:00-2030-01-01 00:00:00")),
                               cumulative = network.covariates.test.build.expected(list("2016-07-12 15:00:00-2016-07-12 16:00:00"),
                                                                                   list(c("2016-07-12 16:00:00-2016-07-12 16:05:00",
                                                                                          "2016-07-12 16:05:00-2030-01-01 00:00:00")),
                                                                                   list(c("2016-07-12 16:00:00-2016-07-12 16:05:00",
                                                                                          "2016-07-12 16:05:00-2030-01-01 00:00:00"),
                                                                                        "2016-07-12 16:05:00-2030-01-01 00:00:00",
                                                                                        "2016-07-12 16:05:00-2030-01-01 00:00:00")),
                               project = network.covariates.test.build.expected(list("2016-07-12 15:00:00-2016-07-12 16:00:00"),
                                                                                list(c("2016-07-12 16:00:00-2016-07-12 16:05:00",
                                                                                       "2016-07-12 16:05:00-2030-01-01 00:00:00")),
                                                                                list(c("2016-07-12 16:00:00-2016-07-12 16:05:00",
                                                                                       "2016-07-12 16:05:00-2030-01-01 00:00:00"),
                                                                                     "2016-07-12 16:05:00-2030-01-01 00:00:00",
                                                                                     "2016-07-12 16:05:00-2030-01-01 00:00:00")))

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        networks.with.attr = add.vertex.attribute.active.ranges(networks.and.data[["networks"]],
                                                                 networks.and.data[["project.data"]])

        actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "active.ranges"))

        expect_identical(expected.attributes[[level]], actual.attributes)
    })
})

#' Test the add.vertex.attribute.author.role.simple method
test_that("Test add.vertex.attribute.author.role.simple", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks()

    expected.attributes = list(range = list(commit.count = network.covariates.test.build.expected(list("core"),
                                                                                                  list("core"),
                                                                                                  list("core", "core", "peripheral")),
                                            loc.count = network.covariates.test.build.expected(list("core"),
                                                                                               list("core"),
                                                                                               list("core", "core", "peripheral"))),
                               cumulative = list(commit.count = network.covariates.test.build.expected(list("core"),
                                                                                                      list("core"),
                                                                                                      list("core", "core", "peripheral")),
                                                loc.count = network.covariates.test.build.expected(list("core"),
                                                                                                   list("peripheral"),
                                                                                                   list("core", "core", "peripheral"))),
                               project = list(commit.count = network.covariates.test.build.expected(list("core"),
                                                                                                    list("core"),
                                                                                                    list("core", "core", "peripheral")),
                                              loc.count = network.covariates.test.build.expected(list("core"),
                                                                                                 list("core"),
                                                                                                 list("core", "core", "peripheral"))))

    ## Test

    lapply(AGGREGATION.LEVELS, function(level) {
        lapply(c("commit.count", "loc.count"), function(type) {
            networks.with.attr = add.vertex.attribute.author.role.simple(networks.and.data[["networks"]],
                                                                         networks.and.data[["project.data"]],
                                                                         type = type,
                                                                         aggregation.level = level)

            actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "author.role"))
            actual.attributes = lapply(actual.attributes, function(net.attr) lapply(net.attr, as.character))

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

    expected.attributes = network.covariates.test.build.expected(list(1L),
                                                                 list(1L),
                                                                 list(3L))

    ## Test

    networks.with.attr = add.vertex.attribute.artifact.editor.count(networks.and.data[["networks"]],
                                                           networks.and.data[["project.data"]])

    actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "editor.count"))

    expect_identical(expected.attributes, actual.attributes)
})

#' Test the add.vertex.attribute.artifact.first.occurrence method
test_that("Test add.vertex.attribute.artifact.first.occurrence", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes = network.covariates.test.build.expected(dateList("2016-07-12 15:58:59 UTC"),
                                                                 dateList("2016-07-12 16:00:45 UTC"),
                                                                 dateList("2016-07-12 16:05:41 UTC"))

    ## Test

    networks.with.attr = add.vertex.attribute.artifact.first.occurrence(networks.and.data[["networks"]],
                                                                    networks.and.data[["project.data"]])

    actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "first.occurrence"))

    expect_equal(expected.attributes, actual.attributes)
})

#' Test the add.vertex.attribute.artifact.change.count method
test_that("Test add.vertex.attribute.artifact.change.count", {

    ## Test setup

    networks.and.data = get.network.covariates.test.networks("artifact")

    expected.attributes = network.covariates.test.build.expected(list(1L),
                                                                 list(1L),
                                                                 list(3L))

    ## Test

    networks.with.attr = add.vertex.attribute.artifact.change.count(networks.and.data[["networks"]],
                                                                    networks.and.data[["project.data"]])

    actual.attributes = lapply(networks.with.attr, function(net) igraph::vertex_attr(net, "change.count"))

    expect_identical(expected.attributes, actual.attributes)
})
