## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de


context("Network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


##
## Tests for author.all.authors and author.only.committers
##

test_that("Amount of authors (author.all.authors, author.only.committers).", {

    ## author sets
    authors.committing = c("Claus Hunsen", "Olaf", "Thomas", "Karl")
    authors.mailing = c("Claus Hunsen", "Olaf", "Thomas", "georg", "Hans", "udo", "Fritz fritz@example.org")
    authors.issue = c("Karl", "Olaf", "Thomas", "Claus Hunsen", "udo", "Max")

    authors.all = union(union(authors.committing, authors.mailing), authors.issue)

    authors.all.aoc = intersect(authors.all, authors.committing)
    authors.committing.aoc = intersect(authors.committing, authors.committing)
    authors.mailing.aoc = intersect(authors.mailing, authors.committing)
    authors.issue.aoc = intersect(authors.issue, authors.committing)

    ## expected sets of vertices in the networks for artifact.relation = "cochange"
    expected = list(
        ## author.relation
        cochange = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.all.aoc, bipartite = authors.all.aoc, multi = authors.all.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.all, bipartite = authors.all, multi = authors.all
                )
            ),
            "FALSE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.committing.aoc, bipartite = authors.committing.aoc, multi = authors.committing.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.committing, bipartite = authors.committing, multi = authors.committing
                )
            )
        ),
        mail = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.all.aoc, bipartite = authors.all.aoc, multi = authors.all.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.all, bipartite = authors.all, multi = authors.all
                )
            ),
            "FALSE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.mailing.aoc, bipartite = authors.committing.aoc, multi = authors.mailing.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.mailing, bipartite = authors.committing, multi = authors.mailing
                )
            )
        ),
        issue = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.all.aoc, bipartite = authors.all.aoc, multi = authors.all.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.all, bipartite = authors.all, multi = authors.all
                )
            ),
            "FALSE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.issue.aoc, bipartite = authors.committing.aoc, multi = authors.issue.aoc
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.issue, bipartite = authors.committing, multi = authors.issue
                )
            )
        )
    )

    ## run all tests
    for (author.relation in c("cochange", "mail", "issue")) {
        for (author.all.authors in c(TRUE, FALSE)) {
            for (author.only.committers in c(TRUE, FALSE)) {

                ## configurations
                proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
                proj.conf$update.value("artifact.filter.base", FALSE)
                net.conf = NetworkConf$new()

                ## update network configuration
                net.conf$update.values(updated.values = list(
                    author.relation = author.relation, artifact.relation = "cochange",
                    author.all.authors = author.all.authors, author.only.committers = author.only.committers)
                )

                ## construct objects
                x.data = ProjectData$new(proj.conf)
                x = NetworkBuilder$new(x.data, net.conf)

                ## author network
                x$reset.environment()
                a = x$get.author.network()
                expect_true(
                    setequal(
                        igraph::V(a)[ type == TYPE.AUTHOR ]$name,
                        expected[[author.relation]][[as.character(author.all.authors)]][[as.character(author.only.committers)]][["author"]]
                    ),
                    info = sprintf("Author network (author.relation = %s, author.all.authors = %s, author.only.committers = %s",
                                   author.relation, author.all.authors, author.only.committers)
                )

                ## bipartite network
                x$reset.environment()
                b = x$get.bipartite.network()
                expect_true(
                    setequal(
                        igraph::V(b)[ type == TYPE.AUTHOR ]$name,
                        expected[[author.relation]][[as.character(author.all.authors)]][[as.character(author.only.committers)]][["bipartite"]]
                    ),
                    info = sprintf("Bipartite network (author.relation = %s, author.all.authors = %s, author.only.committers = %s",
                                   author.relation, author.all.authors, author.only.committers)
                )

                ## multi network
                x$reset.environment()
                m = x$get.multi.network()
                expect_true(
                    setequal(
                        igraph::V(m)[ type == TYPE.AUTHOR ]$name,
                        expected[[author.relation]][[as.character(author.all.authors)]][[as.character(author.only.committers)]][["multi"]]
                    ),
                    info = sprintf("Multi network (author.relation = %s, author.all.authors = %s, author.only.committers = %s",
                                   author.relation, author.all.authors, author.only.committers)
                )

            }
        }
    }

})
