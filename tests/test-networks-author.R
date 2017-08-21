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
    authors.both = c("Claus Hunsen", "Olaf", "Thomas")
    authors.all = union(union(authors.committing, authors.mailing), authors.both)

    ## expected sets of vertices in the networks
    expected = list(
        ## author.relation
        cochange = list(
            ## author.all.authors
            "TRUE" = list(
                ## author.only.committers
                "TRUE" = list(
                    ## network type
                    author = authors.committing, bipartite = authors.committing, multi = authors.committing
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
                    author = authors.committing, bipartite = authors.committing, multi = authors.committing
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
                    author = authors.committing, bipartite = authors.committing, multi = authors.committing
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
                    author = authors.both, bipartite = authors.both, multi = authors.committing
                ),
                "FALSE" = list(
                    ## network type
                    author = authors.mailing, bipartite = authors.mailing, multi = authors.all
                )
            )
        )
    )

    ## run all tests
    for (author.relation in c("cochange", "mail")) {
        for (author.all.authors in c(TRUE, FALSE)) {
            for (author.only.committers in c(TRUE, FALSE)) {

                ## configurations
                proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
                proj.conf$set.artifact.filter.base(FALSE)
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
