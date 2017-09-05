## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de


context("Motif functionality.")

network = get.sample.network()


##
## Tests for motifs.search.in.network(..., remove.duplicates = TRUE)
##

test_that("Motifs are found in sample network (remove.duplicates = TRUE).", {
    ## Line motif
    expected = list(
        igraph::V(network)[ c("D1", "D2") ],
        igraph::V(network)[ c("D1", "D4") ],
        igraph::V(network)[ c("D3", "D4") ],
        igraph::V(network)[ c("D4", "D5") ]
    )
    matchings = motifs.search.in.network(network, MOTIFS.LINE, color.attr = "type", remove.duplicates = TRUE)
    expect_equal(matchings, expected, info = "Line motif")

    ## Triangle motif (positive)
    expected = list(
        igraph::V(network)[ c("D1", "D2", "A1") ]
    )
    matchings = motifs.search.in.network(network, MOTIFS.TRIANGLE.POSITIVE, color.attr = "type", remove.duplicates = TRUE)
    expect_equal(matchings, expected, info = "Triangle motif (positive)")

    ## Triangle motif (negative)
    expected = list(
        igraph::V(network)[ c("D1", "D2", "A1") ],
        igraph::V(network)[ c("D5", "D6", "A6") ]
    )
    matchings = motifs.search.in.network(network, MOTIFS.TRIANGLE.NEGATIVE, color.attr = "type", remove.duplicates = TRUE)
    expect_equal(matchings, expected, info = "Triangle motif (negative)")

    ## Square motif (positive)
    expected = list(
        igraph::V(network)[ c("D4", "D5", "A5", "A6") ]
    )
    matchings = motifs.search.in.network(network, MOTIFS.SQUARE.POSITIVE, color.attr = "type", remove.duplicates = TRUE)
    expect_equal(matchings, expected, info = "Square motif (positive)")

    ## Square motif (negative)
    expected = list(
        igraph::V(network)[ c("D1", "D3", "A1", "A3") ],
        igraph::V(network)[ c("D2", "D3", "A1", "A3") ],
        igraph::V(network)[ c("D4", "D5", "A5", "A6") ],
        igraph::V(network)[ c("D4", "D6", "A5", "A6") ]
    )
    matchings = motifs.search.in.network(network, MOTIFS.SQUARE.NEGATIVE, color.attr = "type", remove.duplicates = TRUE)
    expect_equal(matchings, expected, info = "Square motif (negative)")

})


##
## Tests for motifs.search.in.network(..., remove.duplicates = FALSE)
##

test_that("Motifs are found in sample network (remove.duplicates = FALSE).", {
    ## Line motif
    expected = list(
        igraph::V(network)[ c("D1", "D2") ],
        igraph::V(network)[ c("D1", "D4") ],
        igraph::V(network)[ c("D1", "D2") ], # unordered: D2, D1
        igraph::V(network)[ c("D1", "D4") ], # unordered: D4, D1
        igraph::V(network)[ c("D3", "D4") ],
        igraph::V(network)[ c("D4", "D5") ],
        igraph::V(network)[ c("D3", "D4") ], # unordered: D4, D3
        igraph::V(network)[ c("D4", "D5") ]  # unordered: D5, D4
    )
    matchings = motifs.search.in.network(network, MOTIFS.LINE, color.attr = "type", remove.duplicates = FALSE)
    expect_equal(matchings, expected, info = "Line motif")

    ## Triangle motif (positive)
    expected = list(
        igraph::V(network)[ c("D1", "D2", "A1") ],
        igraph::V(network)[ c("D1", "D2", "A1") ]  # unordered: D2, D1, A1
    )
    matchings = motifs.search.in.network(network, MOTIFS.TRIANGLE.POSITIVE, color.attr = "type", remove.duplicates = FALSE)
    expect_equal(matchings, expected, info = "Triangle motif (positive)")

    ## Triangle motif (negative)
    expected = list(
        igraph::V(network)[ c("D1", "D2", "A1") ],
        igraph::V(network)[ c("D1", "D2", "A1") ], # unordered: D2, D1, A1
        igraph::V(network)[ c("D5", "D6", "A6") ],
        igraph::V(network)[ c("D5", "D6", "A6") ]  # unordered: D6, D5, A1
    )
    matchings = motifs.search.in.network(network, MOTIFS.TRIANGLE.NEGATIVE, color.attr = "type", remove.duplicates = FALSE)
    expect_equal(matchings, expected, info = "Triangle motif (negative)")

    ## Square motif (positive)
    expected = list(
        igraph::V(network)[ c("D4", "D5", "A5", "A6") ],
        igraph::V(network)[ c("D4", "D5", "A5", "A6") ]  # unordered: D5, D4, A5, A6
    )
    matchings = motifs.search.in.network(network, MOTIFS.SQUARE.POSITIVE, color.attr = "type", remove.duplicates = FALSE)
    expect_equal(matchings, expected, info = "Square motif (positive)")

    ## Square motif (negative)
    expected = list(
        igraph::V(network)[ c("D1", "D3", "A1", "A3") ],
        igraph::V(network)[ c("D2", "D3", "A1", "A3") ],
        igraph::V(network)[ c("D4", "D5", "A5", "A6") ],
        igraph::V(network)[ c("D4", "D6", "A5", "A6") ],
        igraph::V(network)[ c("D1", "D3", "A1", "A3") ], # unordered: D3, D1, A1, A3
        igraph::V(network)[ c("D2", "D3", "A1", "A3") ], # unordered: D3, D2, A1, A3
        igraph::V(network)[ c("D4", "D5", "A5", "A6") ], # unordered: D5, D4, A5, A6
        igraph::V(network)[ c("D4", "D6", "A5", "A6") ]  # unordered: D6, D4, A5, A6
    )
    matchings = motifs.search.in.network(network, MOTIFS.SQUARE.NEGATIVE, color.attr = "type", remove.duplicates = FALSE)
    expect_equal(matchings, expected, info = "Square motif (negative)")

})


##
## Tests for motifs.count(..., remove.duplicates = TRUE) and motifs.count(network)
##

test_that("Motif counts for sample network (remove.duplicates = TRUE).", {

    ## Triangle motif
    expected.triangle = list(
        "authors" = 6, # total number of authors
        "artifacts" = 6, # total number of artifacts
        "complete" = 15, # total number of author pairs
        "collaborating" = 2, # number of author pairs that collaborate
        "communicating" = 4, # number of author pairs that communicate
        "collaborating.and.communicating" = 1 # number of author pairs that collaborate and communicate
    )
    attr(expected.triangle, "raw") = list(
        "collaborating" = list(
            igraph::V(network)[ c("D1", "D2", "A1") ],
            igraph::V(network)[ c("D5", "D6", "A6") ]
        ),
        "communicating" = list(
            igraph::V(network)[ c("D1", "D2") ],
            igraph::V(network)[ c("D1", "D4") ],
            igraph::V(network)[ c("D3", "D4") ],
            igraph::V(network)[ c("D4", "D5") ]
        ),
        "collaborating.and.communicating" = list(
            igraph::V(network)[ c("D1", "D2", "A1") ]
        )
    )
    result.triangle = motifs.count(
        network,
        motifs = list(
            collaborating = MOTIFS.TRIANGLE.NEGATIVE,
            communicating = MOTIFS.LINE,
            collaborating.and.communicating = MOTIFS.TRIANGLE.POSITIVE
        ),
        remove.duplicates = TRUE,
        raw.data = TRUE
    )
    expect_equal(result.triangle, expected.triangle, info = "Triangle motif.")

    ## Square motif
    expected.square = list(
        "authors" = 6, # total number of authors
        "artifacts" = 6, # total number of artifacts
        "complete" = 15, # total number of author pairs
        "collaborating" = 4, # number of author pairs that collaborate
        "communicating" = 4, # number of author pairs that communicate
        "collaborating.and.communicating" = 1 # number of author pairs that collaborate and communicate
    )
    attr(expected.square, "raw") = list(
        "collaborating" = list(
            igraph::V(network)[ c("D1", "D3", "A1", "A3") ],
            igraph::V(network)[ c("D2", "D3", "A1", "A3") ],
            igraph::V(network)[ c("D4", "D5", "A5", "A6") ],
            igraph::V(network)[ c("D4", "D6", "A5", "A6") ]
        ),
        "communicating" = list(
            igraph::V(network)[ c("D1", "D2") ],
            igraph::V(network)[ c("D1", "D4") ],
            igraph::V(network)[ c("D3", "D4") ],
            igraph::V(network)[ c("D4", "D5") ]
        ),
        "collaborating.and.communicating" = list(
            igraph::V(network)[ c("D4", "D5", "A5", "A6") ]
        )
    )
    result.square = motifs.count(
        network,
        motifs = list(
            collaborating = MOTIFS.SQUARE.NEGATIVE,
            communicating = MOTIFS.LINE,
            collaborating.and.communicating = MOTIFS.SQUARE.POSITIVE
        ),
        remove.duplicates = TRUE,
        raw.data = TRUE
    )
    expect_equal(result.square, expected.square, info = "Square motif.")

    ##
    ## All motifs.
    ##

    ## expected results
    expected.triangle.all = c(
        expected.triangle,
        list(
            "p1" = 4 / 15, # fraction p1
            "p2" = 1 / 2 # fraction p2
        )
    )
    attr(expected.triangle.all, "raw") = attr(expected.triangle, "raw")

    expected.square.all = c(
        expected.square,
        list(
            "p1" = 4 / 15, # fraction p1
            "p2" = 1 / 4 # fraction p2
        )
    )
    attr(expected.square.all, "raw") = attr(expected.square, "raw")

    ## All motifs (with raw data)
    expected.all = list(
        triangle = expected.triangle.all,
        square = expected.square.all
    )
    result.all = motifs.count.all(network, raw.data = TRUE)
    expect_equal(result.all, expected.all, info = "All motifs.")

    ## All motifs (no raw data)
    attr(expected.triangle.all, "raw") = NULL
    attr(expected.square.all, "raw") = NULL
    expected.all = list(
        triangle = expected.triangle.all,
        square = expected.square.all
    )
    result.all = motifs.count.all(network, raw.data = FALSE)
    expect_equal(result.all, expected.all, info = "All motifs (no raw data).")

})


##
## Tests for motifs.remove.artifacts.from.matched.motifs(...)
##

test_that("Motif modification (motifs.remove.artifacts.from.matched.motifs).", {

    ## Test 1
    vs = igraph::V(network)[ c("D1", "D2") ]
    expected = list(vs)
    result = motifs.remove.artifacts.from.matched.motifs(network, vs)
    expect_equal(result, expected, info = "No artifact (1).")

    ## Test 2
    vs = list(
        igraph::V(network)[ c("D1", "D2") ],
        igraph::V(network)[ c("D5", "D6") ]
    )
    expected = vs
    result = motifs.remove.artifacts.from.matched.motifs(network, vs)
    expect_equal(result, vs, info = "No artifact (2).")

    ## Test 3
    vs = igraph::V(network)[ c("D1", "D2", "A1") ]
    expected = list(vs[ c(1, 2) ])
    result = motifs.remove.artifacts.from.matched.motifs(network, vs)
    expect_equal(result, expected, info = "One artifact (1).")

    ## Test 4
    vs = igraph::V(network)[ c("D1", "A1", "D2") ]
    expected = list(vs[ c(1, 3) ])
    result = motifs.remove.artifacts.from.matched.motifs(network, vs)
    expect_equal(result, expected, info = "One artifact (2).")

    ## Test 5
    vs = igraph::V(network)[ c("A1", "A2") ]
    expected = list(vs[ c() ])
    result = motifs.remove.artifacts.from.matched.motifs(network, vs)
    expect_equal(result, expected, info = "All artifacts.")

})


