## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de
## (c) Claus Hunsen, 2017, 2018
## hunsen@fim.uni-passau.de
## (c) Thomas Bock, 2017
## bockthom@fim.uni-passau.de

test_that("Parse range", {
    range.input = c("2012-07-10 15:58:00-2012-07-15 16:02:00",
                    "2012-07-10-2012-07-15 16:02:00",
                    "2012-07-10 15:58:00-2012-07-15",
                    "f86391e7d7eaf4234fc742c1b61f32cb8a65782e-63654c1b089b8abe9a52d21fd1b53b1631539e10",
                    "v1.0.0-v2.0.0_alpha")

    expected.output = list(
        get.date.from.string(c("2012-07-10 15:58:00", "2012-07-15 16:02:00")),
        get.date.from.string(c("2012-07-10 00:00:00", "2012-07-15 16:02:00")),
        get.date.from.string(c("2012-07-10 15:58:00", "2012-07-15 00:00:00")),
        c("f86391e7d7eaf4234fc742c1b61f32cb8a65782e", "63654c1b089b8abe9a52d21fd1b53b1631539e10"),
        c("v1.0.0", "v2.0.0_alpha")
    )

    actual.output = lapply(range.input, get.range.bounds)

    expect_equal(actual.output, expected.output, "Parsed Range")
})
