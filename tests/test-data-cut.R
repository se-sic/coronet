## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


context("Cutting functionality on ProjectData side.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("Cut commit and mail data to same date range.", {

    ## configurations

    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    data.sources = c("mails", "commits")

    ## construct objects

    x.data = ProjectData$new(proj.conf)

    commit.data.expected = data.frame(commit.id=sprintf("<commit-%s>", c(32712,32712,32713,32713)),
                                      date=as.POSIXct(c("2016-07-12 15:58:59","2016-07-12 15:58:59","2016-07-12 16:00:45",
                                                        "2016-07-12 16:00:45")),
                                      author.name=c("Claus Hunsen","Claus Hunsen","Olaf","Olaf"),
                                      author.email=c("hunsen@fim.uni-passau.de","hunsen@fim.uni-passau.de","olaf@example.org",
                                                     "olaf@example.org"),
                                      hash=c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0","72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                             "5a5ec9675e98187e1e92561e1888aa6f04faa338","5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                                      changed.files=as.integer(c(1,1,1,1)),
                                      added.lines=as.integer(c(1,1,1,1)),
                                      deleted.lines=as.integer(c(1,1,0,0)),
                                      diff.size=as.integer(c(2,2,1,1)),
                                      file=c("test.c","test.c","test.c","test.c"),
                                      artifact=c("A","defined(A)","A","defined(A)"),
                                      artifact.type=c("Feature","FeatureExpression","Feature","FeatureExpression"),
                                      artifact.diff.size=as.integer(c(1,1,1,1)))

    commit.data = x.data$get.data.cut.to.same.date(data.sources = data.sources)$get.commits.raw()

    expect_identical(commit.data, commit.data.expected, info = "Cut Raw commit data.")

})
