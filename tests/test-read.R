## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


context("Reading functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("Read the raw commit data.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    commit.data.read = read.commits.raw(proj.conf$get.value("datapath"), proj.conf$get.value("artifact"))

    ## build the expected data.frame
    commit.data.expected = data.frame(commit.id=sprintf("<commit-%s>", c(32712,32712,32713,32713,32710,32710,32714,32711,32711)),
                                      date=as.POSIXct(c("2016-07-12 15:58:59","2016-07-12 15:58:59","2016-07-12 16:00:45",
                                                        "2016-07-12 16:00:45","2016-07-12 16:05:41","2016-07-12 16:05:41",
                                                        "2016-07-12 16:06:10","2016-07-12 16:06:32","2016-07-12 16:06:32")),
                                      author.name=c("Claus Hunsen","Claus Hunsen","Olaf","Olaf","Olaf","Olaf","Karl","Thomas","Thomas"),
                                      author.email=c("hunsen@fim.uni-passau.de","hunsen@fim.uni-passau.de","olaf@example.org",
                                                     "olaf@example.org","olaf@example.org","olaf@example.org","karl@example.org",
                                                     "thomas@example.org","thomas@example.org"),
                                      hash=c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0","72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0",
                                             "5a5ec9675e98187e1e92561e1888aa6f04faa338","5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                             "3a0ed78458b3976243db6829f63eba3eead26774","3a0ed78458b3976243db6829f63eba3eead26774",
                                             "1143db502761379c2bfcecc2007fc34282e7ee61","0a1a5c523d835459c42f33e863623138555e2526",
                                             "0a1a5c523d835459c42f33e863623138555e2526"),
                                      changed.files=as.integer(c(1,1,1,1,1,1,1,1,1)),
                                      added.lines=as.integer(c(1,1,1,1,1,1,1,1,1)),
                                      deleted.lines=as.integer(c(1,1,0,0,0,0,0,0,0)),
                                      diff.size=as.integer(c(2,2,1,1,1,1,1,1,1)),
                                      file=c("test.c","test.c","test.c","test.c","test2.c","test2.c","test3.c","test2.c","test2.c"),
                                      artifact=c("A","defined(A)","A","defined(A)","Base_Feature","Base_Feature","Base_Feature",
                                                 "Base_Feature","Base_Feature"),
                                      artifact.type=c("Feature","FeatureExpression","Feature","FeatureExpression","Feature",
                                                      "FeatureExpression","Feature","Feature","FeatureExpression"),
                                      artifact.diff.size=as.integer(c(1,1,1,1,1,1,1,1,1)))

    ## check the results
    expect_identical(commit.data.read, commit.data.expected, info = "Raw commit data.")

    ## check order of commits (ordered by date?)
    dates = order(commit.data.read[["date"]])
    dates.expected = seq_len(nrow(commit.data.expected))
    expect_identical(dates, dates.expected, info = "Ordering by date.")
})


test_that("Read the synchronicity data.", {
    ## configuration object
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    synchronicity.data.read = read.synchronicity(proj.conf$get.value("datapath.synchronicity"), proj.conf$get.value("artifact"),
                                                 proj.conf$get.value("synchronicity.time.window"))

    ## build the expected data.frame
    synchronicity.data.expected = data.frame(hash=c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774","0a1a5c523d835459c42f33e863623138555e2526"),
                                             synchronous = c(TRUE, TRUE, FALSE, FALSE))

    ## check the results
    expect_identical(synchronicity.data.read, synchronicity.data.expected, info = "Synchronicity data.")
})

test_that("Read the mail data.", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    mail.data.read = read.mails(proj.conf$get.value("datapath"))

    ## build the expected data.frame
    ## NOTE: the empty date is set to a real date here in order to build the data.frame without problems
    mail.data.expected = data.frame(author.name=c("Claus Hunsen","Claus Hunsen","udo","Fritz fritz@example.org","georg","Hans",
                                                  "Hans","Hans","Hans","Hans","Hans","Hans","Thomas","Claus Hunsen","Olaf",
                                                  "Thomas","Olaf"),
                                    author.email=c("hunsen@fim.uni-passau.de","hunsen@fim.uni-passau.de","udo@example.org",
                                                   "asd@sample.org","heinz@example.org","hans1@example.org","hans1@example.org",
                                                   "hans1@example.org","hans1@example.org","hans1@example.org","hans1@example.org",
                                                   "hans1@example.org","thomas@example.org","hunsen@fim.uni-passau.de","olaf@example.org",
                                                   "thomas@example.org","olaf@example.org"),
                                    message.id=c("<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>","<1107974989.17910.6.camel@jmcmullan>",
                                                 "<asddghdswqeasdasd@mail.gmail.com>","<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>",
                                                 "<dfhglkjdgjkhnwrd@mail.gmail.com>","<hans1@mail.gmail.com>","<hans2@mail.gmail.com>",
                                                 "<hans3@mail.gmail.com>","<hans4@mail.gmail.com>","<hans5@mail.gmail.com>",
                                                 "<hans6@mail.gmail.com>","<hans7@mail.gmail.com>","<saf54sd4gfasf46asf46@mail.gmail.com>",
                                                 "<4cbaa9ef0802201124v37f1eec8g89a412dfbfc8383a@mail.gmail.com>",
                                                 "<6784529b0802032245r5164f984l342f0f0dc94aa420@mail.gmail.com>",
                                                 "<65a1sf31sagd684dfv31@mail.gmail.com>",
                                                 "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"),
                                    date=as.POSIXct(c("2004-10-09 18:38:13","2005-02-09 18:49:49","2010-07-12 10:05:36",
                                                           "2010-07-12 11:05:35","2010-07-12 12:05:34","2010-07-12 12:05:40",
                                                           "2010-07-12 12:05:41","2010-07-12 12:05:42","2010-07-12 12:05:43",
                                                           "2010-07-12 12:05:44","2010-07-12 12:05:45","2010-07-12 12:05:46",
                                                           "2016-07-12 15:58:40","2016-07-12 15:58:40","2016-07-12 15:58:50",
                                                           "2016-07-12 16:04:40","2016-07-12 16:05:37")),
                                    date.offset=as.integer(c(200,-500,200,200,200,200,200,200,200,200,200,200,0,0,-400,100,200)),
                                    subject=c("Re: Fw: busybox 202 with tab","Doubled date","Only mail address","name is mail address",
                                              "name is mail address","name is mail address","name is mail address","name is mail address",
                                              "name is mail address","name is mail address","name is mail address","name is mail address",
                                              "=?KOI8-R?Q?=EF=D4=D7=C5=D4:_Some_patches?= 2","Re: busybox 1",
                                              "=?KOI8-R?Q?=EF=D4=D7=C5=D4:_Some_patches?= tab","Re: Fw: busybox 2 tab",
                                              "Re: Fw: busybox 10"),
                                    thread=sprintf("<thread-%s>", c(1,2,3,4,5,6,6,6,6,6,6,7,8,8,8,9,9))
                                    )
    ## delete the line with the empty date
    mail.data.expected = mail.data.expected[-13,]

    ## check the results
    expect_identical(mail.data.read, mail.data.expected, info = "Mail data.")

    ## check order of commits (ordered by date?)
    dates = order(mail.data.read[["date"]])
    dates.expected = seq_len(nrow(mail.data.expected))
    expect_identical(dates, dates.expected, info = "Ordering by date.")
})

test_that("Read the author data.", {

    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    author.data.read = read.authors(proj.conf$get.value("datapath"))

    ## build the expected data.frame
    author.data.expected = data.frame(author.id=as.integer(c(4936,4937,4938,4939,4940,4941,4942,4943,4944)),
                                      author.name=c("Thomas","Olaf","Claus Hunsen","udo","Fritz fritz@example.org","georg","Hans","Karl","Max"))

    ## check the results
    expect_identical(author.data.read, author.data.expected, info = "Author data.")
})

test_that("Read and parse the pasta data.", {
    ## configuration object for the datapath
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

    ## read the actual data
    pasta.data.read = read.pasta(proj.conf$get.value("datapath.pasta"))

    ## build the expected data.frame
    pasta.data.expected = data.frame(message.id=c("<adgkljsdfhkwafdkbhjasfcjn@mail.gmail.com>","<asddghdswqeasdasd@mail.gmail.com>",
                                                  "<jlkjsdgihwkfjnvbjwkrbnwe@mail.gmail.com>","<hans1@mail.gmail.com>",
                                                  "<hans2@mail.gmail.com>","<hans3@mail.gmail.com>","<saf54sd4gfasf46asf46@mail.gmail.com>"),
                                     commit.hash=c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0","5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                   "3a0ed78458b3976243db6829f63eba3eead26774","1143db502761379c2bfcecc2007fc34282e7ee61",
                                                   "1143db502761379c2bfcecc2007fc34282e7ee61","1143db502761379c2bfcecc2007fc34282e7ee61",
                                                   "0a1a5c523d835459c42f33e863623138555e2526"))

    ## check the results
    expect_identical(pasta.data.read, pasta.data.expected, info = "PaStA data.")
})


## FIXME add test for reading issue data
