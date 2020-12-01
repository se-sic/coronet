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
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## All Rights Reserved.


context("Cutting functionality on NetworkBuilder side.")

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
    net.conf = NetworkConf$new()
    net.conf$update.value(entry = "unify.date.ranges", value = TRUE)

    ## construct objects

    x.data = ProjectData$new(proj.conf)
    x = NetworkBuilder$new(x.data, net.conf)

    commit.data.expected = data.frame(commit.id = sprintf("<commit-%s>", c(32712, 32713)),
                                      date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:45")),
                                      author.name = c("Björn", "Olaf"),
                                      author.email = c("bjoern@example.org", "olaf@example.org"),
                                      committer.date = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-20 10:00:44")),
                                      committer.name = c("Björn", "Björn"),
                                      committer.email = c("bjoern@example.org", "bjoern@example.org"),
                                      hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338"),
                                      changed.files = as.integer(c(1, 1)),
                                      added.lines = as.integer(c(1, 1)),
                                      deleted.lines = as.integer(c(1, 0)),
                                      diff.size = as.integer(c(2, 1)),
                                      file = c("test.c", "test.c"),
                                      artifact = c("A", "A"),
                                      artifact.type = c("Feature", "Feature"),
                                      artifact.diff.size = as.integer(c(1, 1)))

    mail.data.expected = data.frame(author.name = c("Thomas", "Olaf"),
                                    author.email = c("thomas@example.org", "olaf@example.org"),
                                    message.id = c("<65a1sf31sagd684dfv31@mail.gmail.com>", "<9b06e8d20801220234h659c18a3g95c12ac38248c7e0@mail.gmail.com>"),
                                    date = get.date.from.string(c("2016-07-12 16:04:40", "2016-07-12 16:05:37")),
                                    date.offset = as.integer(c(100, 200)),
                                    subject = c("Re: Fw: busybox 2 tab", "Re: Fw: busybox 10"),
                                    thread = sprintf("<thread-%s>", c(9, 9)),
                                    artifact.type = c("Mail", "Mail"))

    commit.data = x$get.project.data()$get.commits()
    rownames(commit.data) = 1:nrow(commit.data)

    mail.data = x$get.project.data()$get.mails()
    rownames(mail.data) = 1:nrow(mail.data)

    expect_identical(commit.data, commit.data.expected, info = "Cut Raw commit data.")
    expect_identical(mail.data, mail.data.expected, info = "Cut mail data.")

})
