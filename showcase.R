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
## Copyright 2016-2018, 2020 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020-2021, 2024 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019 by Klara Schlueter <schluete@fim.uni-passau.de>
## Copyright 2020 by Anselm Fehnker <anselm@muenster.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Initialization ----------------------------------------------------------

source("util-init.R")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Logging -----------------------------------------------------------------

requireNamespace("logging")
logging::basicConfig(level = "DEBUG")
if (file.exists("showcase.log")) file.remove("showcase.log")
logging::addHandler(logging::writeToFile, file = "showcase.log", level = "DEBUG")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## System variables and R settings -----------------------------------------

options(mc.cores = 6L) # configure parallelism


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Case-study configuration ------------------------------------------------

## for testing, use the test case-study
CF.DATA = file.path("tests", "codeface-data") # path to codeface data
CF.SELECTION.PROCESS = "testing" # releases, threemonth(, testing)
CASESTUDY = "test"

# ## to run the showcase on real data, use data from project BusyBox (or any other existing project)
# CF.DATA = "/scratch/codeface/codeface-data/" #"/path/to/codeface-data" # path to codeface data
# CF.SELECTION.PROCESS = "threemonth" # releases, threemonth(, testing)
# CASESTUDY = "busybox"

ARTIFACT = "feature" # function, feature, file, featureexpression (only relevant for cochange)

AUTHOR.RELATION = "mail" # mail, cochange, issue
ARTIFACT.RELATION = "cochange" # cochange, callgraph, mail, issue
COMMIT.RELATION = "commit.interaction" # commit.interaction, cochange


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Project and network configuration ---------------------------------------

## initialize project configuration
proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
proj.conf$update.value("commits.filter.base.artifact", TRUE)
proj.conf$update.value("commit.interactions", TRUE)
## specify that custom event timestamps should be read from 'custom-events.list'
proj.conf$update.value("custom.event.timestamps.file", "custom-events.list")
proj.conf$print()

## initialize network configuration
net.conf = NetworkConf$new()
net.conf$update.values(updated.values = list(author.relation = AUTHOR.RELATION,
                                             artifact.relation = ARTIFACT.RELATION,
                                             commit.relation = COMMIT.RELATION))
net.conf$print()

## get ranges
ranges = proj.conf$get.value("ranges")
revisions.callgraph = proj.conf$get.value("revisions.callgraph")

## choose some ranges to be used for testing
if (CASESTUDY == "busybox") {
  first.test.range = ranges[[22]]
  second.test.range = ranges[[23]]
  empty.test.range = ranges[[1]]
} else {
  first.test.range = ranges[[1]]
  second.test.range = ranges[[2]]
  empty.test.range = ranges[[1]] # not empty, just specify anything here and empty data later on manually
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Project-level data ------------------------------------------------------

x.data = ProjectData$new(project.conf = proj.conf)
x.name = x.data$get.project.conf.entry("repo")
x = NetworkBuilder$new(project.data = x.data, network.conf = net.conf)

## * Evaluation plots ------------------------------------------------------

edit.types = plot.commit.edit.types.in.project(x.data)
edit.types.scaled = plot.commit.edit.types.in.project(x.data, TRUE)
editor.types = plot.commit.editor.types.by.author(x.data)
editor.types.scaled = plot.commit.editor.types.by.author(x.data, TRUE)

## * Data retrieval --------------------------------------------------------

x.data$get.commits.unfiltered()
x.data$update.project.conf(updated.values = list(commit.messages = "title"))
x.data$get.commit.messages()
x.data$get.commits.unfiltered()
x.data$update.project.conf(updated.values = list(synchronicity = TRUE, synchronicity.time.window = 1))
x.data$get.synchronicity()
x.data$group.artifacts.by.data.column("commits", "author.name")
x.data$get.commits()
x.data$get.commits.uncached(remove.untracked.files = TRUE, remove.base.artifact = FALSE, filter.bots = FALSE)
x.data$get.mails.unfiltered()
x.data$get.mails()
x.data$get.issues.unfiltered()
x.data$get.issues()
x.data$get.authors()
x.data$get.data.path()
x.data$group.artifacts.by.data.column("mails", "author.name")
x.data$group.artifacts.by.data.column("commits", "hash")
x.data$filter.bots(x.data$get.commits.uncached(remove.untracked.files = TRUE, remove.base.artifact = FALSE, filter.bots = FALSE))
x.data$get.custom.event.timestamps()

## * Network construction --------------------------------------------------

x$update.network.conf(updated.values = list(author.directed = TRUE))
x$get.author.network()
x$update.network.conf(updated.values = list(author.directed = FALSE))
x$get.author.network()
x$get.artifact.network()
x$get.commit.network()
x$reset.environment()
x$get.networks()
x$update.network.conf(updated.values = list(author.only.committers = FALSE, author.directed = FALSE))
h = x$get.bipartite.network()
plot.network(h)
g = x$get.multi.network()
#plot.network(g)

## * Backups ---------------------------------------------------------------

## save binary objects
net = x$get.author.network()
save(net, file = sprintf("%s_%s.network", x.name, x$get.network.conf.entry("author.relation")))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Calculate EDCPTD centrality ---------------------------------------------

## get author networks for each relation
author.networks = get.author.networks.for.multiple.relations(x, c("cochange", "mail", "issue"))

## create fourth-order tensor
fourth.order.tensor = FourthOrderTensor$new(author.networks)

## calculate EDCPTD scores
edcptd.scores = calculate.EDCPTD.centrality(fourth.order.tensor)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Range-level data --------------------------------------------------------

y.data = RangeData$new(project.conf = proj.conf, range = ranges[[2]], built.from.range.data.read = TRUE)
y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)

## * Data retrieval --------------------------------------------------------

y.data$get.commits.unfiltered()
y.data$update.project.conf(updated.values = list(commit.messages = "message"))
y.data$get.commit.messages()
y.data$get.commits.unfiltered()
y.data$update.project.conf(updated.values = list(synchronicity = TRUE, synchronicity.time.window = 10))
y.data$get.synchronicity()
y.data$group.artifacts.by.data.column("commits", "author.name")
y.data$get.commits()
y.data$get.commits.uncached(remove.untracked.files = TRUE, remove.base.artifact = FALSE, filter.bots = FALSE)
y.data$get.mails.unfiltered()
y.data$get.mails()
y.data$get.issues.unfiltered()
y.data$get.issues()
y.data$get.authors()
y.data$get.data.path()
y.data$group.artifacts.by.data.column("mails", "author.name")
y.data$group.artifacts.by.data.column("commits", "hash")

## * Network construction --------------------------------------------------

y$update.network.conf(updated.values = list(edge.attributes = c("date")))
y$get.author.network()
y$update.network.conf(updated.values = list(edge.attributes = c("hash")))
y$get.artifact.network()
y$get.commit.network()
y$get.networks()
y$update.network.conf(updated.values = list(author.only.committers = FALSE, author.directed = TRUE))
h = y$get.bipartite.network()
plot.network(h)
g = y$get.multi.network()
plot.network(g)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vertex attributes -------------------------------------------------------

## define bins for network construction
mybins = c("2012-07-10 15:58:00", "2012-07-15 16:02:00", "2012-07-20 16:04:00", "2012-07-25 16:06:30")
## split data into ranges
cf.data = split.data.time.based(x.data, bins = mybins)
## construct (author) networks from range data
my.networks = lapply(cf.data, function(range.data) {
    y = NetworkBuilder$new(project.data = range.data, network.conf = net.conf)
    return(y$get.author.network())
})
## add commit-count vertex attributes
sample = add.vertex.attribute.author.commit.count(my.networks, x.data, aggregation.level = "range")
sample.cumulative = add.vertex.attribute.author.commit.count(my.networks, x.data, aggregation.level = "cumulative")
## add email-address vertex attribute
sample.mail = add.vertex.attribute.author.email(my.networks, x.data, "author.email")
sample.mail.thread = add.vertex.attribute.author.mail.thread.count(my.networks, x.data)
sample.issues.created = add.vertex.attribute.author.issue.creation.count(my.networks, x.data)
sample.pull.requests = add.vertex.attribute.author.issue.count(my.networks, x.data, issue.type = "pull.requests")
## add vertex attributes for the project-level network
x.net.as.list = list("1970-01-01 00:00:00-2030-01-01 00:00:00" = x$get.author.network())
sample.entire = add.vertex.attribute.author.commit.count(x.net.as.list, x.data, aggregation.level = "complete")
## add vertex attributes to commit network. Default value 'NO_AUTHOR' is used if vertex is not in commit data
add.vertex.attribute.commit.network(x$get.commit.network(), x.data, attr.name = "author.name", default.value = "NO_AUTHOR")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bulk methods for Codeface ranges (SHOULD NOT BE USED!!!) ----------------

net.conf = NetworkConf$new()

## author networks

auth = collect.author.networks(proj.conf, net.conf)

for (net in auth) {
    plot.network(net)
}

## artifact networks

art = collect.artifact.networks(proj.conf, net.conf)

for (net in art) {
    plot.network(net)
}

## bipartite networks

bp = collect.bipartite.networks(proj.conf, net.conf)

for (net in bp) {
    plot.network(net)
}

## multi networks

multi = collect.multi.networks(proj.conf, net.conf)

for (net in multi) {
     plot.network(net)
}

## lapply on data objects
data = construct.data(proj.conf, callgraphs = TRUE)
run.lapply(data, "get.data.path.callgraph")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Splitting data and networks ---------------------------------------------

## * Data-based splitting --------------------------------------------------

## split time-based using commits as the data source to split by (with sliding windows)
cf.data = split.data.time.based(x.data, time.period = "18 months", split.basis = "commits", sliding.window = TRUE)
for (range in names(cf.data)) {
    y.data = cf.data[[range]]
    y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
    plot.network(y$get.bipartite.network())
}
print(run.lapply(cf.data, "get.class.name"))

## split time-based using commits and issues as the data sources to split by (without sliding windows)
cf.data = split.data.time.based(x.data, time.period = "18 month", split.basis = c("commits", "issues"))
for (range in names(cf.data)) {
    y.data = cf.data[[range]]
    y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
    plot.network(y$get.bipartite.network())
}
print(run.lapply(cf.data, "get.class.name"))

mybins = c("2012-07-10 15:58:00", "2012-07-15 16:02:00", "2012-07-20 16:04:00", "2012-07-25 16:06:30")
cf.data = split.data.time.based(x.data, bins = mybins)
for (range in names(cf.data)) {
    y.data = cf.data[[range]]
    y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
    plot.network(y$get.bipartite.network())
}
print(run.lapply(cf.data, "get.class.name"))

## we can also use custom event timestamps for splitting
cf.data = split.data.time.based.by.timestamps(x.data)
for (range in names(cf.data)) {
    y.data = cf.data[[range]]
    y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
    plot.network(y$get.bipartite.network())
}
print(run.lapply(cf.data, "get.class.name"))

cf.data = split.data.activity.based(x.data, activity.amount = 10000, activity.type = "mails")
for (range in names(cf.data)) {
    y.data = cf.data[[range]]
    y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
    plot.network(y$get.bipartite.network())
}
print(run.lapply(cf.data, "get.class.name"))

# * Network-based splitting -----------------------------------------------

g = y$get.bipartite.network()
nets = split.network.time.based(g, time.period = "1 month", sliding.window = TRUE)
for (net in nets) {
    plot.network(net)
}

g = y$get.bipartite.network()
mybins = c("2004-10-23", "2004-11-11", "2004-12-27 00:12:23")
nets = split.network.time.based(g, bins = mybins)
for (net in nets) {
    plot.network(net)
}

g = y$get.bipartite.network()
nets = split.network.activity.based(g, number.windows = 2)
for (net in nets) {
    plot.network(net)
}

g = y$get.bipartite.network()
nets = split.network.activity.based(g, number.edges = 500, sliding.window = TRUE)
for (net in nets) {
    plot.network(net)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Motif identification ----------------------------------------------------

## as the used motifs (MOTIFS.LINE, MOTIFS.TRIANGLE.NEGATIVE, MOTIFS.TRIANGLE.POSITIVE,
## MOTIFS.SQUARE.NEGATIVE, MOTIFS.SQUARE.POSITIVE) are defined as undirected networks,
## these motifs can only be identified on undirected networks
y$update.network.conf(updated.values = list(author.directed = FALSE))

g = save.and.load("g", "y.multi.dat", skip = FALSE, if.not.found = function() {
    g = y$get.multi.network()
    return(g)
})
g.motifs = motifs.count(network = g,
                        motifs = list(motif.collaborating = MOTIFS.TRIANGLE.NEGATIVE,
                                      motif.communicating = MOTIFS.LINE,
                                      motif.collaborating.and.communicating = MOTIFS.TRIANGLE.POSITIVE),
                        remove.duplicates = TRUE, raw.data = FALSE)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network simplification --------------------------------------------------

## construct sample network
g = y$get.multi.network()
g = igraph::delete_edges(g, c(5, 6))
g = igraph::delete_vertices(g, c(2, 4, 5, 6, 7, 8))
g = g + igraph::edges(c("Björn", "Olaf", "Björn", "Olaf"), type = TYPE.EDGES.INTRA, weight = 1,
                      relation = "cochange", artifact.type = "Feature")

## merge edges between vertice pairs that stem from the same data source
g.simplified = simplify.network(g)
plot.network(g.simplified)

## merge all edges between vertice pairs
g.simplified = simplify.network(g, simplify.multiple.relations = TRUE)
plot.network(g.simplified)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Plots -------------------------------------------------------------------

## construct sample network for plotting
g = get.sample.network()
g = igraph::as_directed(g, mode = "arbitrary")
g = g + igraph::edges("A6", "A5", type = TYPE.EDGES.INTRA, weight = 2,
                      relation = "callgraph", artifact.type = "Feature")
g = simplify.network(g)

## print directly
plot.print.network(g, labels = TRUE)

## set a layout and print directly
lay = matrix(c(  20, 179, 693, 552, 956, 1091, 124, 317, 516, 615, 803, 1038,
                245, 175, 255, 185, 253, 225,   73,   8,  75,   0,  96,   86),
             nrow = 12, byrow = FALSE) # for sample graph
g = igraph::set_graph_attr(g, "layout", lay)
plot.print.network(g, labels = TRUE)

## get the plot object and modify it before plotting
p = plot.get.plot.for.network(g, labels = FALSE)
p = p +
    ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggraph::facet_edges( ~ edge.type)
#   ggraph::facet_edges( ~ weight)
#   ggraph::facet_nodes( ~ vertex.type)
#   ggraph::facet_graph(edge.type ~ vertex.type)
print(p)

## generate network plot from README file and save it to disk
p = plot.get.plot.for.network(g)
p = p +
    ggplot2::ggtitle("Exemplary multi network") +
    ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 15),
        plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
ggplot2::ggsave("plot-multi.png", plot = p, bg = "white", width = 6.57, height = 4.114)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Core/peripheral classification ------------------------------------------

range.data = RangeData$new(project.conf = proj.conf, range = first.test.range, built.from.range.data.read = TRUE)
range.data2 = RangeData$new(project.conf = proj.conf, range = second.test.range, built.from.range.data.read = TRUE)
empty.range.data = RangeData$new(project.conf = proj.conf, range = empty.test.range, built.from.range.data.read = TRUE)

range.net = NetworkBuilder$new(project.data = range.data, network.conf = net.conf)
range.net2 = NetworkBuilder$new(project.data = range.data2, network.conf = net.conf)
empty.range.net = NetworkBuilder$new(project.data = empty.range.data, network.conf = net.conf)

network = range.net$get.author.network()
network2 = range.net2$get.author.network()
empty.network = empty.range.net$get.author.network()

if (CASESTUDY == "test") {
  # as the data in the test project is not empty, empty it manually
  empty.range.data$set.commits(NULL)
  empty.range.data$set.mails(NULL)
  empty.range.data$set.issues(NULL)
  empty.network = create.empty.network(directed = net.conf$get.entry("author.directed"), add.attributes = TRUE)
}

network.list = list(empty.network, network, network2)
range.list = list(empty.range.data, range.data, range.data2)

## test functions for single range
author.class = get.author.class.by.type(network = network, type = "network.degree")
get.author.class.by.type(network = network, type = "network.eigen")
get.author.class.by.type(proj.data = range.data, type = "commit.count")
get.author.class.by.type(proj.data = range.data, type = "loc.count")

## test functions for single range with "empty" range data (network without edges)
author.class.empty.range = get.author.class.by.type(network = empty.network, type = "network.degree")
get.author.class.by.type(network = empty.network, type = "network.eigen")
get.author.class.by.type(proj.data = empty.range.data, type = "commit.count")
get.author.class.by.type(proj.data = empty.range.data, type = "loc.count")

## test function for multiple ranges (evolution)
author.class.overview = get.author.class.overview(network.list = network.list, type = "network.degree")
get.author.class.overview(network.list = network.list, type = "network.eigen")
get.author.class.overview(range.data.list = range.list, type = "commit.count")
author.class.overview.loc = get.author.class.overview(range.data.list = range.list, type = "loc.count")

recurring.authors = get.recurring.authors(author.class.overview = author.class.overview, class = "both")
longterm.core = get.recurring.authors(author.class.overview = author.class.overview, class = "core")

## role stability cannot be tested on the test project, as it will end up in an endless loop
if (CASESTUDY != "test") {
  role.stability = get.role.stability(author.class.overview = author.class.overview)
}

calculate.cohens.kappa(author.classification.list = author.class.overview,
                       other.author.classification.list = author.class.overview.loc)

get.class.turnover.overview(author.class.overview = author.class.overview)
get.unstable.authors.overview(author.class.overview = author.class.overview, saturation = 2)

