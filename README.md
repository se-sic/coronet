# codeface-extraction-r - The network utility library

> > TODO more detailled project information

This project is an addendum to the project `codeface-extraction` [https://github.com/clhunsen/codeface-extraction].
It reads the written/extracted data from disk and constructs intermediate data structures for convenient data handling.


## Integration

### Submodule

Please insert the project into yours by use of [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules).
Furthermore, the file `install.R` installs all needed R packages (see below) into your R library.
Although, the use of of [packrat](https://rstudio.github.io/packrat/) with your project is recommended.

This library is written in a way to not interfere with the loading order of your project's `R` packages (i.e., `library()` calls), so that the library does not lead to masked definitions.

Best, you source all files of the library in your project using the following command:
```
source("path/to/util-init.R", chdir = TRUE)
```

### Needed R packages

- `yaml`: To read YAML configuration files (i.e., Codeface configuration files)
- `R6`: For proper classes
- `igraph`: For the construction of networks
- `plyr`: For the `dlply` splitting-function and `rbind.fill`
- `parallel`: For parallelization
- `logging`: Logging
- `sqldf`: For advanced aggregation of `data.frame` objects
- `testthat`: For the test suite
- `ggplot2`: For plotting of data
- `ggraph`: For plotting of networks (needs `udunits2` system library, e.g., `libudunits2-dev` on Ubuntu!)
- `markovchain`: For core/peripheral transition probabilities
- `lubridate`: For convenient date conversion and parsing


## How-To

In this section, we give a short example on how to initialize all needed objects and build a bipartite network.
For more examples, please see the file `test.R`.

```
CF.DATA = "/path/to/codeface-data" # path to codeface data

CF.SELECTION.PROCESS = "threemonth" # releases, threemonth(, testing)

CASESTUDY = "busybox"
ARTIFACT = "feature" # function, feature, file, featureexpression

AUTHOR.RELATION = "mail" # mail, cochange
ARTIFACT.RELATION = "cochange" # cochange, callgraph

## create the configuration Objects
proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
net.conf = NetworkConf$new()

## update the values of the NetworkConf object to the specific needs
net.conf$update.values(list(author.relation = AUTHOR.RELATION,
                            artifact.relation = ARTIFACT.RELATION))

## get ranges information from project configuration
ranges = proj.conf$get.entry(entry.name = "ranges")

## create data object which actually holds and handles data
cf.data = ProjectData$new(proj.conf, net.conf)

## create and get the bipartite network
## (construction configured by net.conf's "artifact.relation")
bpn = cf.data$get.bipartite.network()

## plot the retrieved network
plot.bipartite.network(bpn)
```

There are two different classes of configuration objects in this library:
- the `ProjectConf` class, which determines all configuration parameters needed for the configured project (mainly data paths) and
- the `NetworkConf` class, which is used for all configuration parameters concerning data retrieval and network construction.

You can find an overview on all the parameters in these classes below in this file.
For examples on how to use both classes and how to build networks with them, please look in the file `test.R`.

## Configuration Classes

### NetworkConf

In this section, we give an overview on the parameters of the `NetworkConf` class and their meaning.

All parameters can be retrieved with the method `NetworkConf$get.variable(...)`, by passing one parameter name as method parameter.
Updates to the parameters can be done by calling `NetworkConf$update.variables(...)` and passing a list of parameter names and their respective values.

**Note**: Default values are shown in *italics*.

- `author.relation`
  * The relation among authors, encoded as edges in an author network
  * **Note**: The  author--artifact relation in bipartite and multi networks is configured by `artifact.relation`!
  * possible values: [*`"mail"`*, `"cochange"`, `"issue"`]
- `author.directed`
  * The (time-based) directedness of edges in an author network
  * [`TRUE`, *`FALSE`*]
- `author.all.authors`
  * Denotes whether all available authors (from all analyses and data sources) shall be added to the network as a basis
  * **Note**: Depending on the chosen author relation, there may be isolates then
  * [`TRUE`, *`FALSE`*]
- `author.only.committers`
  * Remove all authors from an author network (including bipartite and multi networks) who are not present in an author network constructed with `artifact.relation` as relation, i.e., all authors that have no biparite relations in a bipartite/multi network are removed.
  * [`TRUE`, *`FALSE`*]
- `artifact.relation`
  * The relation among artifacts, encoded as edges in an artifact network
  * **Note**: This relation configures also the author--artifact relation in bipartite and multi networks!
  * possible values: [*`"cochange"`*, `"callgraph"`, `"mail"`, `"issue"`]
- `artifact.directed`
  * The (time-based) directedness of edges in an artifact network
  * **Note**: This parameter does not take effect for now, as the co-change relation is always undirected, while the call-graph relation is always directed.
  * [`TRUE`, *`FALSE`*]
- `edge.attributes`
  * The list of edge-attribute names and information
  * a subset of the following as a single vector:
       - timestamp information: *`"date"`*
       - author information: `"author.name"`, `"author.email"`
       - e-mail information: *`"message.id"`*, *`"thread"`*, `"subject"`
       - commit information: *`"hash"`*, *`"file"`*, *`"artifact.type"`*, *`"artifact"`*, `"changed.files"`, `"added.lines"`, `"deleted.lines"`, `"diff.size"`, `"artifact.diff.size"`, `"synchronicity"`
       - PaStA information: `"pasta"`,
       - issue information: *`"issue.id"`*, *`"event.name"`*, `"issue.state"`, `"creation.date"`, `"closing.date"`, `"is.pull.request"`
  * **Note**: `"date"` is always included as this information is needed for several parts of the library, e.g., time-based splitting.
  * **Note**: For each type of network that can be built, only the applicable part of the given vector of names is respected.
  * **Note**: For the edge attributes `"pasta"` and `"synchronicty"`, the network configuration's parameters `pasta` and `synchronicity` need to be set to `TRUE`, respectively (see below).
- `simplify`
  * Perform edge contraction to retrieve a simplified network
  * [`TRUE`, *`FALSE`*]
- `skip.threshold`
  * The upper bound for total amount of edges to build for a subset of the data, i.e., not building any edges for the subset exceeding the limit
  * any positive integer
  * **Example**: The amount of `mail`-based directed edges in an author network for one thread with 100 authors is 5049.
    A value of 5000 for `skip.threshold` would lead to the omission of this thread from the network.

The classes `ProjectData` and `RangeData` hold instances of  the `NetworkConf` class, just pass the object as parameter to the constructor.
You can also update the object at any time, but as soon as you do so, all
cached data of the data object are reset and have to be rebuilt.

For more examples, please look in the file `test.R`.

## ProjectConf

In this section, we give an overview on the parameters of the `ProjectConf` class and their meaning.

All parameters can be retrieved with the method `ProjectConf$get.entry(...)`, by passing one parameter name as method parameter.
There is no way to update the entries, except for the revision-based parameters.

### Basic Information

- `project`
  * The project name from the Codeface analysis
  * E.g., `busybox_feature`
- `repo`
  * The repository subfolder name used by Codeface
  * E.g., `busybox`
  * **Note**: This is the casestudy name given as parameter to constructor!
- `description`
  * The description of the project from the Codeface configuration file
- `mailinglists`
  * A list of the mailinglists of the project containing their name, type and source

### Artifact-Related Information

- `artifact`
  * The artifact of the project used for all data retrievals
  * **Note**: Given as parameter to the class constructor
- `artifact.short`
  * The abbreviation of the artifact name used in file names for call-graph data
- `artifact.codeface`
  * The artifact name as in the Codeface database
  * Used to identify the right commits during data retrieval
- `tagging`
  * The Codeface tagging parameter for the project, based on the `artifact` parameter
  * Either `"proximity"` or `"feature"`

### Revision-Related Information

**Note**: This data is updated after performing a data-based splitting (i.e., by calling the functions `split.data.*`).
**Note**: These parameters can be updated using the method `ProjectConf$set.splitting.info()`, but you should *not* do that manually!

- `revisions`
  * The analyzed revisions of the project, retrieved from the Codeface database
- `revisions.dates`
  * The dates for the `revisions`
- `revisions.callgraph`
  * The revisions as used in call-graph file name
- `ranges`
  * The revision ranges constructed from the list of `revisions`
  * The ranges are constructed in sliding-window manner when a data object is split using the sliding-window approach
- `ranges.callgraph`
  * The revision ranges based on the list `revisions.callgraph`

### Data Paths

- `datapath`
  * The data path to the Codeface results folder of this project
- `datapath.callgraph`
  * The data path to the call-graph data
- `datapath.synchronicity`
  * The data path to the synchronicity data
- `datapath.pasta`
  * The data path to the pasta data

### Splitting Information

**Note**: This data is added to the `ProjectConf` object only after performing a data-based splitting (by calling the functions `split.data.*`).
**Note**: These parameters can be updated using the method `ProjectConf$set.splitting.info()`, but you should *not* do that manually!

- `split.type`
  * Either `"time-based"` or `"activity-based"`, depending on splitting function
- `split.length`
  * The string given to time-based splitting (e.g., "3 months") or the activity amount given to acitivity-based splitting
- `split.basis`
  * The data used as basis for splitting (either `"commits"` or `"mails"`)
- `split.sliding.window`
  * Logical indicator whether a sliding-window approach has been used to split the data or network (either `"TRUE"` or `"FALSE"`)
- `split.revisions`
  * The revisions used for splitting (list of character strings)
- `split.revisions.dates`
  * The respective date objects for `split.revisions`
- `split.ranges`
  * The ranges constructed from `split.revisions` (either in sliding-window manner or not, depending on `split.sliding.window`)

### Data-Retrieval-Related Parameters (Configurable!)

**Note**: These parameters can be configured using the method `ProjectConf$update.values()`.

- `artifact.filter.base`
  - Remove all artifact information regarding the base artifact
    (`Base_Feature` or `File_Level` for features and functions, respectively, as artifacts)
  - [*`TRUE`*, `FALSE`]
- `synchronicity`
  * Read and add synchronicity data to commits and co-change-based networks
  * [`TRUE`, *`FALSE`*]
  * **Note**: To include synchronicity-data-based edge attributes, you need to give the `"synchronicity"` edge attribute for `edge.attributes`.
- `synchronicity.time.window`:
  * The time-window (in days) to use for synchronicity data if enabled by `synchronicity = TRUE`
  * [1, *5*, 10, 15]
  * **Note**: If, at least, one artifact in a commit has been edited by more than one developer within the configured time window, then the whole commit is considered to be synchronous.
- `pasta`
  * Read and integrate [PaStA](https://github.com/lfd/PaStA/) data
  * [`TRUE`, *`FALSE`*]
  * **Note**: To include PaStA-based edge attributes, you need to give the `"pasta"` edge attribute for `edge.attributes`.


## File overview

- `util-conf.R`
  * The configuration classes of the project
- `util-data.R`
  * All representations of the data classes
- `util-plot.R`
  * Everything needed for plotting networks
- `util-misc.R`
  * Helper functions and also legacy functions, both needed in the other files
- `util-split.R`
  * Splitting functionality for data objects and networks (time-based and activity-based, using arbitrary ranges)
- `util-motifs.R`
  * Functionality for the identifaction of network motifs (subgraph patterns)
- `util-bulk.R`
  * Collection functionality for the different network types (using Codeface revision ranges)
- `util-core-peripheral.R`
  * Author classification (core and peripheral) and related functions
- `util-init.R`
  * Initialization file that can be used by other analysis projects (see Section *Submodule*)
- `test.R`
  * Showcase file (see Section *How-To*)
- `tests.R`
  * Test suite (running all tests in `tests/` subfolder)


## Work in progress

To see what will be the next things to be implemented, please have a look at the [list of issues](https://github.com/se-passau/codeface-extraction-r/issues).
