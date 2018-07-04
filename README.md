# codeface-extraction-r - The network library

Have you ever wanted to build socio-technical developer networks the way you want? Here, you are in the right place. Using this network library, you are able to construct such networks based on various data sources (commits, e-mails, issues) in a configurable and modular way. Additionally, we provide, e.g., analysis methods for network motifs, network metrics, and developer classification.

The network library `codeface-extraction-r` can be used to construct analyzable networks based on data extracted from `Codeface` [https://github.com/siemens/codeface] and its companion tool `codeface-extraction` [https://github.com/se-passau/codeface-extraction]. The library reads the written/extracted data from disk and constructs intermediate data structures for convenient data handling, either *data containers* or, more importantly, *developer networks*.

![Examplary plot of multi network](plot-multi.png)

## Table of Contents

- [Integration](#integration)
    * [Submodule](#submodule)
    * [Selecting the correct version](#selecting-the-correct-version)
    * [Requirements](#requirements)
        * [R](#r)
        * [packrat](#packrat)
        * [Data structure](#data-structure)
        * [Needed R packages](#needed-r-packages)
- [How-to](#how-to)
- [Functionality](#functionality)
    * [Data sources](#data-sources)
    * [Network construction](#network-construction)
        * [Types of networks](#types-of-networks)
        * [Vertex and edge attributes](#vertex-and-edge-attributes)
    * [Further functionalities](#further-functionalities)
    * [File/Module overview](#file/Module-overview)
- [Configuration classes](#configuration-classes)
    * [ProjectConf](#ProjectConf)
        * [Basic information](#basic-information)
        * [Artifact-related information](#Artifact-related-information)
        * [Revision-related information](#Revision-related-information)
        * [Data paths](#Data-paths)
        * [Splitting information](#Splitting-information)
        * [(Configurable) Data-retrieval-related parameters](#(Configurable)-Data-retrieval-related-parameters)
    * [NetworkConf](#NetworkConf)
- [License](#license)
- [Work in progress](#Work-in-progress)

## Integration

### Submodule

Please insert the project into yours by use of [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules).
Furthermore, the file `install.R` installs all needed R packages (see [below](#needed-r-packages)) into your R library.
Although, the use of of [packrat](https://rstudio.github.io/packrat/) with your project is recommended.

This library is written in a way to not interfere with the loading order of your project's `R` packages (i.e., `library()` calls), so that the library does not lead to masked definitions.

To initialize the library in your project, you need to source all files of the library in your project using the following command:
```R
source("path/to/util-init.R", chdir = TRUE)
```
It may lead to unpredictable behavior, when you do not do this, as we need to set some system and environment variables to ensure correct behavior of all functionality (e.g., parsing timestamps in the correct timezone and reading files from disk using the correct encoding).

### Selecting the correct version

When selecting a version to work with, you should consider the following points:
- Each version (i.e., a tag) contains, at least, a major and a minor version in the form `v{major}.{minor}[.{bugfix}]`.
- On the branch `master`, there is always the most recent and complete version.
- You should always work with the current version on the `master` branch. If you, nentheless, work on a former version, there might be a branch called `{your_version}-fixes` (e.g., `v2.3-fixes`) when we have fixed some extreme bugs in the current version, then select this one as it contains backported bugfixes for the former version. We will backport some very important bugfixes only in special cases and only for the last minor version of the second last major version.
- If you are confident enough, you can use the `dev` branch.

### Requirements

While using the package, we require the following infrastructure.

#### [`R`](https://www.r-project.org/) `3.3.1`

  Later `R` versions should work, but, for reliability reasons and
  `packrat` compatibility, only version `3.3.1` is supported.

#### [`packrat`](http://rstudio.github.io/packrat/)

  The local package manager of `R` enables the use to store all needed R
  packages for this repository inside the repository itself. All `R`
  tools and IDEs should be able to handle this automatically
  ([RStudio](https://www.rstudio.com/) does).

#### Data structure

  The data folder which can result from `Codeface` [https://github.com/siemens/codeface] and `codeface-extraction`,  needs to have the following structure (roughly):
  ```
  codeface-data
  ├── configurations
  │   ├── releases
  │   │     └──{project-name}.conf
  │   └── threemonth
  │         └──{project-name}.conf
  └── results
      ├── releases
      │      └──{project-name}
      │             ├── authors.list
      │             ├── commits.list
      │             ├── emails.list
      │             ├── revisions.list
      │             └── issues.list
      └── threemonth
              └──{project-name}
                    ├── authors.list
                    ├── ...
  ```


#### Needed R packages
To manage the following packages, we recommand to use `packrat`.

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
- `viridis`: For plotting of networks with nice colours


## How-to

In this section, we give a short example on how to initialize all needed objects and build a bipartite network.
For more examples, please see [the types of networks we can construct](#types-of-networks) and the file `showcase.R`.

```R
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
                            artifact.relation = ARTIFACT.RELATION,
                            simplify = TRUE))

## get ranges information from project configuration
ranges = proj.conf$get.entry("ranges")

## create data object which actually holds and handles data
data = ProjectData$new(proj.conf)

## create network builder to construct networks from the given data object
netbuilder = NetworkBuilder$new(data, net.conf)

## create and get the bipartite network
## (construction configured by net.conf's "artifact.relation")
bpn = netbuilder$get.bipartite.network()

## plot the retrieved network
plot.network(bpn)
```

There are two different classes of configuration objects in this library:
- the `ProjectConf` class which determines all configuration parameters needed for the configured project (mainly data paths) and
- the `NetworkConf` class which is used for all configuration parameters concerning data retrieval and network construction.

You can find an overview on all the parameters in these classes below in this file.
For examples on how to use both classes and how to build networks with them, please look in the file `showcase.R`.


## Functionality

### Data sources

There are two distinguishable types of data sources handled by the `ProjectData` and `RangeData` classes. The important difference is that the *main data sources* are used internally to construct artifact vertices in relevant types of networks. Additionally, these data sources can be used as a basis for splitting `ProjectData` in a time-based or activity-based manner (see file `split.R` and the contained functions). The *additional data sources* are orthogonal to the main data sources, can augment them by additional information, and, thus, are not split at any time.

All data sources are accessible from the `ProjectData` objects through their respective getter methods. For some data sources, there are additional methods available to access, for example, a more aggregated version of the data.

- Main data sources (artifact networks, splittable)
    * Commit data (called `"commits"` internally)
    * E-Mail data (called `"mails"` internally)
    * Issue data (called `"issues"` internally)

- Additional orthogonal data sources (augmentable to main data sources, not splittable)
    * PaStA data (see also the parameter `pasta` in the [`ProjectConf`](#configurable-data-retrieval-related-parameters) class))
    * Synchronicity information (see also the parameter `synchronicity` in the [`ProjectConf`](#configurable-data-retrieval-related-parameters) class)

###  Network construction

When constructing networks by using a `NetworkBuilder` object, we basically construct `igraph` objects. You can find more information on how to handle these objects on the [`igraph` project website](http://igraph.org/r/).

#### Types of networks

There are four types of networks that can be built using this library: author networks, artifact networks, bipartite networks, and multi networks which are a combination of author, artifact and bipartite networks. In the following, we give some more details on the various types. All types and their incorporated relations can be configured using a [`NetworkConf`](#networkconf)  object supplied to an `NetworkBuilder` object.

- Author networks
     * The vertices in an author network denote authors who are uniquely identifiable by their name. There are only unipartite edges among authors in this type of network.
     * The relations (i.e., the edges' meaning and source) can be configured using the [`NetworkConf`](#networkconf) attribute `author.relation`.

- Artifact networks
     * The vertices in an artifact network denote any kind of artifact, e.g., source-code artifact (such as features or files) or communication artifact (such as mail threads or issues). All artifact-type vertices are uniquely identifiable by their name. There are only unipartite edges among artifacts in this type of network.
     * The relations (i.e., the edges' meaning and source) can be configured using the [`NetworkConf`](#networkconf) attribute `artifact.relation`. The relation also describes which types of artifacts are in the network. (For example, if `mail` is selected as `artifact.relation`, only mail-thread nodes are included in the network.)

- Bipartite networks
     * The vertices in a bipartite network denote both authors and artifacts. There are only bipartite edges from authors to artifacts in this type of network.
     * The relations (i.e., the edges' meaning and source) can be configured using the [`NetworkConf`](#networkconf) attribute `artifact.relation`.

- Multi networks
     * The vertices in a multi network denote both authors and artifacts. There are both unipartite
     and bipartite edges among the vertices in this type of network. Essentially, a multi network
     is the combination of all other types of networks.
     * The relations (i.e., the edges' meaning and source) can be configured using the
     [`NetworkConf`](#networkconf) attributes `author.relation` and `artifact.relation`, respectively.

#### Vertex and edge attributes

There are some mandatory attributes that are added to vertices and edges in the process of network construction. These are not optional.

- Mandatory *vertex* attributes
    * `type`
        - The abstract type of data represented by the respective vertex, either an author or any type of artifact (e.g., source-code artifact or mail thread)
        - possible values: [`"Author"`, `"Artifact"`]
    * `kind`
        - The specific type of data represented by the respective vertex, augmenting the vertex attribute `type`
        - possible values: [`"Author"`,`"File"`, `"Feature"`, `"Function"`, `"MailThread"`, `"Issue"`,`"FeatureExpression"`]
    * `name`
        - The name for the data represented by the respective vertex (e.g., the author's name or a file path)

- Mandatory *edge* attributes
    * `type`
        - The abstract type of edge, either unipartite (among same-type vertices) or bipartite (among different-type vertices)
        - [`"Unipartite"`, `"Bipartite"`]
    * `relation`
        - The specific type of relation of this edge, augmenting the edge attribute `type` (see also the attributes `artifact.relation` and `author.relation` in the `NetworkConf` class)
        -  [`"mail"`, `"cochange"`, `"issue"`, `"callgraph"`]
    * `artifact.type`
        - The specific artifact type associated with the event causing the respective edge
        - [`"File"`, `"Feature"`, `"Function"`, `"Mail"`, `"IssueEvent"`,`"FeatureExpression"`]
    * `date`
        - The date of the event causing the respective edge

To add further edge attributes, please see the parameter `edge.attributes` in the [`NetworkConf`](#networkconf) class). To add further vertex attributes – which can only be done *after constructing a network* –, plese see the file `util-networks-covariantes.R` for the set of corresponding functions to call.

### Further functionalities

Often it is interessting to build the development networks not only for the whole project history
but also to split the data in smaller ranges. One benefit is to observe changes in the network over
time. Further details can be find in the section [*Splitting information*](#splitting-information).

In some cases it is not necessary to build a network to get the informations, we need. Therefore we
offer the possibility to  get the raw data or mapppings between e.g. an author and his edited
files. Example can be find the file `showcase.R`.

### File/Module overview

- `util-init.R`
    * Initialization file that can be used by other analysis projects (see Section [*Submodule*](#submodule))
- `util-conf.R`
    * The configuration classes of the project
- `util-read.R`
    * Functionality to read data file from disk
- `util-data.R`
    * All representations of the data classes
- `util-networks.R`
    * The `NetworkBuilder` class and all corresponding helper functions to construct networks
- `util-split.R`
    * Splitting functionality for data objects and networks (time-based and activity-based, using arbitrary ranges)
- `util-bulk.R`
    * Collection functionality for the different network types (using Codeface revision ranges, deprecated)
- `util-networks-covariates.R`
    * Functionality to add vertex attributes to existing networks
- `util-networks-metrics.R`
    * A set of network-metric functions
- `util-core-peripheral.R`
    * Author classification (core and peripheral) and related functions
- `util-motifs.R`
    * Functionality for the identifaction of network motifs (subgraph patterns)
- `util-plot.R`
    * Everything needed for plotting networks
- `util-misc.R`
    * Helper functions and also legacy functions, both needed in the other files
- `showcase.R`
    * Showcase file (see Section also [*How-To*](#how-to))
- `tests.R`
    * Test suite (running all tests in `tests/` subfolder)


## Configuration classes

### ProjectConf

In this section, we give an overview on the parameters of the `ProjectConf` class and their meaning.

All parameters can be retrieved with the method `ProjectConf$get.entry(...)`, by passing one parameter name as method parameter.
There is no way to update the entries, except for the revision-based parameters.

#### Basic information

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

#### Artifact-related information

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

#### Revision-related information

**Note**: This data is updated after performing a data-based splitting (i.e., by calling the functions `split.data.*(...)`).
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

#### Data paths

- `datapath`
    * The data path to the Codeface results folder of this project
- `datapath.callgraph`
    * The data path to the call-graph data
- `datapath.synchronicity`
    * The data path to the synchronicity data
- `datapath.pasta`
    * The data path to the pasta data

#### Splitting information

**Note**: This data is added to the `ProjectConf` object only after performing a data-based splitting (by calling the  functions `split.data.*(...)`).
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

#### (Configurable) Data-retrieval-related parameters

**Note**: These parameters can be configured using the method `ProjectConf$update.values()`.

- `artifact.filter.base`
    * Remove all artifact information regarding the base artifact
    (`"Base_Feature"` or `"File_Level"` for features and functions, respectively, as artifacts)
    * [*`TRUE`*, `FALSE`]
- `issues.only.comments`
    * Only use comments from the issue data on disk and no further events such as references and label changes
    * [*`TRUE`*, `FALSE`]
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

### NetworkConf

In this section, we give an overview on the parameters of the `NetworkConf` class and their meaning.

All parameters can be retrieved with the method `NetworkConf$get.variable(...)`, by passing one parameter name as method parameter.
Updates to the parameters can be done by calling `NetworkConf$update.variables(...)` and passing a list of parameter names and their respective values.

**Note**: Default values are shown in *italics*.

- `author.relation`
    * The relation(s) among authors, encoded as edges in an author network
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
    * The relation(s) among artifacts, encoded as edges in an artifact network
    * **Note**: This relation configures also the author--artifact relation in bipartite and multi networks!
    * possible values: [*`"cochange"`*, `"callgraph"`, `"mail"`, `"issue"`]
- `artifact.directed`
    * The (time-based) directedness of edges in an artifact network
    * **Note**: This parameter does not take effect for now, as the co-change relation is always undirected, while the call-graph relation is always directed.
  * [`TRUE`, *`FALSE`*]
- `edge.attributes`
    * The list of edge-attribute names and information
    * a subset of the following as a single vector:
         - timestamp information: *`"date"`*, `"date.offset"`
         - general information: *`"artifact.type"`*
         - author information: `"author.name"`, `"author.email"`
         - committer information: `"committer.date"`, `"committer.name"`, `"committer.email"`
         - e-mail information: *`"message.id"`*, *`"thread"`*, `"subject"`
         - commit information: *`"hash"`*, *`"file"`*, *`"artifact"`*, `"changed.files"`, `"added.lines"`, `"deleted.lines"`, `"diff.size"`, `"artifact.diff.size"`, `"synchronicity"`
       - PaStA information: `"pasta"`,
       - issue information: *`"issue.id"`*, *`"event.name"`*, `"issue.state"`, `"creation.date"`, `"closing.date"`, `"is.pull.request"`
    * **Note**: `"date"` and `"artifact.type"` are always included as this information is needed for several parts of the library, e.g., time-based splitting.
    * **Note**: For each type of network that can be built, only the applicable part of the given vector of names is respected.
    * **Note**: For the edge attributes `"pasta"` and `"synchronicity"`, the project configuration's parameters `pasta` and `synchronicity` need to be set to `TRUE`, respectively (see below).
- `simplify`
    * Perform edge contraction to retrieve a simplified network
    * [`TRUE`, *`FALSE`*]
- `skip.threshold`
    * The upper bound for total amount of edges to build for a subset of the data, i.e., not building any edges for the subset exceeding the limit
    * any positive integer
    * **Example**: The amount of `mail`-based directed edges in an author network for one thread with 100 authors is 5049.
    A value of 5000 for `skip.threshold` (as it is smaller than 5049) would lead to the omission of this thread from the network.
- `unify.date.ranges`
    * Cut the data sources to the largest start date and the smallest end date across all data sources
    * **Note**: This parameter does not affect the original data object, but rather creates a clone.
    * [`TRUE`, *`FALSE`*]

The class `NetworkBuilder` holds an instance of the `NetworkConf` class, just pass the object as parameter to the constructor.
You can also update the `NetworkConf` object at any time by calling `NetworkBuilder$update.network.conf(...)`, but as soon as you do so, all cached data of the `NetworkBuilder` object are reset and have to be rebuilt.

For more examples, please look in the file `showcase.R`.


## License

This project is licensed under [GNU General Public License v2.0](LICENSE).


## Work in progress

To see what will be the next things to be implemented, please have a look at the [list of issues](https://github.com/se-passau/codeface-extraction-r/issues).
