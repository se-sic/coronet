# codeface-extraction-r – Changelog

## unversioned

### Add relations to authors and artifacts (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- add for new relation types for each edge
- accept vector with more than one relation for `author.relation` and `artifact.relation` in util-conf.R

### Changes in util-networks (#98, 2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)

#### Build Networks (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- edit function `get.author.network` to handle more than one relation
- edit function `get.artifact.network` to handle more than one artifact relation
- add loop for handle more than one relation type and merge the resulting vertex lists and edge lists in `get.bipartite.network`
- add loops for different relations for `authors.to.artifacts` in `get.multi.network`, add information about the relation
  and merge vertex sets
- add new vertex attribute `kind`, which include `artifact.type` and the `type` of author vertices
(7c628fb93eb21f280c7d9da66680f817e107fa24, 7ad49c4ad937c9a6c7398a45179e25d5d5c03faa)
- remove vertex attribute `id` in artifact vertices (7ad49c4ad937c9a6c7398a45179e25d5d5c03faa)

#### Network and Edge Construction (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- function `construct.network.from.list` split in two functions  `construct.edge.list.from.key.value.list` and `construct.network.from.edge.list`
- add function `merge.network.data` und `merge.networks`
- add loop for relations in function `add.edges.for.bipartite.relation` and set the edge attribute `relation`
- add function `create.empty.edge.list`
- add loop for relations in `get.bipartite.relation` and save the type of the relation in an attribute `relation`

#### Simplify (021ac8b88e9a181364a51e89807df55cb741ed44)
- iterate over the different types of relations and simplfy the subnetworks relating to the `relation` attribute
- the `EDGE.ATTR.HANDLING` of the attribute `relation` is "first"

### Changes in test suite (784c417c50eb1de5d0143908a390ead6ba22dbbf, 7ad49c4ad937c9a6c7398a45179e25d5d5c03faa, be6ee8cd48dc7692e02b7f1c512870591300fa8a)
- add relation attribute `relation` to result data frames
- remove vertex attribute `id`
- write new tests for networks with more than one relation type

### Changes in util-plot (b55d3e84a5f9b122dacd0ee52784d930f22d1f4b, f190ca130a15a82e5eed836e9ffc53b8a34aac20)
- colors of the edges depending on the relation type
- line shape depend on the edge type (inter or intra)
- change colors of edges
- remove colors from `plot.fix.type.attributes`
- use palette 'viridis'
- different colors for artifacts


## unversioned

### Added
- Possibility to contract imperfect ranges in the end (#104, 8ebcf20b0aba0cb82dcd7e1d1b95e261a866d04e)


## 3.1.2

### Changed/Improved
- Vertex and edge types (attribute `"type"`) are now a character string (e.g., "Author" or "Unipartite") (#110, 3ca6ed99cf377200adb94a4b27ed1ea7d3a6981a)
- Default plotting layout is now `igraph::layout.kamada.kawai` (#109, 909965453c47c26c902612cb0c9aa16a5b56746a)
- Remove parameter 'color.attr' from 'motifs.search.in.network' (d33f6863aaf05ae1a8acf7f5667784713796b734)
- Fix and clean-up of both the plotting and the motif modules (3ca6ed99cf377200adb94a4b27ed1ea7d3a6981a, consequence of #110)
- Use vectors as vertex attributes instead of lists if there is only one value per vertex (#112, abc0dcbe2aa4ae6ff1c8387c3e961b95e57b2af2)
- Add a notice that the function `get.date.from.string` does ignore time-zone suffixes (9a51877f32a51850259b323c975182f2155b5302)

### Fixed
- Probably fixed segfaults during plotting by changing the default layout (see above and issue #109)
- Fix gray-scale plotting of networks (730cc544edbb30ea3aa89a91e123e74b18a942c6)


## 3.1.1

### Changed/Improved
- Improve performance of several functions used for adding vertex attributes (#102, PR #105)
- Change default values for aggregation levels for some functions (#102, PR #105)
- Add missing `remove.isolates` parameter to some more network-splitting functions (011328e881b09bd736dc83475ba7e6cab663bebe)
- Removed caching of artifacts in data objects (1bed431bf21d78936391a5e7278de5f762922eac)
- Some minor improvements to the test suite

### Fixed
- Fix outdated statements in README.md (PR #105)
- Fix range construction when difference between `start` and `end` is smaller than `time.period` (#103, PR #105, 975ae4d2e1b954d92f945c5853959ff2b3e47083)
- Fix caching of data objects in network builders (2b327a969427eb8ba5e4e20af33b09e243987a21)
- Fix activity-based splitting with sliding window of data objects (9860d7d0c80f9373b2887e0058a1941e2be277f2)


## 3.1

### Added
- Vertex attributes (#34, PRs #67 and #93, thanks to @flx5)
- Possibility to construct cumulative and overlapping ranges (#61, PR #96)
- Possibility to aggregate existing ranges (#61, PR #96, see also 52eae7fe478e6b60ebe7a8259d06e4cebb83522e)
- Add functions to split data and networks by ranges (#61)
- Add functions to split data by networks (#34, a8ca4061929cdf075f66e480891ed4562515f7e3)

### Changed/Improved
- Add 'issues.only.comments' to ProjectConf to construct issue-communication networks (#89, part of PR #95, e011782f68e38f277d003b154a0afefc9743a0b5)
- Use lubridate package for all POSIXct conversions (#77, PRs #85 and #94)
- The list of authors now contains e-mail addresses (#69, 8ebfcb720ad158bb385202a03e23d6feb7a85984, 7bfbe8403d6fdfb76d8856f3c2885028958ff12d)
- Add ability to the earliest and latest data timestamps (217e41c15b8ab2ea58e54e363d37764731957ad0)
- Some more refactorings and minor fixes, also to the test suite

### Fixed
- Add 'date.offset' to possible edge attributes (0c1868a8d0c2186fd5fd431f502e797168576d27)
- Fix `sqldf` queries (35ea503c2b11cc1d9392be4841402cc2d3781f25)


## 3.0.2

### Changed/Improved
- Move showcase log to distinct file (d2cd88b41c2ca21cff9fb40a18d89074a82bd264)

### Fixed
- Fix bug in reading commits for the file relation (3c878f11df1e019c39fce270733513e3904f46de)


## 3.0.1

### Added
- Add committer data to commit data source (#35, 251cfdbac0ab31584a7fab8bbaf8398a53ae8d11)
- Add function to delete isolate vertices from a network (5d91ddd89a488212eabf2ce110ec7210fc3c971d)

### Changed/Improved
- Remove warning when reading non-existent PaStA data (3d7f326e8279d1ea8b9b23b4b13c5036ffcc5067)
- Minor fixes and improvements to code style (0e421b58c4178eae9f6c12bc835944172077ddfc), logging (4899b1c04961199aa7e41f2db9a6fd8da6b99bd4), indentation (d8b77c2274a4a678f436514a5ae3bfda35fb97df), and test suite (7a8c4d4bfc31d2d0144d6a62bc7308fccfab2b99).


## v3.0

### Added
- Read GitHub issues and incorporate the data in network construction (main props to @Roger1995)
- Network metrics (#73, part of PR #78, thanks to @Roger1995)
- Add a method to calculate the bounds of a range (#58, b246fbe4237e3c9d9a897fb0a2371715e8a49d45, thanks to @flx5)
- Support for TravisCI (bb595f6c7aa265db7440ac2045d0da847c5d0de6)
- Cut data to avoid incomplete ranges resp. missing data (#38, PR #78, thanks to @hechtlC)
- Obtain earliest/latest timestamps from data sources (#38, PR #78, thanks to @hechtlC)
- Add function to split a list of networks (#57, 6db23d2358a650a99bc688397ec198727f8cb9d6)
- Much more documentation at functions and modules (thanks to @hechtlC)

### Changed/Improved
- Split network-building process into `ProjectData` and `NetworkBuilder` class (e.g., 768ffa4) (main props to @Roger1995 and @hechtlC)
- Use `ggraph` for plotting of networks, incorporating `ggplot2`-like plotting functionality (615928473b843587b85854c6819c1cced5c5a759)
- Give progess during edge construction (65650f6)
- Streamline data-source configuration options in network configuration and data columns after reading data (e.g., 5a58ad4, 50693da5dd04bbbcf8fbeea973e2d5e2f469f446, fe6dbb8b3e0eb66547803f8ea1418ff3b9da7ae0)
- New class structure for `ProjectConf` and `NetworkConf`, incorporating better and more transparent handling of parameters (e.g.,
629d108 and 605ab871a93df2fe69b377e6f250b63e92124ffd)
- The sample network is now built from sample project data (45218e6)
- Improvements to motif search (1c4a390)
- There are now functions suporting multi-network views (#56, 2e90c9ac70b25f7c07c35bd27fb9112c0a253521)
- Improve tests (e.g., 6081c80551f1ab499df3b897c65b9d4170072540, 7b8cf996daa3c0025a35808ef2cd877b3b9c5673)
- Better printing of configurations (5a58ad4a5b0b82c2c69a956b92d646058594ad0e)
- The showcase file is now named `showcase.R` (caa38c1a28a4f12b7e59792770e8413ef17068c2)
- Remove system-variable initialization from module files (#68, dff6844c4cd01e144bab8c223d28cda0bb346828)
- Add range attribute to built networks (#62, PR #64)
- Improve plotting module by changing to the `ggraph` package (615928473b843587b85854c6819c1cced5c5a759)

### Fixed
- Fix time-zone information by setting UTC as default time zone (4a3f8b91317fe7953670a83366cb3ad9c74b4d42, props to @sofie-kemper)
- Fix encoding when reading raw data (ea1925b880bbe7b77528c077f6244f3564007330, props to @sofie-kemper)
- Many, many more bug fixes...
