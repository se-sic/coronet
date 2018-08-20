# codeface-extraction-r – Changelog


## Unversioned

### Changed/Improved
- Add possibility to add multiple first activities for different activity types in one vertex attribute (04f18b3097d17fe6b3486c656a807133e0ac0a42)
- Add possibility to decide, if first activity should be computed per activity type oder over all activity types when added as vertex attribute (86962a313ceeb09c0e0675dc509d91e10647d6b6)


## 3.3

### Added
- Possibility to add the commit count per person as vertex attribute, counting either commits, where the person is committer AND author, or committer OR author (#92 (second task), da87c06cf971559873fe1fef28aa035cf8c25c57, 0f0a90fd236b8fbf059275e637678b5411f356bd, 5df541d0662e71ed2bec4df9c04a1557301d25c9, 3f9739705e9b37c5383c60fa067cc21c880657e5)
- Add method `metrics.is.scale.free()` to decide whether a network is scale-free or not (80f47512ce7634c81f3708865eb1697b0151f549, 97161b1228a157cbe747c2e98b88f27f572d695e)
- Add tests for comparing networks that are created differently (66d37ceb8227ba120e4e06fb7b8334a73b19c076, 4a9d6b9a543a18c2e2f38aa7e05592f84039a745, a37c27731bbd2e96bb0b3730a8e6b429616248f0)
- Add method `clear.edge.attributes` to clear the edge attributes list of the network configuration (15f7587ed6590637991c2f811a26f9e860229288)
- Add network configuration parameter `author.respect.temporal.order` for determining the edge-construction algorithm (#6, 4fc59a0ff68c8600b574e868458e5d53dbdc405b, fd0b07da55a1ecdead7e5a5677bbd9ae810c391c)

### Changed/Improved
- Add committer information to the commit list in the test data
- Set the locale to "english-us" on Windows (b3da10d1d8b0b7667883964f11ca9a47b2f7b417)
- Update templates for pull requests and issues (0b9ecb7d9958d41c2bfc80c3489c3f4ca9ecac2a)
- Update the contribution guide regarding things to be done for a pull request (0b9ecb7d9958d41c2bfc80c3489c3f4ca9ecac2a)
- Update TravisCI script to run a job matrix with R 3.3, 3.4, and 3.5 (9bf7fcb383809903f9f11dcbb31969d4bc5fae12, b34bf75b55283ab36aa723dab87531e7a49d8126)
- Update README file regarding functionality, network types, data sources, and mandatory attributes (#121, da68b94445a2ac0c145ef73dc73cddfa64a1a499, 3200c57cb16d478712bf5a8f63c406b898e19762, baf41aa422a1e28f21d419572c6d23ed2c1b33e1, bec3a4704201e9b015c596591d1d70948dd4e929)
- Adjust legend orientation and placement in plots (now column-oriented) (c93ad2a48b2d18c725675637235d3b1939a99cd4)
- Refactor 'add.edges.for.bipartite.relation' for better readability (#118, 3d98b401664e601e9bb6fe202fddeb262417e0a4)
- Remove function 'combine.networks' (#118, b3496313e61eb1f6fe1e62e647cf0ab9a0d5b255)
- Do not support missing committer data anymore (871008e6f86ac728e616bd1de734a3203d524f4a)
- Do not serialize Strings when calculating the sha1 hash to generate an event ID for issues (basically due to encoding issues, eb56a8791ba5991781566f0073276ba216474ea9)
- Add implementation of Codeface to compute the scale-free attributes for small networks (80f47512ce7634c81f3708865eb1697b0151f549)
- Remove data inconsistencies when re-setting the commit, mail, and PaStA data (569552687e70ec7a67bed7da2c77cf56f5434dc6)
- Switch the order of the `type` and `kind` attributes of vertices in bipartite networks for testing reasons (351311a30f545d31e43e0065a6147185edec647b)
- Update README file (8380dc62f53591e53762a3692e44973535b5dcea, f590453aadde2dc9224d9f418f4a92c59cd49795, 792cb9558e04868bcad230dfdc50a5ace6d63d35, 8c2aI8255966cc6c38ea16e64a28b135ef8456e58, 5cfc5aec95eb3be5afcc549671477dcaf73d21b6, ebae9f8480033c073128e09dfae3873512cadafb, c66321e4ce6a9a08f681fd6f4d4afbde6c0bf260, 38e7c5d3f8e47450e9a71453ef09ff5ad5c39de4)
- Distinguish directedness of author networks and edge-construction algorithm (#6, 4fc59a0ff68c8600b574e868458e5d53dbdc405b, 70b3c82d73d1fee37829a61952dc1e2e993fa06f)

### Fixed
- Change the type of all commit count default values to Integer (62c033948d3449ed3bb64ec044036bcda56afdae)
- Retain network attributes in `simplify.network` (in `igraph` language, graph attributes) (424b2bce24a56ee594f5600327843fc240a376fd)
- Fix showcase file regarding outdated plotting parameters (29d5ac6485d679d1b2cc42c0633ab4cd343c73b7)
- Eliminate duplicated lines in the raw commit data (dec0005e8e6af1bdecd3028d074ddd2120c7b2cc)
- Fix the `split.networks.time.based` method by now splitting the networks from the earliest timestamp to the latest (1f65db382396e701655812a8ef4e9def61e8981b)
- Fix TravisCI build regarding `sudo` commands (baca08e33ff336fe91caaca5ca8ceb1cb77f3ded)
- Fix direction of edges in exemplary network plot in README file (5c80c256b214e03bdc7881774ba8b1b9f43c21ed)


## 3.2

### Added
- Handling of multiple relations for all types of networks (#98, #15, #11, PR #115)
    - Allow several entries for the entries `author.relation` and `artifact.relation` in the `NetworkConf` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
    - Add the mandatory edge attribute `relation` representing `author.relation` or `artifact.relation`, respectively, for all types of networks (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
    - Return data for several relations in `get.bipartite.relation` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
    - Retain one edge for each available value of edge attribute `relation` during network simplification (021ac8b88e9a181364a51e89807df55cb741ed44)
    - Add new tests and adapt existing ones (784c417c50eb1de5d0143908a390ead6ba22dbbf, 7ad49c4ad937c9a6c7398a45179e25d5d5c03faa, be6ee8cd48dc7692e02b7f1c512870591300fa8a)
- Add the mandatory vertex attribute `kind` describing the actual vertex kind (7c628fb93eb21f280c7d9da66680f817e107fa24, 784c417c50eb1de5d0143908a390ead6ba22dbbf)
- Respect new vertex and edge attributes in plot functions (b55d3e84a5f9b122dacd0ee52784d930f22d1f4b)
- Possibility to merge networks with function `merge.networks` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- Possibility to merge edge and vertex lists with function `merge.network.data` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- Add function `create.empty.edge.list` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- Possibility to contract imperfect ranges in the end (#104, 8ebcf20b0aba0cb82dcd7e1d1b95e261a866d04e)
- Add method `ProjectData$equals` (#116, 00df306a3e6dbdeb81ddc116e88a4854b07afe72)
- Add author classification by hierarchy to the core-peripheral module (8139f34fd809d6750064514a549024df4cbf5863)

### Changed/Improved
- Remove the mandatory vertex attribute `artifact.type` due to inconsistent use ()
- Remove the mandatory vertex attribute `id` from artifact vertices due to inconsistent use (7ad49c4ad937c9a6c7398a45179e25d5d5c03faa)
- Streamline edge attribute `artifact.type` for uniformity ()
- Use color palette 'viridis' for plotting for better flexibility  (f190ca130a15a82e5eed836e9ffc53b8a34aac20)
- Edge width in network plots now depends on edge weight, i.e., `width = 0.3 + 0.5 * log(weight)` (d791df8e2c41314f86c36b3af566141e7713f46c)
- Split function `construct.network.from.list` into the two functions `construct.edge.list.from.key.value.list` and `construct.network.from.edge.list` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- Handle data for more than one relation in function `add.edges.for.bipartite.relation` (2f1b4d9b0d6a629163a6dd3111b20930e15fcc13)
- Retain one edge for each available value of edge attribute `relation` during network simplification (021ac8b88e9a181364a51e89807df55cb741ed44)
- Read also lines from the PaStA data without the `message.id` being mapped to a `commit.hash` (992ddf8d582a7a023f000b4fc57f9ff85a7f38f6)
- Add column `revision.set.id` to PaStA data to indicate which e-mails are concerned with the same patch (992ddf8d582a7a023f000b4fc57f9ff85a7f38f6)
- Add PaStA data to the unfiltered commit data if configured (70d9b8bd4cb16636086ca7ab90e817b89844f172)

### Fixed
- Check whether a given object to the `ProjectConf` setter in the `ProjectData` class really is a object of type `ProjectConf` (ab00c962e164428df2d59de7292eed3c3b1352aa)
- The method for eigenvector centrality now properly considers whether the network is directed or not (c0277c36e4ff45cfbb421317a42b6ea8736afe53)
- Fix a bug that caused errors when the core classification within a core-periphery classification is empty (c0277c36e4ff45cfbb421317a42b6ea8736afe53)


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
