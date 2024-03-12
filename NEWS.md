<link rel="shortcut icon" type="image/png" href="logo/3.favicon_radius.png">

# coronet – Changelog

## unversioned

### Added

- Add commit-interaction data and add functions `read.commit.interactions` for reading, as well as `get.commit.interactions`, `set.commit.interactions` and utility functions for working with commit-interaction data (PR #252, 5da0e60e029bdf427520be440fedb0f71d9f7a15, 7792a4e9a087c042a3ef9b7f79a67490305ce85e, 178265dcc69abc0d6e430dfcbc4b87e7565ce615, 80e6ac5f24e6d0248e77be391f93a59b6b17862d, 1ffa607bbe400bd212388dc543263ba5bec4e34c)
- Add commit-interaction networks that can be created with `create.author.network` and `create.artifact.network` if the `artifact.relation` and `author.relation` is configured to be `interaction` (PR #252, 5da0e60e029bdf427520be440fedb0f71d9f7a15, deddd4ce9d2a570ea57088ea73d4312f81e73049, 0e269af77bc098f2d3157fac349d2032efd6cf49, d96b10b45ec55cdf2dd02c60833d4116358d6d31)
- Add tests for new commit-interaction functionality (PR #252, 3e5b8962e18c3dde45085fa764c9d084327e2773, 7685ec4745bd43fba7a373bf5544f41bff346ed9, b291cb338e1b3896c8fd9769f45c515bddb8cf48, eea1b053350094084bab957975e1b306e6c9dc23, 3d4a521e47dc81aaae8ae01ff78ca8d514bb7d85, 05ea1ce1c3330f3fb8fb28ccbc08b85fbd4ec2c8, 99103f27ad0c8ee1bd62cdcee10778a98020db70, fd6064a83a7735020ad5250d092e266af5bbada0)

### Changed/Improved

### Fixed

## 4.4

### Announcement

- Due to a bug in package `igraph` (https://github.com/igraph/rigraph/issues/1158), which is present in their versions 2.0.0 to 2.0.3, the functions `metrics.scale.freeness` and `metrics.is.scale.free` can currently not be used with these `igraph` versions. If you need to call any of these two functions, you either need to install `igraph` version 1.6.0 or wait until the bug in `igraph` is fixed in a future version of `igraph`.

### Added

- Add issue-based artifact-networks, in which issues form vertices connected by edges that represent issue references. If possible, disambiguate duplicate JIRA issue references that originate from [codeface-extraction](https://github.com/se-sic/codeface-extraction) (PR #244, PR #249, 98a93ee721a293410623aafe46890cfba9d81e72, 771bcc8d961d419b53a1e891e9dc536371f1143b, 368e79264adf5a5358c04518c94ad2e1c13e212b, fa3167c289c9785f3a5db03d9724848f1441a63d, 4646d581d5e1f63260692b396a8bd8f51b0da48fda, ed77bd726bf92e06c2fc9145a5847787a8d0588b)
- Add a new `split.data.by.bins` function (not to be confused with a previously existing function that had the same name and was renamed in this context), which splits data based on given activity-based bins (PR #244, ece569ceaf557bb38cd0cfad437b69b30fe8a698, ed5feb214a123b605c9513262f187cfd72b9e1f4)
- Add `get.bin.dates.from.ranges` function to convert date ranges into bins format (PR #249, a1842e9be46596321ee86860fd87d17a3c88f50f, 858b1812ebfc3194cc6a03c99f3ee7d161d1ca15)
- Add the possibility to simplify edges of multiple-relation networks into a single edge at all instead of a single edge per relation (PR #250, PR #255, 2105ea89b5227e7c9fa78fea9de1977f2d9e8faa, a34b5bd50351b9ccf3cc45fc323cfa2e84d65ea0, 34516415fed599eba0cc7d3cc4a9acd6b26db252, 78f43514962d7651e6b7a1e80ee22ce012f32535, d310fdc38690f0d701cd32c92112c33f7fdde0ff, 58d77b01ecc6a237104a4e72ee5fb9025efeaaf2)
- Add network simplification to showcase file (PR #255, dc32d44f9db7dfc3cc795ef5d6b86609d6c1936f)
- Add tests for network simplification (PR #255, 338b06941eec1c9cfdb121e78ce0d9db6b75da19, 8a6f47bc115c10fbbe4eee21985d97aee5c9dc91, e01908c94eccc4dda5f2b3c0746b0eab0172dc07, 7b6848fb86f69db088ce6ef2bea8315ac94d48f9, 666d78444ffcb3bc8b36f2121284e4840176618e)
- Add an `assert.sparse.matrices.equal` function to compare two sparse matrices for equality for testing purposes (PR #248, 9784cdf12d1497ee122e2ae73b768b8c334210d4, d9f1a8d90e00a634d7caeb5e7f8f262776496838)
- Add tests for file `util-networks-misc.R` (#242, PR #248, PR #258, f3202a6f96723d11c170346556d036cf087521c8, 030574b9d0f3435db4032d0e195a3d407fb7244b, 380b02234275127297fcd508772c69db21c216de, 8b803c50d60fc593e4e527a08fd4c2068d801a48, 7335c3dd4d0302b024a66d18701d9800ed3fe806, 6b600df04bec1fe70c272604f274ec5309840e65, a53fab85358b223af43749a088ad02e9fbcb0a30, faf19fc369beb901b556ecb8c4fa0bf6f1bd6304)

### Changed/Improved

- Add input validation for the `bins` parameter in `split.data.time.based` and `split.data.by.bins` (PR #244, ed0a5302ea8c8934d7200b95be7ac1446305af07, 5e5ecbac44d07927b953ae9d4330a616f8224ba7)
- Test for the presence and validity of the `bins` attribute on network-, and data-splits (PR #249, c064affcfff2eb170d8bdcb39d837a7ff62b2cbd, 93051ab848ec94de138b0513dac22f6da0d20885)
- Simplify call chain-, and branching-routes in network-splitting functions and consequently set the `bins` attribute on every output network-split (while minimizing recalculations) (PR #249, #256, PR #257, a1842e9be46596321ee86860fd87d17a3c88f50f, 8695fbe7f21ccaa3ccd6d1016e754017d387b1fa)
- Rename `split.data.by.bins` into `split.dataframe.by.bins` as this it what it does (PR #244, ed5feb214a123b605c9513262f187cfd72b9e1f4)
- Throw an error in `split.data.time.based.by.timestamps` if no custom event timestamps are available in the ProjectData object (6305adcee7f18747141994b00bdd94641f95e86f)
- Enhance testing data by adding `add_link` and `referenced_by` issue events, which connect issues to form edges in issue-based artifact-networks. This includes duplicate edge information in JIRA data as produced by [codeface-extraction](https://github.com/se-sic/codeface-extraction) (PR #244, 9f840c040d552e8639aa82c3dd537c189679b348, ea4fe8d3c84f948af6147cf0137e80181ebb7a1e, 6eb731102301b1af08f4affb40d1f8df94500e34)
- Add a check for empty networks in the functions `metrics.scale.freeness` and `metrics.is.scale.free` and return `NA` if the network is empty (29418f2da38de8c39ec2a1fb3d445b63f320be40)
- Enhance `get.author.names.from.network` and `get.author.names.from.data` to always have the same output format. Now it doesn't depend on the `global` flag anymore (PR #248, d87d32564156f13c83ebe3361c2b68e5d0ac16ac, ddbfe68d3e628e82f34e09b36fffe886646986c5)
- Change `util-tensor.R` to correctly use the new output format of `get.author.names.from.network` (PR #248, 72b663ebf7169c0da5c687fe215529f3be0c08c5)
- Throw an error in `convert.adjacency.matrix.list.to.array` if the function is called with wrong parameters (PR #248, ece2d38b4972745af3a83e06f32317a06465a345, 1a3e510df15f5fa4e920e9fce3e0e162c27cd6d1)
- Rename `compare.networks` to `assert.networks.equal` to better match the purpose of the function (PR #248, d9f1a8d90e00a634d7caeb5e7f8f262776496838)
- Explicitly add R version 4.3 to the CI test pipeline (9f346d5bc3cfc553f01e5e80f0bbe51e1dc2b53e)

### Fixed

- Reformat `event.info.1` column of issue data according to the <issue-%source-%id> format, if the content of the `event.info.1` field references another issue (PR #244, 62ff9d0f31adbefb3381936237dc4ab984e33acb)
- Rename vertex attribute `IssueEvent` to `Issue` in multi-networks, to be consistent with bipartite-networks (PR #244, 26d7b7e9fd6d33d1c0a8a08f19c5c2e30346a3d9)
- Fix an issue in activity-based splitting where elements close to the border of bins might be assigned to the wrong bin. The issue was caused by the usage of `split.data.time.based` inside `split.data.activity.based` to split data into the previously derived bins, when elements close to bin borders share the same timestamps. It is fixed by replacing `split.data.time.based` by `split.data.by.bins` (PR #244, ece569ceaf557bb38cd0cfad437b69b30fe8a698)
- Remove the last range when using a sliding-window approach and the last range's elements are fully contained in the second last range (PR #244, 48ef4fa685adf6e5d85281e5b90a8ed8f6aeb197, 943228fbc91eed6854dacafa7075441e58b22675)
- Fix broken error logging in `metrics.smallworldness` (03e06881f06abf30d44b69d7988873f20b95232d)
- Fix `get.expanded.adjacency` to work if the provided author list does not contain all authors from the network and add a warning when that happens since it causes some authors from the network to be lost in the resulting matrix (PR #248, ff59017e114b10812dcfb1704a19e01fc1586a13)
- Fix `get.expanded.adjacency.matrices` to have correct names for the columns and rows (PR #248, PR #258, e72eff864a1cb1a4aecd430e450d4a6a5044fdf2, a53fab85358b223af43749a088ad02e9fbcb0a30)
- Fix `get.expanded.adjacency.cumulated` so that it works if `weighted` parameter is set to `FALSE` (PR #248, 2fb9a5d446653f6aee808cbfc87c2dafeb9a749a)
- Fix multi-network construction to work with `igraph` version 2.0.1.1, which does not allow to add an empty list of vertices (PR #250, 5547896faa279f6adaae4b2b77c7ab9623ddf256)


## 4.3

### Added

- Add function `verify.data.frame.columns` to check that a dataframe includes all required columns, optionally with a specified datatype (PR #231, d1d9a039f50480ec5b442dc7e8b518648d1f9d9d)
- Add helper function `is.single.na` to check whether an element is of length 1 and is `NA` (ddff2b8bbca6405f5c7c1cf4e7e97374fb1426ca, ccfc2d12a68dfa412f05159e8e3b03118694e748)
- Add CI support for GitHub Actions (PR #234, fa1fc4af65751402ae6b23298dd4ed821930c6d2)

### Changed/Improved

- Include structural verification to almost all functions that read dataframes from files or set a dataframe (setter-functions) (PR #231, b7a95881da72ccaa548c6cd5d94bd558a25caa6f)
- Include removal of empty and deleted users in the setters of mails, commits, issues, and authors. For commits, also the `committer.name` column is now checked for deleted or empty users (PR #235, 08fbd3e11e33d060f42cbc6f729eaf60b48a6de7)
- Check for empty values (i.e., values of length < 1) when updating configuration attributes and throw an error if a value is empty (9f36c544637ab4f4173408152d223b9b5098ce5a)

### Fixed

- Fix check for empty input files in utility read functions. Compared to unpresent files, empty files do not throw an error when reading them, a check for `nrow(commit.data) < 1` is therefore required (PR #231, ecfa643cbc15975c3062af95c50ead02730b580f)
- Fix various problems regarding the default classes of edge attributes and vertex attributes, and also make sure that the edge attributes for bipartite edges are chosen correctly (PR #240, 4275b93867c78d20d0bd116749c1e7603cd9d473, 98a6deb1b178a1fcf799c741906e99770c46a8d0, b8232c09b91df3412f703dd26c21c685bacd0321, a9535550d93207f466b315f33ea263a50e6c8924, 820a7631093d03ac5ccb7bf9923bd498f669120a)
- Add argument to `construct.edge.list.from.key.value.list` function which differentiates if constructed edges are supposed to be artifact edges, in which case we check if the `artifact` attribute is present for edges and replace it by `author.name`. (PR #238, e2c9d6c39fb757c566ef4c4b18780cca247477cb, 7f42a91d4aa84e8c28c048925190637051e358a9)
- Change edge construction algorithm for cochange-based artifact networks to respect the temporal order of data. This avoids duplicate edges. (PR #238, e2c9d6c39fb757c566ef4c4b18780cca247477cb)
- Clarify that edges in issue-based artifact-networks are not available yet in `README.md`. (PR #238, 18a54f0241a28675dba4cdcbd433e88ec68d515a)
- Fix bugs related to expanded adjacency matrices and update the initiation of sparse matrices to the most recent version of package Matrix, to replace deprecated and disfunct function calls. Due to this update, package versions prior to 1.3.0 of the Matrix package cannot be used any more. If the 'install.R' detects that a version prior to 1.3.0 is installed, it now automatically tries to re-install package Matrix once (PR #241, 573fab22a290e826e2bdd6e1f063cd2e87ed2167, 2f06252750354e4ed53b768bd212aacf1a350217)
- Prevent R warnings `'length(x) = 2 > 1' in coercion to 'logical(1)'` in `if` conditions for updating configuration values, in update functions of additional data sources, and in `get.first.activity.data()` (PR #237, PR #241, ddff2b8bbca6405f5c7c1cf4e7e97374fb1426ca, e1579cab9bf8cdfee4105426c144350d092fffbd)
- Prevent R warnings `In xtfrm.data.frame(x) : cannot xtfrm data frames` (PR #237, c24aee7d8f0b6ff4b641c8922e6ee1dce6f5999c)
- Fix wrong bracket in pasted logging message (PR #241, 50c68cb60114b49c32dc5be15014745cb8d42ded)
- Replace deprecated R function calls (PR #237, ed433821c04711a96501887b315d1b0ea8681f5a)


## 4.2

### Added
- Incorporate custom event timestamps, i.e., add a configuration entry to the project configuration that allows specifying a file from which timestamps can be read, as well as an entry that allows locking this data; add corresponding functions `get.custom.event.timestamps`, `set.custom.event.timestamps` and `clear.custom.event.timestamps` (PR #227, 0aa342430ad3b354b9cf954dbe0838b056cf328a, 0f237d03913d2c940a008ea8fe84ba44817e77ea, c1803982357a3272b108f60cb1c976e3c2d9b1e5, 54e089db0ceea07db94914d02655a7f1f67d3117, 54673f8f88ca276ba06396116d802425093544d4, c5f5403430d55ceff6b6d5acbbca1ae9c5c231e2)
- Add function `split.data.time.based.by.timestamps` to allow using custom event timestamps for splitting. Alternatively, timestamps can be specified manually (PR #227, 5b8515f97da4a24f971be453589595d259ab1fa1, 43f23a83bc66e485fea371f958bbb2ce3ddbd8d0)
- Add the following vertex attributes for artifact vertices and corresponding helper functions (PR #229, 20728071ca25e1d20cfa05bc15feb3ecc0a1c434, 51b5478ae15598ed3e6115b22e440929f8084660, 56ed57a21cc8004262ebba88429d0649cb238a52, 9b060361b1d1352b5a431df3990e468df7cab572, 52d40ba3657e3c806516653626afd81018a14863, e91161c79b53be7ba8ce3bec65de01ea6be1c575)
    - `add.vertex.attribute.artifact.last.edited`
    - `add.vertex.attribute.mail.thread.contributer.count`, `get.mail.thread.contributor.count`
    - `add.vertex.attribute.mail.thread.message.count`, `get.mail.thread.message.count`
    - `add.vertex.attribute.mail.thread.start.date`, `get.mail.thread.start.date`
    - `add.vertex.attribute.mail.thread.end.date`, `get.mail.thread.end.date`
    - `add.vertex.attribute.mail.thread.originating.mailing.list`, `get.mail.thread.originating.mailing.list`
    - `add.vertex.attribute.issue.contributor.count`, `get.issue.contributor.count`
    - `add.vertex.attribute.issue.event.count`, `get.issue.event.count`
    - `add.vertex.attribute.issue.comment.event.count`, `get.issue.comment.count`
    - `add.vertex.attribute.issue.opened.date`, `get.issue.opened.date`
    - `add.vertex.attribute.issue.closed.date`, `get.issue.closed.date`
    - `add.vertex.attribute.issue.last.activity.date`, `get.issue.last.activity.date`
    - `add.vertex.attribute.issue.title`, `get.issue.title`
    - `add.vertex.attribute.pr.open.merged.or.closed`, `get.pr.open.merged.or.closed`
    - `add.vertex.attribute.issue.is.pull.request`, `get.issue.is.pull.request`

### Changed/Improved
- **Breaking Change**: Rename existing vertex attributes for author vertices to be distinguishable from attributes for artifact vertices. With this change, the first word after `add.vertex.attribute.` now signifies the type of vertex the attribute applies to (PR #229, 75e8514d1d2f6222d2093679f4418e9171d3abf2)
    - `add.vertex.attribute.commit.count.author` -> `add.vertex.attribute.author.commit.count`
    - `add.vertex.attribute.commit.count.author.not.committer` -> `add.vertex.attribute.author.commit.count.not.committer`
    - `add.vertex.attribute.commit.count.committer` -> `add.vertex.attribute.author.commit.count.committer`
    - `add.vertex.attribute.commit.count.committer.not.author` -> `add.vertex.attribute.author.commit.count.committer.not.author`
    - `add.vertex.attribute.commit.count.committer.and.author` -> `add.vertex.attribute.author.commit.count.committer.and.author`
    - `add.vertex.attribute.commit.count.committer.or.author` -> `add.vertex.attribute.author.commit.count.committer.or.author`
    - `add.vertex.attribute.artifact.count` -> `add.vertex.attribute.author.artifact.count`
    - `add.vertex.attribute.mail.count` -> `add.vertex.attribute.author.mail.count`
    - `add.vertex.attribute.mail.thread.count` -> `add.vertex.attribute.author.mail.thread.count`
    - `add.vertex.attribute.issue.count` -> `add.vertex.attribute.author.issue.count`
    - `add.vertex.attribute.issues.commented.count` -> `add.vertex.attribute.author.issues.commented.count`
    - `add.vertex.attribute.issue.creation.count` -> `add.vertex.attribute.author.issue.creation.count`
    - `add.vertex.attribute.issue.comment.count` -> `add.vertex.attribute.author.issue.comment.count`
    - `add.vertex.attribute.first.activity` -> `add.vertex.attribute.author.first.activity`
    - `add.vertex.attribute.active.ranges` -> `add.vertex.attribute.author.active.ranges`
- Add parameter `use.unfiltered.data` to `add.vertex.attribute.issue.*`. This allows selecting whether the filtered or unfiltered issue data is used for calculating the attribute (PR #229, b77601dfa1372af5f58fb552cdb015401a344df7, 922258cb743614e0eeffcf38028acfc0a42a0332)
- Improve handling of issue type in vertex attribute name for `add.vertex.attribute.issue.*`. The default attribute name still adjusts to the issue type, but this no longer happens if the same name is specified manually (PR #229, fe5dc61546b81c7779643c3b2b37c101a55217f8)


## 4.1

### Added
- Incorporate gender data, i.e., add a configuration entry to the project configuration, add function `read.gender` for reading gender data, add functions `get.gender` and `set.gender` and corresponding utility functions to automatically merge gender data to the author data (PR #216, 8868ff47900cf804553ec98683b736b07350fc64, bfbe4deb9d14faeed56bdf37f9f732e01c41af57, 0a23862c6308c27fe4f93835c3a4480eac03ca91, a7744b548ac5ab697a4eb3d71987ddedef180d59, 6a50fd15bdd6382fa3d868a21a41a4b0a36ffce7, 413e24c18532d06144ef996184192594a0893ca3, 39db3158e931fa627e974451ae66c57bd0b77b12, 1e4026def1995a23b3f42eac5eb343ee5a518798)
- Add testing utillity file `tests/testing-utils.R` to make parameterized tests on the cross-product of multiple parameter values possible (d876f776439a52a3d34647d16a49ff39379e6da2, 9a1982051cc9849e07f6337ee8141d69def709db, 4dd5896d743c958bbcd6dda16feb50dc03c3a518)

### Changed/Improved
- Add `mode` parameter to `metrics.vertex.degrees` to allow choosing between indegree, outdegree, and total (#219, ae14eb4cb83c6ab8f387886228cdf7ea6f3258c4)
- Adjust `.drone.yml` CI config to prevent pipeline fails: `R` version `3.3` is not tested any more as some packages are not available any more for this `R` version (ca6b474d773c045dd88a19aee905283a373df0a6). Also another docker container in the CI pipeline is used as there are problems with the previously used docker instance (937f797ee04b78a087ea84043d075e7ca7558d70)
- Add `remove.isolates` parameter to `extract.bipartite.network.from.network`. The default value is `FALSE`, chosen to be consistent with `get.bipartite.network` and other network extraction methods. Previously, isolates were always removed (PR #226, b58394bde421e19eab3470f2266dfff9a7a2dca9, 079a256861a7621118b68bf09ba2dc53efc5f70e)

### Fixed
- Fix values in test for the eigenvector centrality as igraph has changed the calculation of this with version 1.2.7. Also put a warning that we recommend version 1.3.0 in `install.R` and document it in the `README.md` (25fb86277c7cc15b94ca0327bff4bb7e818ca09b, 1bcbca96d6dbaa2d4a28e830da963604682eac70)
- Fix the filtering of the deleted user in `util-read.R` to always be lowercase as the deleted user can appear with different spellings (#214, 1b4072c7ec0e33a595e31d9e9d27bb5c133b1556)
- Add check to `get.first.activity.data` to look for missing activity types. If no activities are in the RangeData, the function will print a warning and return an empty list (PR #220, #217, 5707517600c5579095c245b63c745d01cde02799, 42a4befb36e7fd9830924dc7fb2e04ecdf86e209,  d6424c03baff05562448df1b6b87828ca9a37b88, ca8a1b4c628261dcb471e1da3603439e75e4cc56, f6553c6106e5fec3837c6edb906a4d0960c5c5fb)
- Fix setting split.length properly in splitting info of the project configuration when the length is determined by splitting with the number.windows parameter (PR #222, 2bab846be4ca34fdc45047ec2ddb610c7aeaa555, b467a018b1fbd70ba7848196f520a9202dc319b0)


## 4.0

### Announcement
- `coronet` now has a logo and a website: [https://se-sic.github.io/coronet](https://se-sic.github.io/coronet) (#167, PR #196)

### Added
- Add functionality to read and process commit messages in order to merge them to the commit data (see issue #180). Three values are available for the new attribute `commit.messages` in `ProjectConf`: `none`, `title` and `messages` (PR #193, 85b1d0572c0fb9f4c062bceb1363b0398f98b85f, fdc414ade1a640f533e809a25cfe012e42b3cffa, 43e1894998e18faff3a65114fa65ee54e1d2f66e)
- Add functions `cleanup.commit.message.data` and `cleanup.synchronicity.data` to remove commit hashes that are not any more present in the commit data from the commit message data or synchronicity data (PR #193, 98e83b037ecc88d9a29e8e4ca93598a9978e85a2)
- Add function `metrics.is.smallworld` to the metrics module in order to unify checks for smallworldness (similar to scalefreeness) (PR #195, ce1f8124298d73830f3fd96aa84e82ba89181aa6)
- Add function `metrics.vertex.centralities` to metrics module in order to simplify getting a data frame containing author names and their respective centrality values (d3cd528609480f87658601ef13326e950a74cce7, e7182e7185d4f3acd90da5c05987a082bc40d607)
- Add function `get.data.sources.from.relations` to `util-networks.R` which extracts the data sources of a network that were used when building it (PR #195, d1e4413a49ab83a115a7f26719592876371ab264)
- Add tests for the `get.data.sources.from.relations` function (PR #195, add0c746dde8279da41d180deecf52b91a46095c)
- Add logo directory containing several logo variants (PR #196, 82f99719b8a43b0a44914b67b26bf1a34bb076c6, dc4659ea354e97159f8ee6167811db544f0b7087, fdc5e677325225f92d1f99948cb9441bfe1d150d, 752a9b376ffeffd5d6b380b4fdba838a890e3ef7)
- Add function `preprocess.issue.data`, which implements common issue data filtering operations (fcf5cee64c809d62a33275cbd3272b8087869eea, a566caec6d7e649cc495d292a19eca8a7ffccde8, 5ba6feb988c44e2ba398bccce6c88e69d3bb552e)
- Add function `get.issues.uncached`, which gets the issues filtered without poisoning or using the cache. (eb919fad9519d6e1a23261977bb3bfa2b899aaf9)
- Add function `get.issues.unfiltered` to get the unfiltered issues so that these methods follow the naming scheme known from the respective methods for commits (b9dd94c8575b8cab40d0d1185368854f84299d87, e05f3448e1e2e8faaac219ed4ac818f82f3d8ff7)
- Add per-author vertex attributes regarding counting of issues, issue-creations, issue-comments, mails, mail-threads, ... (like mail thread count, issue creation count) (PR #194, issue #188, 9f9150a97ffbb64607df0ddcbce299e16c2580da, 7260d62cf6f1470584753f76970d19664638eeed, 139f70b67903909bcd4c57e26afa458681a869f2, eb4f649c04155e22195627072e0f08bb8fe65dc4, 627873c641410182ca8fee0e78b95d7bda1e8e6b, 1e1406f4a0898cac3e61a7bc9a5aa665dceef79f, 98e11abc651b5fe0ec994eddea078635b0d6f0b2, a566caec6d7e649cc495d292a19eca8a7ffccde8)
- Add functionality that allows to read any data source at any point in time, even after splitting. In this case, the read data is automatically cut to the corresponding range on the `RangeData` object (PR #201, 7f9394f528068f52957be67ab7e2efb733011c6b). Additionally, when changing the configuration parameters concerning additional data sources, the environment of a `ProjectData` object is no longer reset (PR #201, eed45ac1f97b4915ec42fb869f34359da1df4c60)
- Add new configuration parameters `commits.locked`, `mails.locked` and `issues.locked` to `ProjectConf` which, when set to `TRUE`, prevent the respective getters from triggering the read of the data if it is not present yet (PR #201, 382167790e7d096b539eeed6464efeb9bb501d43)
- Add support for classifying developers on the basis of more count-based classification metrics, including mail-count, mail-thread count, issue-count, issue-comment count, issue-commented-in count, and issue-created count (issue #70, PR #209, d7b2455ece9840caba7c3a46704d2a62a11bee57, 6f737c8859519867928acc4e545c89bfd91b1e2e)
- Add bot filtering mechanism, which allows removing issues/mails/commits made by bots (838855ffbe6d404bcb447d470a97870b238754ae, dcce82de58e25c6a0f3c27c9a7c46fb35a47fced)
- Ignore the "deleted user", as well as the author having an empty name "" (1a08140884f30366995352af315218cfd315b958, 24c222a21238cf0d21dfa750d8c951f39b4d0500)

### Changed/Improved
- **Breaking Change**: Rename getters for main data sources: Unfiltered date is now acquired using `get.<datasource>.unfiltered`, filtered data is acquired using `get.<datasource>` (edf19cfee9c2855a1d968f37158b0fd2c196766a, e05f3448e1e2e8faaac219ed4ac818f82f3d8ff7)
- Add check for empty network in `metrics.hub.degree` function. In the case of an empty network, a warning is being printed and `NA` is returned (PR #195, 4b164bebea1e8258cb93febf51271a4b6f486779)
- Adjust the function `ProjectData$get.artifacts`: Rename its single parameter to `data.sources` and change the function so that it can extract the artifacts for multiple data sources at once. The default is still that only artifacts from the commit data are extracted. (PR #195, cf795f26652b00de5d717c703c688af55a972943, 70c05ecd1e3c0f10810acc2b2ae06a3eb8856317, 5a46ff4d428af7f301fe57d6e9e10421f553a9cc, fd767bb37ca608c28d9ff4a449415cc0e863d7ee)
- Change the internal representation of empty data from `NULL` to empty data frames and adapt function `get.cached.data.sources()` of `ProjectData` which returns a vector of all data sources that are cached (including additional and filtered data sources) (PR #201, aec898e105ff5478fb75ba488b56c9c258a17a80, e55d088f926ae7a069761864855c179c04ed07dc, 24c222a21238cf0d21dfa750d8c951f39b4d0500); additionally, introduce new function `is.data.source.cached()` in `util-data.R` that returns a logical vector indicating which of the given data sources are cached (PR #201, b49cc5df4da04af9df2ae9bcdc7847fafb6befc9, 491e70c44dd71e4d51b40ee83bc8eb598d413bc0, 24c222a21238cf0d21dfa750d8c951f39b4d0500)
- Change the threshold calculation for the classification of developers to use a quantile approach when classifying on the basis of network centrality metrics (issue #205, PR #209, PR #210, 5128252572ab6471a9dd5437360232622c33e958, 0d6a3a16bac9d11a9b69e77ed082664e2c0d245d)
- Update documentation in `util-network-metrics.R` and `util.conf.R` (PR #195, f929248182594613bd203e100268e3e3dce87f34, de9988cc171cafdd084701d5a2693a74176a802a, PR #199, 059b286a3cebc586e88ce6446d98133ddd619260)
- Splitting no longer loads all (additional) data sources, but only the ones that have already been cached in the `ProjectData` (PR #201, 52a3014b3c4a1d6dda1347adcffed4e90fae1d2b, aec898e105ff5478fb75ba488b56c9c258a17a80, de1bbfe644c52200e80a769e7c0e447c3a087278)
- Improve the documentation in `util-core-peripheral.R` by adding roxygen skeleton documentation to undocumented functions (issue #70, PR #209, a3d5ca7a5f8c021e0bed588219321798388790ac, 6f737c8859519867928acc4e545c89bfd91b1e2e)
- Change the `$` notation to the bracket notation in `util-core-peripehral.R` (issue #70, PR #209, 6f737c8859519867928acc4e545c89bfd91b1e2e)
- Add `.drone.yml` to enable running our CI pipelines on drone.io (PR #191, 1c5804b59c582cf34af6970b435add51452fbd11)
- Not only run test suite in our CI pipeline, but also run the showcase file in our CI pipeline using test data (719a4f0e067c4a68587bcdede0bbf72f6a6a666e, 3eb31d81156a1a9ab74598b48d585b7477358dd3)
- Add R version 4.1 to test suite and adjust missing time-zone attributes on `NA` vectors or empty POSIXct vectors which are correctly added as of R version 4.1 (PR #203, 6b7fb36478325185f80f95e03dd0a0135bf44346, 98c56715502cf5140d98796dc2d6cb18f71760b2, 09d11ab631de8051ca317f36652ab0fee9cc0cce)

### Fixed
- Fix fencing issue timing data so that issue events "happen" after the issue was created. Since only `commit_added` events are affected, that only happens for these. (issue #185, 627873c641410182ca8fee0e78b95d7bda1e8e6b, 6ff585d9da1da3432668605f0c09f8e182ad0d2f)
- Fix the function `reset.environment()` of both the `ProjectData` and `NetworkBuilder` class; they now reset all the data (PR #199, de091a5e6c70fea5161276d6585bea916178e4de, fc4c086bf2691b5bb37614d21c7690f53e52f29e)
- Adjust the functions `update.commit.message.data()`, `update.pasta.data()`, and `update.synchronicity.data()`: no warning is being printed anymore when being called by the corresponding cleanup function (PR #199, e5c60a50f8fa0f5bf9d362fdf49845d58652dd75)
- Fix issue where the data path on `RangeData` objects was wrong in special cases. Introduce the (private) flag `built.from.range.data.read` that is set according to how the object has been created (splitting manually or reading codeface ranges) and calculating the data path accordingly (PR #199, cce95279692b8d29ae464e12cfe48e923f417ac7, 917bf64e7a439c6a33b922e04929030479722bd1, 169c034c2364fc56507573e6f2316cc432631ba6). Also add tests for this new behaviour (PR #199, ef5bac63580d171dd7f68da6c4ec7279dcd400b5, 3aa8e7de4b847924afa350a1fc4a53a7f8fa86e1, d454e5a3468d765247ed4827e708b8576e54f87c, 66ad1272148e3d98f9508ff0e83180d967b6abaa)
- Make splitting no longer modify the original `ProjectConf`, instead create a copy (e82d056b7a67288addb5989ded1e8c921d5ed0a4)
- Fix and update outdated examples in the showcase file (473c09458741c8d24e7025646e70f8e052b0a9c, 287fbfa2f892e586c4955a408d914590573f5e83, 0a5cce4c6cb2813981e6cb05b65fbaf0e51ef3af, PR #207)
- Fix generation of Codeface range directory names from commit hashes (5c90d1cbb6f8ca9b217b99b7a7ecc687d6c0d0e1)
- Fix plotting an empty network via `plot.network` (03f986d760da8f8fec0e7aa530672736ea3068c7)
- Fix behavior of `construct.ranges` when only one range has to bee constructed and `sliding.window = TRUE` (000314b961247c55051f03da3b2034fbc668847e)
- Add package `reshape2` to the install script as this package is used in module `util-plot-evolution.R` for quite a while but never has been added to the list of packages to install (7bb4e7bc15b77c1faf8ddf8655c820822be25f80)
- Fix data tests in `test-data.R` to use deep clones of `ProjectData` objects (PR #209, d75373a56d9de9da3bd6d6f08c1bba0ed10b8425)
- Fix the `update.values()` function in `util-conf.R` to delete the `value` field if the new value is equal to the default value as the comparison of two otherwise equal `Conf` objects fails without this (PR #209, d75373a56d9de9da3bd6d6f08c1bba0ed10b8425)


## 3.7

### Added
- Add a new file `util-tensor.R` containing the class `FourthOrderTensor` to create (author x relation x author x relation) tensors from a list of networks (with each network having a different relation) and its corresponding utility function `get.author.networks.for.multiple.relations` (PR #173, c136b1f6127d73c25f08ae2f317246747aa9ea2b, e4ee0dc926b22ff75d5fd801c1f131bcff4c22eb, 051a5f0287022f97e2367ed0e9591b9df9dbdb3d)
- Add function `calculate.EDCPTD.centrality` for calculating the EDCPTD centrality for a fourth-order tensor in the above described form (c136b1f6127d73c25f08ae2f317246747aa9ea2b, e4ee0dc926b22ff75d5fd801c1f131bcff4c22eb, 051a5f0287022f97e2367ed0e9591b9df9dbdb3d)
- Add new file `util-networks-misc.R` which contains miscellaneous functions for processing network data and creating and converting various kinds of adjacency matrices: `get.author.names.from.networks`, `get.author.names.from.data`, `get.expanded.adjacency`, `get.expanded.adjacency.matrices`, `get.expanded.adjacency.matrices.cumulated`, `convert.adjacency.matrix.list.to.array` (051a5f0287022f97e2367ed0e9591b9df9dbdb3d)
- Add tests for sliding-window functionality and make parameterized tests possible (a3ad0a81015c7f23bce958d5c1922e3b82b28bda, 2ed84ac55d434f62341297b1aa9676c12e383491, PR #184)
- Add function `cleanup.pasta.data` to remove wrong commit hashes and message ids from the PaStA data (1797e0324c39ad7b88dc22a14391340f4d26aea8, PR #189)

### Changed/Improved
- Adjust the function `get.authors.by.data.source`: Rename its single parameter to `data.sources` and change the function so that it can extract the authors for multiple data sources at once. The default value of the parameter is a vector containing all the available data sources (commits, mails, issues) (051a5f0287022f97e2367ed0e9591b9df9dbdb3d)
- Adjust recommended R version to 3.6.3 in README (92be262514277acb774ab2885c1c0d1c10f03373)
- Add R version 4.0 to test suite and adjust package installation in `install.R` to improve compatibility with Travis CI (40aa0d80e2a94434a8be75925dbefbde6d3518b2, 1ba036758a63767e2fcef525c98f5a4fd6938c39, #161)

### Fixed
- Fix sliding-window creation in various splitting functions (`split.network.time.based`, `split.networks.time.based`, `split.data.time.based`, `split.data.activity.based`, `split.network.activity.based`) and also fix the computation of overlapping ranges in the function `construct.overlapping.ranges` to make sure that the last and the second-last range do not cover the same range) (1abc1b8dbfc65ccad0cbbc8e33b209e39d2f8118, c34c42aef32a30b82adc53384fd6a1b09fc75dee, 097cebcc477b1b65056d512124575f5a78229c3e, 9a1b6516f490b72b821be2d5365d98cac1907b2f, 0fc179e2735bec37d26a68c6c351ab43770007d2, cad28bf221f942eb25e997aaa2de553181956680, 7602af2cf46f699b2285d53819dec614c71754c6, PR #184)
- Fix off-by-1 error in the function `get.data.cut.to.same.date` (f0744c0e14543292cccb1aa9a61f822755ee7183)
- Fix missing or wrongly set layout when plotting networks (#186, 720cc7ba7bdb635129c7669911aef8e7c6200a6b, 877931b94f87ca097c2f8f3c55e4b4bcc6087742)
- Fix reading of the PaStA data since the file format has changed (712bbafde3fb8f7b7c0fc847cb9c1838eb4cf86e, PR #189)
- Fix bug that duplicates revision set ids in the mail and commit data when merging the PaStA data and also copy-paste error when merging PaStA data to commit data (1797e0324c39ad7b88dc22a14391340f4d26aea8, PR #189)
- Fix bug that results in an error when there is a variable called 'c' in the R environment (de42eb24be131c261ccad7d807007f27d5559d68, PR #189)
- Fix bug that when applying `filter.patchstack.mails()` to an environment with no mail data, the mail data gets set to `NULL` (82614754fb3d75b0e5856d1ef42ada737859ee37, PR #189)


## 3.6

### Added
- Add a parameter `editor.definition` to the function `add.vertex.attribute.artifact.editor.count` which can be used to define, if author or committer or both count as editors when computing the attribute values. (#92, ff1e147ba563b2d71f8228afd49492a315a5ad48)
- Add the possibility to filter out patchstack mails from the mails of the `ProjectData`. The option can be toggled using the newly added configuration option `mails.filter.patchstack.mails`. (1608e28ca36610c58d2a5447d12ee2052c6eb976, a932c8cdaa6fe5149c798bc09d9e421ba679c48d)
- Add a new file `util-plot-evaluation.R` containing functions to plot commit edit types per author and project. (PR #171, d4af515f859ce16ffaa0963d6d3d4086bcbb7377, aa542a215f59bc3ed869cfefbc5a25fa050b1fc9. 0a0a5903e7c609dfe805a3471749eb2241efafe2)

### Changed/Improved
- Add R version 3.6 to test suite (8b2a52d38475a59c55feb17bb54ed12b9252a937, #161)
- Update `.travis.yml` to improve compatibility with Travis CI (41ce589b3b50fd581a10e6af33ac6b1bbea63bb8)

### Fixed
- Ensure sorting of commit-count and LOC-count data.frames to fix tests with R 3.3 (33d63fd50c4b29d45a9ca586c383650f7d29efd5)


## 3.5

### Announcement
- Rename project to `coronet` (#10, 929f8cec7b52adef1389ce1691b783c235eb815d, ac1ce80b9f5da812f90b5fed63f26dc8c812a4d6)
    * Be sure to update Git remotes and submodules to the new URL!

### Added
- Add the constants `UNTRACKED.FILE`, `UNTRACKED.FILE.EMPTY.ARTIFACT`, and `UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE`: Commits that do not change any artifact are considered to be carried out on a meta-file called `<untracked.file>`. The constant `UNTRACKED.FILE` is added to hold the string constant. Analogously, the constants `UNTRACKED.FILE.EMPTY.ARTIFACT` (currently, `""`) and `UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE` (currently, `""`) hold the constants for any artifacts and their corresponding types, respectively, "changed" in untracked files. (11428d9847fd44f982cd094a3248bd13fb6b7b58, 5ea65b9ac5a22967de87d7fd4ac66b0bc8e07238, dde0dd7c6b36b49aa2b6c91395be8ea6e0cd7969, 2284bbec55e091a4135dc029906ba12446b9f0ad)
- Add the public method `ProjectData$get.commits.filtered.uncached`: The method allows for external filtering of the commits by specifying if untracked files and/or the base artifact should be filtered (this method does not take advantage of caching, whereas the method `ProjectData$get.commits.filtered` does) (11428d9847fd44f982cd094a3248bd13fb6b7b58)
- Add the parameters `commits.filter.base.artifact` and `commits.filter.untracked.files` to the `ProjectConf`: In addition to the `ProjectConf` parameter `commits.filter.base.artifact` (previously called `artifact.filter.base`), which configured whether the base artifact should be included in the `get.commits.filtered` method, there is now a similar parameter called `commits.filter.untracked.files` doing the same thing for untracked files (11428d9847fd44f982cd094a3248bd13fb6b7b58, 466d8eb8e7f39e43985d825636af85ddfe54b13a)
- Add parameter `edges.for.base.artifacts` to `NetworkConf` : In author networks, edges do not get constructed anymore between authors for solely modifying untracked files. For authors involved in changing the base artifact, it can be configured whether edges should be created or not using the new `NetworkConf` parameter `edges.for.base.artifacts` (c60c2f6e44b6f34cccb2714eccc7674158c83dde, 466d8eb8e7f39e43985d825636af85ddfe54b13a)
- Add method `ProjectData$get.authors.by.data.source` to retrieve authors by given data-source name (#149, 65804276dd2ada9b2f00b2cab7b6ad0cecbe733e, 137d8337bc35f5a83aa16a48ef8e47fc0d36b36c)
- Add helper function `create.empty.data.frame`: The function returns empty data.frames (0 rows) with correct columns and, if specified, all the correct data types. In the future, functions, that return data in data.frames, should always return data.frames of the same shape (regarding columns and data types) – especially when they are empty – because this makes later case distinctions easier or unnecessary (67a4fbe4f244b4b6047c2c2be7682d7f9085e9eb, 351364751b3fc286c66b99fe1fa3f52150f67311)
- For the most common types of data.frames (data.frames of commits, mails, issues, and authors) four more utility methods are added, namely `create.empty.authors.list`, `create.empty.commits.list`, `create.empty.issues.list`, `create.empty.mails.list`, `create.empty.synchronicity.list`, `create.empty.pasta.list` as well as corresponding constants holding columns and associated data types for all these empty data.frames (5f0f52936b4433f64fd9b1c9b2571eb26f66395f, 523daef8cf4642a2360396b11f0d74bce565b0f0, f8e021db955d65ff76b1c359706a188c9fef8c62, 351364751b3fc286c66b99fe1fa3f52150f67311, 2f4e6f0657d26dbf84f093ff77b8d43993a69ddc, cd3e34a369435392f9be082df05f9fc504b56239)
- Add mandatory attributes in `create.empty.network` *if wanted* (cae9d4bd6913d9b78b0bc819915011191f87fedf, cc8bd86befe5b9fc56c53816b609be434bfa2953)
- Add function `create.empty.vertex.list` (c00101dd8c78dc03d61bce1b5f88805b9fbb3a5f)
- Add tests for construction of networks without data (a4b3524676a0df88ec544db99c951b4461437099)
- Add tests for construction of networks without vertices (6eb214c1aca0899567529aa514352440f3005d5c)
- Add a note on mailing-list threads to README (c6dca275c3571e396529a0178c8300de8cd8aa26)
- Add cutting functionality to README descriptions (fb40c506d8dd838cc8853d426e83840ed93b10d4)
- Add the parameter `restrict.classification.to.authors` to the functions `get.author.class.by.type`, `get.author.class.overview`, `get.author.class.network.degree`, `get.author.class.network.eigen`, `get.author.class.network.hierarchy`, `get.author.class.commit.count` and `get.author.class.loc.count`. The parameter allows to perform classifications on a limited group of authors whose names are specified in this parameter. (2492dd0de5909c41031541ffb365eee40a342b65, #148)
- Add test cases for `util-core-peripheral.R` by adding the new file `test-core-peripheral.R` along with test cases (2627d6c9aaa4b066cf3043f9cf167fb470bdec6c)
- Add project-configuration parameter `issues.from.source` to choose if only issues from JIRA, only issues from GitHub, or all issues shall be read in (PR #159, d677949bedc3567b02cc7a1f3daffa0a785aa7a8, a3e71326d8deb861fcc4434c580e12570f7f8fa0, ea2618134efbfd8159ea49bd6fd21f4d11f3faeb). Therefore two test cases, one that reads in only JIRA issues and one that reads in only GitHub issues, are added to the issue read test (65b1acd7895b5330fab1d53d7bd27ab5dbd25192, 2d897cbb38853b20e1adba88f617908033142aef)
- Add class documentation (#157, 6e33d0aa49d5a432c1fad786944bd1ac855ecbf3, 250f9e04b18775f3c5f40b114236afcde9e760ae)

### Changed/Improved
- Always add mandatory vertex and edge attributes (#154, 0526755da68aa79efc3e86e34eb60a8d9b3116d7)
- Heavily improve addition of PaStA data (cd3e34a369435392f9be082df05f9fc504b56239)
- The method `read.issues` in `util-read.R` now supports the new issue data format (PR #147, 77c750c034c270f007c75abca0f0630573f195a2, e04ce3080b3cc3e305d8be5aa47ed5b144a9c9c0, 67b818a955c5e75ac3735d2e09af1f564d82f736, 402048735d0c33214be194b7535593786104e32e, 351364751b3fc286c66b99fe1fa3f52150f67311). Therefore, the test issue data and all related tests are updated (39971eea6d51793c88a35c4e604bc5e3a13bb123, 0ec6c6c3243e79fce30f8c9c39dc3c4bee2dee7b, 6a9f4ad89f5b9d6d4b19c837006a429637b22c04, fda000fe6208760f18138e65feca3b6e8ff553b2, 351364751b3fc286c66b99fe1fa3f52150f67311)
- Rename `ProjectConf` parameter `artifact.filter.base` to `commits.filter.base.artifact` (PR #149, 466d8eb8e7f39e43985d825636af85ddfe54b13a)
- The constant `BASE.ARTIFACTS` is extended by adding untracked files (i.e. the new meta-file `UNTRACKED.FILE`), which is now considered to be a new base artifact in the case of file-level analyses. This implies, that, in case of file-level analyses, the base artifact and the untracked files fall together, while in feature-level and function-level analyses they are treated differently (d11d0fb585397fdb3a2641484248f74752db9331)
- Filtering by artifact kind (e.g. filtering out either `"Feature"` or `"FeatureExpression"`) is now being done in the method `ProjectData$get.commits` instead of the method `ProjectData$get.commits.filtered` (894c9a5c181fef14dcb71fa23699bebbcbcd2b4f)
- Remove `get.commits.filtered.empty` and corresponding `filter.commits.empty` method, the functionality is now included into the methods `get.commits.filtered` and `filter.commits` respectively (11428d9847fd44f982cd094a3248bd13fb6b7b58)
- The private method `ProjectData$filter.commits` now takes parameters which configure whether untracked files and/or the base artifact are to be filtered (11428d9847fd44f982cd094a3248bd13fb6b7b58)
- Remove `get.commits.raw`, `set.commits.raw` and `read.commits.raw` functions (64a94863c9e70ac8c75e443bc15cd7facbf2111d, c26e582e4ad6bf1eaeb08202fc3e00394332a013)
- Add commits on untracked files to test suite (#153, d9f527c5602a3f463e5ccb0d395abe1d6a837ea3)
- In the class `Conf` (and its sub-classes `NetworkConf` and `ProjectConf`), default parameters are not validated anymore to avoid confusion by logging output (ec8c6dd72746a0506b3e03dccc4fcaf7a03325ea)
- In the class `Conf` (and its sub-classes `NetworkConf` and `ProjectConf`), `stop` is called on errors during parameter updates now (ec8c6dd72746a0506b3e03dccc4fcaf7a03325ea)
- Change shape of `Vertices` in the legend of plots to avoid confusion (f4fb4807cfd87d9d552a9ede92ea65ae4a386a04)
- Refactor `ProjectData$get.cached.data.sources` to be more concise (a4e7a213dce6d4709e92e22d2f55971b7bde8037)
- Update contribution guide regarding `roxygen2` conventions (#157, fbc2d5487fe08d072f22578c4954601315f8aee7, 783ee58ebeb9865df25a7f95d23a881b4d7de96b, 6e33d0aa49d5a432c1fad786944bd1ac855ecbf3)
- Update README regarding mandatory edge attributes (641624b077d403a34b52718c7aaea25dd1ce626d)
- Rename misleading parameter names for functions `get.author.class.by.type`, `get.author.class.overview`, `get.author.class.network.degree`, `get.author.class.network.eigen`, `get.author.class.network.hierarchy`, `get.author.class.commit.count` and `get.author.class.loc.count`. Most importantly, the parameter `range.data` was renamed to `proj.data` for these functions. (587ef99c1eb93751180bba6037c7f2fe6e24aca5, 81568b12ffdc7637bd0d5a05d0f56a96a88ee6ac, #70)
- Remove the unused functions `get.commit.count.threshold` and `get.loc.count.threshold`. (2534d73283b6e7f9703b22f605298eaa2c158d93, #70)
- The function `verify.argument.for.parameter` was adjusted to be suitable in more general use-cases (557bdcd65940d7a098354b19a5c24cec018e3533)
- Do not redundantly initialize data sources when splitting (35698a1b41c25b9ad7c598977d0afd0add16044f)
- Read PaStA and synchronicity data only if enabled (79bf3ca2b42d0f5c22f7ba3e9ec50c95586a3831)
- Add and enforce coding convention to use 'vertices' and not 'nodes'. Most importantly, the function `metrics.node.degrees` is renamed to `metrics.vertex.degrees`. (d35ce616db76adae06b34b4b241a35bfbe77e10d)
- Adjust range directories' names to start with a consecutive range number and to conform with the directories created by [Codeface](https://github.com/se-sic/codeface) (b3e29472a57e26935a31645b96fbef7d7785c25a, f6b28fbe3bb3599784a42e102fa4fc1e480c2a7a)
- Remove the two functions `get.author.class.activity` and `get.author.class.activity.overview` from the file `util-core-peripheral.R` (61b344a8ce6725ecf0415b108ada9ee08e1121d9)
- Remove function `get.commit.data` from `util-data.R` and replace all calls to this function with statements of equivalent functionality despite the fact that they are now retrieving the commit data via `get.commits.filtered` instead of `get.commits` which was internally used in the function `get.commit.data` (#70, 4fc6b450cd70bc6c1c63f268aec805d6328849c6, 7fc454e9d8f3c951fcb9ac820f056f2fd08e6945, c4cf8d25d62d9448c4c2571ed973387835ac87c6)
- Add possibility to decide whether the vertex attribute `active.ranges` should be computed per activity type or over all activity types (#92, aba8af959b39bbc16747786eab9781fe40e08ed3, 1bb81e86f1f8d62527de83c6489d1c9d5666f19d, 8f35a6b5194b664ef186e57650e6705b38ec610f)
- During the computation of the vertex attribute `first.activity`, the default value is now used analogous to active-ranges computation: The given value is used as default per author and type. (#92, 18a065c9b0d93c83795bf1be2319e6470b70622a, edf864a5e9a4f54336ecd5c884be12672084e332)

### Fixed
- Remove the empty artifact from all types of networks (#153, 4eba7f6d77d48f00959ec26d3182d29bd1282444)
- Fix vertices for networks without edges (#150, PR #149, 0d7c2226da67f3537f3ff9d013607fe19df8a4c0, 7e27a182de282f054f08e3a2fb04d852c2c55102)
- Fix merging of networks without vertices (0666f1fffb718063024351b9ccf3c0885bec4acf)
- Fix extraction functions regarding handling of empty networks (#155, 3e403e49cf547586db1edb15f8711f1a744bed19)
- Fix hierarchy calculations and classifications (#151, 59740189ba4c9502a041e5387337e0537024a1d4)
- Rework 'tests.R' and fix logger initialization for tests (#152, c93c4fda30482eafa1f9e0174b9aaea357c6917b)
- Fix handling of empty vertex list in `construct.network.from.edge.list` (01f31d685f7e324c7e2fdd16cd376e764afcdec9)
- Fix error when resetting an `ProjectData` environment (c64cab84e928a2a4c89a6df12440ba7ca06e6263)
- Fix missing time-zone attribute `tzone` on `POSIXct` items (5f6cc6922b95bf5cbdd9b2cbf16be4bf4937d0db)
- Fix author classification which was incorrect in certain situations and adjust test cases to this change (9294a37d98f9ff3d14756d56300b0d171f3f3b4c, c7288c3690b68f367a9f451bec7c584897971a31)
- Fix wrong behavior of `get.author.class` when using `result.limit` and when classifying zero or passing invalid input (9437b4f07da599fde017596af2290b24601f9f8d, #164, d93b906993089e35f0b539fe3b06b2f36ae3d4c6, 8060caa4930ef05a48b59a328ebf928a64109294, 70e4de5d83541eaad4714d7761b1b35503aaebbf)
- Add vertex attribute also to empty networks (#165, 74845d4b179d2830c1483b42897a8bd0fcee19e9)
- Fix outdated function calls in the bulk module (4e0354078449ef57637297fd3670830e10342ecd)
- Add special case handling for the classification of networks without edges (7e14492640cbf504c45431c03cf0167c455a4b77)


## 3.4

### Added
- Create global constant named `BASE.ARTIFACTS` (7031d450cfbfdd17e07e3c18290d8b1ff6612181)
- Split data into time-based equally-sized windows in function `split.data.time.based` (#49, 40974bada77b9184b025e47ebd7f0c4fb98e1475, a17475368e0beb3a4a9a71c9e6ec02a763e2bc32)
- Split networks into time-based equally-sized windows in functions `split.network(s).time.based` (#49, 94cc87bd1789b211aff13b7c26235ccee77c2b7b, a17475368e0beb3a4a9a71c9e6ec02a763e2bc32, 5ac149262fa15a661748ebbee0d31c775bc41dd0)
- Add function to delete authors without specific edges from networks `delete.authors.without.specific.edges` (#76, b9319e3ed4c3b0ea6860b4fabea2993b97e186e0, 107854c27f555ac9ba8439e041e1485d58d97c1c, 4e211f0ea559b3affea09becb8e846bab9836682, 4850666a46a086649faebee4aa3829005fad95af)
- Add methods `ProjectData$group.authors.by.data.column` and `ProjectData$group.artifacts.by.data.column` (#97, 11f71899a514910c1f44ed1d9648127e93057ea2)
- Add method `ProjectData$group.data.by.column` (b78f54fd36b5152bc64db5eaa0d2627e142dcf7f, 11f71899a514910c1f44ed1d9648127e93057ea2, related to #97)

### Changed/Improved
- Add possibility to add multiple first activities for different activity types in one vertex attribute (#92, 04f18b3097d17fe6b3486c656a807133e0ac0a42)
- Add possibility to decide whether the vertex attribute `first activity` should be computed per activity type or over all activity types (#92, 86962a313ceeb09c0e0675dc509d91e10647d6b6)
- Refactor computation of vertex attribute `first.activity` for better performance (40b7d879e323275d308c408cca4913b805ddacf8, f5188904e51ddc08558842f6e357f8fa8edbb105)
- Move `RELATION.TO.DATASOURCE` to module 'networks' (1ac09f64d202ba4279d05a9765bbbefdc57d4e1b)
- Determine list of artifacts more reasonably in ProjectData (#97, 23a8aa3e8b20cd0d735cb4987e4a397b922e01ad, 11f71899a514910c1f44ed1d9648127e93057ea2)
- Adapt `ProjectData$get.artifacts` to work with all data sources (#97, 0d184b82dac1a6d7edb3a3339e8352343cbe020f)
- Improve function `save.and.load` to work without assignment (7f6ab1a5220b555a8123ff04b01547f7704fb31b)
- Handle incorrect keys and values in `get.key.to.value.from.df` (5b74038bb5abc7748b2f2b7f1da1b5c2169717bd, related to #97)

### Fixed
- Fix computation of vertex attribute `first.activity` to handle empty data sources (4a9ad23bc203fe756e11665ae5c11ee97e97ca8b, 425c46bc41ae3446bd00ae3c8f7eb39434b77a0b)
- Fortify check on callgraph revision in NetworkBuilder (dcf56ad9f4a3390d58dfb369a6e5f4fa76d18523)
- Move pull-request template to take effect (6df72e94c1ea9749e798ed8354a6100a59cb9fc9)
- Fix function `split.networks.time.based` regarding case that provided list of networks only contains one element (010a9358c91ad4a2fd29aa27301b7723c2f6b6e2)
- Fix problem with fractional time periods in `generate.date.sequence` (8d80fa9939e6c5d95a7881a774c92764250f9341)
- Handle ARPACK errors in eigen-centrality calculation (c5413c24f643c9981d35991db3f4a5b5f6b9b194, f213648bd4f1fa1056fef4d6e250f9e5b2aeb3c1)
- Allow merge of empty networks (edges and/or vertices) (#142, 26e3bef6cc82c0705894384571775eba4e44010a)


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
- Use color palette 'viridis' for plotting for better flexibility (f190ca130a15a82e5eed836e9ffc53b8a34aac20)
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
