# codeface-extraction-r - Changelog


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

## Added
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
