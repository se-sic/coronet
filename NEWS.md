## v3.0

### Added
- read GitHub issues and incorporate the data in network construction (main props to @Roger1995)
- much more documentation at functions and modules (thanks to @hechtlC)

### Changed/Improved
- split network-building process into `ProjectData` and `NetworkBuilder` class (e.g., 768ffa4) (main props to @Roger1995 and @hechtlC)
- use `ggraph` for plotting of networks, incorporating `ggplot2`-like plotting functionality (615928473b843587b85854c6819c1cced5c5a759)
- give progess during edge construction (65650f6)
- streamline data-source configuration options in network configuration and data columns after reading data (e.g., 5a58ad4, 50693da5dd04bbbcf8fbeea973e2d5e2f469f446, fe6dbb8b3e0eb66547803f8ea1418ff3b9da7ae0)
- new class structure for `ProjectConf` and `NetworkConf`, incorporating better and more transparent handling of parameters (e.g., 
629d108 and 605ab871a93df2fe69b377e6f250b63e92124ffd)
- the sample network is now built from sample project data (45218e6)
- improvements to motif search (1c4a390)
- there are now functions suporting multi-network views (2e90c9ac70b25f7c07c35bd27fb9112c0a253521)
- improve tests (e.g., 6081c80551f1ab499df3b897c65b9d4170072540, 7b8cf996daa3c0025a35808ef2cd877b3b9c5673)
- better printing of configurations (5a58ad4a5b0b82c2c69a956b92d646058594ad0e)

### Fixed
- fix time-zone information by setting UTC as default time zone (4a3f8b91317fe7953670a83366cb3ad9c74b4d42, props to @sofie-kemper)
- fix encoding when reading raw data (ea1925b880bbe7b77528c077f6244f3564007330, props to @sofie-kemper)
- many, many more bug fixes...
