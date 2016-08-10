= Codeface Network Utilities

This repository is to be used for parsing Codeface output folders.
With it, the user is able to read also the data dunped by the tool 'codeface-extraction'.

= Usage

Please insert the project into yours by use of git submodules.

== Needed R packages
- yaml: To read YAML configuration files (i.e., Codeface configuration files)
- R6: For a proper class syntax and usage
- igraph: For construction of networks
- plyr: For the dlply splitting-function and rbind.fill
- parallel: For mclapply
- logging
- sqldf
