# Codeface Extraction - R Utilities

This project is an addendum to the project ' codeface-extraction' [1].
It reads the written/extracted data from disk and constructs intermediate data structures for convenient data handling.

    [1] https://github.com/clhunsen/codeface-extraction

## Usage

Please insert the project into yours by use of git submodules.
Furthermore, the file `install.R` installs all needed R packages (see below) into your R library.

Best, you source all files from this project using the following command:
```
source("path/to/util-init.R", chdir = TRUE)
```

## Needed R packages

- `yaml`: To read YAML configuration files (i.e., Codeface configuration files)
- `R6`: For a proper class syntax and usage
- `igraph`: For construction of networks
- `plyr`: For the dlply splitting-function and rbind.fill
- `parallel`: For mclapply
- `logging`: logging ;)
- `sqldf`: for advanced aggregation of data.frame objects
