<link rel="shortcut icon" type="image/png" href="logo/3.favicon_radius.png">

# Contributing to the network library `coronet`

The following is a set of guidelines for contributing to the network library `coronet`, which is hosted in the [se-sic](https://github.com/se-sic) organization on GitHub.
These are mostly guidelines, not rules. Use your best judgment, and feel free to propose changes to this document in a pull request.

#### Table Of Contents

[FAQ](#faq)

[How Can I Contribute?](#how-can-i-contribute)
  * [Reporting Bugs](#reporting-bugs)
  * [Suggesting Enhancements](#suggesting-enhancements)
  * [Your First Code Contribution](#your-first-code-contribution)
  * [Pull Requests](#pull-requests)

[Style Conventions](#style-conventions)
  * [Commit Messages](#commit-messages)
  * [R Coding Conventions](#r-coding-conventions)


## FAQ

1. **What is this project?**
   Using the code in this project, you can use Codeface data to construct sophisticated developer networks for further analysis.
2. **How can I use the project in my project?**
   A small introduction is given in our [README](README.md) file.


## How Can I Contribute?

### Reporting Bugs

This section guides you through submitting a bug report for our project. Following these guidelines helps maintainers and the community understand your report, reproduce the behavior, and find related reports.

Before creating bug reports, please check [this list](#before-submitting-a-bug-report) as you might find out that you don't need to create one. When you are creating a bug report, please include as many details as possible.

**Note**: If you find a `closed` issue that seems like it is the same thing that you are experiencing, open a new issue and include a link to the original issue in the body of your new one.

#### Before Submitting A Bug Report

* **Check the code.**
  You might be able to find the cause of the problem and fix things yourself. Most importantly, check if you can reproduce the problem in the latest version of the library (see [branch `dev`](https://github.com/se-sic/coronet/tree/dev)).
* **Search for previous issues describing the same problem.**
  If an old issue includes also a fix or a workaround for your problem, you do not need to file a new issue. Although, if the problem still persists after applying potential fixes, please file a new issue including detailed information to reproduce the problem. If there is an old issue that is still open, add a comment to the existing issue instead of opening a new one.
* **Run the test suite.**
  If the test suite does not finish successfully, you may have changed some core code, so that the basic functionality of the library is not consistent anymore. Please reset your changes and try again.

#### How Do I Submit A (Good) Bug Report?

Bugs are tracked as GitHub issues. Create an issue and provide needed information by filling in [the template](.github/ISSUE_TEMPLATE/issue.md).

Explain the problem and include additional details to help maintainers reproduce the problem:

* **Use a clear and descriptive title** for the issue to identify the problem.
* **Describe the exact steps which reproduce the problem** in as many details as possible. For example, start by explaining how you initialize the library in your code, e.g., which command exactly you used in in your script, or whether you modified the library code or not. Give also details on what your goal is, what you want to achieve by using the library.
* **Provide specific examples to demonstrate the steps**. Include links to files or GitHub projects, or copy/pasteable snippets, or even minimal working examples (MWEs) to reproduce the problem. Especially, include your project and network configurations so that we can understand your setting.
* **Describe the behavior you observed after following the steps** and point out what exactly is the problem with that behavior.
* **Explain which behavior you expected to see instead and why.**
* **If you're reporting that the library's code crashed somehow**, include the R stack trace if available.

Provide more context by answering these questions:

* **Did the problem start happening recently** (e.g., after updating to a new version of the library) or was this always a problem?
* If the problem started happening recently, **can you reproduce the problem in an older version?** What's the most recent version in which the problem doesn't happen?
* **Can you reliably reproduce the issue?** If not, provide details about how often the problem happens and under which conditions it normally happens.

Include details about your configuration and environment:

* **Which version of the library are you using?** You can get the exact version by running `git rev-parse HEAD` and `git rev-parse --abbrev-ref HEAD` in your terminal.
* **What's the name and version of the OS you're using**?
* **Are you running the library in a virtual machine?** If so, which VM software are you using and which operating systems and versions are used for the host and the guest?
* **Which R packages do you have installed and do you use [packrat](http://rstudio.github.io/packrat/)?**

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion, including completely new features and minor improvements to existing functionality. Following these guidelines helps maintainers and the community understand your suggestion and find related suggestions.

Before creating enhancement suggestions, please check [this list](#before-submitting-an-enhancement-suggestion) as you might find out that you don't need to create one. When you are creating an enhancement suggestion, please [include as many details as possible](#how-do-i-submit-a-good-enhancement-suggestion). Fill in [the template](.github/ISSUE_TEMPLATE/enhancement.md), including the steps that you imagine you would take if the feature you're requesting existed.

#### Before Submitting An Enhancement Suggestion

* **Check the code if the functionality exists already somewhere.**
* **Search for previous suggestions describing the same thing.**
  If an old issue already suggests the same enhancement, you do not need to file a new issue. Although, if the problem still persists after applying potential fixes, please file a new issue including detailed information to reproduce the problem. If there is an old issue that is still open, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A (Good) Enhancement Suggestion?

Enhancements are tracked as GitHub issues, but slightly different information is needed for them.

Create an issue and provide the needed information:

* **Use a clear and descriptive title** for the issue to identify the suggestion.
* If possible, **provide a step-by-step description of the suggested enhancement** in as many details as possible.
* **Provide specific examples to demonstrate the enhancement**. Include copy/pasteable snippets which you use in those examples.
* **Describe the current behavior** and **explain which behavior you want to see instead** and why.
* **Explain why this enhancement would be useful** to all users of the library and isn't something that can or should be implemented as a functionality in your code.
* **Which version of the library are you using?** You can get the exact version by running `git rev-parse HEAD` and `git rev-parse --abbrev-ref HEAD` in your terminal.

### Your First Code Contribution

#### Installation

Install the library using the guide in our [README](README.md) file. Start implementing your code additions. Please keep the [style conventions](#style-conventions) in mind when implementing.

#### Our branching and tagging system

In our development process, we pursue the following idea:
- Each version (i.e., a tag) contains, at least, a major and a minor version in the form `v{major}.{minor}[.{bugfix}]`.
- The branch `master` should always contain the most recent and complete version.
- Usually, we release a new version (i.e., a tag) after bugfixes to the most recent version (e.g., when fixing some bugs after release `v3.1`, we introduce a new version `v3.1.1` containing the bugfixes.
- When fixing some extreme bugs, we maintain a stable branch `v{major}.{minor}-fixes` (e.g., `v2.0-fixes`) for the second last major version containing backported fixes for this particular version (e.g., when we fix a extreme bug in version `v3.1` by introducing version `v3.1.1`, we might backport the fix to major version 2 by maintaining a branch `v2.x-fixes` where `x` denotes the last minor version belonging to major version 2.)
- The current development will be performed on the branch `dev`, i.e., all incoming pull requests are against this branch.

The current build status is as follows:
- `master`: [![Build Status](https://cloud.drone.io/api/badges/se-sic/coronet/status.svg)](https://cloud.drone.io/se-sic/coronet)
- `dev`: [![Build Status](https://cloud.drone.io/api/badges/se-sic/coronet/status.svg?ref=refs/heads/dev)](https://cloud.drone.io/se-sic/coronet)

### Pull Requests

* Do not include issue numbers in the PR title, but rather in the PR message body.
* Base your changes on the current `dev` branch and open your PR against the `dev` branch.
* Follow the [style conventions](#style-conventions).
* Document new code and provide proper tests (for a detailed description of the test projects see the [test-README](tests/README.md)).
* Add your enhancements and fixes to the [changelog file](NEWS.md).
* Add yourself to the copyright headers in the files you changed/added.
* Check whether the showcase file `showcase.R` needs to be adjusted to your changes. If so, update the showcase file accordingly.
* End all files with a newline.
* Merge policy:
  * Tests must run successfully.
  * Code must be reviewed by one other project member and, if needed, be properly adapted/fixed.
  * We add the `Reviewed-by` tag only for the merge commit.

There will be another checklist for you when you open an actual pull request provided by [the corresponding template](.github/PULL_REQUEST_TEMPLATE.md).

## Style Conventions

### Commit Messages

* Most importantly, **[sign off](https://developercertificate.org/) your changes.** This is required for legal reasons, and to give correct attribution to contributions.
* **Structure one logical change into a single commit.** The change can (and will often) encompass changes to multiple files.
* Use **present tense for commit messages** ("Add feature", not: "Added feature").
* **Break the commit message** at 80 characters at maximum. Limit the first line to 72 characters or less.
* The **commit message is composed of multiple parts**:
    * The first line contains a concise summary of the changes in a single line.
    * Then follows one empty line.
    * Then follows a longer description of the change.
    * Then follows an empty line.
    * Then follows the signed-off tags.
* Configure git to use your **complete name**, not your username.
* **Do not commit incremental development**, that is, half-baked, really not working code. Please work until a change is complete, and then make one commit. Naturally, you will need to fix bugs in later commits, this is no problem. This means also that you do not need to squash all your changes to address an issue into one single commit.
* When you introduce new files, please make sure they have a correct copyright headers and stick to the current naming scheme.
* Reference GitHub issues by adding their identifier (e.g., #11) in the commit message.
* Here's an example commit message in the expected form:
  ```
  Add functions 'get.author2mail' and 'get.author2thread'

  Add the functions 'get.author2mail' and 'get.author2thread' to
  CodefaceProjectData in order to allow similar data access for mail data
  as for commit data.

  This fixes #44.

  Signed-off-by: Thomas Bock <bockthom@fim.uni-passau.de>
  Reviewed-by: Claus Hunsen <hunsen@fim.uni-passau.de>
  ```

### R Coding Conventions

* Use [Google's style guide for R](https://google.github.io/styleguide/Rguide.xml) as a start. Recently, Google has, unfortunately, changed its naming conventions, but coronet still uses the previously established naming conventions, which can be found [here](https://github.com/google/styleguide/blob/505ba68c74eb97e6966f60907ce893001bedc706/Rguide.xml).
* Additionally or, rather, in contrast, we settle on the following style:
    * **indentation width**: 4 spaces,
    * **line length**: more than 80 characters are allowed, 120 characters should be the maximum,
    * **identifiers**: substrings in identifiers are separated only with `.` (and *not* camel-casing or `_`),
    * **assignments**: only with `=`,
    * **spacing**: spaces around all binary operators (`=`, `+`, `-`, etc.); spaces before left parenthesis, except in a function call; spaces before opening curly braces that denote a code block,
    * **quoting** (e.g., of strings): always use double quotes instead of single quotes,
    * **comments**: comments on the same line as source code start with a single hash sign (`#`), while comments on an own line start with two hash signs (`##`),
    * **Booleans**: always write `TRUE` instead of `T` (analogously for `FALSE` and `F`),
    * **square-brackets notation**: always access values in lists or data.frame using the square-brackets notation (e.g., `df["column1"]` or `list1[["item1"]]`, and
    * **return statements**: always use the function `return()` to return a value from a function; do not use one when you intend to not return anything.
* To **include packages**, always use `requireNamespace` (and *not* `library`). Consequently, you need to use package prefixes (e.g., `igraph::`) when calling a package's functions.
* To handle dates and date objects (i.e. mainly, `POSIXct` objects in R), always use the functions `get.date.from.string`, `get.date.from.unix.timestamp`, and `get.date.string`. This ensures proper date handling in all parts of the code. If you need to modify dates (e.g., to add a week), always use the `lubridate` package.
* **Logging** is performed using the `logging` package.
* **Documentation**:
    * We use **networks**, not "graphs".
    * We use **vertices**, not "nodes".
    * We talk about **authors**, not "developers".
    * Function, method, and class documentation is defined in **`roxygen2`** format (see [RStudio documentation](https://support.rstudio.com/hc/en-us/articles/200532317-Writing-Package-Documentation) for details). In particular, we settle on the following style of the **`roxygen2`** documentation (see example below):
        * The introductory sentence(s) which describes a function, method, or class, in general, must always start with an upper-case letter and end with a period. Use 2nd-person imperative ("Get the label.") rather than 3rd-person declarative ("Gets the label.").
        * Variable names, class names, or other code snippets which are part of the function documentation must be placed within `\code{...}`.
          **Note:** This does not hold for inline documentation within a function. There we only use `'...'` for variable names, class names, and other code snippets.
        * Class documentation must include a basic, but abstract description of its functionality and the data it holds. If the class inherits from another class, the superclass must be mentioned explicitly in the description and also using a referenced (`@seealso SuperClassName`). The introductory sentence should be phrased something like this: `The class \code{X} (is a subclass of \code{SuperClass} and) provides ...`.
        * For each parameter of a function or method, a parameter description (`@param parameter.name description`) must be added. The same holds for descriptions of return values (`@return description`) – if something is returned. For both items, the descriptions should always be a phrase (i.e., no verbs should be used), should start with a lower-case letter, and should *not* end with a period – *unless* there are further phrases or sentences following. If the description covers multiple lines, all but the first lines are indented along the start of the description in the first line.
        * For parameters that take a default value, the default value must be placed directly after the description of the parameter using the following format: `[default: value]`.
          **Note:** As `value` usually is code, we omit the `\code{...}` notation within `[default: ...]`.
        * Add references to notable other classes, methods, constants, and functions using `@seealso reference`. As `reference` usually is code, we omit the `\code{...}` notation for it.
        * Example for a function/method documentation:
          ```R
          #' Group the data items of the given \code{data.source} by the given \code{group.column}.
          #'
          #' For each group, the column \code{data.column} is duplicated and prepended to each group's
          #' data as first column (see \code{get.key.to.value.from.df} for details).
          #'
          #' Example: To obtain the authors who touched the same source-code artifact,
          #' call \code{group.data.by.column("commits", "artifact", "author.name")}.
          #'
          #' @param data.source the specified data source, one of \code{"commits"},
          #'                    \code{"mails"}, and \code{"issues"} [default: "commits"]
          #' @param group.column the column to group the data of the given \code{data.source} by
          #' @param data.column the column that gets duplicated as first column \code{data.vertices}
          #'
          #' @return a list mapping each distinct item in \code{group.column} to all corresponding
          #'         data items from \code{data.source}, with \code{data.column} duplicated as first
          #'         column (with name \code{"data.vertices"})
          #'
          #' @seealso get.key.to.value.from.df
          group.data.by.column = function(data.source = c("commits", "mails", "issues"),
                                          group.column, data.column) {
            logging::loginfo("Grouping artifacts by data column.")

            ## check given data source given by 'data.source'
            data.source = match.arg.or.default(data.source, several.ok = FALSE)
            data.source.func = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]

            ## get the key-value mapping/list for the given parameters
            mylist = get.key.to.value.from.df(self[[data.source.func]](), group.column, data.column)

            return(mylist)
          }
          ```
    * For all other stuff, look at the source code and get inspired.
* Also **add tests** to the test suite for each new functionality you add!
* Keep the code as simple as possible. So, for example, no complex computation inside the `return` statement.


## Disclaimer

This file is heavily based on the analogous file from the [Atom project](https://github.com/atom/atom), along with refinements from the [Codeface project](https://github.com/siemens/codeface).
