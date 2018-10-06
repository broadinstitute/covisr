# Contributing to covisr
First of all, thanks for taking your time to contribute!
You could contribute to this project by:

Filing a bug report or feature request in an issue.
Suggesting a change via a pull request.

## Issues
To file an issue about a possible bug, please try to include:

* Relevant package versions
* Necessary code and data to reproduce the issue

## Pull Requests
To suggest a change via pull requests, please:

1. Fork the repository into your GitHub account.
2. Clone the forked repository to local machine, make the changes.
3. Commit and push the changes to GitHub. Create a pull request.

## Style guide
Please try to follow the coding style in this repo:
1. One line should be no longer than 80 characters, you can get ideas of how to introduce newlines from Python's [PEP8](https://pep8.readthedocs.io) standard.
2. For each function, include documentation of the function and input variables with [R-oxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html) compatible format.
3. One function should not be longer than 30 lines (otherwise create two functions).
4. Use `CamelCase` rather than `under_score`. Use meaningful variable names, e.g.: `chrVec` rather than `vec1`. I found it helpful to include the type of variables as suffix of the variable. For example, `chrVec` is better than `chr`.
5. When using external functions, show the origin of the function. For example, use `hash::hash()` rather than `hash()`.
