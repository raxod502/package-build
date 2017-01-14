# pbl

> `package-build` *lite*

## What is all this stuff

* `package.el` is the package manager that ships with Emacs. It allows
  you to easily download the code for Emacs packages and add them to
  your Emacs.

* `melpa` is a collection of formulas detailing where to find the
  source code for various Emacs packages, as well as a collection of
  scripts to build that source code into a format compatible with
  `package.el`. Users of the packages only need to use `package.el`,
  because `melpa` can be run on a dedicated server.

* `package-build` is a sub-library of `melpa` that handles actually
  retrieving the source code of packages and building them into the
  `package.el`-compatible format.

* `quelpa` is a modified version of `melpa` that allows users to
  download and build the source code of Emacs packages locally,
  instead of having it done for them on a server.

* `straight.el` is a new package manager intended to replace
  `package.el`, `melpa`, and `quelpa` by providing all of their
  functionality in a simpler, more flexible way.

* `pbl` is a modified version of `package-build` that contains only
  the functions needed by `straight.el`.

## Why is it here

It would be simpler to put these functions directly into
`straight.el`. However, `package-build` is GPL-licensed and
`straight.el` is MIT-licensed, so I have no choice but to keep them
separate. Sorry! You can [complain about it] if you want.

[complain about it]: https://github.com/melpa/melpa/issues
