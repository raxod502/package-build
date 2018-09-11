# DEPRECATED: pbl

> `package-build` *lite*

This project was formerly a dependency of my Emacs Lisp package
manager, [`straight.el`][straight.el]. It is a fork of the
[`package-build`][package-build] project by the [MELPA] team, from
which I stripped out all unnecessary functions, leaving just a few
routines that `straight.el` used to clone repositories and to parse
[MELPA-style][recipes] `:file` specifications.

Since MELPA and `package-build` are licensed under the GNU Public
License, and `straight.el` is licensed under the MIT License, it was
impossible to include any code from `package-build` directly in
`straight.el`, unfortunately. This is why pbl was maintained in a
separate repository; I included it in `straight.el` as a Git
submodule.

Eventually, this grew tiresome, and furthermore the features provided
by `package-build` proved inadequate for the needs of `straight.el`,
so I replaced all the functions with clean-room implementations that
are included directly in `straight.el`. This repository no longer has
a purpose except to ensure that versions of `straight.el` prior to
June 2017 can still be checked out. (I'm pretty darned serious about
backwards compatibility for `straight.el`.)

[melpa]: https://github.com/melpa/melpa
[package-build]: https://github.com/melpa/package-build
[recipes]: https://github.com/melpa/melpa#recipe-format
[straight.el]: https://github.com/raxod502/straight.el
