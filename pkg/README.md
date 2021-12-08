# CRAN Task Views <img src="man/figures/logo_alpha.png" align="right" alt="" width="180px" style="padding: 0 0 0 0.5em;" />

CRAN task views aim to provide some guidance which packages on the [Comprehensive
R Archive Network (CRAN)](https://CRAN.R-project.org/) are relevant for tasks
related to a certain topic. They give a brief overview of the included packages
and can be automatically installed using the
[ctv](https://CRAN.R-project.org/package=ctv) package. The views are intended to
have a sharp focus so that it is sufficiently clear which packages should be
included (or excluded) - and they are not meant to endorse the "best" packages
for a given task.

To automatically install the views, the [ctv](https://CRAN.R-project.org/package=ctv)
package needs to be installed, e.g., via

```
install.packages("ctv")
```

and then the views can be installed via `install.views()` or `update.views()`
(where the latter only installs those packages which are not installed and up-to-date),
e.g.,

```
ctv::install.views("Econometrics")
ctv::update.views("Econometrics")
```

The task views as a whole are overseen and coordinated by the _CRAN Task View
Editors_ and each individual task view is maintained by a group of volunteers.
See the repository of the
[CRAN Task View Initiative](https://github.com/cran-task-views/ctv/)
for details on how to contribute to an existing task view or propose a new one.
