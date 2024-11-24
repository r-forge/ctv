# ctv 0.9-6

* Add header information about canonical CRAN URLs for all task views as
  `<link rel="canonical" href="https://CRAN.R-project.org/view=..." />`.


# ctv 0.9-5

* New `citation("ctv")`: Zeileis A, Bivand R, Eddelbuettel D, Hornik K, Vialaneix N (2023).
  "CRAN Task Views: The Next Generation." arXiv 2305.17573, _arXiv.org E-Print Archive_.
  [doi:10.48550/arXiv.2305.17573](https://doi.org/10.48550/arXiv.2305.17573).

* For CRAN web pages produce `<span class="...">pkgname</pkg>` for packages on
  CRAN, Bioconductor, GitHub, R-Forge, OmegaHat, etc., where the custom classes
  yield custom colors in the CRAN style.

* Updated Bioconductor URLs from `bioc("<pkgname>")` tags to
  `https://www.bioconductor.org/packages/<pkgname>`.


# ctv 0.9-4

* Improved and extended documentation for CRAN task view maintainers in
  `vignette("ctv-howto", package = "ctv")`.

* CRAN information about packages and archivals is loaded from local files
  (rather than downloaded) if run on CRAN. Also, it is preserved between subsequent
  runs of `read.ctv(..., cran = TRUE)` (and hence also `ctv2html(..., cran = TRUE)`
  etc.).

* `read.ctv(..., cran = TRUE)` now warns if a `pkg()` is not available from CRAN.

* Add Facebook OpenGraph and Twitter tags in task view HTML pages.

* New convenience function `ctv()` that obtains a single `ctv` object
  with information about a certain task view on CRAN with a given name.

* New list element `$citation` in `ctv` objects containing a citation object
  inheriting from `bibentry` with information on how to cite a task view.

* The auxiliary `view()` function gained a second argument so that specific
  sections in other task views can be referenced, e.g.,
  `view("Econometrics", "Instrumental variables")`.

* Fix typo in task view header it should be `install.views()` and `update.views()`
  rather than `install.packages()` and `update.packages()`.

* Fix detection of "core" packages if the `priority` is not declared in the
  first `pkg()` call but in subsequent calls.

* Improve HTML5 output from `ctv2html()`.


# ctv 0.9-3

* Enhance handling of archived packages on CRAN when `read.ctv(..., cran = TRUE)`
  and `ctv2html(..., cran = TRUE)`.

* Further improvements in `ctv2html()`: Generate HTML5 with `pandoc` now, improve
  preservation of link breaks (`--wrap=preserve`).


# ctv 0.9-2

* Bug fix in `read.ctv()` when `pkg(..., priority = "core")` is used more than
  once on the same package name (reported by Rocio Joo).
  
* Sorting the package list is done ignoring the case now.

* Various improvements in `ctv2html()`: Include explicit citation in header table,
  include instructions for contributions and installation (if `cran = TRUE`), more
  compact layout of package list, handle task views without links section correctly.


# ctv 0.9-1

* Bug fix in `read.ctv(file)` when `file` is actually a full path and not just
  the name of file in the local working directory.

* Make all examples of `read.ctv()` conditional on `knitr` and `rmarkdown`
  being available (or `xml2` being available in case of the legacy XML format).


# ctv 0.9-0

* Various substantial changes to support the launch of the
  [CRAN Task View Initiative](https://github.com/cran-task-views/ctv/):
  Development and maintenance of CRAN task views will be overseen by a team
  of CRAN Task View Editors who review and approve new task views and help
  maintainers with onbaording. Infrastructure for this is moved from R-Forge
  to GitHub (with only the maintenance of the `ctv` package remaining on R-Forge).
  The file format for task view files changes from the old XML-based format
  to a new leaner R-Markdown based format.

* For the R package `ctv` itself there is now a pkgdown web page at:
  <https://ctv.R-Forge.R-project.org/>.

* The `vignette("ctv-howto", package = "ctv")` has been completely rewritten,
  now describing the new R/Markdown-based format. (The vignette itself is now
  in HTML rather than in PDF format.)

* For processing the legacy XML-based files the `xml2` package is used now
  (instead of the `XML` package).

* Added `<doi>` tag in the legacy XML format to add hyperlinks to DOIs, e.g.,
  `<doi>10.18637/jss.v067.i01</doi>` will be turned into
  <https://doi.org/10.18637/jss.v067.i01>.

* The `<gcode>` tag now explicitly resolves to <https://code.google.com/archive/>
  (including the `archive/` part) as Google Code has been archived in 2016.


# ctv 0.8-5

* The ctv-server tools now generate a somewhat more verbose index.html page
  that explains what task views are and how they could be extended.


# ctv 0.8-4

* `<github>` tag can now be used to refer to projects on GitHub using markup like
  `<github>user/project</github>`.

* To support older versions of R (up to R 3.4.3), the server-side tools now
  explicitly use `saveRDS(..., version = 2)` to store task view information.


# ctv 0.8-3

* `<ohat>` packages are now at Omegahat.net rather than Omegahat.org.
  

# ctv 0.8-2

* There is a new optional tag `<url>` that can be used to display an official
  task view URL (and insert it into the metainformation tags). For CRAN task
  views the official URL is used by default.


# ctv 0.8-1

* Bug fix in `check_ctv_packages()`: Packages in `<packagelist>` but not in
  `<info>` were not computed correctly.


# ctv 0.8-0

* HTML generation improved. Now XHTML 1.0 is produced with both DublinCore and
  HighWire Press metainformation tags.


# ctv 0.7-9

* `<br/>` tags are preserved as such in the `<info>` section.


# ctv 0.7-8

* Use `system.file()` instead of `.find.package()` in vignette.


# ctv 0.7-7

* Use `system.file()` instead of `.find.package()` in `?read.ctv`.


# ctv 0.7-6

* New function `check_ctv_packages()` for maintainers of CRAN task views.
  This checks whether `<info>` and `<packagelist>` are consistent and whether
  all packages are actually available in the repository.
  

# ctv 0.7-5

* Package depends on R >= 2.13.0 now in order to use only
  `saveRDS()`/`readRDS()` and not `.saveRDS()`/`.readRDS()`.
  

# ctv 0.7-4

* `update.views()` gains a `filter` argument passed to `available.packages()`,
  so that the packages can be filtered with respect to operating system type or
  free and open-source license etc.
  

# ctv 0.7-3

* `saveRDS()` instead of `.saveRDS()` is now used in the server-side tools if
  available.
  

# ctv 0.7-2

* Avoid usage of `&#8810;` in `Cluster.ctv` because it is not processed
  correctly on all platforms

* Further improvement to `available.views()` which now uses `readRDS()` if
  available.
  

# ctv 0.7-1

* Fixed `available.views()` for R 2.13.0 as the underlying `.readRDS()` needed
  to be changed.
  

# ctv 0.7-0

* Added `download.views()` function (as suggested by Peter Ruckdeschel).
  
* Restructured internals of `install.views()` and `update.views()` so that all
  three functions can use the same code for determining the list of packages
  that is going to be installed/updated/downloaded.

  
# ctv 0.6-0

* `update.views()` was updated reflecting that bundles are not supported
  anymore.
  

# ctv 0.5-6

* Modified code to avoid non-standard evaluation in `subset()`.
  

# ctv 0.5-5

* Simplified printing of warnings (with `useFancyQuotes = FALSE`).
  

# ctv 0.5-4

* `grep(..., extended = FALSE)` replaced by the more appropriate
  `grep(..., fixed = TRUE)`.
  

# ctv 0.5-3

* The R-Forge links created from `<rforge>` are now forced to be in lower case.
  

# ctv 0.5-2

* Minor changes to improve generation of tidy HTML.
  

# ctv 0.5-1

* In .ctv files a new tag is allowed: `<ohat>` for links to Omegahat projects.
  
* The tags `<rforge>` and `<gcode>` replace the previous `<forge>` and
  `<googlecode>`, respectively, in order to be more consistent with the
  terminology used in the CRAN Web pages.

* Further improvements of server-side tools in order to provide tidy XHTML code.

* The default style file included in the server-side tools is now `CRAN_web.css`
  (rather than `R.css`).


# ctv 0.5-0

* Improved server-side tools: `read.ctv()` and `ctv2html()` now try harder to
  provide tidy XHTML code.


# ctv 0.4-9

* Added `CITATION` file.


# ctv 0.4-8

* New `<googlecode>` tag.


# ctv 0.4-7

* Temoved `\itemize` in .Rd files for new R-devel.


# ctv 0.4-6

* Changed interface of `install.views()` and `update.views()`: Argument
  `dependencies = TRUE` has been removed (but can still be specified through
  `...`). Hence, by default, the defaults of `install.packages()` are inherited.
  However, it is still possible to get the prior behaviour by setting
  dependencies explicitly in `install.views()`/`update.views()`.


# ctv 0.4-5

* Enhanced support for R-Forge and Bioconductor links (new `<bioc>` tag,
  similar to `<forge>`).


# ctv 0.4-4

* Enhanced UTF-8 support.


# ctv 0.4-3

* Enabled UTF-8 support for character fields in CTV files.


# ctv 0.4-2

* Updated vignette to better reflect the latest changes.
  

# ctv 0.4-1

* Added new tags `<code>` and `<forge>` for CTV files.


# ctv 0.4-0

* New function update.views() which only installs the packages from a view that
  are not installed or not up-to-date.

* Added infrastructure for view version/date and contact e-mail address of the
  maintainer.
  
* Adapted the server-side tools to the new web-structure of CRAN.
