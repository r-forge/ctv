\name{ctv-server}
\alias{read.ctv}
\alias{ctv2html}
\alias{check_ctv_packages}
\alias{repos_update_views}
\title{CRAN Task Views: Tools for Maintainers}
\description{
Server-side tools for maintaining CRAN task views.
}

\usage{
read.ctv(file)

ctv2html(x, file = NULL, css = "../CRAN_web.css",
  packageURL = "../packages/", reposname = "CRAN")

check_ctv_packages(file, repos = TRUE, \dots)

repos_update_views(repos = ".", css = "../CRAN_web.css", reposname = "CRAN", \dots)
}
\arguments{
  \item{file}{character specifying a file path: for \code{read.ctv} a CRAN
    task view \file{.ctv} file, and for \code{ctv2html} the corresponding
    output \file{.html} file.}
  \item{x}{an object of class \code{"ctv"} as returned by \code{read.ctv}.}
  \item{css}{character specifying the path and name of the cascade style
    sheet that should be included in the HTML files.}
  \item{packageURL}{character specifying the path (relative to the view directory)
    to the package descriptions.}
  \item{reposname}{character giving the name of the CRAN-style repository,
    used for generating HTML pages.}
  \item{repos}{character, the base URL of the CRAN-style repository where the \file{Views.rds}
    and \file{.html} files should be installed. The \file{.ctv} files should
    be located in the \file{web/views/} directory.}
  \item{\dots}{further arguments passed to \code{available.packages} or
    \code{ctv2html}, respectively.}
}

\details{
CRAN Task views are generated from an XML-based format \file{.ctv} that is described in
the vignette of this package.

\code{read.ctv} can read a \file{.ctv} file with a CRAN task view specification
and returns an object of class \code{"ctv"}. This functions requires the availability
of the \pkg{XML} package.

\code{ctv2html} generates a \file{.html} file with the information contained in
a \code{"ctv"} object.

\code{check_ctv_packages} checks whether the info and packagelist sections of the
\file{.ctv} file are consistent with each other and whether all packages are available
from the repository.

\code{repos_update_views} reads all \file{.ctv} files in a specified directory, generates
a \file{.html} file for each and an index \file{.html} file. Furthermore, it stores
all \code{"ctv"} objects in a \code{"ctvlist"} object in a file \file{Views.rds} that can be queried
by \code{\link{install.views}} or \code{\link{CRAN.views}}.
}

\value{
  \code{updateViews} returns an object of class \code{"ctvlist"} containing
  the \code{"ctv"} objects available.
  
  \code{ctv2html} returns invisibly a vector with the HTML code generated.
  
  \code{read.ctv} returns a list of class \code{"ctv"} with elements:
  \item{name}{character, name of the task view (must be a valid name for an R object).}
  \item{topic}{character, topic of the task view.}
  \item{maintainer}{character, maintainer of the task view.}
  \item{email}{character, valid e-mail address (optional).}
  \item{version}{character, version specified via date in ISO format.}
  \item{url}{character, valid task view URL (optional).}
  \item{info}{character, HTML code with informations about the task view.}
  \item{packagelist}{data frame with the columns \code{name}
    (character, name of package) and \code{core} (logical, Is priority core?).}
  \item{links}{character vector, HTML code with links for the task view.}
}

\references{
Zeileis A (2005). CRAN Task Views.
  \emph{R News}, \bold{5}(1), 39--40.
  \url{https://CRAN.R-project.org/doc/Rnews/}.
}

\seealso{\code{\link{install.views}}}

\examples{
## read .ctv file
x <- read.ctv(system.file("ctv", "Econometrics.ctv", package = "ctv"))
x

\dontrun{
## generate corresponding .html file
ctv2html(x)

## check packagelist
check_ctv_packages(x)
}
}

\keyword{utilities}
