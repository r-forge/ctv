\name{ctv-server}
\alias{read.ctv}
\alias{ctv2html}
\alias{updateViews}
\title{CRAN Task Views: Server-side Tools}
\description{
Server-side tools for maintaining CRAN task views.
}

\usage{
updateViews(repos = ".", css = "../../../R.css", \dots)

read.ctv(file)
ctv2html(x, file = NULL, css = "../../../R.css", packageURL = "../Descriptions/")
}
\arguments{
  \item{repos}{character, the base URL of the repository where the \file{Views.rds}
    and \file{.html} files should be installed. The \file{.ctv} files should
    be located in the \file{src/contrib/Views} directory.}
  \item{css}{character specifying the path and name of the cascade style
    sheet that should be included in the HTML files.}
  \item{\dots}{further arguments passed to \code{ctv2html}.}
  \item{file}{character specifying a CRAN task view \file{.ctv} file.}
  \item{x}{an object of class \code{"ctv"} as returned by \code{read.ctv}.}
  \item{packageURL}{character specifying the path (relative to the view directory)
    to the package descriptions.}
}

\details{
CRAN Task views are generated from an XML-based format \file{.ctv} that is described in
the vignette of this package.

\code{read.ctv} can read a \file{.ctv} file with a CRAN task view specification
and returns an object of class \code{"ctv"}. This functions requires the availability
of the \pkg{XML} package.

\code{ctv2html} generates a \file{.html} file with the information contained in
a \code{"ctv"} object.

\code{updateViews} reads all \file{.ctv} files in a specified directory, generates
a \file{.html} file for each and an index \file{.html} file. Furthermore, it stores
all \code{"ctv"} objects in a \code{"ctvlist"} object in a file \file{Views.rds} that can be queried
by \code{\link{install.views}} or \code{\link{CRAN.views}}.
}

\value{
  \code{updateViews} returns an object of class \code{"ctvlist"} containing
  the \code{"ctv"} objects available.
  
  \code{ctv2html} returns invisibly a vector with the HTML code generated.
  
  \code{read.ctv} returns a list of class \code{"ctv"} with slots:
  \itemize
  \item{name}{character, name of the task view (must be a valid name for an R object).}
  \item{topic}{character, topic of the task view.}
  \item{maintainer}{character, maintainer of the task view.}
  \item{info}{character, HTML code with informations about the task view.}
  \item{packagelist}{data frame with the columns \code{name}
    (character, name of package) and \code{core} (logical, Is priority core?).}
  \item{links}{character vector, HTML code with links for the task view.}
}

\seealso{\code{\link{install.views}}}

\examples{
## read .ctv file
x <- read.ctv(file.path(.find.package("ctv"), "ctv", "Econometrics.ctv"))
x

\dontrun{
## generate corresponding .html file
ctv2html(x)
}
}

\keyword{utilities}
