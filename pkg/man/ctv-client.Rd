\name{ctv-client}
\alias{install.views}
\alias{CRAN.views}
\alias{available.views}
\alias{download.views}
\alias{update.views}
\alias{print.ctv}
\alias{print.ctvlist}
\title{CRAN Task Views: Client-side Tools}
\description{
Client-side tools for installing CRAN task views.
}

\usage{
available.views(repos = NULL, \dots)
install.views(views, coreOnly = FALSE, repos = NULL, \dots)
update.views(views, coreOnly = FALSE, repos = NULL, lib.loc = NULL, \dots)
download.views(views, destdir, coreOnly = FALSE, repos = NULL, \dots)

\method{print}{ctv}(x, packagelist = TRUE, \dots)
\method{print}{ctvlist}(x, packagelist = FALSE, \dots)
}
\arguments{
  \item{views}{character vector with the short names of the task views whose
    associated packages should be downloaded and installed. Alternatively,
    \code{views} can also be an object of class \code{"ctvlist"} (as returned
    by \code{available.views}) or an object of class \code{"ctv"} (i.e., an element
    of a \code{"ctvlist"}).}
  \item{coreOnly}{logical. Should all packages or only core packages be installed?
    (recycled to the same length as \code{views})}
  \item{repos}{character, the base URL of the repository. By default \code{getOption("repos")}
    is tried and otherwise \code{getOption("CRAN")} is used.}
  \item{lib.loc}{character vector describing the location of R library trees
    to search through (and update packages therein).}
  \item{destdir}{directory where downloaded packages are to be stored.}
  \item{\dots}{further arguments passed to \code{\link{install.packages}} or
    \code{\link{download.packages}} respectively.}
  \item{x}{an object of class \code{"ctv"} or \code{"ctvlist"} respectively.}
  \item{packagelist}{logical. Should the packagelist also be printed?}
}

\details{
\code{install.views} queries the file \file{Views.rds} located at
the \file{src/contrib} directory of \file{repos} and then simply calls
\code{\link{install.packages}} to install the packages associated with the
view specified. For each view it can be specified whether all packages or
only the core packages should be installed.

\code{available.views} returns the names of the task views currently
available in the file \file{Views.rds}. In earlier versions, this was called
\code{CRAN.views} (which still works and provides the same functionality).

\code{update.views} queries which packages from a view are not yet
installed (using \code{\link{installed.packages}}) and which of the installed
packages are older than the packages available (using \code{\link{available.packages}}).
It subsequently installs only the packages that are not current or not
installed yet.

\code{download.views} works exactly as \code{install.views} except that it
\code{\link{download.packages}} instead of \code{install.packages}.

For a closer description of the arguments see also \code{\link{install.packages}}.
}

\value{
  \code{available.views} returns an object of class \code{"ctvlist"} of the 
  available task views whose elements are of class \code{"ctv"}.
  
  \code{install.views} and \code{update.views} have no return value.
}

\seealso{\code{\link{install.packages}}}

\examples{
\dontrun{
  ## query names of CRAN task views available
  available.views()
  
  ## install Econometrics view
  install.views("Econometrics")
  ## only with core packages
  install.views("Econometrics", coreOnly = TRUE)

  ## update Econometrics view
  update.views("Econometrics")
}
}
\keyword{utilities}
