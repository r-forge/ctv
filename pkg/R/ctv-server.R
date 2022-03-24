read.ctv <- function(file, ...)
{
  x <- readLines(file)
  if(any(grepl("<CRANTaskView", x, fixed = TRUE))) return(read_ctv_xml(file))
  if(any(grep("^---", x))) return(read_ctv_rmd(file, ...))
  stop("unknown ctv specification")
}

read_ctv_xml <- function(file) 
{
    ## read raw XML
    x <- xml2::read_xml(file)
    ## Old code had
    ##   x <- XML::xmlTreeParse(file)
    ##     if(XML::xmlSize(x$doc$children) > 1) warning("ctv should contain only one view")
    ## but apparently xml2::read_xml() throws an error for repeated top level
    ## entries.
    if(xml2::xml_name(x) != "CRANTaskView")
        stop("ctv should contain only one view")
    
    ## valid task view?
    ctvNames <- c("name", "topic", "maintainer", "version", "info", "packagelist", "links")
    missingChildren <-
        !(ctvNames %in% xml2::xml_name(xml2::xml_children(x)))
    if(any(missingChildren))
        stop("The following ctv nodes are missing: ",
             paste(ctvNames[missingChildren], collapse = ", "))

    xmlCode <- function(x) htmlify(xml2::xml_text(x, trim = TRUE))

    ## convenience function for transforming
    ## XMLNodes into a character vector
    xmlPaste <- function(x, indent = "", prefix = FALSE,
                         packageURL = "../packages/",
                         viewURL = "")
    {

        ## set prefixes
        if(prefix) {    
            viewprefix <- "CRAN Task View: "
            biocprefix <- "Bioconductor Package: "
            ohatprefix <- "Omegahat Package: "
            rforgeprefix <- "R-Forge Project: "
            gcodeprefix <- "Google Code Project: "
            githubprefix <- "GitHub Project: "
            target <- "" #used to be# " target=\"_top\"" #but this is not strict XHTML#
        } else {
            viewprefix <- ""
            biocprefix <- ""
            ohatprefix <- ""
            rforgeprefix <- ""
            gcodeprefix <- ""
            githubprefix <- ""
            target <- ""
        }

        ## get tag name
        name <- xml2::xml_name(x, xml2::xml_ns(x))

        ## if final node, return text
        if(name == "text") {
            s <- xmlCode(x)
            if(nzchar(s))
                return(paste0(indent, s))
            else
                return(NULL)
        }

        ## if pkg or view, return link
        if(name == "pkg")
            return(paste0("<a href=\"",
                          packageURL, xmlCode(x), "/index.html\">", xmlCode(x),
                          "</a>"))
        if(name == "view")
            return(paste0(viewprefix,
                          "<a href=\"",
                          viewURL, xmlCode(x), ".html\">", xmlCode(x),
                          "</a>"))
        if(name == "info")
            name <- "div"
        if(name == "comment")
            return(NULL)
        if(name == "code")
            return(paste0("<tt>", xmlCode(x), "</tt>"))
        if(name == "rforge")
            return(paste0(rforgeprefix,
                          "<a href=\"https://R-Forge.R-project.org/projects/",
                          tolower(xmlCode(x)), "/\"", target,
                          "><span class=\"Rforge\">", xmlCode(x),
                          "</span></a>"))
        if(name == "gcode")
            return(paste0(gcodeprefix,
                          "<a href=\"https://code.google.com/archive/p/",
                          xmlCode(x), "/\"", target,
                          "><span class=\"Gcode\">", xmlCode(x),
                          "</span></a>"))
        if(name == "bioc")
            return(paste0(biocprefix,
                          "<a href=\"https://www.Bioconductor.org/packages/release/bioc/html/",
                          xmlCode(x), ".html\"", target,
                          "><span class=\"BioC\">", xmlCode(x),
                          "</span></a>"))
        if(name == "ohat")
            return(paste0(ohatprefix,
                          "<a href=\"http://www.Omegahat.net/",
                          xmlCode(x), "/\"", target,
                          "><span class=\"Ohat\">", xmlCode(x),
                          "</span></a>", sep = ""))
        if(name == "github")
            return(paste0(githubprefix,
                          "<a href=\"https://github.com/",
                          xmlCode(x), "/\"", target,
                          "><span class=\"GitHub\">", strsplit(xmlCode(x), "/", fixed = TRUE)[[1L]][2L],
                          "</span></a>", sep = ""))
        if(name == "doi")
            return(paste0("<a href=\"https://doi.org/",
                          xmlCode(x), "\"", target,
                          ">doi:", xmlCode(x),
                          "</a>", sep = ""))
        if(name == "br")
            return("<br/>")

        ## get attributes
        tmp <- if(length(att <- xml2::xml_attrs(x)))
                   paste(names(att),
                         paste0("\"", htmlify(att), "\""),
                         sep = "=", collapse = " ")
               else ""

        ## start tag
        rval <- paste0(indent, "<", name, ifelse(tmp != "", " ", ""), tmp, ">")
        ## content
        subIndent <- paste0(indent, "  ")
        for(i in xml2::xml_contents(x)) {
            xmlPaste_i <- xmlPaste(i, indent = subIndent,
                                   packageURL = packageURL,
                                   viewURL = viewURL)
            if(length(xmlPaste_i) && nzchar(xmlPaste_i))
                rval <- paste(rval, xmlPaste_i, sep = "\n")
        }
        ## end tag
        rval <- paste(rval, paste0(indent, "</", name, ">"), sep = "\n")

        return(rval)
    }

    newlineSub <- function(x) {
        ## FIXME: This returns latin1 in a latin1 locale even if
        ## the input was UTF-8
        for(i in c(":", ",", ";", ")", ".", "?", "!"))
            x <- gsub(paste0("\n[ ]*\\", i), i, x)
        gsub("(\n<a", "(<a", x, fixed = TRUE)
    }

    ## extraction functions
    name <- function(x)
        xml2::xml_text(xml2::xml_child(x, "name"), trim = TRUE)
    topic <- function(x)
        xml2::xml_text(xml2::xml_child(x, "topic"), trim = TRUE)
    maintainer <- function(x)
        xml2::xml_text(xml2::xml_child(x, "maintainer"), trim = TRUE)
    email <- function(x)
        xml2::xml_attr(xml2::xml_child(x, "maintainer"), "email")
    ctvversion <- function(x)
        xml2::xml_text(xml2::xml_child(x, "version"), trim = TRUE)
    ctvurl <- function(x) {
        url <- xml2::xml_text(xml2::xml_child(x, "url"))
        if(is.na(url)) url <- NULL
        if(identical(url, "")) url <- NULL
        url
    }
    ctvsource <- function(x) {
        source <- xml2::xml_text(xml2::xml_child(x, "source"))
        if(is.na(source)) source <- NULL
        if(identical(source, "")) source <- NULL
        source
    }
    info <- function(x)
        newlineSub(xmlPaste(xml2::xml_child(x, "info"),
                            indent = "    "))
    package1 <- function(x) {
        rval <- xml2::xml_attr(x, "priority")
        rval <- if(!is.na(rval) && rval == "core") "core" else "normal"
        as.vector(c(xml2::xml_text(x), rval))
    }
    packagelist <- function(x) {
        kids <- xml2::xml_children(xml2::xml_child(x, "packagelist"))
        ## FIXME KH: this can now be simplified and vectorized
        rval <- t(sapply(kids, package1))
        colnames(rval) <- NULL
        rownames(rval) <- NULL
        if(NCOL(rval) < 2L)
            stop("packagelist needs to contain at least one pkg")
        rval <- data.frame(name = I(rval[,1]),
                           core = rval[,2] == "core")
        rval[order(tolower(rval[,1])), ]
    }
    links <- function(x)
        unlist(lapply(xml2::xml_contents(xml2::xml_child(x, "links")),
                      function(z) xmlPaste(z, prefix = TRUE)))

  ## collect nodes and return
  rval <- list(name = name(x),
               topic = topic(x),
	       maintainer = maintainer(x),
	       email = email(x),
	       version = ctvversion(x),
	       url = ctvurl(x),
	       source = ctvsource(x),
	       info = info(x),
	       packagelist = packagelist(x),
	       links = links(x)
               )
  class(rval) <- "ctv"
  return(rval)
}

ctv2html <- function(x,
                     file = NULL,
                     cran = FALSE,
                     css = NULL,
		     packageURL = NULL,
		     reposname = "CRAN")
{
  if(is.character(x)) x <- read.ctv(x, cran = cran)
  if(is.null(file)) file <- paste0(x$name, ".html")

  if(is.null(css) & cran) css <- "../CRAN_web.css"
  if(is.null(x$url) & cran) x$url <- paste0("https://CRAN.R-project.org/view=", x$name)
  if(is.null(packageURL)) {
    packageURL <- if(cran) "../packages/" else "https://CRAN.R-project.org/package=%s"
  }
  if(cran) {
    contrib <- paste(c(
      'Suggestions and improvements for this task view are very welcome and can be made',
      'through issues or pull requests on GitHub or via e-mail to the maintainer address.',
      'For further details see the',
      '<a href="https://github.com/cran-task-views/ctv/blob/main/Contributing.md">Contributing guide</a>.'
    ), collapse = " ")
    inst <- paste(c(
      'The packages from this task view can be installed automatically using the',
      '<a href="../packages/ctv/index.html">ctv</a> package. For example,',
      sprintf('<code>ctv::install.packages("%s", coreOnly = TRUE)</code>', x$name),
      'installs all the core packages or',
      sprintf('<code>ctv::update.packages("%s")</code>', x$name),
      'installs all packages that are not yet installed and up-to-date.',
      'See the <a href="https://github.com/cran-task-views/ctv/">CRAN Task View Initiative</a> for more details.'
    ), collapse = " ")
  } else {
    contrib <- inst <- NULL
  }

  ## auxiliary functions
  ampersSub <- function(x) gsub("&", "&amp;", x)
  obfuscate <- function(x) paste(sprintf("&#x%x;",
    as.integer(sapply(unlist(strsplit(gsub("@", " at ", x), NULL)), charToRaw))), collapse = "")    

  ## utf8 <- any(unlist(sapply(x[sapply(x, is.character)], Encoding)) == "UTF-8")
  for(i in 1:length(x)) if(is.character(x[[i]])) Encoding(x[[i]]) <- "unknown"

  ## create HTML
  ## header
  title <- paste0(reposname, " Task View: ", htmlify(x$topic))

  ## citation
  cit <- sprintf("%s (%s). %s Task View: %s. Version %s.%s",
    htmlify(x$maintainer),
    substr(x$version, 1L, 4L),
    reposname,
    htmlify(x$topic),
    htmlify(x$version),
    if(is.null(x$url)) "" else paste0(" URL ", htmlify(x$url), "."))
  
  htm1 <- c("<!DOCTYPE html>",
            "<html>",
            "<head>",
            paste0("  <title>", title, "</title>"),
            if(!is.null(css)) paste0("  <link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\" />"),
            "  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />",
            sprintf("  <meta name=\"citation_title\" content=\"%s\" />", title),
            sprintf("  <meta name=\"citation_author\" content=\"%s\" />",
                    htmlify(x$maintainer)),
            sprintf("  <meta name=\"citation_publication_date\" content=\"%s\" />",
                    x$version),
            ## See <http://www.monperrus.net/martin/accurate+bibliographic+metadata+and+google+scholar>:
            if(!is.null(x$url))
	    sprintf("  <meta name=\"citation_public_url\" content=\"%s\" />",
                    x$url),
            sprintf("  <meta name=\"DC.title\" content=\"%s\" />", title),
            sprintf("  <meta name=\"DC.creator\" content=\"%s\" />",
                    htmlify(x$maintainer)),
            sprintf("  <meta name=\"DC.issued\" content=\"%s\" />",
                    x$version),
            if(!is.null(x$url))
            sprintf("  <meta name=\"DC.identifier\" content=\"%s\" />",
                    x$url),
            "</head>",
	    "",
	    "<body>",
     paste0("  <h2>", reposname, " Task View: ", htmlify(x$topic), "</h2>"),
     paste0("  <table>"),
     paste0("    <tr><td valign=\"top\"><b>Maintainer:</b></td><td>", htmlify(x$maintainer), "</td></tr>"),
     if(!is.null(x$email)) paste0("    <tr><td valign=\"top\"><b>Contact:</b></td><td>", obfuscate(x$email), "</td></tr>"),
     paste0("    <tr><td valign=\"top\"><b>Version:</b></td><td>", htmlify(x$version), "</td></tr>"),
     if(!is.null(x$url)) paste0("    <tr><td valign=\"top\"><b>URL:</b></td><td><a href=\"", htmlify(x$url), "\">", htmlify(x$url), "</a></td></tr>"),
     if(!is.null(x$source)) paste0("    <tr><td valign=\"top\"><b>Source:</b></td><td><a href=\"", htmlify(x$source), "\">", htmlify(x$source), "</a></td></tr>"),
     if(!is.null(contrib)) paste0("    <tr><td valign=\"top\"><b>Contributions:</b></td><td>", contrib, "</td></tr>"),
     paste0("    <tr><td valign=\"top\"><b>Citation:</b></td><td>", cit, "</td></tr>"),
     if(!is.null(inst)) paste0("    <tr><td valign=\"top\"><b>Installation:</b></td><td>", inst, "</td></tr>"),
            "  </table>")

  ## info section
  htm2 <- x$info

  ## package list
  pkg2html <- if(grepl("%s", packageURL, fixed = TRUE)) {
    function(name,  core = FALSE)
      paste0("<a href=\"", sprintf(packageURL, name), "\">", name, "</a>", if(core) " (core)" else "")
  } else {
    function(name, core = FALSE)
      paste0("<a href=\"", packageURL, name, "/index.html\">", name, "</a>", if(core) " (core)" else "")
  }

  htm3 <- c(paste0("  <h3>", reposname, " packages</h3>"),
            "  <table>",
            sprintf("    <tr valign=\"top\"><td><i>Core:</i></td><td>%s.</td></tr>",
              if(!any(x$packagelist[, 2L])) "<i>None</i>" else paste(sapply(x$packagelist[x$packagelist[, 2L], 1L], pkg2html), collapse = ", ")),
            sprintf("    <tr valign=\"top\"><td><i>Regular:</i></td><td>%s.</td></tr>",
              if(all(x$packagelist[, 2L])) "<i>None</i>" else paste(sapply(x$packagelist[!x$packagelist[, 2L], 1L], pkg2html), collapse = ", ")),
            if(length(x$archived) > 0L) sprintf("    <tr valign=\"top\"><td><i>Archived:</i></td><td>%s.</td></tr>",
              paste(sapply(x$archived, pkg2html), collapse = ", ")),
            "  </table>"
            )

  ## further links
  htm4 <- if(!is.null(x$links)) {
          c("  <h3>Related links</h3>",
            "  <ul>",
            sapply(x$links, function(x) paste0("    <li>", x, "</li>")),
	    "  </ul>")
          } else {
            NULL 
          }

  if(!is.null(x$otherlinks)) {
  htm4 <- c(htm4, "",
            "  <h3>Other resources</h3>",
            "  <ul>",
            sapply(x$otherlinks, function(x) paste0("    <li>", x, "</li>")),
	    "  </ul>")
  }

  ## collect code chunks
  htm <- c(htm1, "", htm2, "", htm3, "", htm4, "", "</body>", "</html>")

  ## write and return results
  writeLines(htm, con = file)
  invisible(htm)
}

repos_update_views <- function(repos = ".", cran = TRUE,
			       css = NULL,
		   	       reposname = "CRAN",
			       ...)
{
  ## These could easily be changed, but are currently not
  ## exported arguments
  viewsrds <- "Views.rds"
  index <- "index.html"

  ## default css specification
  if(is.null(css) & cran) css <- "../CRAN_web.css"

  ## compute correct installation dirs
  viewdir <- file.path(repos, "web", "views")
  index <- file.path(viewdir, index)
  contribdir <- file.path(repos, "src", "contrib")
  viewsrds <- file.path(contribdir, viewsrds)

  ## available views
  files <- dir(viewdir, pattern = "\\.(ctv|md)$")
  if(length(files) < 1) stop(paste("no .ctv files found at path", viewdir))

  ## available packages in repos
  pkgs <- as.vector(read.dcf(file.path(contribdir, "PACKAGES"), fields = "Package"))

  ## setup object which will be stored in Views.rds
  rval <- list()
  class(rval) <- "ctvlist"

  ## setup information for index.html
  idx <- matrix(rep("", 2 * length(files)), ncol = 2)

  for(i in 1:length(files)) {
    ## read .ctv and store in list
    x <- read.ctv(file.path(viewdir, files[i]), cran = cran)

    ## generate HTML code
    ctv2html(x, file = file.path(viewdir, paste0(x$name, ".html")),
             cran = cran, css = css, reposname = reposname, ...)

    ## to save space we eliminate the HTML slots
    x$info <- NULL
    x$links <- NULL

    ## check whether all packages used in the ctv are also
    ## present in repos
    if(!all(x$packagelist[,1] %in% pkgs)) {
      nopkgs <- as.vector(x$packagelist[,1])
      nopkgs <- nopkgs[!nopkgs %in% pkgs]
      options(useFancyQuotes = FALSE)
      warning(paste("The packages", paste(nopkgs, collapse = ", "),
        "in task view", sQuote(x$name), "are not available in repository",
	dQuote(repos)))
    }

    ## store index information and ctv in ctvlist
    idx[i,] <- c(x$name, x$topic)
    rval[[i]] <- x
  }
  names(rval) <- idx[, 1L]

  ## save all views
  saveRDS(rval, file = viewsrds, version = 2) ## compress = TRUE currently does not work for reading from an url

  ## generate index HTML file
  if(is.character(index)) {
    idx <- c("<!DOCTYPE html>",
             "<html>",
             "",
	     "<head>",
             paste0("  <title>", reposname, " Task Views</title>"),
             paste0("  <link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\" />"),
            "  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />",
             "</head>",
	     "",
	     "<body>",
             paste0("<h1>", reposname, " Task Views</h1>"),
	     "",
	     sprintf("<p>%s task views aim to provide some guidance which packages on %s", reposname, reposname),
	     "are relevant for tasks related to a certain topic. They give a brief overview of the included packages",
	     "and can be automatically installed using the <a href=\"https://CRAN.R-project.org/package=ctv\">ctv</a>",
	     "package. The views are intended to have a sharp focus so that it is sufficiently",
	     "clear which packages should be included (or excluded) - and they are <em>not</em> meant",
	     "to endorse the \"best\" packages for a given task.</p>",
             "",
             "<p>To automatically install the views, the <a href=\"https://CRAN.R-project.org/package=ctv\">ctv</a> package needs to be installed, e.g., via<br />",
	     "<code>install.packages(\"ctv\")</code><br />",
	     "and then the views can be installed via <code>install.views</code> or <code>update.views</code>",           
	     "(where the latter only installs those packages are not installed and up-to-date), e.g.,<br />",
	     "<code>ctv::install.views(\"Econometrics\")</code><br />",
	     "<code>ctv::update.views(\"Econometrics\")</code></p>",
	     "",
             "<p>The resources provided by the <a href=\"https://github.com/cran-task-views/ctv\">CRAN Task View Initiative</a>",
             "provide further information on how to contribute to existing task views and how to propose new task views.</p>",
	     "",
	     "<h3>Topics</h3>",
	     "",
             paste0("<table>"),
	     apply(idx, 1, function(x) {
                 paste0("  <tr valign=\"top\">\n    <td><a href=\"",
                        x[1], ".html\">", x[1], "</a></td>\n    <td>",
                        gsub("&", "&amp;", x[2]), "</td>\n  </tr>")
             }),
	     "</table>",
	     "",
	     "</body>",
	     "</html>")
    writeLines(idx, con = index)
  }

  invisible(rval)
}

check_ctv_packages <- function(file, repos = TRUE, ...)
{
  pkg_list <- read.ctv(file)$packagelist[, 1]

  ## for R/Markdown-based task views the package list is by construction identical
  ## to that in the info section but for XML-based task views this needs to be checked
  pkg_info <- if(any(grepl("<CRANTaskView", readLines(file), fixed = TRUE))) {
    unique(xml2::xml_text(xml2::xml_find_all(xml2::read_xml(file), "//info//pkg")))  
  } else {
    pkg_list
  }

  ## packages in repository and archived in repository (CRAN)
  if(!identical(repos, FALSE)) {
    if(identical(repos, TRUE)) {
      pkg_repos <- cran_package_names()
      pkg_archived <- setdiff(cran_archive_names(), pkg_repos)
    } else {
      pkg_repos <- as.vector(available.packages(contriburl = contrib.url(repos, "source"), ..., filters = "duplicates")[, 1L])
      pkg_archived <- character(0L)
    }
  } else {
    pkg_repos <- character(0L)  
    pkg_archived <- character(0L)  
  }

  ## return list of package names
  rval <- list(
    "Packages in info but not in packagelist"       = setdiff(pkg_info, pkg_list),
    "Packages in packagelist but not in info"       = setdiff(pkg_list, pkg_info),
    "Packages in packagelist but not in repos"      = setdiff(pkg_list, c(pkg_repos, pkg_archived)),
    "Packages in packagelist but archived in repos" = intersect(pkg_list, pkg_archived)
  )
  if(identical(repos, TRUE)) names(rval) <- gsub("in repos", "on CRAN", names(rval), fixed = TRUE)
  return(rval)
}

htmlify <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s <- gsub('"', "&quot;", s, fixed = TRUE)
    s
}

cran_package_names <- function() {
  con <- gzcon(url(sprintf("%s/web/packages/packages.rds", cran_repos_url()), open = "rb"))
  on.exit(close(con))
  as.vector(readRDS(con)[, "Package"])
}

cran_archive_names <- function() {
  con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", cran_repos_url()), open = "rb"))
  on.exit(close(con))
  names(readRDS(con))
}

cran_repos_url <- function() {
  repos <- getOption("repos")
  if (!is.null(repos) && !is.na(repos["CRAN"]) && (repos["CRAN"] != "@CRAN@")) {
    repos["CRAN"]
  } else {
    "https://CRAN.R-project.org"
  }
}
