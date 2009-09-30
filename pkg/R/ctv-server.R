read.ctv <- function(file)
{
  ## XML package is only needed here (i.e., only server-side)
  if(!("package:XML" %in% search())) stopifnot(require("XML"))

  ## read raw XML
  x <- xmlTreeParse(file)
  if(xmlSize(x$doc$children) > 1) warning("ctv should contain only one view")
  x <- xmlChildren(x$doc$children$CRANTaskView)

  ## valid task view?
  ctvNames <- c("name", "topic", "maintainer", "version", "info", "packagelist", "links")
  missingChildren <- !(ctvNames %in% names(x))
  if(any(missingChildren))
    stop("The following ctv nodes are missing: ",
      paste(ctvNames[missingChildren], collapse=", "))

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
      target <- " target=\"_top\""
    } else {
      viewprefix <- ""
      biocprefix <- ""
      ohatprefix <- ""
      rforgeprefix <- ""
      gcodeprefix <- ""
      target <- ""
    }

    ## get tag name
    name <- xmlName(x, full = TRUE)

    ## if final node, return text
    if(name == "text")
      return(paste(indent, xmlValue(x), sep = ""))

    ## if pkg or view, return link
    if(name == "pkg")
      return(paste("<a href=\"", packageURL, xmlValue(x), "/index.html\">", xmlValue(x), "</a>", sep = ""))
    if(name == "view")
      return(paste(viewprefix, "<a href=\"", viewURL, xmlValue(x), ".html\">", xmlValue(x), "</a>", sep = ""))
    if(name == "info")
      name <- "div"
    if(name == "comment")
      return(NULL)
    if(name == "code")
      return(paste("<tt>", xmlValue(x), "</tt>", sep = ""))
    if(name == "rforge")
      return(paste(rforgeprefix, "<a href=\"http://R-Forge.R-project.org/projects/",
        tolower(xmlValue(x)), "/\"", target, "><span class=\"Rforge\">", xmlValue(x), "</span></a>", sep = ""))
    if(name == "gcode")
      return(paste(gcodeprefix, "<a href=\"http://code.google.com/p/",
        xmlValue(x), "/\"", target, "><span class=\"Gcode\">", xmlValue(x), "</span></a>", sep = ""))
    if(name == "bioc")
      return(paste(biocprefix, "<a href=\"http://www.Bioconductor.org/packages/release/bioc/html/",
        xmlValue(x), ".html\"", target, "><span class=\"BioC\">", xmlValue(x), "</span></a>", sep = ""))
    if(name == "ohat")
      return(paste(ohatprefix, "<a href=\"http://www.Omegahat.org/",
        xmlValue(x), "/\"", target, "><span class=\"Ohat\">", xmlValue(x), "</span></a>", sep = ""))

    ## get attributes
    tmp <- if(!is.null(xmlAttrs(x)))
      paste(names(xmlAttrs(x)), paste("\"", xmlAttrs(x), "\"", sep = ""), sep = "=", collapse = " ")
      else ""

    ## start tag
    rval <- paste(indent, "<", name, ifelse(tmp != "", " ", ""), tmp, ">", sep = "")
    ## content
    subIndent <- paste(indent, "  ", sep = "")
    for(i in xmlChildren(x)) {
      xmlPaste_i <- xmlPaste(i, indent = subIndent, packageURL = packageURL, viewURL = viewURL)
      if(!is.null(xmlPaste_i)) rval <- paste(rval, xmlPaste_i, sep = "\n")
    }
    ## end tag
    rval <- paste(rval, paste(indent, "</", name, ">", sep = ""), sep = "\n")

    return(rval)
  }
  newlineSub <- function(x) {
  ## FIXME: This returns latin1 in a latin1 locale even if
  ## the input was UTF-8
    for(i in c(":", ",", ";", ")", ".", "?", "!"))
      x <- gsub(paste("\n[ ]*\\", i, sep = ""), i, x)
    x <- gsub("(\n<a", "(<a", x, fixed = TRUE)
    return(x)
  }


  ## extraction functions
  name <- function(x) xmlValue(x$name)
  topic <- function(x) xmlValue(x$topic)
  maintainer <- function(x) xmlValue(x$maintainer)
  email <- function(x) as.vector(xmlAttrs(x$maintainer)["email"])
  ctvversion <- function(x) xmlValue(x$version)
  info <- function(x) newlineSub(xmlPaste(x$info, indent = "    "))
  package1 <- function(x) {
    rval <- xmlAttrs(x)["priority"]
    rval <- if(!is.null(rval) && rval == "core") "core" else "normal"
    as.vector(c(xmlValue(x), rval))
  }
  packagelist <- function(x) {
    rval <- t(sapply(xmlChildren(x$packagelist), package1))
    colnames(rval) <- NULL
    rownames(rval) <- NULL
    rval <- data.frame(name = I(rval[,1]), core = rval[,2] == "core")
    rval[order(tolower(rval[,1])), ]
  }
  links <- function(x) unlist(xmlSApply(x$links, function(z) xmlPaste(z, prefix = TRUE)))

  ## collect nodes and return
  rval <- list(name = name(x),
               topic = topic(x),
	       maintainer = maintainer(x),
	       email = email(x),
	       version = ctvversion(x),
	       info = info(x),
	       packagelist = packagelist(x),
	       links = links(x))
  class(rval) <- "ctv"
  return(rval)
}

ctv2html <- function(x,
                     file = NULL,
                     css = "../CRAN_web.css",
		     packageURL = "../packages/",
		     reposname = "CRAN")
{
  if(is.character(x)) x <- read.ctv(x)
  if(is.null(file)) file <- paste(x$name, ".html", sep = "")

  ## auxiliary functions
  ampersSub <- function(x) gsub("&", "&amp;", x)
  zpaste <- function(..., sep = "", collapse = NULL) paste(..., sep = sep, collapse = collapse)
  obfuscate <- function(x) paste(sprintf("&#x%x;",
    as.integer(sapply(unlist(strsplit(gsub("@", " at ", x), NULL)), charToRaw))), collapse = "")    

  utf8 <- any(unlist(sapply(x[sapply(x, is.character)], Encoding)) == "UTF-8")
  strip_encoding <- function(x) {
    if(is.character(x)) Encoding(x) <- "unknown"
    return(x)
  }
  for(i in 1:length(x)) x[[i]] <- strip_encoding(x[[i]])

  ## create HTML
  ## header
  htm1 <- c("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">",
            "<html xmlns=\"http://www.w3.org/1999/xhtml\">",
            "<head>",
     zpaste("  <title>", reposname, " Task View: ", ampersSub(x$topic), "</title>"),
     zpaste("  <link rel=stylesheet type=\"text/css\" href=\"", css, "\" />"),
     if(utf8)
            "  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />",          
            "</head>",
	    "",
	    "<body>",
     zpaste("  <h2>", reposname, " Task View: ", ampersSub(x$topic), "</h2>"),
     zpaste("  <table summary=\"", x$name, " task view information\">"),
     zpaste("    <tr><td valign=\"top\"><b>Maintainer:</b></td><td>", ampersSub(x$maintainer), "</td></tr>"),
     if(!is.null(x$email)) zpaste("    <tr><td valign=\"top\"><b>Contact:</b></td><td>", obfuscate(x$email), "</td></tr>"),
     zpaste("    <tr><td valign=\"top\"><b>Version:</b></td><td>", ampersSub(x$version), "</td></tr>"),
            "  </table>")

  ## info section
  htm2 <- ampersSub(x$info)

  ## package list
  pkg2html <- function(a, b)
    zpaste("    <li><a href=\"", packageURL, a, "/index.html\">", a, "</a>",
           if(b) " (core)" else "", "</li>")
  htm3 <- c(zpaste("  <h3>", reposname, " packages:</h3>"),
            "  <ul>",
	    sapply(1:NROW(x$packagelist), function(i) pkg2html(x$packagelist[i,1], x$packagelist[i,2])),
	    "  </ul>")

  ## further links
  htm4 <- c("  <h3>Related links:</h3>",
            "  <ul>",
            sapply(x$links, function(x) paste("    <li>", ampersSub(x), "</li>", sep = "")),
	    "  </ul>")

  ## collect code chunks
  htm <- c(htm1, "", htm2, "", htm3, "", htm4, "", "</body>", "</html>")

  ## write and return results
  writeLines(htm, con = file)
  invisible(htm)
}

repos_update_views <- function(repos = ".",
			       css = "../CRAN_web.css",
		   	       reposname = "CRAN",
			       ...)
{
  ## These could easily be changed, but are currently not
  ## exported arguments
  viewsrds <- "Views.rds"
  index <- "index.html"

  ## compute correct installation dirs
  viewdir <- file.path(repos, "web", "views")
  index <- file.path(viewdir, index)
  contribdir <- file.path(repos, "src", "contrib")
  viewsrds <- file.path(contribdir, viewsrds)

  ## available views
  files <- dir(viewdir, pattern = "\\.ctv$")
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
    x <- read.ctv(file.path(viewdir, files[i]))

    ## generate HTML code
    ctv2html(x, file = file.path(viewdir, paste(x$name, ".html", sep = "")),
             css = css, reposname = reposname, ...)

    ## to save space we eliminate the HTML slots
    x$info <- NULL
    x$links <- NULL

    ## check whether all packages used in the ctv are also
    ## present in repos
    if(!all(x$packagelist[,1] %in% pkgs)) {
      nopkgs <- as.vector(x$packagelist[,1])
      nopkgs <- nopkgs[!nopkgs %in% pkgs]
      warning(paste("The packages", paste(nopkgs, collapse = ", "),
        "in task view", sQuote(x$name), "are not available in repository",
	dQuote(repos)))
    }

    ## store index information and ctv in ctvlist
    idx[i,] <- c(x$name, x$topic)
    rval[[i]] <- x
  }

  ## save all views
  .saveRDS(rval, file = viewsrds) ## compress = TRUE currently does not work for reading from an url

  ## generate index HTML file
  if(is.character(index)) {
    idx <- c("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">",
             "<html xmlns=\"http://www.w3.org/1999/xhtml\">",
             "",
	     "<head>",
       paste("  <title>", reposname, " Task Views</title>", sep = ""),
       paste("  <link rel=stylesheet type=\"text/css\" href=\"", css, "\" />", sep = ""),
             "</head>",
	     "",
	     "<body>",
       paste("<h1>", reposname, " Task Views</h1>", sep = ""),
	     "",
       paste("<table summary=\"", reposname," Task Views\">", sep = ""),
	     apply(idx, 1, function(x) paste("  <tr valign=\"top\">\n    <td><a href=\"",
	       x[1], ".html\">", x[1], "</a></td>\n    <td>", gsub("&", "&amp;", x[2]), "</td>\n  </tr>", sep = "")),
	     "</table>",
	     "",
             "<p>To automatically install these views, the ctv package needs to be installed, e.g., via<br />",
	     "   <tt>install.packages(\"ctv\")</tt><br />",
	     "   <tt>library(\"ctv\")</tt><br />",
	     "   and then the views can be installed via <tt>install.views</tt> or <tt>update.views</tt>",	     
	     "   (which first assesses which of the packages are already installed and up-to-date), e.g.,<br />",
	     "   <tt>install.views(\"Econometrics\")</tt><br />",
	     "   or<br />",
	     "   <tt>update.views(\"Econometrics\")</tt></p>",
	     "",
	     "</body>",
	     "</html>")
    writeLines(idx, con = index)
  }

  invisible(rval)
}
