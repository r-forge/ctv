read.ctv <- function(file)
{
  ## XML package is only needed here (i.e., only server-side)
  if(!("package:XML" %in% search())) stopifnot(require(XML))

  ## read raw XML
  x <- xmlTreeParse(file)
  if(xmlSize(x$doc$children) > 1) warning("ctv should contain only one view")
  x <- xmlChildren(x$doc$children[[1]])
  
  ## valid task view?
  ctvNames <- c("name", "topic", "maintainer", "info", "packagelist", "links")
  missingChildren <- !(ctvNames %in% names(x))
  if(any(missingChildren)) stop(paste("The following ctv nodes are missing:", ctvNames[missingChildren]))
  
  ## convenience function for transforming
  ## XMLNodes into a character vector
  xmlPaste <- function(x, indent = "",
                       viewprefix = "",
                       packageURL = "../Descriptions/",
                       viewURL = "")
  {
    ## get tag name
    name <- xmlName(x, full = TRUE)
  
    ## if final node, return text
    if(name == "text")
      return(paste(indent, xmlValue(x), sep = ""))

    ## if pkg or view, return link
    if(name == "pkg") 
      return(paste("<a href=\"", packageURL, xmlValue(x), ".html\">", xmlValue(x), "</a>", sep = ""))
    if(name == "view") 
      return(paste(viewprefix, "<a href=\"", viewURL, xmlValue(x), ".html\">", xmlValue(x), "</a>", sep = ""))    
    if(name == "info")
      name <- "p"
  
    ## get attributes
    tmp <- if(!is.null(xmlAttrs(x)))
      paste(names(xmlAttrs(x)), paste("\"", xmlAttrs(x), "\"", sep = ""), sep = "=", collapse = " ")
      else ""

    ## start tag
    rval <- paste(indent, "<", name, ifelse(tmp != "", " ", ""), tmp, ">", sep = "")  
    ## content
    subIndent <- paste(indent, "  ", sep = "")
    for(i in xmlChildren(x))
      rval <- paste(rval, xmlPaste(i, indent = subIndent, packageURL = packageURL, viewURL = viewURL), sep = "\n")
    ## end tag
    rval <- paste(rval, paste(indent, "</", name, ">", sep = ""), sep = "\n")

    return(rval)
  }
  newlineSub <- function(x) {
    for(i in c(":", ",", ";", ")", ".", "?", "!"))
      x <- gsub(paste("\n[ ]*\\", i, sep = ""), i, x)
    x <- gsub("(\n<a", "(<a", x, extended = FALSE)
    return(x)
  }


  ## extraction functions
  name <- function(x) xmlValue(x$name)
  topic <- function(x) xmlValue(x$topic)
  maintainer <- function(x) xmlValue(x$maintainer)
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
  links <- function(x) as.vector(xmlSApply(x$links, function(z) xmlPaste(z, viewprefix = "CRAN Task View: ")))


  ## collect nodes and return
  rval <- list(name = name(x),
               topic = topic(x),
	       maintainer = maintainer(x),
	       info = info(x),
	       packagelist = packagelist(x),
	       links = links(x))
  class(rval) <- "ctv"
  return(rval)
}

ctv2html <- function(x,
                     file = NULL,
                     css = "../../../R.css",
		     packageURL = "../Descriptions/")
{
  if(is.character(x)) x <- read.ctv(x)
  if(is.null(file)) file <- paste(x$name, ".html", sep = "")
  
  ## auxiliary functions
  ampersSub <- function(x) gsub("&", "&amp;", x)
  zpaste <- function(..., sep = "", collapse = NULL) paste(..., sep = sep, collapse = collapse)

  ## create HTML  
  ## header
  htm1 <- c("<html>",
            "<head>",
     zpaste("  <title>CRAN Task View: ", ampersSub(x$topic), "</title>"),
     zpaste("  <link rel=stylesheet type=\"text/css\" href=\"", css, "\">"),
            "</head>",
	    "",
	    "<body>",
     zpaste("  <h2>CRAN Task View: ", ampersSub(x$topic), "</h2>"),
     zpaste("  <h3>Maintainer: ", ampersSub(x$maintainer), "</h3>"))

  ## info section	    
  htm2 <- ampersSub(x$info)

  ## package list
  pkg2html <- function(a, b)
    zpaste("    <li><a href=\"", packageURL, a, ".html\">", a, "</a>",
           if(b) " (core)" else "", "</li>")
  htm3 <- c("  <h3>CRAN packages:</h3>",
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

updateViews <- function(repos = ".",
			css = "../../../R.css",
			...)
{
  ## These could easily be changed, but are currently not
  ## exported arguments
  viewsrds <- "Views.rds"
  index <- "index.html"

  ## list all .ctv files
  path <- file.path(repos, "src", "contrib", "Views")
  files <- dir(path, pattern = "\\.ctv$")
  if(length(files) < 1) stop(paste("no .ctv files found at path", path))
  
  ## compute correct installdirs
  contribdir <- file.path(repos, "src", "contrib")
  viewdir <- file.path(contribdir, "Views")
  viewsrds <- file.path(contribdir, viewsrds)
  index <- file.path(viewdir, index)
  
  ## available packages in repos
  pkgs <- as.vector(read.dcf(file.path(contribdir, "PACKAGES"), fields = "Package"))
  
  ## setup object which will be stored in Views.rds
  rval <- list()
  class(rval) <- "ctvlist"
  
  ## setup information for index.html
  idx <- matrix(rep("", 2 * length(files)), ncol = 2)
  
  for(i in 1:length(files)) {
    ## read .ctv and store in list
    x <- read.ctv(file.path(path, files[i]))
    
    ## generate HTML code
    ctv2html(x, file = file.path(viewdir, paste(x$name, ".html", sep = "")), css = css, ...)

    ## to save space we eliminate the HTML slots
    x$info <- NULL
    x$links <- NULL

    ## check whether all packages used in the ctv are also
    ## present in repos
    if(!all(x$packagelist[,1] %in% pkgs))
      warning(paste("Not all packages in task view", sQuote(x$name), "are also available in repository", dQuote(repos)))

    ## store index information and ctv in ctvlist
    idx[i,] <- c(x$name, x$topic)
    rval[[i]] <- x
  }
  
  ## save all views 
  .saveRDS(rval, file = viewsrds) ## compress = TRUE currently does not work for reading from an url
  
  ## generate index HTML file
  if(is.character(index)) {
    idx <- c("<html>", "", "<head>", "  <title>CRAN Task Views</title>",
             paste("  <link rel=stylesheet type=\"text/css\" href=\"", css, "\">", sep = ""),
             "</head>", "", "<body>", "<h1>CRAN Task Views</h1>",
	     ## add some instructions how to call install.views()
	     "", "<table>",
	     apply(idx, 1, function(x) paste("  <tr valign=\"top\">\n    <td><a href=\"",
	       x[1], ".html\">", x[1], "</a></td>\n    <td>", gsub("&", "&amp;", x[2]), "</td>\n  </tr>", sep = "")),
	     "</table>", "", "</body>", "</html>")
    writeLines(idx, con = index)
  }  
  
  invisible(rval)
}
