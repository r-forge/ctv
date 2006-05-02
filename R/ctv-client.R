print.ctv <- function(x, packagelist = TRUE, ...)
{
  cat(paste("\nCRAN Task View\n--------------\nName:       ",
            x$name, "\nTopic:      ",
	    x$topic, "\nMaintainer: ",
	    x$maintainer, "\n",
	    ifelse(is.null(x$repository), "", paste("Repository: ", x$repository, sep = "")),
	    "\n", sep = ""))
  if(packagelist) {
    pkgs <- paste(strwrap(paste(x$packagelist$name, ifelse(x$packagelist$core, "*", ""), sep = "", collapse = ", "),
      width = getOption("width"), prefix = "            "), collapse = "\n")
    substr(pkgs, 1, 9) <- "Packages:"
    cat(pkgs)
    cat(ifelse(any(x$packagelist$core), "\n            (* = core package)\n\n", "\n"))
  }
  invisible(x)
}

print.ctvlist <- function(x, packagelist = FALSE, ...)
{
  cat("\nCRAN Task Views\n")
  n <- length(x)
  if(n < 1)
    cat("no views found\n")
  else {
    for(i in seq(along = x)) {    
      cat(paste("---------------\nName: ",
                x[[i]]$name, "\nTopic: ",
	        x[[i]]$topic, "\nMaintainer: ",
	        x[[i]]$maintainer, "\n",
	        ifelse(is.null(x[[i]]$repository), "", paste("Repository: ", x[[i]]$repository, "\n", sep = "")),
	        sep = ""))
      if(packagelist) {
        cat(paste("Packages:",
            paste(x[[i]]$packagelist$name, ifelse(x[[i]]$packagelist$core, "*", ""), sep = "", collapse = ", "),
            "\n"))
        cat(ifelse(any(x[[i]]$packagelist$core), "(* = core package)\n", ""))
      }
    }
    cat("\n")
  }
  invisible(x)
}

available.views <- CRAN.views <- function(repos = NULL, ...)
{
  ## getOption("repos") replaces getOption("CRAN") from 2.1.0 on
  if(is.null(repos)) repos <- ifelse(is.null(getOption("repos")), getOption("CRAN"), getOption("repos"))

  if("@CRAN@" %in% repos && interactive()) {
      cat(gettext("--- Please select a CRAN mirror for use in this session ---\n"))
      flush.console()
      chooseCRANmirror()
      m <- match("@CRAN@", repos)
      nm <- names(repos)
      repos[m] <- getOption("repos")["CRAN"]
      if(is.null(nm)) nm <- rep("", length(repos))
      nm[m] <- "CRAN"
      names(repos) <- nm
  }
  if("@CRAN@" %in% repos) stop("trying to use CRAN without setting a mirror")

  contriburl <- paste(repos, "/src/contrib", sep = "")
  rval <- list()
  
  for(i in seq(along = contriburl)) {
    ## load Views.rds from repository    
    x <- suppressWarnings(try(.readRDS(viewurl <- url(paste(contriburl[i], "Views.rds", sep = "/"),
      open = "rb")), silent = TRUE))
    if(inherits(x, "try-error")) next else close(viewurl)

    ## add repository information    
    for(j in seq(along = x)) x[[j]]$repository <- repos[i]
    rval <- c(rval, x)
  }
  class(rval) <- "ctvlist"
  return(rval)
}

install.views <- function(views,
                          coreOnly = FALSE,
			  repos = NULL,
                          dependencies = TRUE,
			  ...)
{
  if(inherits(views, "ctv")) {
    views <- list(views)
    class(views) <- "ctvlist"
  }
  if(!inherits(views, "ctvlist")) {
    ## get CRAN views and extract names of available views
    cranviews <- available.views(repos = repos)
    availnames <- sapply(seq(along = cranviews), function(i) cranviews[[i]]$name)

    whichviews <- lapply(views, function(z) {
      rval <- which(z == availnames)
      if(length(rval) > 0) rval[1] else numeric(0)
    })
    unavail <- which(sapply(whichviews, length) < 1)
    if(length(unavail) > 0) warning(paste("CRAN task view", views[unavail], "not available", collapse = "\n"))
    views <- cranviews[as.vector(unlist(whichviews))]
    class(views) <- "ctvlist"
  }
  
  ## install packages for each view
  coreOnly <- rep(coreOnly, length.out = length(views)) 
   
  for(i in seq(along = views)) {
    pkgs <- if(coreOnly[i]) subset(views[[i]]$packagelist, core)[,1] else views[[i]]$packagelist[,1]
    install.packages(pkgs, repos = views[[i]]$repository, dependencies = dependencies, ...)
  }
  
  invisible()
}
