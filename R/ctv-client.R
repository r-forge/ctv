print.ctv <- function(x, packagelist = TRUE, ...)
{
  cat(paste("\nCRAN Task View\n--------------\nName: ",
            x$name, "\nTopic: ",
	    x$topic, "\nMaintainer: ",
	    x$maintainer, "\n",
	    ifelse(is.null(x$repository), "", paste("Repository: ", x$repository, "\n", sep = "")),
	    "\n", sep = ""))
  if(packagelist) {
    cat(paste("Packages:",
        paste(x$packagelist$name, ifelse(x$packagelist$core, "*", ""), sep = "", collapse = ", "),
        "\n"))
    cat(ifelse(any(x$packagelist$core), "(* = core package)\n\n", "\n"))
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

CRAN.views <- function(repos = NULL, ...)
{
  ## getOption("repos") replaces getOption("CRAN") from 2.1.0 on
  if(is.null(repos)) repos <- ifelse(is.null(getOption("repos")), getOption("CRAN"), getOption("repos"))

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
  ## get CRAN views and extract names of available views
  cranviews <- CRAN.views(repos = repos)
  availnames <- sapply(seq(along = cranviews), function(i) cranviews[[i]]$name)
  
  ## install packages for each view
  coreOnly <- rep(coreOnly, length.out = length(views)) 
   
  for(i in seq(along = views)) {
    thisview <- which(views[i] == availnames)
    if(length(thisview) > 0) {
      x <- cranviews[[thisview[1]]]
      xpack <- if(coreOnly[i]) subset(x$packagelist, core)[,1] else x$packagelist[,1]
      install.packages(xpack, CRAN = x$repository, dependencies = dependencies, ...)
    } else {
      warning(paste("CRAN task view", views[i], "not available"))
    }
  }
  
  invisible()
}
