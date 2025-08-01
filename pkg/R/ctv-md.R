initialize_ctv_env <- function(cran = FALSE)
{
  .new_ctv_env <- new.env()

  ## templates for package and task view URLs to be used
  .new_ctv_env$pkg_url  <- if(cran) "../packages/%s/index.html" else "https://CRAN.R-project.org/package=%s"
  .new_ctv_env$view_url <- if(cran) "%s.html" else "https://CRAN.R-project.org/view=%s"
  .new_ctv_env$span <- if(cran) function(pkg, class) sprintf('[%s]{class="%s"}', pkg, class) else function(pkg, class) pkg

  ## data frame with (active) package names in task view
  .new_ctv_env$packagelist <- data.frame(
    name = character(0L),
    core = logical(0L),
    stringsAsFactors = FALSE
  )

  ## vector of archived package names
  .new_ctv_env$archivelist <- character(0L)

  ## links
  .new_ctv_env$viewlist <- character(0L)
  .new_ctv_env$otherlist <- data.frame(
    name = character(0L),
    source = character(0L),
    stringsAsFactors = FALSE
  )

  ## vectors of packages active or archived on CRAN (if cran = TRUE)
  ## (copy from existing environment if available)
  if(cran) {
    .new_ctv_env$cranlist <- if(length(.ctv_env$cranlist) > 0L) {
      .ctv_env$cranlist
    } else {
      cran_package_names()
    }
    .new_ctv_env$cranarchivelist <- if(length(.ctv_env$cranarchivelist) > 0L) {
      .ctv_env$cranarchivelist
    } else {
      setdiff(cran_archive_names(), .new_ctv_env$cranlist)
    }
  } else {
    .new_ctv_env$cranlist <- character(0L)
    .new_ctv_env$cranarchivelist <- character(0L)
  }

  return(.new_ctv_env)
}

.ctv_env <- initialize_ctv_env()

pkg <- function(name, priority = "normal", register = TRUE) {
  ## try to check CRAN status of packages
  if(length(.ctv_env$cranlist) > 0L || length(.ctv_env$cranarchivelist) > 0L) {
    status <- if(name %in% .ctv_env$cranlist) {
      "active"
    } else if(name %in% .ctv_env$cranarchivelist) {
      "archived"
    } else {
      "unavailable"
    }
  } else {
    status <- "active"  
  }

  ## register package
  if(register && status == "active") {
    if(name %in% .ctv_env$packagelist$name) {
      if(identical(priority, "core")) .ctv_env$packagelist$core[.ctv_env$packagelist$name == name] <- TRUE
    } else {
      .ctv_env$packagelist <- rbind(.ctv_env$packagelist,
        data.frame(name = name, core = identical(priority, "core"), stringsAsFactors = FALSE))
    }
  }
  if(register && status == "archived") {
    if(!(name %in% .ctv_env$archivelist)) .ctv_env$archivelist <- c(.ctv_env$archivelist, name)
  }
  if(register && status == "unavailable") {
    warning(sprintf("package '%s' is not available on CRAN", name))
  }
  
  ## return URL
  txt <- if(status == "active") {
    sprintf("[%s](%s)", .ctv_env$span(name, "CRAN"), sprintf(.ctv_env$pkg_url, name))
  } else if(status == "archived") {
    sprintf("[%s](%s)", .ctv_env$span(paste(name, "_(archived)_"), "CRAN"), sprintf(.ctv_env$pkg_url, name))
  } else {
    sprintf("_%s (unavailable)_", name)
  }
  return(txt)
}

view <- function(name, section = NULL, register = TRUE) {
  ## register view
  if(register) .ctv_env$viewlist <- unique(c(.ctv_env$viewlist, name))
  ## return URL
  if(is.null(section)) {
    sprintf("[%s](%s)", .ctv_env$span(name, "CRAN"), sprintf(.ctv_env$view_url, name))
  } else {
    sprintf("[%s](%s#%s)", .ctv_env$span(section, "CRAN"), sprintf(.ctv_env$view_url, name), gsub(" ", "-", tolower(section), fixed = TRUE))
  }
}

bioc <- function(name, register = TRUE) {
  ## register package
  if(register && !(name %in% .ctv_env$otherlist$name)) {
    .ctv_env$otherlist <- rbind(.ctv_env$otherlist,
      data.frame(name = name, source = "bioc", stringsAsFactors = FALSE))
  }
  ## return URL
  sprintf("[%s](https://www.Bioconductor.org/packages/%s)", .ctv_env$span(name, "BioC"), name)
}

rforge <- function(name, register = TRUE) {
  ## register package
  if(register && !(name %in% .ctv_env$otherlist$name)) {
    .ctv_env$otherlist <- rbind(.ctv_env$otherlist,
      data.frame(name = name, source = "rforge", stringsAsFactors = FALSE))
  }
  ## return URL
  sprintf("[%s](https://R-Forge.R-project.org/projects/%s)", .ctv_env$span(name, "Rforge"), tolower(name))
}

gcode <- function(name, register = TRUE) {
  ## register package
  if(register && !(name %in% .ctv_env$otherlist$name)) {
    .ctv_env$otherlist <- rbind(.ctv_env$otherlist,
      data.frame(name = name, source = "gcode", stringsAsFactors = FALSE))
  }
  ## return URL
  sprintf("[%s](https://code.google.com/archive/p/%s)", .ctv_env$span(name, "Gcode"), name)
}

ohat <- function(name, register = TRUE) {
  ## register package
  if(register && !(name %in% .ctv_env$otherlist$name)) {
    .ctv_env$otherlist <- rbind(.ctv_env$otherlist,
      data.frame(name = name, source = "ohat", stringsAsFactors = FALSE))
  }
  ## return URL
  sprintf("[%s](http://www.Omegahat.net/%s)", .ctv_env$span(name, "Ohat"), name)
}

github <- function(name, register = TRUE) {
  ## register package
  if(register && !(name %in% .ctv_env$otherlist$name)) {
    .ctv_env$otherlist <- rbind(.ctv_env$otherlist,
      data.frame(name = name, source = "github", stringsAsFactors = FALSE))
  }
  ## return URL
  sprintf("[%s](https://github.com/%s)", .ctv_env$span(sapply(strsplit(name, "/", fixed = TRUE), "[", 2L), "GitHub"), name)
}

doi <- function(name) {
  sprintf("[doi:%s](https://doi.org/%s)", name, name)
}


read_ctv_rmd <- function(file, cran = FALSE, format = "html")
{
  ## clear .ctv_env for registering package/view links
  assignInNamespace(".ctv_env", initialize_ctv_env(cran = cran), ns = "ctv")
  
  ## read Rmd
  rmd <- readLines(file)

  ## file names
  file_rmd <- gsub("\\.([[:alnum:]]+)$", ".Rmd", basename(file))
  file_md <- gsub("\\.Rmd$", ".md", file_rmd)

  ## output format
  format <- match.arg(format, c("html", "markdown"))

  ## read header
  x <- which(substr(rmd, 1L, 3L) == "---")
  if(length(x) < 2L) stop("YAML header missing or malformatted")
  x <- strsplit(rmd[(x[1L] + 1L):(x[2L] - 1L)], ": ")
  names(x) <- sapply(x, "[", 1L)
  x <- lapply(x, "[", 2L)
  if(!("url" %in% names(x))) x$url <- NULL
  if(!("source" %in% names(x))) x$source <- NULL
  
  ## call knitr in temp dir
  odir <- getwd()
  tdir <- tempfile()
  if(!file.exists(tdir)) dir.create(tdir)
  setwd(tdir)
  on.exit(setwd(odir))
  writeLines(rmd[-(1L:(which(substr(rmd, 1L, 3L) == "---")[2L]))], file_rmd)
  knitr::knit(file_rmd, quiet = TRUE)
  md <- readLines(file_md)

  ## extract links (if any)
  li <- which(substr(md, 1L, 9L) == "### Links")
  if(length(li) < 1L) {
    links <- NULL
  } else {
    links <- md[(li + 1L):length(md)]
    md <- md[1L:(li - 1L)]

    ## collapse links to single lines
    links <- gsub("^\\* ", "\\\\item ", links)
    links <- gsub("^- ", "\\\\item ", links)
    links <- paste(links, collapse = " ")
    links <- gsub("^ *\\\\item *", "", links)
    links <- paste0(links, " ")
    links <- strsplit(links, " *\\\\item")[[1L]]
    links <- gsub("^ +", "", links)
    links <- gsub(" +$", "", links)
    links <- gsub(" +", " ", links)
  }
  ## add "other" links
  otherlinks <- NULL
  if(length(.ctv_env$viewlist) > 0L) otherlinks <- c(otherlinks, paste("CRAN Task View:", view(sort(.ctv_env$viewlist))))
  if(NROW(.ctv_env$otherlist) > 0L) {
    olinks <- .ctv_env$otherlist
    olinks <- olinks[order(tolower(sapply(strsplit(olinks$name, "/", fixed = TRUE), function(x) x[length(x)]))), , drop = FALSE]
    olinks$source <- factor(olinks$source, levels = c("bioc", "rforge", "github", "ohat", "gcode"))
    olinks <- olinks[order(olinks$source), , drop = FALSE]
    for(i in levels(olinks$source)) {
      ii <- which(olinks$source == i)
      if(length(ii) > 0L) otherlinks <- c(otherlinks, paste0(
        switch(i,
	  "bioc" = "Bioconductor Package",
	  "rforge" = "R-Forge Project",
	  "github" = "GitHub Project",
	  "ohat" = "Omegahat Package",
	  "gcode" = "Google Code Project"),
	": ",
	do.call(i, list(name = olinks$name[ii], register = FALSE))
      ))
    }
  }

  ## info
  x$info <- if(format == "html") pandoc(md) else md

  ## packagelist
  x$packagelist <- .ctv_env$packagelist[order(tolower(.ctv_env$packagelist$name)), ]

  ## archived packages
  x$archived <- .ctv_env$archivelist[order(tolower(.ctv_env$archivelist))]

  ## links
  x$links <- links  
  if(!is.null(x$links) && format == "html") {
    x$links <- as.character(pandoc(as.list(x$links), options = "--wrap=preserve"))
    x$links <- gsub("(^<p>)(.*)(</p>$)", "\\2", x$links)
  }
  x$otherlinks <- otherlinks  
  if(!is.null(x$otherlinks) && format == "html") {
    x$otherlinks <- as.character(pandoc(as.list(x$otherlinks), options = "--wrap=preserve"))
    x$otherlinks <- gsub("(^<p>)(.*)(</p>$)", "\\2", x$otherlinks)
  }

  ## add citation for CRAN views
  x$citation <- utils::bibentry(
    bibtype = "Manual",
    author = as.person(x$maintainer),
    title = sprintf("%sTask View: %s", if(cran) "CRAN " else "", x$topic),
    year = as.numeric(substr(x$version, 1L, 4L)),
    note = sprintf("Version %s", x$version),
    url = if(cran) {
      paste0("https://CRAN.R-project.org/view=", x$name)
    } else if(!is.null(x$url)) {
      x$url
    } else {
      x$source
    },
    header = sprintf("To cite the %s task view in publications use:", x$name)
  )
  class(x$citation) <- c("citation", class(x$citation))
  
  class(x) <- "ctv"
  return(x)
}

ctv_xml_to_rmd <- function(x) {
  ## output file name
  file <- gsub("\\.ctv$", ".md", x)
  
  ## raw and parsed XML
  x0 <- xml2::read_xml(x)
  x <- read.ctv(x)

  ## CRAN packages from <packagelist>
  reg <- x$packagelist
  reg$core <- c("pkg", "corepkg")[1L + reg$core]
  names(reg)[2L] <- "type"

  ## CRAN packages from <info>
  pkg <- sort(unique(xml2::xml_text(xml2::xml_find_all(x0, "//info//pkg"))))
  if(length(setdiff(pkg, reg$name)) > 0L) {
    warning("not all <pkg> listed in <packagelist>")
    reg <- rbind(reg, data.frame(name = setdiff(pkg, reg$name), type = "pkg", stringsAsFactors = FALSE))
  }
  
  ## other tags from <info>
  add_reg <- function(name) {
    rval <- sort(unique(xml2::xml_text(xml2::xml_find_all(x0, sprintf("//info//%s", name)))))
    rval <- if(length(rval) < 1L) {
      NULL
    } else {
      data.frame(name = rval, type = name, stringsAsFactors = FALSE)
    }
    return(rval)
  }
  reg <- rbind(reg, add_reg("view"), add_reg("rforge"), add_reg("gcode"), add_reg("bioc"), add_reg("ohat"), add_reg("github")) 

  ## replace tags in info with R/Markdown code
  for(i in which(reg$type == "corepkg")) {
    nami <- reg$name[i]
    urli <- sprintf('<a href="../packages/%s/index.html">%s</a>', nami, nami)
    rmdi <- sprintf('`r_chunk_pkg("%s",priority="core")`', nami)
    x$info <- sub(urli, rmdi, x$info, fixed = TRUE)
  }
  make_url <- function(type, name) {
    switch(type,
      "pkg"    = sprintf('<a href="../packages/%s/index.html">%s</a>', name, name),
      "view"   = sprintf('<a href="%s.html">%s</a>', name, name),
      "rforge" = sprintf('<a href="https://R-Forge.R-project.org/projects/%s/"><span class="Rforge">%s</span></a>', tolower(name), name),
      "gcode"  = sprintf('<a href="https://code.google.com/archive/p/%s/"><span class="Gcode">%s</span></a>', name, name),
      "bioc"   = sprintf('<a href="https://www.Bioconductor.org/packages/release/bioc/html/%s.html"><span class="BioC">%s</span></a>', name, name),
      "ohat"   = sprintf('<a href="http://www.Omegahat.net/%s/"><span class="Ohat">%s</span></a>', name, name),
      "github" = sprintf('<a href="https://github.com/%s/"><span class="GitHub">%s</span></a>', name, strsplit(name, "/", fixed = TRUE)[[1L]][2L])
    )
  }
  for(j in c("corepkg", "pkg", "view", "rforge", "gcode", "bioc", "ohat", "github")) {
    for(i in which(reg$type == j)) {
      typj <- if(j == "corepkg") "pkg" else j
      nami <- reg$name[i]
      urli <- make_url(typj, nami)
      rmdi <- sprintf('`r_chunk_%s("%s")`', typj, nami)
      x$info <- gsub(urli, rmdi, x$info, fixed = TRUE)
    }
  }

  ## convert info to md 
  x$info <- gsub("<tt>", "<code>", x$info, fixed = TRUE)
  x$info <- gsub("</tt>", "</code>", x$info, fixed = TRUE)
  x$info <- pandoc(x$info, from = "html", to = "markdown")
  x$info <- gsub("(<div>|</div>)", "", x$info)
  xi <- which(substr(x$info, 1L, 2L) == "**")
  x$info[xi] <- gsub("(\\*\\*)(.*)(\\*\\*)", "### \\2", x$info[xi])
  
  ## convert <a> links to md 
  x$links <- pandoc(c("<ul>", paste0("<li>", x$links[substr(x$links, 1L, 3L) == "<a "], "</li>"), "</ul>"),
    from = "html", to = "markdown")

  ## YAML header
  x$header <- c("---", paste0(names(x)[1L:5L], ": ", unlist(x[1L:5L])), "---")

  ## write R/Markdown file
  writeLines(c(
    x$header,
    "",
    x$info,
    "",
    "### Links",
    x$links
  ), file)
  
  invisible(x)
}

pandoc <- function(x, ..., from = "markdown", to = "html5", options = "--wrap=none",
  fixup = (from == "html" && to == "markdown"))
{
  ## check availability
  stopifnot(rmarkdown::pandoc_available())

  ## temporary files
  infile <- tempfile()
  outfile <- tempfile()
  on.exit(unlink(c(infile, outfile)))

  ## list of inputs?
  xlist <- is.list(x)
  if(xlist) {
    sep <- "\007\007\007\007\007"
    x <- unlist(lapply(x, c, c("", sep, "")))
  }

  ## call pandoc_convert()
  writeLines(x, infile)
  rmarkdown::pandoc_convert(input = infile, output = outfile, from = from, to = to, options = options, ...)
  rval <- readLines(outfile)
  
  ## fix backticks and quotes (if necessary)
  if(fixup) {
    rval <- gsub("\\_chunk\\_", " ", rval, fixed = TRUE)
    rval <- gsub("_chunk_", " ", rval, fixed = TRUE)
    rval <- gsub("\\`", "`", rval, fixed = TRUE)
    rval <- gsub("\\\"", "\"", rval, fixed = TRUE)
    rval <- gsub("\\'", "'", rval, fixed = TRUE)
    rval <- gsub(',priority="core"', ', priority = "core"', rval, fixed = TRUE)
  }
  
  ## split up list again?
  if(xlist) {
    ix <- grepl(sep, rval, fixed = TRUE)
    rval <- split(rval, c(0, head(cumsum(ix), -1L)))

    ## omit sep from last line in each chunk
    cleansep <- function(x) {
      n <- length(x)
      if(n < 1L) return(x)
      del <- c(
        if(x[1L] == "") 1L else NULL,
        if(n > 1L && grepl(sep, x[n], fixed = TRUE) && x[n - 1L] == "") n - 1L else NULL,
	if(x[n] %in% c(sep, paste0("<p>", sep, "</p>"))) n else NULL
	## FIXME: Depending on the output format the relevant line
	## may just contain the 'sep' or '<p>sep</p>'. But it may
	## a '</p>' may also be in the line _after_ the 'sep'.
	## ...maybe lapply() rather than seperator-based?
      )
      x[n] <- gsub(sep, "", x[n], fixed = TRUE)
      if(length(del) > 0L) return(x[-del]) else return(x)
    }
    rval <- lapply(rval, cleansep)  
  }
  
  return(rval)
}
