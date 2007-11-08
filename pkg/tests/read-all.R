#### Read all CTV's in the package:

library(ctv)

cDIR <- system.file("ctv", package="ctv")
ctvs <- list.files(cDIR, pattern = "\\.ctv$")
ctvs

rr <- sapply(ctvs,
	     function(ctv) {
		 cat(sprintf("%25s  ",ctv))
		 R <- read.ctv(file.path(cDIR, ctv))
		 cat("[Ok]\n")
                 R
	     },
	     simplify=FALSE)

for(n in names(rr)) {
    cat(n," :\n", rep.int("=", nchar(n)),"\n", sep='')
    print(rr[[n]])
    cat("--------------------------------------------------------\n")
}
