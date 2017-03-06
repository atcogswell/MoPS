library(oce)

if (!length(ls(pattern="dir"))) {
    url <- "ftp://ftp1.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF/2017"
    dir <- read.csv(paste(url, "2017667ODFSUMMARY.tsv", sep="/"), skip=2)
    #dir <- tail(dir$FILE, 9)
    print("These are the last 9 (or less) files of the current year:")
    print(dir)
    n <- length(dir$FILE)
    ctds <- vector("list", n)
    for (i in 1:n) {
        file <- paste(url, dir$FILE[i], sep="/")
        print(file)
        ctds[[i]] <- read.oce(file)
    }
}
S <- unlist(lapply(ctds, function(ctd) c(ctd[["salinity"]], NA)))
T <- unlist(lapply(ctds, function(ctd) c(ctd[["temperature"]], NA)))
p <- unlist(lapply(ctds, function(ctd) c(ctd[["pressure"]], NA)))

Slim <- range(S)
Tlim <- range(T)
CTD <- as.ctd(S, T, p)
par(mfrow=c(3,3))
for (ctd in ctds) {
    plotTS(CTD, col='gray', type='l')
    lines(ctd[["salinity"]], ctd[["theta"]])
    mtext(ctd[["startTime"]], line=1, adj=1, cex=0.5)
}

