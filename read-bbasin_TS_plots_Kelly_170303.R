library(oce)

if (!interactive())
    pdf("bedford_basin.pdf", pointsize=9)
all <- FALSE

## cache so we can run this locally, e.g. playing with
## plotting parameters, without waiting for downloads

    url <- "ftp://ftp1.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF/2017"
    dir <- read.csv(paste(url, "2017667ODFSUMMARY.tsv", sep="/"), skip=2)
    print(dir)
    filenames <- dir$FILE
    n <- length(filenames)
    if (!all && n > 9)
        filenames <- tail(filenames, 9)
    ctds <- vector("list", n)
    for (i in seq_along(filenames)) {
        file <- paste(url, dir$FILE[i], sep="/")
        print(file)
        ctds[[i]] <- read.oce(file)
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

par(mfrow=c(1, 1))
# separate graph: water level in Sackville River
d <- read.csv("http://dd.weather.gc.ca/hydrometric/csv/NS/daily/NS_01EJ004_daily_hydrometric.csv")
t <- as.POSIXct(d$Date)
eta <- d$Water.Level...Niveau.d.eau..m.
oce.plot.ts(t, eta, type='p', drawTimeRange=FALSE, ylab="Water Level (m)", xlab="Date")
for (ctd in ctds) {
    ctdTime <- ctd[["startTime"]]
    abline(v=ctdTime, col="darkgray")
    mtext(ctdTime, line=0, at=ctdTime)
}

if (!interactive())
    dev.off()

