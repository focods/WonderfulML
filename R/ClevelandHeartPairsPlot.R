# Functions used to create pairs plots with histograms along the diagonal
# and correlation coeff's in the upper triangular region
panel.hist <- function(x) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <-y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
    dev.copy2eps(file="scatter_a3.eps")
}

panel.cor <- function(x,y,digits=4, prefix="", cex.cor) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0,1,0,1))
    r <- abs(cor(x,y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex=cex*r)
}

diag_hist <- function(y=clevdat) {
    pairs(y, diag.panel=panel.hist, upper.panel=panel.cor)
}