#!/bin/Rscript

require(graphics)

args=commandArgs(trailingOnly=TRUE)
if (length(args)<1) {
	stop("too few arguments")
	exit
}

fndata=args[1]
tdata=read.table(file=fndata)

dpmclsins <- matrix(NA, nrow=nrow(tdata)/1, ncol=ncol(tdata)/2-1)
dpmmeins <- matrix(NA, nrow=nrow(tdata)/1, ncol=ncol(tdata)/2-1)

f.per <- function (x,y) {
	if (y<1e-10) return (0)
	return (x/y*100)
}

r=1
for(i in seq(1,nrow(tdata),1)) {
	curdrowcls <- c(f.per(tdata[i,1],tdata[i,4]), f.per(tdata[i,2],tdata[i,4]), f.per(tdata[i,3],tdata[i,4]))
	dpmclsins[r,] <- curdrowcls

	curdrowme <- c(f.per(tdata[i,5],tdata[i,8]), f.per(tdata[i,6],tdata[i,8]), f.per(tdata[i,7],tdata[i,8]))
	dpmmeins[r,] <- curdrowme

	r <- r+1
}

colors<-c("red","blue","green") #,"black","yellow","darkorange","darkorchid","gold4","darkgrey")
xnames<-c("User code", "3rdparty lib", "SDK")
pches<-c(0:8)


pdf("./gdist-ins-dcls.pdf")
boxplot(dpmclsins, names=xnames,col=colors,ylab="percentage")
pdf("./gdist-ins-dmethod.pdf")
boxplot(dpmmeins, names=xnames,col=colors,ylab="percentage")

#dev.off


