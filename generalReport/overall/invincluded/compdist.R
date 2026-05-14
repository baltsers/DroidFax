#!/bin/Rscript

require(graphics)

args=commandArgs(trailingOnly=TRUE)
if (length(args)<1) {
	stop("too few arguments")
	exit
}

fndata=args[1]
tdata=read.table(file=fndata)

spm<- matrix(NA, nrow=nrow(tdata)/3, ncol=ncol(tdata)-1)
dpm <- matrix(NA, nrow=nrow(tdata)/3, ncol=ncol(tdata)-1)
dpmins <- matrix(NA, nrow=nrow(tdata)/3, ncol=ncol(tdata)-1)

f.per <- function (x,y) {
	if (y<1e-10) return (0)
	return (x/y*100)
}

r=1
for(i in seq(1,nrow(tdata),3)) {
	s<-sum(tdata[i,1:4])
	cursrow <- c(f.per(tdata[i,1],s), f.per(tdata[i,2],s), f.per(tdata[i,3],s), f.per(tdata[i,4],s))
	spm[r,] <- cursrow

	d<-sum(tdata[i+1,1:4])
	curdrow <- c(f.per(tdata[i+1,1],d), f.per(tdata[i+1,2],d), f.per(tdata[i+1,3],d), f.per(tdata[i+1,4],d))
	dpm[r,] <- curdrow

	dins<-sum(tdata[i+2,1:4])
	curdrowins <- c(f.per(tdata[i+2,1],dins), f.per(tdata[i+2,2],dins), f.per(tdata[i+2,3],dins), f.per(tdata[i+2,4],dins))
	dpmins[r,] <- curdrowins

	r <- r+1
}

colors<-c("red","blue","green","yellow") #,"black","darkorange","darkorchid","gold4","darkgrey")
xnames<-c("Activity", "Service", "Receiver", "ContentProvider")
pches<-c(0:8)

pdf("./compdist-uniq-s.pdf")
boxplot(spm, names=xnames,col=colors,ylab="percentage")
pdf("./compdist-uniq-d.pdf")
boxplot(dpm, names=xnames,col=colors,ylab="percentage")
pdf("./compdist-ins-d.pdf")
boxplot(dpmins, names=xnames,col=colors,ylab="percentage")

#dev.off


