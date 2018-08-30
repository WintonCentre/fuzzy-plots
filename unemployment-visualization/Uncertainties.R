library(denstrip)

errorBarPos <- 0
violinPos <- 4
diamondPos <- 2
fanPos <- 6
denStripPos <- 8

sdev <- 20
labelPos <- sdev*2.3

addLabel <- function(label, xpos){
  text(xpos, labelPos, label,cex = 1,srt=90,adj = c(0,0.5))
}

#Violin
x <- seq(-sdev*1.96, sdev*1.96, length=100)
y <- dnorm(x, mean=0, sd=sdev)
par(bty = 'n') 
plot(y*15+violinPos, x, type="l", lwd=1,xlim=c(-1, 10),ylim=c(-sdev*1.96,sdev*1.96*2),ann=FALSE,xaxt='n',yaxt='n')
lines(-15*y+violinPos, x, type="l", lwd=1)
addLabel("Violin", violinPos)

# Fanplot
fan(data = test, type = "interval", style="boxfan", llab=FALSE,rlab=FALSE, medlab='',rcex=0.6,start=fanPos,probs=c(30,60,95),frequency=2) 
addLabel("Fan", fanPos)

#Denstrip
pal <- colorRampPalette(c("black","white"))
x <- seq(-sdev*1.96/0.95, sdev*1.96/0.95, length=10000)
dens <- dnorm(x)
denstrip(x, dens, at=denStripPos,horiz = FALSE,gamma=0.01)
addLabel("Density\nStrip", denStripPos)

#Error bar plot
xerror <- c(errorBarPos)
avg <- 0
points(xerror, avg,pch=19)
arrows(xerror, avg-sdev*1.96, xerror, avg+sdev*1.96, length=0.05, angle=90, code=3)
addLabel("Error Bar", errorBarPos)

#Diamond
bx <- diamondPos
dx <- 0.3

by <- 0
dy <- 40

x1 <- c(bx-dx,bx,bx+dx,bx,bx-dx)
y1 <- c(by,by+dy,by,by-dy,by)
polygon(x1,y1,col=0,border=1)
polygon(c(bx-dx,bx+dx),c(by,by),col=0,border=1)

addLabel("Diamond", diamondPos)

