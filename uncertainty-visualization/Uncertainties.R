library(denstrip)

distribution95 <- 1.96
sdev <- 20
horizontalLimit <- 12
errorBarPos <- 0
violinPos <- 4
diamondPos <- 2
fanPos <- 6
denStripPos <- 8
bottomPos <- 9
confidenceLabelPos <- 10
bottomLabel <-'Hazard ratio associated with immunotherapy'
low95Label <- '0.83'
middle95Label <- '0.94'
up95Label <- '1.06'
offsetAxis <- 0.5
axisLength <- 0.2

labelPos <- sdev*2.3

addLabel <- function(label, xpos){
  text(xpos, labelPos, label,cex = 1,srt=90,adj = c(0,0.5))
}

addConfidenceLabel <- function(label, xpos, ypos){
  text(xpos, ypos, label,cex = 0.8,srt=90)
}

addBottomLabel <- function(label, xpos){
  text(xpos, 0, label,cex = 1,srt=90)
}

#Violin
x <- seq(-sdev*distribution95, sdev*distribution95, length=100)
y <- dnorm(x, mean=0, sd=sdev)
par(bty = 'n') 
# Horizontal and vertical limites are configured here
plot(y*15+violinPos, x, type="l", lwd=1,xlim=c(-1, horizontalLimit),ylim=c(-sdev*distribution95*2,sdev*distribution95*2),ann=FALSE,xaxt='n',yaxt='n')
lines(-15*y+violinPos, x, type="l", lwd=1)
addLabel("Violin", violinPos)

# Fanplot
test <-  data.frame(
  v2 = c(-sdev*distribution95,0,sdev*distribution95)
)
fan(data = test, type = "interval", style="boxfan", llab=FALSE,rlab=FALSE, medlab='',rcex=0.6,start=fanPos,probs=c(30,60,95),frequency=2) 
addLabel("Fan", fanPos)

#Denstrip
pal <- colorRampPalette(c("black","white"))
x <- seq(-sdev*distribution95/0.95, sdev*distribution95/0.95, length=10000)
dens <- dnorm(x)
denstrip(x, dens, at=denStripPos,horiz = FALSE,gamma=0.01)
addLabel("Density\nStrip", denStripPos)

#Error bar plot
xerror <- c(errorBarPos)
avg <- 0
points(xerror, avg,pch=19)
arrows(xerror, avg-sdev*distribution95, xerror, avg+sdev*distribution95, length=0.05, angle=90, code=3)
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

# Label
addBottomLabel(bottomLabel, bottomPos)

# Add confidence labels
addConfidenceLabel(low95Label,confidenceLabelPos, -sdev*distribution95)
addConfidenceLabel(middle95Label,confidenceLabelPos, 0)
addConfidenceLabel(up95Label,confidenceLabelPos, +sdev*distribution95)

# Draw axis (as we are using actual values, it was necessary to manually draw the axis)
segments(confidenceLabelPos-offsetAxis, -sdev*distribution95, confidenceLabelPos-offsetAxis, +sdev*distribution95)
segments(confidenceLabelPos-offsetAxis, -sdev*distribution95, confidenceLabelPos-offsetAxis+axisLength, -sdev*distribution95)
segments(confidenceLabelPos-offsetAxis, 0, confidenceLabelPos-offsetAxis+axisLength, 0)
segments(confidenceLabelPos-offsetAxis, +sdev*distribution95, confidenceLabelPos-offsetAxis+axisLength, +sdev*distribution95)


