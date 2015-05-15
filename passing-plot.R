library(ripa)
library(jpeg)
library(gplots)

#Utility function for plotting a jpeg image
plot_jpeg = function(path, add=FALSE)
    {
          require('jpeg')
            jpg = readJPEG(path, native=T) # read the file
            res = dim(jpg)[1:2] # get the resolution
            if (!add) # initialize an empty plot area if add==FALSE
                    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2])
                         ,asp=0.425,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
            rasterImage(jpg,1,1,res[1],res[2])
      }

#Read data from csv files and create column for game number
game1 <- read.csv('Mtl-Ott-game1.csv')
game1$game <- 1
game2 <- read.csv('Mtl-Ott-game2.csv')
game2$game <- 2
game3 <- read.csv('Mtl-Ott-game3.csv')
game3$game <- 3
game4 <- read.csv('Mtl-Ott-game4.csv')
game4$game <- 4
game5 <- read.csv('Mtl-Ott-game5.csv')
game5$game <- 5
game6 <- read.csv('Mtl-Ott-game6.csv')
game6$game <- 6

#Organize data and select the passing data
events.all <- rbind(game1, game2, game3, game4, game5, game6)
events.passes <- subset(events.all, name=="pass")
events.passes$id <- as.factor(events.passes$id)
events.passes$period <- as.factor(events.passes$period)
events.passes$xPos <- abs(events.passes$xCoord)
events.passes$yPos <- abs(events.passes$yCoord)
netpos.X <- 100.0
netpos.Y <- 0.0
#Distance from nearest net
events.passes$netdist <- sqrt((events.passes$xPos-netpos.X)^2 + (events.passes$yPos-netpos.Y)^2)


events.passes$shorthand <- gsub("\\+", "", events.passes$shorthand)
events.passes$shorthand <- gsub("-", "", events.passes$shorthand)
events.passes$type <- factor(events.passes$type)

#Rink jpg file
rinkpic <- "Rink2.jpg"
jpg <- readJPEG(rinkpic, native=T)
res <- dim(jpg)[1:2]

#Fudge factors for mapping points to Rink image
ff.x <- 0.93
ff.y <- 0.97

#Create the png device and organize the layout
png('pass-viz.png', width=1200, height=2200)
par(mai=c(0.1, 0.1, 0.1, 0.1), cex.main=3, adj=0.0)
layout(matrix(c(1,2,3,4,5,6,7,8,9,9,9,9), 6, 2, byrow = TRUE))

#Loop over passtypes and plot successful and failed passes for each
for (passtype in c('d2d', 'eastwest', 'north', 'outlet', 'rush', 'slot', 'south', 'stretch'))
    {        
        plot_jpeg(rinkpic)
        title(main=passtype, cex.lab=5, line=-2)
        subpass <- subset(events.passes, type==passtype & outcome=='failed')
points(ff.x*(subpass$xAdjCoord/sum(abs(range(events.passes$xAdjCoord)))+0.5/ff.x)*res[1],
      ff.y*(subpass$yAdjCoord/sum(abs(range(events.passes$yAdjCoord)))+0.5/ff.y)*res[2],
       pch="o", col='red', cex=2, main=passtype)
subpass <- subset(events.passes, type==passtype & outcome=='successful')
points(ff.x*(subpass$xAdjCoord/sum(abs(range(events.passes$xAdjCoord)))+0.5/ff.x)*res[1],
       ff.y*(subpass$yAdjCoord/sum(abs(range(events.passes$yAdjCoord)))+0.5/ff.y)*res[2],
       pch="o", col='green', cex=2, main=passtype)

}

#Create the mosaic plot 
par(mai=c(1,1,1,1), cex=2)
pass.types <- subset(events.passes, type=="d2d" | type=="eastwest" | type=="north"
              | type=="outlet" | type=="rush" | type=="slot" | type=="south"
              | type=="stretch")
plot(pass.types$type, pass.types$outcome, xlab="", ylab="", col=c('red','green'))
dev.off()
