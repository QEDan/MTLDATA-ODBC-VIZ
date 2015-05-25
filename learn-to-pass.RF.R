library(randomForest)
library(mlbench)
library(caret)


set.seed(42)

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
events.all <- rbind(game1, game2, game3, game4, game5, game6)
events.passes <- subset(events.all, name=="pass")
events.passes$id <- as.factor(events.passes$id)
events.passes$period <- as.factor(events.passes$period)
events.passes$xPos <- abs(events.passes$xCoord)
events.passes$yPos <- abs(events.passes$yCoord)
netpos.X <- 100.0
netpos.Y <- 0.0
events.passes$netdist <- sqrt((events.passes$xPos-netpos.X)^2 + (events.passes$yPos-netpos.Y)^2)

events.passes$shorthand <- gsub("\\+", "", events.passes$shorthand)
events.passes$shorthand <- gsub("-", "", events.passes$shorthand)
events.passes$type <- factor(events.passes$type)

extractFeatures <- function(data) {
      features <- c("period",
                    "team",
                    "zone",
                    "type",
                    "xPos",
                    "yPos",
                    "game",
                    "netdist",
                    "playerPosition")
        return(data[,features])
  }


trainEvents <- sample(1:dim(events.passes)[1], 2500)
valEvents <- setdiff(1:dim(events.passes)[1], trainEvents)

#model <- lm(outcome ~ period + team + shorthand + zone + type +
#            xAdjCoord + yAdjCoord  + playerPosition, data=events.passes)

rf <- randomForest(extractFeatures(events.passes[trainEvents,]),
                   events.passes[trainEvents,]$outcome, ntree=100,
                   mtry=5, importance=TRUE)
imp <- importance(rf, type=1)
print(imp)

prediction <- predict(rf, events.passes[valEvents,-13], type="prob")

dist <- function(prob, truth)
{
    if (truth == "successful") t <- 1
    else t <- 0
    return(abs(t - prob))
}

distances <- mapply(dist, prediction[,2], events.passes[valEvents,]$outcome)

png('testValErrors.png')
hist(distances.train, col=rgb(1,0,0,0.5), probability=T, breaks=50, main="")
hist(distances.val, col=rgb(0,0,1,0.5), probability=T, add=T, breaks=50)
legend("topright", c("Training Errors", "Validation Errors"),
              col=c("red", "blue"), lwd=10)
dev.off()

print(sum(distances.val <0.5)/sum(distances.val >= 0.0))
