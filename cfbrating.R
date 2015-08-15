cfbrating <- function(week) {
    
    setwd("~/R Files/cfbrating")
    schedule <- read.csv("Schedule.csv",header=TRUE)
    seasonsim <- read.csv("SeasonSim.csv",header=TRUE)
    schedule <- schedule[schedule$Wk>week,]
    seasonsim <- seasonsim[(seasonsim$Losses<2 & seasonsim$Major==1)|(seasonsim$Losses==0 & seasonsim$Major==0),]
    everything <- merge(schedule,seasonsim)
    
    for (i in seasonsim$Team) {
        
        teamsched <- everything[everything$Team==i,]
        losses <- seasonsim$Losses[seasonsim$Team==i]
        
        outcomes <- matrix(runif(nrow(teamsched)*10000),ncol=10000)
        x2 <- function(x) {x>teamsched$WinProb}
        outcomes <- apply(outcomes,2,x2)
        outcomes <- apply(outcomes,2,sum)
        outcomes <- outcomes+losses
        outcomes <- c(outcomes,c(0,1,2))
        outcomes <- as.data.frame(table(outcomes))
        
        seasonsim$LOSS0[seasonsim$Team==i] <- outcomes$Freq[outcomes$outcomes==0]-1
        seasonsim$LOSS1[seasonsim$Team==i] <- outcomes$Freq[outcomes$outcomes==1]-1
        seasonsim$LOSS2[seasonsim$Team==i] <- outcomes$Freq[outcomes$outcomes==2]-1
                
    }
        
    write.csv(seasonsim,"SeasonSim.csv")
    
}