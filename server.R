# Chocolate Screening

# Libraries
        library(shiny)
        library(shinydashboard)
        library(dplyr)
        library(tidyr)
        library(ggplot2)

        
# Constants
       
        
# Functions


# Server Funtion
shinyServer(function(input, output){

        
# Run Simulation

        # Calculate STC
        
        
        
                dfWins <- eventReactive(input$runSim,  {
                        
                        # Us Outcome Probabilities
                        usOffReb <- 1 - input$themDefReb
                        
                        us.outTO <- input$usTO
                        us.out2PT <- (1-input$usTO)*(1-input$usFreq3PT)*input$us2PT
                        us.out3PT <- (1-input$usTO)*input$usFreq3PT*input$us3PT
                        us.outKeep <- (1-input$usTO)*(1-input$usFreq3PT)*(1-input$us2PT)*usOffReb + (1-input$usTO)*input$usFreq3PT*(1-input$us3PT)*usOffReb
                        us.outNoKeep <- (1-input$usTO)*(1-input$usFreq3PT)*(1-input$us2PT)*(1-usOffReb) + (1-input$usTO)*input$usFreq3PT*(1-input$us3PT)*(1-usOffReb)
                        
                        # Check Prob Distribution
                        print(us.outTO + us.out2PT + us.out3PT + us.outKeep + us.outNoKeep)
                        
                        
                        # Their Outcome Probabilities
                        
                        them.outTO <- input$themTO
                        them.out2PT <- (1-input$themTO)*(1-input$themFreq3PT)*input$them2PT
                        them.out3PT <- (1-input$themTO)*input$themFreq3PT*input$them3PT
                        them.outKeep <- (1-input$themTO)*(1-input$themFreq3PT)*(1-input$them2PT)*(1-input$usDefReb) + (1-input$themTO)*input$themFreq3PT*(1-input$them3PT)*(1-input$usDefReb)
                        them.outNoKeep <- (1-input$themTO)*(1-input$themFreq3PT)*(1-input$them2PT)*(input$usDefReb) + (1-input$themTO)*input$themFreq3PT*(1-input$them3PT)*(input$usDefReb)
                        
                        # Check Prob Distribution
                        print(them.outTO + them.out2PT + them.out3PT + them.outKeep + them.outNoKeep)
                        
                        
                        
                        
                        # Simulate K Games
                        
                        
                        
                        mWins <- matrix(NA, nrow=input$kSim)
                        
                        for(i in 1:input$kSim){        
                                
                                # Simulate Game
                                
                                # Play Game US
                                
                                mOutUs <- matrix(c(0, 0, 0, input$nPoss), ncol=1)  
                                row.names(mOutUs) <- c("Their.Ball","PTS2","PTS3","Reset")
                                
                                while(mOutUs[4,1] > 0) {
                                        
                                        usPlays <- runif(mOutUs[4,1])  # Number of Resets
                                        usPlays <- cut(usPlays, 
                                                       breaks=c(0,
                                                                us.outTO+us.outNoKeep, 
                                                                us.outTO+us.outNoKeep+us.out2PT,
                                                                us.outTO+us.outNoKeep+us.out2PT+us.out3PT,
                                                                us.outTO+us.outNoKeep+us.out2PT+us.out3PT+us.outKeep),
                                                       labels=c("Their.Ball","PTS2","PTS3","Reset"))
                                        
                                        mTemp <- as.matrix(table(usPlays))
                                        
                                        # Update Game
                                        
                                        mOutUs[1:3,1] <- mOutUs[1:3,1] + mTemp[1:3,1]
                                        mOutUs[4,1] <- mTemp[4,1]
                                        
                                }
                                
                                # Free Throws (Accounts for Variability in # of Free Throws and Percent Made)
                                usFT.Pts <- rbinom(n=1, size=round(rnorm(1, input$usFT, 3.7)), prob = input$usFTPcnt)
                                if(is.na(usFT.Pts)){usFT.Pts = 0}
                                
                                # Total Points
                                PtsUs <- 2*mOutUs[2,1] + 3*mOutUs[3,1] + usFT.Pts
                                
                                
                                
                                # Play Game Them
                                
                                mOutThem <- matrix(c(0, 0, 0, input$nPoss), ncol=1)  
                                row.names(mOutThem) <- c("Our.Ball","PTS2","PTS3","Reset")
                                
                                while(mOutThem[4,1] > 0) {
                                        
                                        themPlays <- runif(mOutThem[4,1])  # Number of Resets
                                        themPlays <- cut(themPlays, 
                                                         breaks=c(0,
                                                                  them.outTO+them.outNoKeep, 
                                                                  them.outTO+them.outNoKeep+them.out2PT,
                                                                  them.outTO+them.outNoKeep+them.out2PT+them.out3PT,
                                                                  them.outTO+them.outNoKeep+them.out2PT+them.out3PT+them.outKeep),
                                                         labels=c("Our.Ball","PTS2","PTS3","Reset"))
                                        
                                        mTemp <- as.matrix(table(themPlays))
                                        
                                        # Update Game
                                        
                                        mOutThem[1:3,1] <- mOutThem[1:3,1] + mTemp[1:3,1]
                                        mOutThem[4,1] <- mTemp[4,1]
                                        
                                }
                                
                                # Free Throws (Accounts for Variability in # of Free Throws and Percent Made)
                                themFT.Pts <- rbinom(n=1, size=round(rnorm(1, input$themFT, 3.7)), prob = input$themFTPcnt)
                                if(is.na(themFT.Pts)){themFT.Pts = 0}
                                
                                # Total Points
                                PtsThem <- 2*mOutThem[2,1] + 3*mOutThem[3,1] + themFT.Pts
                                
                                iWin <- NA
                                tieBreaker <- runif(1,min=-0.1, max=0.1)
                                PtsUs <- PtsUs + tieBreaker
                                
                                if (PtsUs > PtsThem) {iWin <- 1} else {iWin <- 0}
                                
                                mWins[i,1] <- iWin
                                
                        }
                        
                        dfWins <- data.frame(mWins)
                        dfWins
                        
                })
                
                
                
                
                
        
       output$myTable <- renderDataTable({
               dfSummary <- tbl_df(dfWins())
               names(dfSummary) <- c("Winner")
               
               dfSummary$Winner <- factor(dfSummary$Win, levels=c(0,1), labels=c("Them","Us"))
               
               dfSummary <- dfSummary %>% 
                       group_by(Winner) %>% 
                       summarise(Freq = n()) %>% 
                       mutate (Percent = round(100*Freq / sum(Freq),1)) %>% 
                       arrange(desc(Winner))
               dfSummary
               
       }, options=list(searching=FALSE, paging=FALSE))
       
       output$myPlot <- renderPlot({
               
               dfSummary <- tbl_df(dfWins())
               names(dfSummary) <- c("Win")
               dfSummary <- dfSummary %>% group_by(Win) %>% summarise(Freq = n()) %>% mutate (Percent = round(100*Freq / sum(Freq),1))
               dfSummary$Win <- factor(dfSummary$Win, levels=rev(c(0,1)), labels=rev(c("Loss","Win")))
       
               
               p <- ggplot(data=dfSummary, aes(x=Win, y=Percent))
               p <- p + geom_bar(fill=rgb(114,158,206, maxColorValue = 255), stat="identity")
               p <- p + theme_minimal() + xlab("")
               p <- p + theme(text = element_text(size=16))
               print(p)
               
       })



})




