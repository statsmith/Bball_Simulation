# Basketball Simulation


# Libraries
        library(shiny)
        library(shinydashboard)
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        
        
header <- dashboardHeader(
        
        title = "Bball Simulation",
        
        dropdownMenu(
                
                messageItem (from="Created", message="March 2016 by Dave Smith", icon=icon("male")),
                messageItem (from="Updated", message="June 2016 by Dave Smith", icon=icon("male"))
                
        )
)       
        


sidebar <- dashboardSidebar(disable = TRUE)
        
body <- dashboardBody(
                        
                        fluidRow(
                                
                                box(
                                        title = "Note", solidHeaer = FALSE, collapsible=TRUE, collapsed=TRUE, width=12,      
                                        
                                        tags$b("These metrics represent team averages.  The Web App accounts for the fact that
                                               teams will perform better on some days than others"),
                                        tags$br(),
                                        tags$li("Turnover Rate:  Percent of Plays That End in a Turnover"),
                                        tags$li("3-Pt FGP:  Percent of 3-Pt Shots Made"),
                                        tags$li("2-Pt FGP:  Percent of 2-Pt Shots Made"),
                                        tags$li("% of Shots 3-Pt:  Percent of All Shots That Are 3-Pt Shots"),
                                        tags$li("Ave FT / Game:  Average Number of Free Throws Per Game"),
                                        tags$li("FTP:  Percent of Free Throws Made"),
                                        tags$li("% Def Reb:  Percent of Defensive Rebounds (And Loose Balls) Team Gets.  Note:  % Offensive Rebounds = 1- Opponent's % Defensive Rebounds"),
                                        tags$li("# Possessions / Game:  Typical Number of Possessions For Each Team in a Game.  Use to ID if Team Should Force or Slow Temp..."),
                                        tags$li("# of Simulated Games:  More Games Gives Better Estimates, But Requires More Computing Time"),
                                        tags$br(),
                                        tags$b("Modify the inputs to determine how sensitive outcomes (Wins & Losses) are to changes in average performance. 
                                               Identify best opportunities to improve the likelihood of a win.")
                                        
                                        
                                ),

                                
                                
                                box(
                                        title="Inputs", width=6,
                                           
                                                box(
                                                        
                                                        title = "Possessions", solidHeaer = FALSE, collapsible=TRUE, collapsed=FALSE, width=6, 
                                                        numericInput("nPoss", "# Poss / Game (Each)", 50, min=0, max=200, step=1)
                                                        
                                                ),
                                                
                                                box(
                                                        
                                                        title = "Simulations", solidHeaer = FALSE, collapsible=TRUE, collapsed=FALSE, width=6,
                                                        numericInput("kSim", "# of Games", 2000, min=100, max=10000, step=100)
                                                ),
                                        
                                       
                                                
                                                box(
                                                        
                                                        title = "Us",  solidHeaer = FALSE, collapsible=TRUE, collapsed=FALSE, width=6, icon=icon("download"),
                                                        
                                                        sliderInput("usTO", "Turnover Rate", 0.15, min=0, max=1, step=0.01),
                                                        sliderInput("us3PT", "3-Pt FGP", 0.20, min=0, max=1, step=0.01),
                                                        sliderInput("us2PT", "2-Pt FGP", 0.40, min=0, max=1, step=0.01),
                                                        sliderInput("usFreq3PT", "% of Shots 3 Pt", 0.25, min=0, max=1, step=0.01),
                                                        sliderInput("usFT", "Ave FT / Game", 9, min=0, max=30, step=1),
                                                        sliderInput("usFTPcnt", "FTP", 0.45, min=0, max=1, step=0.01),
                                                        sliderInput("usDefReb", "% Def Reb", 0.55, min=0, max=1, step=0.01)
                                                        
                                                ),
                                                
                                                box(
                                                        
                                                        title = "Them", solidHeaer = FALSE, collapsible=TRUE, collapsed=FALSE, width=6, icon=icon("download"),
                                                        
                                                        sliderInput("themTO", "Turnover Rate", 0.15, min=0, max=1, step=0.01),
                                                        sliderInput("them3PT", "3-Pt FGP", 0.20, min=0, max=1, step=0.01),
                                                        sliderInput("them2PT", "2-Pt FGP", 0.40, min=0, max=1, step=0.01),
                                                        sliderInput("themFreq3PT", "% of Shots 3 Pt", 0.25, min=0, max=1, step=0.01),
                                                        sliderInput("themFT", "Ave FT / Game", 9, min=0, max=30, step=1),
                                                        sliderInput("themFTPcnt", "FTP", 0.45, min=0, max=1, step=0.01),
                                                        sliderInput("themDefReb", "% Def Reb", 0.55, min=0, max=1, step=0.01)
                                                        
                                                )
                                        
                                ),
                        
                                box(title = "Outputs", width=6,
                                    
                                    box(width=12, actionButton("runSim", "Run Simulation")),
                                    
                                    box(
                                            title = "Table",  solidHeaer = FALSE, collapsible=TRUE, collapsed=FALSE, width=12,      
                                            
                                            dataTableOutput("myTable")
                                    ),
                                    
                                    
                                    box(
                                            title = "Plot",  solidHeaer = FALSE, collapsible=TRUE, collapsed=FALSE, width=12,      
                                            
                                            plotOutput("myPlot")
                                    )
                                )
                        )
)
       
        
        
dashboardPage(header, sidebar, body)