library('shiny')
library('shinydashboard')
library('readxl')
library('ggplot2')
library('dplyr')
library('magrittr')
library('ggjoy')

#Import Data Step ********************************************************************************************************************************************
League <- read_excel("FFData_Sep5.xlsx", sheet = "League")
Top3Place <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Place")
Top3Draft <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Draft")
AllStats <- read_excel("FFData_Sep5.xlsx", sheet = "All")

#get all #1 draft picks
g <- subset(Top3Draft,Top3Draft$`Draft Number`==1)

#########Some PreProcessing ********************************************************************************************************************************************

#AllStats$Pts_For <- as.numeric(AllStats$Pts_For)

#AllStats$Pts_For <- format(round(AllStats$Pts_For,0),nsmall = 0, format= "d", big.mark = ",")

AllStats$Pts_For <- as.integer(as.character(AllStats$Pts_For))
AllStats$Pt_Diff <- as.integer(AllStats$Pt_Diff)

AllStats$Avg_Pts_For<- formatC(AllStats$Avg_Pts_For,digits = 0, format = "f", big.mark = ",")
AllStats$Avg_Pts_Against<- formatC(AllStats$Avg_Pts_Against,digits = 0, format = "d", big.mark = ",")
AllStats$Avg_Pt_Diff <- formatC(AllStats$Avg_Pt_Diff,digits = 0, format = "d", big.mark = ",")
AllStats$Win_Percent <- sprintf("%.0f %%",AllStats$Win_Percent*100)
#AllStats$Pts_For <- formatC(AllStats$Pts_For,digits = 0, format = "d", big.mark = ",")
AllStats$Pts_Against <- formatC(AllStats$Pts_Against,digits = 0, format = "d", big.mark = ",")

League$Year <- as.integer(League$Year)



Top3Draft$`Draft Number` <- as.integer(Top3Draft$`Draft Number`)

# Num Games Played since 2004
gamesPlayed <- 173


str(AllStats)

#################################################Create Add'l Tables###################################################################################
   #table for total record

  totalRecordTable <-  select(AllStats,Owner, Wins, Losses, Ties) %>% group_by(Owner) %>%summarise_all(funs(sum)) %>% arrange(desc(Wins))
totalRecordTableWithWin <- totalRecordTable %>% mutate(Win_Percent = sprintf("%.1f %%",totalRecordTable$Wins/(totalRecordTable$Wins + totalRecordTable$Losses + totalRecordTable$Ties)*100))
   
   #per game metrics

  perGameDF <-  select(AllStats,Owner,Pts_For,Pt_Diff) %>% group_by(Owner)  %>% summarise_all(function(x) sum(x)/gamesPlayed)


# perGameDF <- aggregate(AllStats[,c(5,7)],list(AllStats$Owner), function(x) sum(x)/173)
  perSeasonDF <-  select(AllStats,Owner,Wins,Place,Moves) %>% group_by(Owner) %>% summarise_all(function(x) mean(x))
  countDF <- select(AllStats,Owner,Playoffs,Top_3_Finsh) %>% group_by(Owner) %>% summarise_all(function(x) length(which(x=="Y")))
#BUILD THE UI********************************************************************************************************************************************

#configure Header
dbHeader <- dashboardHeader(title = "13 Glorious Years of Fantasy Football", titleWidth = 450)


#configure Sidebar
dbSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("League Overview",tabName = 'db1', icon = icon("dashboard")),
    menuItem("The Original 5", tabName = 'db2', icon = icon("th")),
    menuItem("Thru the Years by Owner", tabName = 'db3', icon = icon("th"))
  )  
)

#configure body
dbBody <- dashboardBody(
  tabItems(
      #FirstTab Open
    tabItem(tabName ='db1',h1("League Overview"),
                fluidPage(fluidRow(box(tableOutput('LeagueOverview'), title = "2004 - 2016 Leagues",solidHeader = TRUE, status = 'primary', width = 6),
                                   box(plotOutput('LeagueWinners'),title = 'League Winners by Frequency', solidHeader = TRUE, status = 'primary', width = 6, background = 'light-blue')
                    
                    
                ),#close1stFluidRow
                          fluidRow(column(7,box(plotOutput('Top3Finishes'), title = '# of Top 3 Finishes', solidHeader = TRUE, status = 'primary', width = 12)),
                                   column(5,box(plotOutput('Top3DraftPicks'), title ='Top Draft Picks since 2004', solidHeader = TRUE, status ='primary', width = 12, background = 'light-blue')))

               
                
                 )#closeFluidPage
            
            #FirstTabClose
            ),
    
    #SecondTab Open
    tabItem(tabName = 'db2',h2("The Original 5"),
            fluidPage(fluidRow(column(3, box(tableOutput('TotalRecord'), title = 'Record since 2004', solidHeader = TRUE, status ='primary',width = 12 )),
                              column(9,infoBox("Jgord","52.6% Win Rate", subtitle = "91-79-3", icon = icon('thumbs-up'), color ='light-blue'),
                                     infoBox("Jose","52.6% Win Rate", subtitle = "91-81-1", icon = icon('thumbs-up'), color ='lime'),
                                     infoBox("Lip","50.9% Win Rate", subtitle = "88-84-1", icon = icon('thumbs-up'), color ='orange'),
                                     infoBox("Z","49.7% Win Rate", subtitle = "86-84-3", icon = icon('thumbs-down'), color ='fuchsia'),
                                     infoBox("B","45.1% Win Rate", subtitle = "78-92-1", icon = icon('thumbs-down'), color ='aqua')
                                     
                                     )#end column 9
                            
                #div(style = "height:50px;width:100%;background-color: #999999;border-style: solid;border-color: #000000"),
                
            ),# close fluidRow
            
                    fluidRow(column(6,box(plotOutput("Orig5Place"),title ='A distribution of Final Rankings', solidHeader = TRUE, status = 'primary',width = 12)),
                         column(6, tabBox(title = "", id = "tabSet1", height='400px', width = 12,
                                 tabPanel("PPG",plotOutput("PPGPlot")),
                                 tabPanel('Diff/Gm',plotOutput("DiffGMPlot")),
                                  tabPanel("Wins/Yr",plotOutput("WinsPerSeason")),
                                 tabPanel('Avg Finish',plotOutput("AvgFinPlot")),
                                 tabPanel('Moves/Yr',plotOutput("MovesYrPlot")),
                                 tabPanel("Playoff Count",plotOutput("Top3Plot")))
                                 
                                )#closeColumn
                             )#closeFluidRow
                
                
            )#close 2nd fluid page
            
            ), #SecondTabClose
    
    #Third tab open
    tabItem(tabName = 'db3',h2("Performance By Year"),
        fluidPage(selectInput("OwnerInput", "Choose Team Owner:", list("B", "Z","Lip","Jose","Jgord")),
                  fluidRow(box(plotOutput("WinsByYear"), 
                    title = "Wins By Year", solidHeader = TRUE, status = 'primary', width = 12)),
                  
                  fluidRow(box(plotOutput("RankByYear"), 
                               title = "Ranking By Year", solidHeader = TRUE, status = 'primary', width = 12)),
                           
                  fluidRow(box(plotOutput("PPGByYear"), 
                               title = "PPG By Year", solidHeader = TRUE, status = 'primary', width = 12))       
            
            
        )  #close 3rd fluid page
  )#third tab close
  
)#close tabitems
)#close Body

#pass elements into dashboardPage function and pass to UI Variable
ui <- dashboardPage(dbHeader,dbSidebar,dbBody)

#SERVER FUNCTION********************************************************************************************************************************************
server <- function(input, output){
   
    ################################# TAB #1 SERVER#####################################################################################
    
    #League Table
    output$LeagueOverview <- renderTable(League, align = 'c', width = 'auto')
  #output$LeagueOverview <- renderDataTable(League, options = list(ScrollY = '800px', pageLength = 1000))
    
    #League Graph
    output$LeagueWinners <- renderPlot(
        ggplot(League, aes(x=reorder(League$`Winner (owner)`,League$`Winner (owner)`, function(x)-length(x)))) + geom_bar(color = "blue", fill = "dodgerblue3") + labs(x ="",y="")
        +theme(axis.text.x = element_text(size = 16, angle = 45, family = 'calibri', face = 'bold'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        geom_text(stat='count', aes(label = ..count..), vjust =-1, face = 'bold', family = 'calibri', size = 6) + scale_y_discrete()
        , width = 'auto', height = 'auto'
        
    )#closeRenderplot
    
    #Top3Finishes Graph
    output$Top3Finishes <- renderPlot(ggplot(Top3Place, aes(x = reorder(Top3Place$'Team Owner', Top3Place$'Team Owner', function(x) length(x)))) + 
                              geom_bar(aes(fill = as.factor(Top3Place$Place)))  + coord_flip()
                    + labs(x="", y = "" ) + theme(panel.background = element_blank()) + geom_text(stat = 'count', aes(label = ..count..), hjust = -2) 
                     + guides( fill = guide_legend(title = "1st, 2nd, or 3rd", title.hjust = 2)) + scale_fill_brewer(palette= 'Blues')
    
      
    )#close Render Plot
    
    #Top3 Draft Picks Graph
    output$Top3DraftPicks <- renderPlot(
        ggplot(g, aes(x=reorder(g$`Draft Pick`,g$`Draft Pick`, function(x)-length(x)))) + geom_bar(color = "blue", fill = "dodgerblue3") + labs(x ="",y="")
        +theme(axis.text.x = element_text(size = 12, angle = 90, family = 'calibri', face = 'bold'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
            geom_text(stat='count', aes(label = ..count..), vjust =-1, face = 'bold', family = 'calibri', size = 6) + scale_y_discrete()
        , width = 'auto', height = 'auto'
        
    )#closeRenderplot
    
    ################################# TAB #2 SERVER#####################################################################################
    
    #League Table
    #output$TotalRecord <- renderTable(select(AllStats,Owner, Wins, Losses, Ties,) %>% group_by(Owner) %>%summarise_all(funs(sum)) %>% arrange(desc(Wins)), align = 'c', width = 'auto', digits = 0)
    output$TotalRecord <- renderTable(totalRecordTableWithWin, align = 'c', digits = 0)
    
    output$Orig5Place <- renderPlot(
      ggplot(AllStats, aes(x=Place, y=Owner, fill= Owner)) + geom_joy(stat = 'binline',binwidth=1,scale=0.9) + theme_joy() + scale_fill_cyclical(values = c(" navy blue", "light blue"))+ 
        labs(x="Final Ranking")+ theme(axis.title.y = element_blank(), axis.title.x = element_text(family = 'Calibri', size = 12)) + scale_x_discrete(limits = seq(1,12,1))
        
     
        )#close renderplot
    
    output$WinsPerSeason <- renderPlot(
        ggplot(perSeasonDF, aes(x=perSeasonDF$Owner, y=perSeasonDF$Wins)) + geom_col()+ xlab("") + ylab("")
        
    )#close Renderplot

    
        #####Charts for TabBox######################
    
    output$PPGPlot <- renderPlot(
        ggplot(perGameDF, aes(x=perGameDF$Owner, y= perGameDF$Pts_For, label = perGameDF$Pts_For)) + geom_point(size =10, color = "blue" )+ geom_segment(aes(x=perGameDF$Owner, xend = perGameDF$Owner, y = 0, yend = perGameDF$Pts_For))
                +theme(panel.background = element_blank(), axis.title.y = element_text(margin = unit(c(0,25,0,0),"mm"), angle = 90)) +xlab("") + ylab("Pts Per Game") + geom_text()
        
    )#close Renderplot
    
    output$DiffGMPlot <- renderPlot (
            ggplot(perGameDF, aes(x = perGameDF$Owner, y = perGameDF$Pt_Diff)) + geom_col() + coord_flip()
            
        )#closeRenderPlot
    
    output$AvgFinPlot <- renderPlot (
        ggplot(perSeasonDF, aes(x = perSeasonDF$Owner, y = perSeasonDF$Place, label = perSeasonDF$Place)) + geom_point(stat = 'identity', size = 10) + ylim(0,12) + coord_flip()
        
    )#closeRenderPlot
    
    output$MovesYrPlot <- renderPlot(
        ggplot(perSeasonDF, aes(x =perSeasonDF$Owner, y = perSeasonDF$Moves )) + geom_col() +geom_text(stat = 'identity',aes(label = perSeasonDF$Moves))
        
    )# close Render Plot
    
    output$Top3Plot <- renderPlot(
        ggplot(countDF, aes(x =countDF$Owner, y = countDF$Playoffs )) + geom_col() + coord_flip()
        
    )# close Render Plot
    
    output$WinsByYear <- renderPlot(
    AllStats %>% filter(Owner == input$OwnerInput) %>% ggplot (aes(x =Year,y = Wins )) + geom_col()
        
    )# close Render plot
    
    output$RankByYear <- renderPlot(
        AllStats %>% filter(Owner == input$OwnerInput) %>% ggplot (aes(x =Year,y = Place )) + geom_col()
        
    )# close Render plot
    
    
    output$PPGByYear <- renderPlot(
        AllStats %>% filter(Owner == input$OwnerInput) %>% ggplot (aes(x =Year,y = Avg_Pts_For )) + geom_col()
        
    )# close Render plot
    
    
    }# Close Server Function


#BUILD THE APP *******************************************************************************************************************************************
shinyApp (ui,server)
