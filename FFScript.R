library('shiny')
library('shinydashboard')
library('readxl')
library('ggplot2')
library('dplyr')
library('magrittr')

#Import Data Step ********************************************************************************************************************************************
League <- read_excel("FFData_Sep5.xlsx", sheet = "League")
Top3Place <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Place")
Top3Draft <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Draft")
AllStats <- read_excel("FFData_Sep5.xlsx", sheet = "All")

#get all #1 draft picks
g <- subset(Top3Draft,Top3Draft$`Draft Number`==1)

# Some PreProcessing ******************************************************************************************************************************************** 
AllStats$`Avg Pts For`<- formatC(AllStats$`Avg Pts For`,digits = 0, format = "d", big.mark = ",")
AllStats$`Avg Pts Against`<- formatC(AllStats$`Avg Pts Against`,digits = 0, format = "d", big.mark = ",")
AllStats$`Avg Pt Diff` <- formatC(AllStats$`Avg Pt Diff`,digits = 0, format = "d", big.mark = ",")
AllStats$`Win Percent` <- sprintf("%.0f %%",AllStats$`Win Percent`*100)
AllStats$`Pts For` <- formatC(AllStats$`Pts For`,digits = 0, format = "d", big.mark = ",")
AllStats$`Pts Against` <- formatC(AllStats$`Pts Against`,digits = 0, format = "d", big.mark = ",")

League$Year <- as.integer(League$Year)

Top3Draft$`Draft Number` <- as.integer(Top3Draft$`Draft Number`)


#################################################Create Add'l Tables###################################################################################
   #table for total record

  totalRecordTable <-  select(AllStats,Owner, Wins, Losses, Ties) %>% group_by(Owner) %>%summarise_all(funs(sum)) %>% arrange(desc(Wins)) %>% mutate('Win %' = sprintf("%.1f %%",totalRecordTable$Wins/(totalRecordTable$Wins + totalRecordTable$Losses + totalRecordTable$Ties)*100))
    #totalRecordTable$Wins<- formatC(totalRecordTable$Wins,digits = 0, format = "g")
    #totalRecordTable$Losses<- formatC(totalRecordTable$Losses,digits = 0, format = "d")
    #totalRecordTable$Ties<- formatC(totalRecordTable$Ties,digits = 0, format = "d")
    
    #totalRecordTable %>% mutate('Win %' = sprintf("%.1f %%",totalRecordTable$Wins/(totalRecordTable$Wins + totalRecordTable$Losses + totalRecordTable$Ties)*100))
    

#BUILD THE UI********************************************************************************************************************************************

#configure Header
dbHeader <- dashboardHeader(title = "13 Glorious Years of Fantasy Football", titleWidth = 450)


#configure Sidebar
dbSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("League Overview",tabName = 'db1', icon = icon("dashboard")),
    menuItem("The Original 5", tabName = 'db2', icon = icon("th"))
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
                          fluidRow(box(plotOutput('Top3Finishes'), title = '# of Top 3 Finishes', solidHeader = TRUE, status = 'primary', width = 6),
                                   box(plotOutput('Top3DraftPicks'), title ='Top Draft Picks since 2004', solidHeader = TRUE, status ='primary', width = 6, background = 'light-blue'))

               
                
                 )#closeFluidPage
            
            #FirstTabClose
            ),
    
    #SecondTab Open
    tabItem(tabName = 'db2',h2("The Original 5"),
            fluidPage(fluidRow(column(width = 6, box(tableOutput('TotalRecord'), title = 'Record since 2004', solidHeader = TRUE, status ='success' )),
                               column(width = 6,valueBox("20","TEST"))
                            
                
                
            )# close fluidRow
                
                
            )#close 2nd fluid page
            
            ) #SecondTabClose
    
  )
  
)


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
        ggplot(League, aes(x=reorder(League$`Winner (owner)`,League$`Winner (owner)`, function(x)-length(x)))) + geom_bar(color = "blue", fill = "gray") + labs(x ="",y="")
        +theme(axis.text.x = element_text(size = 16, angle = 45), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        geom_text(stat='count', aes(label = ..count..), vjust =-1) + scale_y_discrete()
        , width = 'auto', height = 'auto'
        
    )#closeRenderplot
    
    #Top3Finishes Graph
    output$Top3Finishes <- renderPlot(ggplot(Top3Place, aes(x = reorder(Top3Place$'Team Owner', Top3Place$'Team Owner', function(x) length(x)))) + 
                              geom_bar(aes(fill = as.factor(Top3Place$Place)))  + coord_flip()
                    + labs(x="", y = "" ) + theme(panel.background = element_blank()) + geom_text(stat = 'count', aes(label = ..count..), hjust = -2) 
                     + guides( fill = guide_legend(title = "1st, 2nd, or 3rd", title.hjust = 2))
    
      
    )#close Render Plot
    
    #Top3 Draft Picks Graph
    output$Top3DraftPicks <- renderPlot(
        ggplot(g, aes(x=reorder(g$`Draft Pick`,g$`Draft Pick`, function(x)-length(x)))) + geom_bar(color = "blue", fill = "gray") + labs(x ="",y="")
        +theme(axis.text.x = element_text(size = 12, angle = 45), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
            geom_text(stat='count', aes(label = ..count..), vjust =-1) + scale_y_discrete()
        , width = 'auto', height = 'auto'
        
    )#closeRenderplot
    
    ################################# TAB #2 SERVER#####################################################################################
    
    #League Table
    #output$TotalRecord <- renderTable(select(AllStats,Owner, Wins, Losses, Ties,) %>% group_by(Owner) %>%summarise_all(funs(sum)) %>% arrange(desc(Wins)), align = 'c', width = 'auto', digits = 0)
    output$TotalRecord <- renderTable(totalRecordTable, align = 'c', digits = 0)
    
}

#BUILD THE APP *******************************************************************************************************************************************
shinyApp (ui,server)
