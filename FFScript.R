library('shiny')
library('shinydashboard')
library('readxl')
library('ggplot2')

#Import Data Step ********************************************************************************************************************************************
League <- read_excel("FFData_Sep5.xlsx", sheet = "League")
Top3Place <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Place")
Top3Draft <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Draft")
AllStats <- read_excel("FFData_Sep5.xlsx", sheet = "All")

# Some PreProcessing ******************************************************************************************************************************************** 
AllStats$`Avg Pts For`<- as.integer(AllStats$`Avg Pts For`)
AllStats$`Avg Pts Against`<- as.integer(AllStats$`Avg Pts Against`)
AllStats$`Avg Pt Diff` <- as.integer(AllStats$`Avg Pt Diff`)
AllStats$`Win Percent` <- sprintf("%.0f %%",AllStats$`Win Percent`*100)
AllStats$`Pts For` <- formatC(AllStats$`Pts For`,digits = 0, format = "d", big.mark = ",")
AllStats$`Pts Against` <- formatC(AllStats$`Pts Against`,digits = 0, format = "d", big.mark = ",")

League$Year <- as.integer(League$Year)


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
                          fluidRow(box(plotOutput('Top3Finishes'), title = '# of Top 3 Finishes', solidHeader = TRUE, status = 'primary', width = 6))
                )#closeFluidPage
            
            #FirstTabClose
            ),
    
    #SecondTab Open
    tabItem(tabName = 'db2',h2("The Big 5")
            
            
            
           
            ) #SecondTabClose
    
  )
  
)


#pass elements into dashboardPage function and pass to UI Variable
ui <- dashboardPage(dbHeader,dbSidebar,dbBody)

#SERVER FUNCTION********************************************************************************************************************************************
server <- function(input, output){
    
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
}

#BUILD THE APP *******************************************************************************************************************************************
shinyApp (ui,server)
