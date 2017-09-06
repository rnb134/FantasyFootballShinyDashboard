library('shiny')
library('shinydashboard')
library('readxl')

#Import Data Step
League <- read_excel("FFData_Sep5.xlsx", sheet = "League")
Top3Place <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Place")
Top3Draft <- read_excel("FFData_Sep5.xlsx", sheet = "Top3Draft")
AllStats <- read_excel("FFData_Sep5.xlsx", sheet = "All")

# Some PreProcessing
AllStats$`Avg Pts For`<- as.integer(AllStats$`Avg Pts For`)
AllStats$`Avg Pts Against`<- as.integer(AllStats$`Avg Pts Against`)
AllStats$`Avg Pt Diff` <- as.integer(AllStats$`Avg Pt Diff`)

#configure Header
dbHeader <- dashboardHeader(title = "Header Title")


#configure Sidebar
dbSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",tabName = 'db1', icon = icon("dashboard")),
    menuItem("Widgets", tabName = 'db2', icon = icon("th"))
  )  
)

#configure body
dbBody <- dashboardBody(
  tabItems(
    tabItem(tabName ='db1',h1("1st Tab")),
    tabItem(tabName = 'db2',h2("new Text"))
    
  )
  
)


#pass elements into dashboardPage function and pass to UI Variable
ui <- dashboardPage(dbHeader,dbSidebar,dbBody)

#server Function
server <- function(input, output){
  
}

#build ShinyApp
shinyApp (ui,server)