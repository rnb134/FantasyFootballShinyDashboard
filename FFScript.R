library('shiny')
library('shinydashboard')

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