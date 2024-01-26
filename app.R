#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(chromote)
library(webshot2)
library(rvest)
library(lubridate)
library(rsconnect)
library(jsonlite)
library(dplyr)
library(stringr)
library(DT)

json_file <- "https://sheets.googleapis.com/v4/spreadsheets/1jPA-iftpPkg-91ST9ALzTtVJDqIv-ovyfSJaNHjpwtg/values/2022-23?alt=json&key=AIzaSyBGleujEZPzO5R3TWOhn7OdeE1jgrxby0k"

data <- fromJSON(json_file)

data <- data$values

df <- as.data.frame(matrix(ncol=3,nrow=length(data)))

for (i in 1:length(data)) {
  curr_row <- data[[i]]
  for (j in 1:length(curr_row)) {
    df[i,j] <- curr_row[j]
  }
}

colnames(df) <- df[1,]
df <- df[-c(1),]

df <- df %>% filter(Role!="Role") %>%
  mutate(Role=ifelse(Role=="Studetn","Student",Role))

gf <- df %>% mutate(`Notification date`=mdy(`Notification date`))

gf <- gf %>% mutate(`Role`=str_to_title(gf$`Role`))

# transpose of dataframe
transpose <- t(df)

# converting the result to dataframe
transpose <- as.data.frame(transpose)

# calculating reverse of dataframe
tf <- rev(transpose)

# transpose of reverse dataframe
tf <- t(tf)

# converting the result to dataframe
tf <- as.data.frame(tf)

#Building dates on campus
cf <- gf
cf <- cbind(cf, startDate=NA)
cf <- cbind(cf, endDate=NA)
colnames(cf) <- c("Notification","Role","Dates","startDate","endDate")
cf <- cf %>% filter(Dates != "none")
cf$Dates <- gsub(", ","-",cf$Dates)

testDf <- as.data.frame(strsplit(cf$Dates,"-"))
transpose <- t(testDf)
transpose <- as.data.frame(transpose)
testDf <- rev(transpose)
colnames(testDf) <- c("endDate","startDate")
cf$startDate <- testDf$startDate
cf$endDate <- testDf$endDate
cf$startDate <- mdy(cf$startDate)
cf$endDate <- mdy(cf$endDate)

for (i in 1:length(cf$Dates)) {
  if (!is.na(cf$startDate[i]) & !is.na(cf$endDate[i]) & !(cf$startDate[i] %within% interval(ydm("2000-01-01"),cf$endDate[i]))) {
    start <- cf$endDate[i]
    end <- cf$startDate[i]
    cf$startDate[i] <- start
    cf$endDate[i] <- end
  }
}

countDf <- data.frame(matrix(ncol = 1, nrow = 0))
roleDf <- data.frame(matrix(ncol = 1, nrow = 0))
for (i in 1:length(cf$Dates)) {
  if (!is.na(cf$startDate[i]) & !is.na(cf$endDate[i])) {
    tempDf <- as.data.frame(seq(cf$startDate[i],cf$endDate[i], by = 'days'))
    colnames(tempDf) <- c("Date")
    
    tempRoleDf <- as.data.frame(matrix(ncol = 1, nrow = length(tempDf$Date)))
    colnames(tempRoleDf) <- c("Role")
    tempRoleDf$Role <- cf$Role[i]
    
    countDf <- rbind(countDf,tempDf)
    roleDf <- rbind(roleDf,tempRoleDf)
  }
}

countDf <- cbind(countDf,Role=roleDf$Role)

end <- dim(countDf)[1]
countDf <- countDf[c(1:100),]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #dec4de;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }
        .well {
          min-height: 20px;
          padding: 19px;
          margin-bottom: 20px;
          background-color: #ffffff !important;
          border: none;
          border-radius: 4px;
          -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
          box-shadow: none;
          margin-top: 0px;
        }
        .h2, h2 {
          font-size: 30px;
          font-family: times;
        }
        p {
          margin: 0 0 10px;
          font-family: times;
        }
        .paginate_button {
          border-radius: 5px !important;
        }
        .paginate_button.current {
          color: yellow !important;
          border: 1px solid #333;
          background: #ffffff !important;
        }
        .dataTables_wrapper:hover .dataTables_paginate:hover .paginate_button:hover {
          color: #000000 !important;
          background: #ffffff;
          box-shadow: 0 1px 3px #666;
          border: 1px solid #333;
        }
    ')
  )),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("COVID-19 cases at Reed College by day, updated whenever Reed's official data is updated. For more information, see https://www.reed.edu/coronavirus/plan/case-notifications.html"),
      DT::dataTableOutput("table")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Cases by Dates on Campus"),
      plotOutput("campusPlot"),
      h2("Cases by Notification Date"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    ggplot(data=gf, aes(x=`Notification date`,fill=Role,colour=Role)) + 
      geom_histogram(binwidth = 5,stat="count") + 
      scale_x_date(date_labels="%b %d",date_breaks  ="1 month") + 
      xlab("Notification Date") + 
      ylab("Reported Cases") + 
      theme_bw()
  })
  
  output$campusPlot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    ggplot(data=countDf, aes(x=`Date`,fill=Role,colour=Role)) + 
      geom_histogram(binwidth = 5,stat="count") + 
      scale_x_date(date_labels="%b %d",date_breaks  ="2 weeks") + 
      xlab("Date on Campus") + 
      ylab("Reported Cases") + 
      theme_bw()
  })
  
  output$table <- DT::renderDataTable(tf,options = list(scrollX = TRUE, pageLength=10),
                                      rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
