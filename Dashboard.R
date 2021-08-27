####################### Libraries  ################################
library(shiny)
library(shinythemes)
library(datasets)
library(dplyr)
library(plotrix)
library(ggplot2)
##################################################################

################### Data #######################################
vs2018 <- read.csv("vvs18.csv")#area
vs2013 <- read.csv("vvs13.csv")#area
ls2014 <- read.csv("lls14.csv")#area
ls2019 <- read.csv("lls19.csv")#area
data1 <- read.csv("surnames109.csv")#surname
##################################################################

############## Updating data ####################################
'stating_booth_no <- c(3,25,9,56,58,30)
end_booth_no <- c(4,25,13,57,62,39)'
stating_booth_no <- c(3,6,9,22,23,25,56,27,28,30,47,51,53,58,63,78,88,89,93,99,107,111,112,114,123,126,128,129,132,134,148,152,153,155,157,160,164,165,169,173,189,199,202,203,207,228,229,232,236,241,246,254)
end_booth_no <- c(4,7,13,22,23,25,57,27,28,39,50,51,55,62,77,81,88,90,93,99,108,111,113,114,125,127,128,129,132,137,148,152,154,156,158,161,164,165,169,174,190,199,202,205,220,228,229,233,238,242,247,258)
vs2013$booth_start <- stating_booth_no
vs2013$booth_end <- end_booth_no
vs2018$booth_start <- stating_booth_no
vs2018$booth_end <- end_booth_no
ls2014$booth_start <- stating_booth_no
ls2014$booth_end <- end_booth_no
ls2019$booth_start <- stating_booth_no
ls2019$booth_end <- end_booth_no
##################################################################


ui <- fluidPage(    
  
  titlePanel("Mandawa Area Wise Summary"),
  
  navbarPage("Navbar!!",
             tabPanel("Area Analysis",
                      
                      sidebarLayout(      
                        sidebarPanel(
                          selectInput("area", "Region:", 
                                      choices = c(ls2014[,1])#Area in mandawa
                          ),
                          radioButtons("radio","Select Election",
                                       c("Lok Sabha"=1,"Vidhan Sabha"=2))
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Surname",plotOutput("Plot1")),
                            tabPanel("Form 20",plotOutput("Plot2"),plotOutput("Plot3")),
                            tabPanel("Summary",plotOutput("Plot11"),column(6,plotOutput("Plot4")),column(6,plotOutput("Plot5")),plotOutput("Plot6"))
                          )
                        )
                      )
                      
                      
                      ),
             tabPanel("Area Comparison",
                             sidebarLayout( 
                               
                               sidebarPanel(
                                 selectInput("area1", "Region 1:", 
                                             choices = c(ls2014[,1])#Area in mandawa
                                 ),
                                 radioButtons("radio1","Select Election",
                                              c("Lok Sabha"=1,"Vidhan Sabha"=2)),
                                 selectInput("area2", "Region 2:", 
                                             choices = c(ls2014[,1])#Area in mandawa
                                 ),
                                 radioButtons("radio2","Select Election",
                                              c("Lok Sabha"=1,"Vidhan Sabha"=2))
                               ,width = 2),
                                     mainPanel(
                                      
                                       
                                       column(6,
                                              
                                              tabsetPanel(
                                                tabPanel("Surname",plotOutput("Plot12")),
                                                tabPanel("Form 20",plotOutput("Plot21"),plotOutput("Plot31")),
                                                tabPanel("Summary",plotOutput("Plot13"),column(6,plotOutput("Plot41")),column(6,plotOutput("Plot51")),plotOutput("Plot61"))
                                              )
                                              
                                              ),
                                       
                                       column(6,
                                              tabsetPanel(
                                                tabPanel("Surname",plotOutput("Plot14")),
                                                tabPanel("Form 20",plotOutput("Plot22"),plotOutput("Plot32")),
                                                tabPanel("Summary",plotOutput("Plot15"),column(6,plotOutput("Plot42")),column(6,plotOutput("Plot52")),plotOutput("Plot62"))
                                              )
                                              
                                              )
                                     ,width = 10)
                             )
                      )
             ),
)



server <- function(input, output) {
  
################################################## Plot1(surname) ########################################################################
  output$Plot11 <- output$Plot1 <- renderPlot({
    row <- which(ls2014$Area == input$area)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(6,4,4,4))
    area_list <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    sorted_table <- sort(table((area_list$categ)),decreasing = TRUE)
    x<-barplot(sorted_table,ylim=c(0,5/4*as.numeric(sorted_table)[1]),las=2,col=rainbow(10),main=paste(input$area,"Surname",sep = " "))
    text(x = x, y = c(as.numeric(as.numeric(sorted_table))), label = c(as.numeric(as.numeric(sorted_table))), pos = 3, col = "black")
  })
  output$Plot12 <- output$Plot13 <- renderPlot({
    row <- which(ls2014$Area == input$area1)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(6,4,4,4))
    area_list <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    sorted_table <- sort(table((area_list$categ)),decreasing = TRUE)
    x<-barplot(sorted_table,ylim=c(0,5/4*as.numeric(sorted_table)[1]),las=2,col=rainbow(10),main=paste(input$area1,"Surname",sep = " "))
    text(x = x, y = c(as.numeric(as.numeric(sorted_table))), label = c(as.numeric(as.numeric(sorted_table))), pos = 3, col = "black")
  })
  output$Plot14 <- output$Plot15 <- renderPlot({
    row <- which(ls2014$Area == input$area2)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(6,4,4,4))
    area_list <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    sorted_table <- sort(table((area_list$categ)),decreasing = TRUE)
    x<-barplot(sorted_table,ylim=c(0,5/4*as.numeric(sorted_table)[1]),las=2,col=rainbow(10),main=paste(input$area2,"Surname",sep = " "))
    text(x = x, y = c(as.numeric(as.numeric(sorted_table))), label = c(as.numeric(as.numeric(sorted_table))), pos = 3, col = "black")
  })
  
##################################################################################################################################
  
 
############################################# Plot2(form20-13,14) ################################################################################
  output$Plot2 <- renderPlot({
    if(input$radio==1){
      data <- ls2014
      name = paste(input$area,"Lok Sabha 2014",sep = " ")
    }else if(input$radio==2){
      data <- vs2013
      name = paste(input$area,"Vidhan Sabha 2013",sep = " ")
    }
    
    row <- which(data$Area == input$area)
    
    a<-2
    b<-length(colnames(data))-4
    
    names <- strsplit(colnames(data)[a:b],"\\.")
    par(mar=c(11,4,4,4))
    
    lbs<-c()
    t<-1
    for(i in names){
      lbs[t]<-paste(paste(i[1],i[2],sep = " "), paste("(",i[length(i)],")"),sep = " ")
      t<-t+1
    }
    lbs[t]<-"NOTA"
   
    lbs1<-c()
    t<-1
    b<-b+1
   
    for(i in a:b){
      lbs1[t]<-paste(as.numeric(data[row,i]),paste(round(as.numeric(data[row,i])/as.numeric(data[row,b+1])*100,1),"%",sep=""),sep = "\n")
      
      t<-t+1
    }
   
    barData = as.numeric(data[row,a:b])
    x<-barplot(barData,names.arg = lbs,main = name,ylim=c(0,max(c(as.numeric(data[row,a:b])))*5/4),las = 2,col = c("orange","green","blue","red","yellow","purple","pink","brown","purple","purple","purple","purple","purple","purple","purple"))#,cex.names=.8
    text(x = x, y = c(as.numeric(data[row,a:b])), label = lbs1, pos = 3, col = "black")#c(as.numeric(data[row,a:b]))
  })
  
  
  output$Plot21 <- renderPlot({
    if(input$radio1==1){
      data <- ls2014
      name = paste(input$area1,"Lok Sabha 2014",sep = " ")
    }else if(input$radio1==2){
      data <- vs2013
      name = paste(input$area1,"Vidhan Sabha 2013",sep = " ")
    }
    
    row <- which(data$Area == input$area1)
    
    a<-2
    b<-length(colnames(data))-4
    
    names <- strsplit(colnames(data)[a:b],"\\.")
    par(mar=c(11,4,4,4))
    
    lbs<-c()
    t<-1
    for(i in names){
      lbs[t]<-paste(paste(i[1],i[2],sep = " "), paste("(",i[length(i)],")"),sep = " ")
      t<-t+1
    }
    lbs[t]<-"NOTA"
   
    lbs1<-c()
    t<-1
    b<-b+1
   
    for(i in a:b){
      lbs1[t]<-paste(as.numeric(data[row,i]),paste(round(as.numeric(data[row,i])/as.numeric(data[row,b+1])*100,1),"%",sep=""),sep = "\n")
      
      t<-t+1
    }
   
    barData = as.numeric(data[row,a:b])
    x<-barplot(barData,names.arg = lbs,main = name,ylim=c(0,max(c(as.numeric(data[row,a:b])))*5/4),las = 2,col = c("orange","green","blue","red","yellow","purple","pink","brown","purple","purple","purple","purple","purple","purple","purple"))#,cex.names=.8
    text(x = x, y = c(as.numeric(data[row,a:b])), label = lbs1, pos = 3, col = "black")#c(as.numeric(data[row,a:b]))
  })
  
  
  output$Plot22 <- renderPlot({
    if(input$radio2==1){
      data <- ls2014
      name = paste(input$area2,"Lok Sabha 2014",sep = " ")
    }else if(input$radio2==2){
      data <- vs2013
      name = paste(input$area2,"Vidhan Sabha 2013",sep = " ")
    }
    
    row <- which(data$Area == input$area2)
    
    a<-2
    b<-length(colnames(data))-4
    
    names <- strsplit(colnames(data)[a:b],"\\.")
    par(mar=c(11,4,4,4))
    
    lbs<-c()
    t<-1
    for(i in names){
      lbs[t]<-paste(paste(i[1],i[2],sep = " "), paste("(",i[length(i)],")"),sep = " ")
      t<-t+1
    }
    lbs[t]<-"NOTA"
   
    lbs1<-c()
    t<-1
    b<-b+1
   
    for(i in a:b){
      lbs1[t]<-paste(as.numeric(data[row,i]),paste(round(as.numeric(data[row,i])/as.numeric(data[row,b+1])*100,1),"%",sep=""),sep = "\n")
      
      t<-t+1
    }
   
    barData = as.numeric(data[row,a:b])
    x<-barplot(barData,names.arg = lbs,main = name,ylim=c(0,max(c(as.numeric(data[row,a:b])))*5/4),las = 2,col = c("orange","green","blue","red","yellow","purple","pink","brown","purple","purple","purple","purple","purple","purple","purple"))#,cex.names=.8
    text(x = x, y = c(as.numeric(data[row,a:b])), label = lbs1, pos = 3, col = "black")#c(as.numeric(data[row,a:b]))
  })
  
######################################################################################################################################
 
################################################# Plot3 (form20 -19,18)############################################################################ 
  output$Plot3 <- renderPlot({
    if(input$radio==1){
      data <- ls2019
      name = paste(input$area,"Lok Sabha 2019",sep = " ")
    }else if(input$radio==2){
      data <- vs2018
      name = paste(input$area,"Vidhan Sabha 2018",sep = " ")
    }
    
    row <- which(data$Area == input$area)
    a<-2
    b<-length(colnames(data))-4
   
    names <- strsplit(colnames(data)[a:b],"\\.")
    par(mar=c(11,4,4,4))
    
    lbs<-c()
    t<-1
    for(i in names){
      lbs[t]<-paste(paste(i[1],i[2]," "), paste("(",i[length(i)],")"),sep = " ")
      t<-t+1
    }
    lbs[t]<-"NOTA"
    
    lbs1<-c()
    t<-1
    b<-b+1
    for(i in a:b){
      lbs1[t]<-paste(as.numeric(data[row,i]),paste(round(as.numeric(data[row,i])/as.numeric(data[row,b+1])*100,1),"%",sep=""),sep = "\n")
      t<-t+1
    }
    
    barData = as.numeric(data[row,a:b])
    x<-barplot(barData,names.arg = lbs,main = name,ylim=c(0,max(c(as.numeric(data[row,a:b])))*5/4),las = 2,col = c("orange","green","blue","red","yellow","purple","pink","brown","purple","purple","purple","purple","purple","purple","purple"))#,cex.names=.8
    text(x = x, y = c(as.numeric(data[row,a:b])), label = lbs1, pos = 3, col = "black")#cex = 0.8
  })
  
  
  output$Plot31 <- renderPlot({
    if(input$radio1==1){
      data <- ls2019
      name = paste(input$area1,"Lok Sabha 2019",sep = " ")
    }else if(input$radio1==2){
      data <- vs2018
      name = paste(input$area1,"Vidhan Sabha 2018",sep = " ")
    }
    
    row <- which(data$Area == input$area1)
    a<-2
    b<-length(colnames(data))-4
   
    names <- strsplit(colnames(data)[a:b],"\\.")
    par(mar=c(11,4,4,4))
    
    lbs<-c()
    t<-1
    for(i in names){
      lbs[t]<-paste(paste(i[1],i[2]," "), paste("(",i[length(i)],")"),sep = " ")
      t<-t+1
    }
    lbs[t]<-"NOTA"
    
    lbs1<-c()
    t<-1
    b<-b+1
    for(i in a:b){
      lbs1[t]<-paste(as.numeric(data[row,i]),paste(round(as.numeric(data[row,i])/as.numeric(data[row,b+1])*100,1),"%",sep=""),sep = "\n")
      t<-t+1
    }
    
    barData = as.numeric(data[row,a:b])
    x<-barplot(barData,names.arg = lbs,main = name,ylim=c(0,max(c(as.numeric(data[row,a:b])))*5/4),las = 2,col = c("orange","green","blue","red","yellow","purple","pink","brown","purple","purple","purple","purple","purple","purple","purple"))#,cex.names=.8
    text(x = x, y = c(as.numeric(data[row,a:b])), label = lbs1, pos = 3, col = "black")#cex = 0.8
  })
  
  
  output$Plot32 <- renderPlot({
    if(input$radio2==1){
      data <- ls2019
      name = paste(input$area2,"Lok Sabha 2019",sep = " ")
    }else if(input$radio2==2){
      data <- vs2018
      name = paste(input$area2,"Vidhan Sabha 2018",sep = " ")
    }
    
    row <- which(data$Area == input$area2)
    a<-2
    b<-length(colnames(data))-4
   
    names <- strsplit(colnames(data)[a:b],"\\.")
    par(mar=c(11,4,4,4))
    
    lbs<-c()
    t<-1
    for(i in names){
      lbs[t]<-paste(paste(i[1],i[2]," "), paste("(",i[length(i)],")"),sep = " ")
      t<-t+1
    }
    lbs[t]<-"NOTA"
    
    lbs1<-c()
    t<-1
    b<-b+1
    for(i in a:b){
      lbs1[t]<-paste(as.numeric(data[row,i]),paste(round(as.numeric(data[row,i])/as.numeric(data[row,b+1])*100,1),"%",sep=""),sep = "\n")
      t<-t+1
    }
    
    barData = as.numeric(data[row,a:b])
    x<-barplot(barData,names.arg = lbs,main = name,ylim=c(0,max(c(as.numeric(data[row,a:b])))*5/4),las = 2,col = c("orange","green","blue","red","yellow","purple","pink","brown","purple","purple","purple","purple","purple","purple","purple"))#,cex.names=.8
    text(x = x, y = c(as.numeric(data[row,a:b])), label = lbs1, pos = 3, col = "black")#cex = 0.8
  })

########################################################################################################################################
  
  
######################################################## Plot4 (voter turnout)######################################################################
  output$Plot4 <-renderPlot({
    row <- which(ls2014$Area == input$area)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(6,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    
    male <-0
    female <-0
    for(i in kul$sex){
      if(i=="M"){
        male<-male+1
      }else if(i=="F"){
        female <-female+1
      }
    }
    
    
    voters <- nrow(kul)
    v13 <- vs2013$Total[row]
    l14 <- ls2014$Total[row]
    v18 <- vs2018$Total[row]
    l19 <- ls2019$Total[row]
    par(mar=c(11,4,4,4))
    
    if(input$radio==1){
      turnout<-c(voters,l14,l19)
      lbs <- c("Total Voters 2019","Lok Sabha 2014","Lok Sabha 2019")
    }else{
      turnout<-c(voters,v13,v18)
      lbs <- c("Total Voters 2019","Vidhan Sabha 2013","Vidhan Sabha 2018")
    }
    
    xx <- barplot(turnout,names.arg=lbs,ylim=c(0,5/4*voters),las=2,col=rainbow(10),main="Voter Turnout")
    text(x = xx, y = turnout, label = turnout, pos = 3, col = "black")
    
  })
  
  
  output$Plot41 <-renderPlot({
    row <- which(ls2014$Area == input$area1)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(6,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    
    male <-0
    female <-0
    for(i in kul$sex){
      if(i=="M"){
        male<-male+1
      }else if(i=="F"){
        female <-female+1
      }
    }
    
    
    voters <- nrow(kul)
    v13 <- vs2013$Total[row]
    l14 <- ls2014$Total[row]
    v18 <- vs2018$Total[row]
    l19 <- ls2019$Total[row]
    par(mar=c(11,4,4,4))
    
    if(input$radio==1){
      turnout<-c(voters,l14,l19)
      lbs <- c("Total Voters 2019","Lok Sabha 2014","Lok Sabha 2019")
    }else{
      turnout<-c(voters,v13,v18)
      lbs <- c("Total Voters 2019","Vidhan Sabha 2013","Vidhan Sabha 2018")
    }
    
    xx <- barplot(turnout,names.arg=lbs,ylim=c(0,5/4*voters),las=2,col=rainbow(10),main="Voter Turnout")
    text(x = xx, y = turnout, label = turnout, pos = 3, col = "black")
    
  })
  output$Plot42 <-renderPlot({
    row <- which(ls2014$Area == input$area2)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(6,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    
    male <-0
    female <-0
    for(i in kul$sex){
      if(i=="M"){
        male<-male+1
      }else if(i=="F"){
        female <-female+1
      }
    }
    
    
    voters <- nrow(kul)
    v13 <- vs2013$Total[row]
    l14 <- ls2014$Total[row]
    v18 <- vs2018$Total[row]
    l19 <- ls2019$Total[row]
    par(mar=c(11,4,4,4))
    
    if(input$radio==1){
      turnout<-c(voters,l14,l19)
      lbs <- c("Total Voters 2019","Lok Sabha 2014","Lok Sabha 2019")
    }else{
      turnout<-c(voters,v13,v18)
      lbs <- c("Total Voters 2019","Vidhan Sabha 2013","Vidhan Sabha 2018")
    }
    
    xx <- barplot(turnout,names.arg=lbs,ylim=c(0,5/4*voters),las=2,col=rainbow(10),main="Voter Turnout")
    text(x = xx, y = turnout, label = turnout, pos = 3, col = "black")
    
  })
  
######################################################################################################################################
  
##################################################### Plot5(male female percentage) ########################################################################
  
  output$Plot5 <-renderPlot({
    row <- which(ls2014$Area == input$area)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    #par(mar=c(6,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    
    male <-0
    female <-0
    for(i in kul$sex){
      if(i=="M"){
        male<-male+1
      }else if(i=="F"){
        female <-female+1
      }
    }
    lbs <- c(paste("Male",paste(round(male/nrow(kul)*100,1),"%"),sep = " - "),paste("Female",paste(round(female/nrow(kul)*100,1),"%"),sep = " - "))
    pie3D(c(male,female),labels=lbs,explode=0.1,main="Male and Female percentage")
    
  })
  output$Plot51 <-renderPlot({
    row <- which(ls2014$Area == input$area1)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    #par(mar=c(6,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    
    male <-0
    female <-0
    for(i in kul$sex){
      if(i=="M"){
        male<-male+1
      }else if(i=="F"){
        female <-female+1
      }
    }
    lbs <- c(paste("Male",paste(round(male/nrow(kul)*100,1),"%"),sep = " - "),paste("Female",paste(round(female/nrow(kul)*100,1),"%"),sep = " - "))
    pie3D(c(male,female),labels=lbs,explode=0.1,main="Male and Female percentage")
    
  })
  
  
  output$Plot52 <-renderPlot({
    row <- which(ls2014$Area == input$area2)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    #par(mar=c(6,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    
    male <-0
    female <-0
    for(i in kul$sex){
      if(i=="M"){
        male<-male+1
      }else if(i=="F"){
        female <-female+1
      }
    }
    lbs <- c(paste("Male",paste(round(male/nrow(kul)*100,1),"%"),sep = " - "),paste("Female",paste(round(female/nrow(kul)*100,1),"%"),sep = " - "))
    pie3D(c(male,female),labels=lbs,explode=0.1,main="Male and Female percentage")
    
  })

########################################################################################################################################
  
################################################### Plot6(age wise distributions) ############################################################################
  output$Plot6 <-renderPlot({
    row <- which(ls2014$Area == input$area)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(11,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    Age_Group<-cut(kul$AGE,breaks = c(17,22,30,50,80,200),labels = c("New Voters","23-30","31-50","51-80",">80"))
    xx<-barplot(table(Age_Group),las=2,ylim=c(0,5/4*max(table(Age_Group))),col=rainbow(10),main="Age wise Voter distribution")
    text(x = xx, y = as.numeric(table(Age_Group)), label = as.numeric(table(Age_Group)), pos = 3, col = "black")
    
  })
  
  output$Plot61 <-renderPlot({
    row <- which(ls2014$Area == input$area1)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(11,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    Age_Group<-cut(kul$AGE,breaks = c(17,22,30,50,80,200),labels = c("New Voters","23-30","31-50","51-80",">80"))
    xx<-barplot(table(Age_Group),las=2,ylim=c(0,5/4*max(table(Age_Group))),col=rainbow(10),main="Age wise Voter distribution")
    text(x = xx, y = as.numeric(table(Age_Group)), label = as.numeric(table(Age_Group)), pos = 3, col = "black")
    
  })
  
  output$Plot62 <-renderPlot({
    row <- which(ls2014$Area == input$area2)
    start1 = ls2014$booth_start[row]
    end1 =  ls2014$booth_end[row]
    par(mar=c(11,4,4,4))
    kul <- data1 %>% slice(which(data1$bhno==start1)[1]:which(data1$bhno==end1)[length(which(data1$bhno==end1))])
    Age_Group<-cut(kul$AGE,breaks = c(17,22,30,50,80,200),labels = c("New Voters","23-30","31-50","51-80",">80"))
    xx<-barplot(table(Age_Group),las=2,ylim=c(0,5/4*max(table(Age_Group))),col=rainbow(10),main="Age wise Voter distribution")
    text(x = xx, y = as.numeric(table(Age_Group)), label = as.numeric(table(Age_Group)), pos = 3, col = "black")
    
  })
  
########################################################################################################################################
  
}

shinyApp(ui = ui, server = server)