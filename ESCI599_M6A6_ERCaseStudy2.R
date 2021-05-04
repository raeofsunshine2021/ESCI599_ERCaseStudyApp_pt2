# ESCI 599 Module 5: ER Case Study I
# Rachel Yonemura 

# I didn't do a super good job on this assignment, but I would love suggestions on how to: 
# Create forward and backward buttons
# Clean up the bottom of the tables (e.g. make the page numbers smaller)


# 4.8 Exercises 


# 3. Add an input control that lets the user decide how many rows to show in the summary tables.
# I used some code that we learned about a couple weeks ago to create an 
# input control to allow the user to specify the number of rows in each table. 
# Specifically, I used "dataTableOutput" instead of "tableOutput" in the UI 
# and I used "renderDataTable" instead of "renderTable" in the Server. I got 
# rid of the width argument and specify the options in the server. I also 
# cleaned the table up a little bit by adding "searching=FALSE" to get rid 
# of the search feature and added "lengthMenu" to select how many entries to 
# view (up to 6 entries and then all- the default is to show 3 as specified by pageLength).
# I tried to get rid of the paging to make it look less busy, but I wasn't successful. 
# Helpful links: 
# http://dev.sisalert.com.br/videira/map/shiny-examples/018-datatable-options/
# https://stackoverflow.com/questions/45509501/set-names-of-values-in-lengthmenu-page-length-menu-in-r-dt-datatable
# https://datatables.net/reference/option/lengthMenu
# https://mastering-shiny.org/basic-ui.html




# 4. Provide a way to step through every narrative systematically with forward and backward buttons. 
# Oof I tried to figure this out for a while last week and don't have the 
# brain power to work on it more. I'm excited to see how others were able to figure it out! 


#packages
library(shiny)
library(vroom) #"fast file reading" 
library(tidyverse)
library(shinythemes) #theme 

#data
injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("products.tsv")
population <- vroom::vroom("population.tsv")


#"shows product name in UI and returns product code to server"
prod_codes <- setNames(products$prod_code, products$title)

#4.5 polish tables 
# truncate tables
# convert variable to factor, order by frequency, lump all levels after top 5 together 
injuries %>% 
    mutate(diag=fct_lump(fct_infreq(diag), n=5))%>% 
    group_by(diag)%>% 
    summarise(n=as.integer(sum(weight)))
# automate for all variables using function - use this in server! 
count_top <- function(df, var, n=5){
    df %>% 
        mutate({{var}}:=fct_lump(fct_infreq({{var}}), n=n)) %>%
        group_by({{var}}) %>%
        summarise(n=as.integer(sum(weight)))
}




#UI
ui <- fluidPage(
    theme=shinytheme("flatly"),
    titlePanel("ER Injuries"),
    mainPanel("Data from National Electronic Injury Surveillance System (NEISS)"),
    fluidRow( #inputs
        column(8, #change 6 to 8 choose what to visualize 
               selectInput("code", "Product", 
                           choices=setNames(products$prod_code, products$title),#choices
                           width="100%"
               )
        ),
        column(2, selectInput("y", "Y axis", c("rate", "count"))) #to choose selection above
    ),
    fluidRow( #three tables with 4 columns 
        column(4, dataTableOutput("diag")), #make "dynamic" (not just tableOutput)
        column(4, dataTableOutput("body_part")),
        column(4, dataTableOutput("location"))
    ),
    fluidRow( #plot 
        column(12, plotOutput("age_sex"))
    ),
    fluidRow(
        column(2, actionButton("story", "Tell me a story")), #button at end of app 
        column(10, textOutput("narrative")) #narrative of case story 
    )
)


# Server 
server <- function(input, output, session){
    selected <- reactive(injuries %>% filter(prod_code==input$code)) 
    #data = reactive to use below 
    
    output$diag <- renderDataTable(  #diag table w/ count_top function - "dynamic table"
        count_top(selected(), diag), options=list(pageLength=3, #default page length 
                                                  searching=FALSE, #get rid of search bar
                                                  lengthMenu=list(c(1,2,3,4,5,6), #choose how many entries
                                                                  c('1','2','3','4','5','6','All')))) 
    
    output$body_part <- renderDataTable( #body part table w/ count_top function
        count_top(selected(), body_part), options=list(pageLength=3, 
                                                       searching=FALSE, 
                                                       lengthMenu=list(c(1,2,3,4,5,6), 
                                                                       c('1','2','3','4','5','6','All')))) 
    
    output$location <- renderDataTable( #location table w/ count_top function
        count_top(selected(), location), options=list(pageLength=3,  
                                                      searching=FALSE, 
                                                      lengthMenu=list(c(1,2,3,4,5,6), 
                                                                      c('1','2','3','4','5','6','All')))) 
    
    
    summary <- reactive({ #reactive summary of data to plot 
        selected () %>% 
            count(age, sex, wt=weight) %>% 
            left_join(population, by=c("age", "sex")) %>%
            mutate(rate=n/population*1e4)
    })
    
    output$age_sex <- renderPlot({ #plot w/ choice to plot count or rate
        if (input$y == "count"){
            summary () %>% 
                ggplot(aes(age, n, colour=sex)) +
                geom_line(size=1) +
                scale_colour_manual(values=c("violetred4","turquoise4"))+
                theme_dark()+
                labs(y="Estimated number of injuries")
        } else {
            summary () %>% 
                ggplot(aes(age, n, colour=sex)) +
                geom_line(na.rm=TRUE, size=1) + #for rate option
                scale_colour_manual(values=c("violetred4","turquoise4"))+
                theme_dark()+
                labs(y="Injuries per 10,000 people")
        }
    }, res=96)
    narrative_sample <- eventReactive(
        list(input$story, selected()),
        selected() %>% pull(narrative) %>% sample(1)
    )
    output$narrative <- renderText(narrative_sample())
}



#run app 
shinyApp(ui, server)
