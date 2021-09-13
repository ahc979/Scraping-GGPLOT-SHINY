library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
# library(rsconnect)

df <- read.csv('twitch/twitch_data_estream.csv', sep = ',', encoding = 'UTF-8')
df2 <- read.csv('twitch/twitch_data_estream2.csv', sep = ',',encoding = 'UTF-8')
df3 <- read.csv('twitch/twitch_data_estream3.csv', sep = ',', encoding = 'UTF-8')
df4 <- read.csv('twitch/twitch_data_estream4.csv', sep = ',', encoding = 'UTF-8')
df5 <- read.csv('twitch/twitch_data_estream5.csv', sep = ',', encoding = 'UTF-8')
df6 <- read.csv('twitch/twitch_data_estream6.csv', sep = ',', encoding = 'UTF-8')
df7 <- read.csv('twitch/twitch_data_estream7.csv', sep = ',', encoding = 'UTF-8')
df8 <- read.csv('twitch/twitch_data_estream8.csv', sep = ',', encoding = 'UTF-8')
df9 <- read.csv('twitch/twitch_data_estream9.csv', sep = ',', encoding = 'UTF-8')
df10 <- read.csv('twitch/twitch_data_estream10.csv', sep = ',', encoding = 'UTF-8')



ord = function (df){
  df$started_at <- gsub('Z','',df$started_at)
  df <- separate(df, started_at,c('date','time'),sep = 'T')
  return(df)}

df <- ord(df)
df2 <- ord(df2)
df3 <- ord(df3)
df4 <- ord(df4)
df5 <- ord(df5)
df6 <- ord(df6)
df7 <- ord(df7)
df8 <- ord(df8)
df9 <- ord(df9)
df10 <- ord(df10)

twitch_stream <- rbindlist(list(df4,df2,df3,df,df5,df6,df7,df8,df9,df10))

twitch_stream[,1] <- NULL

summary(twitch_stream)

twitch_stream$timelapse <- substr(twitch_stream$time,1,5)
twitch_stream$timelapse <- gsub(':','.',twitch_stream$timelapse)

twitch_stream$timelapse <- as.numeric(twitch_stream$timelapse)
twitch_stream$timelapse <- cut(twitch_stream$timelapse,breaks = c(00.00,4.00,8.00,12.00,16.00,20.00,23.59), 
                               labels = c('00:00-04:00','4:01-8:00','08:00-12:00','12:01-16:00','16:01-20:00','20:01-23:59'))

timelapse_group <- twitch_stream %>% group_by(timelapse,game_name,language) %>% 
  summarise(visitas = sum(viewer_count))



twitch_stream$user_name <- as.character(twitch_stream$user_name)



ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(
    title = 'Twitch'
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Streamer Stats', tabName = 'SS'),
      menuItem('Game Stats', tabName = 'GS')
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'SS',
              fluidRow(
                column(width = 8,
                       wellPanel(selectInput(inputId = 'streamer',
                                             label = 'Streamer',
                                             choices = c(as.character(twitch_stream$user_name)),
                                             multiple = FALSE))),
                column(width = 4,
                       wellPanel(actionButton(inputId = 'lobby',
                                              label = 'Lobby'))),
                
                column(width = 12,
                       plotOutput('hist2')),
                column(width = 12,
                       plotOutput('hist3'))
              )),
      
      
     tabItem(tabName = 'GS',
             fluidRow(
                column(width = 8,
                  wellPanel(sliderInput(inputId = 'timelapse',
                                    label=div(style='width:800px;', 
                                          div(style='float:left;', '00:00-04:00'), 
                                          div(style='float:right;', '20:01-23:59')),
                                    value= 2,
                                    min=0, 
                                    max=5,
                                    step = 1)),
                ),
                column(width = 4,
                       wellPanel(checkboxInput(inputId = 'languages',
                                                    label = 'Streamer Language'
                                                   ))
                       ),
  
              column(width = 12,
                     plotOutput('hist'))
             ))
      )


    )
  )


server <- function(input,output){
  
  languages_react <- reactive({input$languages})
  
  output$hist <- renderPlot({
    if(input$languages){
        
        if (input$timelapse == 0){
          sleeping_time <- 
         
           twitch_stream %>% filter((timelapse == '00:00-04:00')) %>% 
            group_by(game_name,language) %>%  summarise(visitas = sum(viewer_count))
          
          ggplot() + geom_bar(sleeping_time, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,fill= language
          ),stat = 'identity', position = 'dodge')+
            scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a'))+
            theme(line =element_line(color = 'black'),
                  axis.title = element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                  axis.line = element_line(colour = 'black'))+
                  labs(title="Game Stats (In Thousands)",
                      x ="Game", y = "Views")}
    
        else if (input$timelapse == 1){
          madrugada <-  twitch_stream %>% filter(timelapse == '4:01-8:00') %>% 
            group_by(game_name,language) %>%  summarise(visitas = sum(viewer_count))
          
          ggplot() + geom_bar(madrugada, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,fill= language
          ),stat = 'identity', position = 'dodge')+
            scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a'))+
            theme(line =element_line(color = 'black'),
                  axis.title = element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                  axis.line = element_line(colour = 'black'))+
                  labs(title="Game Stats (In Thousands)",
                 x ="Game", y = "Views")}
        
        else if (input$timelapse == 2){
          manana <-  twitch_stream %>% filter(timelapse == '08:00-12:00') %>% 
            group_by(game_name,language) %>%  summarise(visitas = sum(viewer_count))
          
          ggplot() + geom_bar(manana, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,fill= language
          ),stat = 'identity', position = 'dodge')+
            scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a'))+
            theme(line =element_line(color = 'black'),
                  axis.title = element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                  axis.line = element_line(colour = 'black'))+
                  labs(title="Game Stats (In Thousands)",
                 x ="Game", y = "Views")}
        
        else if (input$timelapse == 3){
          mediodia <-  twitch_stream %>% filter(timelapse == '12:01-16:00') %>% 
            group_by(game_name,language) %>%  summarise(visitas = sum(viewer_count))
          
          ggplot() + geom_bar(mediodia, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,fill= language
          ),stat = 'identity', position = 'dodge')+
            scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a'))+
            theme(line =element_line(color = 'black'),
                  axis.title= element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                  axis.line = element_line(colour = 'black'))+
                  labs(title="Game Stats (In Thousands)",
                 x ="Game", y = "Views")}
        
        else if (input$timelapse == 4){
          tarde <-  twitch_stream %>% filter(timelapse == '16:01-20:00') %>% 
            group_by(game_name, language) %>%  summarise(visitas = sum(viewer_count))
          
          ggplot() + geom_bar(tarde, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,fill= language
          ),stat = 'identity', position = 'dodge')+
            scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a'))+
            theme(line =element_line(color = 'black'),
                  axis.title= element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                  axis.line = element_line(colour = 'black'))+
                  labs(title="Game Stats (In Thousands)",
                  x ="Game", y = "Views")}
        
        else if (input$timelapse == 5){
          noche <-  twitch_stream %>% filter(timelapse == '20:01-23:59') %>% 
            group_by(game_name, language) %>%  summarise(visitas = sum(viewer_count))
          
          ggplot() + geom_bar(noche, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,fill= language
          ),stat = 'identity', position = 'dodge')+
            labs(title="Game Stats",
                 x ="Game", y = "Views")+
            scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a'))+
            theme(line =element_line(color = 'black'),
                  axis.title= element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                  axis.line = element_line(colour = 'black'))+
                  labs(title="Game Stats (In Thousands)",
                  x ="Game", y = "Views")}}
    
    else{
      if (input$timelapse == 0){
        sleeping_time <- 
          twitch_stream %>% filter((timelapse == '00:00-04:00')) %>% 
          group_by(game_name,language) %>%  summarise(visitas = sum(viewer_count))
        
        ggplot() + geom_bar(sleeping_time, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000
        ),stat = 'identity', fill="#00344C")+
          theme(line =element_line(color = 'black'),
                axis.title = element_text(size = rel(1.5)),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                panel.background = element_rect(fill='white'),
                panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                axis.line = element_line(colour = 'black'))+
                labs(title="Game Stats (In Thousands)",
               x ="Game", y = "Views")}
      
      else if (input$timelapse == 1){
        madrugada <-  twitch_stream %>% filter(timelapse == '4:01-8:00') %>% 
          group_by(game_name) %>%  summarise(visitas = sum(viewer_count))
        
        ggplot() + geom_bar(madrugada, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,
        ),stat = 'identity', fill='#366a85')+
          theme(line =element_line(color = 'black'),
                axis.title = element_text(size = rel(1.5)),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                panel.background = element_rect(fill='white'),
                panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                axis.line = element_line(colour = 'black'))+
                labs(title="Game Stats (In Thousands)",
               x ="Game", y = "Views")}
      
      else if (input$timelapse == 2){
        manana <-  twitch_stream %>% filter(timelapse == '08:00-12:00') %>% 
          group_by(game_name) %>%  summarise(visitas = sum(viewer_count))
        
        ggplot() + geom_bar(manana, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,
        ),stat = 'identity', fill='#87CEEB')+
          theme(line =element_line(color = 'black'),
                axis.title = element_text(size = rel(1.5)),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                panel.background = element_rect(fill='white'),
                panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                axis.line = element_line(colour = 'black'))+
                labs(title="Game Stats (In Thousands)",
               x ="Game", y = "Views")}
      
      else if (input$timelapse == 3){
        mediodia <-  twitch_stream %>% filter(timelapse == '12:01-16:00') %>% 
          group_by(game_name) %>%  summarise(visitas = sum(viewer_count))
        
        ggplot() + geom_bar(mediodia, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,
        ),stat = 'identity', fill='#f39f18')+
          theme(line =element_line(color = 'black'),
                axis.title= element_text(size = rel(1.5)),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                panel.background = element_rect(fill='white'),
                panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                axis.line = element_line(colour = 'black'))+
                labs(title="Game Stats (In Thousands)",
               x ="Game", y = "Views")}
      
      else if (input$timelapse == 4){
        tarde <-  twitch_stream %>% filter(timelapse == '16:01-20:00') %>% 
          group_by(game_name) %>%  summarise(visitas = sum(viewer_count))
        
        ggplot() + geom_bar(tarde, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,
        ),stat = 'identity',fill='#ebaa76')+
          theme(line =element_line(color = 'black'),
                axis.title= element_text(size = rel(1.5)),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                panel.background = element_rect(fill='white'),
                panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                axis.line = element_line(colour = 'black'))+
                labs(title="Game Stats (In Thousands)",
               x ="Game", y = "Views")}
      
      else if (input$timelapse == 5){
        noche <-  twitch_stream %>% filter(timelapse == '20:01-23:59') %>% 
          group_by(game_name) %>%  summarise(visitas = sum(viewer_count))
        
        ggplot() + geom_bar(noche, mapping = aes(x= reorder(game_name,-visitas/1000), y= visitas/1000,
        ),stat = 'identity', fill='#473a6d')+
          theme(line =element_line(color = 'black'),
                axis.title= element_text(size = rel(1.5)),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                panel.background = element_rect(fill='white'),
                panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
                axis.line = element_line(colour = 'black'))+
                labs(title="Game Stats (In Thousands)",
               x ="Game", y = "Views")}
      
    }
        
      
    })
  

  output$hist2 <- renderPlot({
    
    if(input$streamer %in% twitch_stream$user_name){
      streamer <- twitch_stream %>% filter(twitch_stream$user_name == input$streamer) %>%
        group_by(game_name) %>% summarise(viewers = sum(viewer_count))

      ggplot() + geom_bar(streamer, mapping = aes(x= reorder(game_name,-viewers/1000), y= viewers/1000
      ),stat = 'identity',fill = "#60397f")+
        theme(line =element_line(color = 'black'),
              panel.background = element_rect(fill='white'),
              panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
              axis.line = element_line(colour = 'black'))+
        labs(title="Streamer Stats (In Thousands)",
             x ="Game", y = "Views")
    }
    else {
      ggplot() + geom_bar(twitch_stream, mapping = aes(x= reorder(user_name,-viewer_count/1000), y= viewer_count/1000, fill = language
    ),stat = 'identity')+
        scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a','#880E4F','#EF9A9A'))+
        theme(line =element_line(color = 'black'),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              panel.background = element_rect(fill='white'),
              panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
              axis.line = element_line(colour = 'black'))+
        labs(title="Game Stats (In Thousands)",
             x ="Streamer", y = "Views")
      
    }
    
  })
  
  output$hist3 <- renderPlot({
    
    
    if (input$lobby){
      ggplot() + geom_bar(twitch_stream, mapping = aes(x= reorder(user_name,-viewer_count/1000), y= viewer_count/1000, fill = language
      ),stat = 'identity')+
        scale_fill_manual(values = c('#ad66e5', "#864fb2","#60397f",'#3a224c','#130b19','#F5E1FD','#a30f9d','#401e70','#43189a','#880E4F','#EF9A9A'))+
        theme(line =element_line(color = 'black'),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              panel.background = element_rect(fill='white'),
              panel.grid.major.y = element_line(colour = 'black', linetype = 'dotted' ),
              axis.line = element_line(colour = 'black'))+
        labs(title="Game Stats (In Thousands)",
             x ="Streamer", y = "Views")
      
    }
  })
  
  }

shinyApp(ui = ui, server = server)
