dependencies <- c('shiny', 'ggplot2', 'png', 'ggvis', 'plotly', 'RCurl')
new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages, repos="http://cran.rstudio.com/")}

library('shiny')
library('ggplot2')
library('png')
library('ggvis')
library('plotly')
library('RCurl')
library('stringr')

##### Preprocessing Data #####
df <- read.csv('data/dodgers.csv', stringsAsFactors = FALSE)

# Modify pitch types
df[df$pitch.pitch_type %in% c('IN', 'EP', 'KC') | is.na(df$pitch.pitch_type), 'pitch.pitch_type'] <- 'Other'
df[df$pitch.pitch_type %in% c('FC', 'FF', 'FS', 'FT'), 'pitch.pitch_type'] <- 'Fastball'
df[df$pitch.pitch_type == 'CH', 'pitch.pitch_type'] <- 'Changeup'
df[df$pitch.pitch_type == 'CU', 'pitch.pitch_type'] <- 'Curve'
df[df$pitch.pitch_type == 'SI', 'pitch.pitch_type'] <- 'Sinker'
df[df$pitch.pitch_type == 'SL', 'pitch.pitch_type'] <- 'Slider'

df$result <- factor(df$result, ordered=TRUE,
                    levels <- c('Single', 'Double', 'Triple', 'Home Run', 'Out'))

teams <- list(`National League` = levels(factor(df[which(df$home_lg == "NL"), 'Home.Team'])),
              `American League` = levels(factor(df[which(df$home_lg == "AL"), 'Home.Team'])))

result_colors <- c('#bdd7e7','#6baed6','#3182bd','#08519c', 'black')
names(result_colors) <- levels(df$result)


num_bases <- function(event){
  if (event == "Single") {
    return(1)
  } else if (event == "Double") {
    return(2)
  } else if (event == "Triple") {
    return(3)
  } else if (event == "Home Run") {
    return(4)
  } else {
    return(0)
  }
}

df['numBases'] <- sapply(df$event, num_bases)
df['month'] <- sapply(strsplit(df$gameId, split='_'), function(x) {strtoi(x[3], base=10)})

df_lad <- df[df$field_teamId != 119,]

calc_slg <- function(n_base, isAB){
  return (sum(n_base) / sum(isAB))
}
calc_ba <- function(isHit, isAB){
  return (sum(isHit) / sum(isAB))
}
calc_obp <- function(isHit, isAB, event){
  nom = sum(isHit) + sum(event == 'Walk') + sum(event == 'Intent Walk') + sum(event =='Hit By Pitch')
  denom = sum(isAB) + sum(event == 'Walk') + sum(event == 'Intent Walk') +
    sum(event =='Hit By Pitch') + sum(event =='Sac Fly')
  return (nom/denom)
}

lad_stat<-df_lad %>%
  group_by(batterId, batterName) %>% 
  summarise(
    totPA=sum(isPA),
    totAB=sum(isAB),
    totHit=sum(isHit),
    BA=calc_ba(isHit, isAB),
    OBP=calc_obp(isHit, isAB, event),
    SLG=calc_slg(numBases, isAB),
    TB=sum(numBases)
  )
lad_stat <- lad_stat[order(-lad_stat$totPA),]
batters <- lad_stat[lad_stat$totPA > 10,]$batterName

lad_monthly_stat <- df_lad %>%
  group_by(batterId, batterName, month) %>% 
  summarise(
    totPA=sum(isPA),
    totAB=sum(isAB),
    totHit=sum(isHit),
    BA=calc_ba(isHit, isAB),
    OBP=calc_obp(isHit, isAB, event),
    SLG=calc_slg(numBases, isAB),
    TB=sum(numBases)
  )

# Adding NA rows for missing months.
for (m in c(4,5,6,7,8,9,10)){
  for (batter in batters) {
    df_player <- lad_monthly_stat[lad_monthly_stat$batterName == batter,]
    if (!(m %in% df_player$month)){
      newrow <- data.frame(
        batterId = df_player$batterId[1], batterName = df_player$batterName[1], 
        month = m, totPA=NA, totAB=NA, totHit=NA, BA=NA, OBP=NA, SLG=NA, TB=NA)
      lad_monthly_stat <- rbind(data.frame(lad_monthly_stat), newrow)
    }
  }
}

lad_monthly_stat <- lad_monthly_stat[order(lad_monthly_stat$batterName, lad_monthly_stat$month),]


lad_overall_monthly_stat <- df_lad %>%
  group_by(month) %>% 
  summarise(
    totPA=sum(isPA),
    totAB=sum(isAB),
    totHit=sum(isHit),
    BA=calc_ba(isHit, isAB),
    OBP=calc_obp(isHit, isAB, event),
    SLG=calc_slg(numBases, isAB),
    TB=sum(numBases)
  )

lad_overall_monthly_stat['batterId'] = NA
lad_overall_monthly_stat['batterName'] = 'Average'


lad_pitchtype_stat <- df_lad %>%
  group_by(batterId, batterName, pitch.pitch_type) %>% 
  summarise(
    totPA=sum(isPA),
    totAB=sum(isAB),
    totHit=sum(isHit),
    BA=calc_ba(isHit, isAB),
    OBP=calc_obp(isHit, isAB, event),
    SLG=calc_slg(numBases, isAB),
    TB=sum(numBases)
  )


#####

ui <- fluidPage(
  title = "Batter Statistics",
  
  fluidRow(
    
    column(
      4,
      selectInput(
       inputId = "player",
       label = "Player",
       choices = batters[order(batters)],
       selected = "Gonzalez, A"
      )
    )
  ),
  
  # hr(),
  
  fluidRow(
    tabsetPanel(
      tabPanel(
        title = "Spray Chart",
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "home",
                        label = "Spray Chart (Home Team):",
                        choices = teams,
                        selected = "Los Angeles Dodgers"
            ),
            radioButtons('spray_buttons',
                         label = "Spray Chart (Plot)",
                         choices = c("Selected Player", "All Players"))
          ),
          mainPanel(
            h3(style= "text-align: center;", 'Spray Chart'),
            plotlyOutput("spray", height = "500px")
          )
        )
      ),
      tabPanel(
        "Time Series Plots",
        tabsetPanel(
          tabPanel("Batting Average", plotlyOutput("plot_BA")),
          tabPanel("On Base Percentage", plotlyOutput("plot_OBP")),
          tabPanel("Slugging Percentage", plotlyOutput("plot_SLG"))
        )
      ),
      tabPanel(
        "Parellel Coordinate Plot",
        plotlyOutput("pcplot")
      )
    )
  )
)


server <- function(session, input, output) {
  # Update Input Labels
  observe({
    # Selected Player populates radio buttons
    updateRadioButtons(session, 'spray_buttons', choices = c(input$player, "All Players"))
    
    # List of Venues based on input player
    updateSelectInput(session, 'home', choices = list(`National League` = levels(factor(df[which(df$home_lg == "NL" & df$batterName == input$player & !is.na(df$our.x)), 'Home.Team'])),
                                                      `American League` = levels(factor(df[which(df$home_lg == "AL" & df$batterName == input$player & !is.na(df$our.x)), 'Home.Team']))))
  })
  
  
  #### TIME SERIES PLOTS ####
  
  output$plot_BA <- renderPlotly({
    batter = input$player
    df_player <- lad_monthly_stat[lad_monthly_stat$batterName == batter,]
    df_player <- rbind(df_player, lad_overall_monthly_stat)
    
    color_values = c("Average" = "#F8766D")
    color_values[batter] <- "#00BFC4"
    
    p <- ggplot(df_player, aes(x=month, y=BA, color=batterName, group = batterName,
                               text=paste0("Total PA: ", totPA, "\n",
                                           "Total AB: ", totAB, "\n",
                                           "Total Hits: ", totHit))) +
      geom_point() + geom_line() +
      scale_color_manual("", values = color_values) +
      scale_x_discrete(limits = 4:10) +
      scale_y_continuous(labels = function(x) {sprintf("%.3f", x)}, limits = c(0, max(df_player$BA,0.4, na.rm=TRUE)))+
      ggtitle(batter) + xlab("Month") + ylab("BA") +
      theme_light(base_size = 18)
    
    plotly_p <- ggplotly(p, tooltip = "text")
    plotly_p
  })
  output$plot_OBP <- renderPlotly({
    batter = input$player
    df_player <- lad_monthly_stat[lad_monthly_stat$batterName == batter,]
    df_player <- rbind(df_player, lad_overall_monthly_stat)
    
    color_values = c("Average" = "#F8766D")
    color_values[batter] <- "#00BFC4"
    
    p <- ggplot(df_player, aes(x=month, y=OBP, color=batterName, group = batterName,
                               text=paste0("Total PA: ", totPA, "\n",
                                           "Total AB: ", totAB, "\n",
                                           "Total Hits: ", totHit))) +
      geom_point() + geom_line() +
      scale_color_manual("", values = color_values) +
      scale_x_discrete(limits = 4:10) +
      scale_y_continuous(labels = function(x) {sprintf("%.3f", x)}, limits = c(0, max(df_player$OBP, 0.5, na.rm=TRUE)))+
      ggtitle(batter) + xlab("Month") + ylab("OBP") +
      theme_light(base_size = 18)
    
    plotly_p <- ggplotly(p, tooltip = "text")
    plotly_p
  })
  output$plot_SLG <- renderPlotly({
    batter = input$player
    df_player <- lad_monthly_stat[lad_monthly_stat$batterName == batter,]
    df_player <- rbind(df_player, lad_overall_monthly_stat)
    
    color_values = c("Average" = "#F8766D")
    color_values[batter] <- "#00BFC4"
    
    p <- ggplot(df_player, aes(x=month, y=SLG, color=batterName, group = batterName,
                               text=paste0("Total PA: ", totPA, "\n",
                                           "Total AB: ", totAB, "\n",
                                           "Total Hits: ", totHit))) +
      geom_point() + geom_line() +
      scale_color_manual("", values = color_values) +
      scale_x_discrete(limits = 4:10) +
      scale_y_continuous(labels = function(x) {sprintf("%.3f", x)}, limits = c(0, max(df_player$SLG, 1, na.rm=TRUE)))+
      ggtitle(batter) + xlab("Month") + ylab("SLG") +
      theme_light(base_size = 18)
    
    plotly_p <- ggplotly(p, tooltip = "text")
    plotly_p
  })
  
  
  #### SPRAY CHART ####
  # Venue for spray chart
  venue <- reactive({ df[which(df$Home.Team == input$home)[1], "venueId"] })
  
  # Reactive Data Frame for the Spray Chart
  spray_df <- reactive({
    df <- df[which(df$venueId == venue()), ]
    df <- df[which(df$our.y < 490), ]
    
    if (input$spray_buttons != 'All Players'){
      df <- df[which(df$batterName == input$player),]
    }
    df
  })
  
  # URI to add venue to background
  img_uri <- reactive({ 
    image_file <- paste0("data/venues/", venue(), ".png")
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
    paste('data:image/png;base64', txt, sep=',')
  })
  
  # Spray Chart (ggplot)
  spray_chart <- function(df){
    ggplot(df, aes(our.x, our.y, colour = result,
                   text = str_wrap(paste("Description:", description), width=30))) +
      geom_point() +
      scale_color_manual(name = "Result", values = result_colors) +
      xlim(c(-310, 310)) +
      ylim(c(-100, 500)) +
      guides(des=FALSE) +
      theme(
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  
  # Spray Chart (Plotly)
  output$spray <- renderPlotly({
    p <- spray_chart(spray_df())
    p <- ggplotly(p, tooltip = c('colour', 'text'), autosize = FALSE, width = 650, height = 500) %>%
      layout(
        images = list(
          list(
            source = img_uri(),
            xref = "x",
            yref = "y",
            x= -300,
            y= 490,
            sizex = 600,
            sizey = 600,
            sizing = "stretch",
            opacity = 1.0,
            layer = "below"
          )
        )
      )
    p
  })
  
  ##### Parallel Coordinate Plot #####
  output$pcplot <- renderPlotly({
    batter = input$player
    color_values = c("Changeup"="#F8766D", "Curve"="#C49A00", "Fastball"="#53B400",
                     "Other"="#00C094", "Overall"="#00B6EB", "Sinker"="#A58AFF", "Slider"="#FB61D7")
    
    df_player <- lad_pitchtype_stat[lad_pitchtype_stat$batterName == batter,]
    player_overall_stat <- lad_stat[lad_stat$batterName == batter,]
    player_overall_stat['pitch.pitch_type'] <- "Overall"
    
    df_player <- data.frame(rbind(df_player, player_overall_stat))
    
    df_player_grouped <- gather(df_player, 'column', 'value', BA:SLG)
    
    p <- ggplot(data = df_player_grouped,
                aes(column, value, col=pitch.pitch_type, group=pitch.pitch_type,
                    text=paste0("Pitch Type: ", pitch.pitch_type, "\n",
                                "Total PA: ", totPA, "\n",
                                "Total AB: ", totAB, "\n",
                                "Total Hits: ", totHit))) +
      geom_line() + geom_point() +
      scale_x_discrete(expand = c(-0.1, 0.1)) +
      scale_color_manual("Pitch Types", values = color_values) +
      ggtitle(batter) + xlab("Statistics") + ylab("Value") +
      theme_light(base_size = 18)
    
    plotly_p <- ggplotly(p, tooltip = "text")
    plotly_p
  })
  
}


shinyApp(ui = ui, server = server)
