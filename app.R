source("global.R")

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
      ),
      tabPanel(
        "Batting Average Grid",
        fixedRow(
          column(3,
                 imageOutput("Left")
          ),
          column(6,
                 h3(style= "text-align: center;", 'Batting Average by Zone'),
                 br(), br(),
                 plotlyOutput("heatmap", height = "500px", width="500px")
          ),
          column(3,
                 imageOutput("Right")
          )
        )
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
        axis.line=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
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
  
  heat_df <- reactive({
    df <- df[which(df$batterName == input$player & df$isAB), c('batterName', 'Zone.X', 'Zone.Y', 'isHit', 'isAB')]
    df <- dcast(df[,c('batterName', 'Zone.X', 'Zone.Y', 'isHit', 'isAB')],
                batterName + Zone.X + Zone.Y ~ isHit + isAB, sum, value.var='isAB')
    df$Hits <- df$TRUE_TRUE
    df$AB <- df$FALSE_TRUE + df$TRUE_TRUE
    df$BA <- df$Hits/df$AB
    df <- df[which(!is.na(df$BA)),c('batterName', 'Zone.X', 'Zone.Y', 'Hits', 'AB', 'BA')]
    df
  })
  
  # HeatMap (ggplot)
  heatmap <- function(df){
    ggplot(df) + 
      geom_tile(aes(x=Zone.X - .5, y=Zone.Y - .5, fill = BA,
                    text = paste0(Hits, " H/", AB, " AB"))) +
      geom_segment(aes(x=1, xend=4, y=4, yend=4), colour='black') +
      geom_segment(aes(x=1, xend=4, y=3, yend=3), colour='black') +
      geom_segment(aes(x=1, xend=4, y=2, yend=2), colour='black') +
      geom_segment(aes(x=1, xend=4, y=1, yend=1), colour='black') +
      geom_segment(aes(x=1, xend=1, y=1, yend=4), colour='black') +
      geom_segment(aes(x=2, xend=2, y=1, yend=4), colour='black') +
      geom_segment(aes(x=3, xend=3, y=1, yend=4), colour='black') +
      geom_segment(aes(x=4, xend=4, y=1, yend=4), colour='black') +
      geom_text(aes(x = Zone.X - .5, y=Zone.Y -.5,
                    group = 1,
                    label = sub("^(-?)0.", "\\1.", sprintf("%.3f", BA)))) +
      xlim(c(0, 5)) +
      ylim(c(0, 5)) +
      scale_fill_gradient2(low='#2166ac', mid='#f7f7f7', midpoint=.275, high='#b2182b') +
      theme(
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none'
      )
  }
  
  # Spray Chart (Plotly)
  output$heatmap <- renderPlotly({
    p <- heatmap(heat_df())
    p <- ggplotly(p, tooltip=c('text'), autosize = FALSE, width = 400, height = 400)
    p
  })
  
  stance <- reactive({ df[which(df$batterName == input$player)[1], "stand"] })
  
  # Left-handed batter
  output$Left <- renderImage({
    if (stance() == 'L'){
      filename <- normalizePath(file.path('./data/batting/L.png'))
    } else {
      filename <- normalizePath(file.path('./data/batting/Blank.png'))
    }
    list(src = filename,
         width = 175)
  }, deleteFile = FALSE)
  
  # Right-handed batter
  output$Right <- renderImage({
    if (stance() == 'R'){
      filename <- normalizePath(file.path('./data/batting/R.png'))
    } else {
      filename <- normalizePath(file.path('./data/batting/Blank.png'))
    }
    list(src = filename,
         width = 175)
  }, deleteFile = FALSE)
  
}

shinyApp(ui = ui, server = server)
