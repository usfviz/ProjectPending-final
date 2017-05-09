dependencies <- c('shiny', 'ggplot2', 'png', 'ggvis', 'plotly', 'RCurl', 'stringr', 'tidyr', 'reshape2')
new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages, repos="http://cran.rstudio.com/")}

library('shiny')
library('ggplot2')
library('png')
library('ggvis')
library('plotly')
library('RCurl')
library('stringr')
library('tidyr')
library('reshape2')

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

x_switch <- function(x){
  ifelse(x < 90, 1,
         ifelse(x < 108, 2,
                ifelse(x < 126, 3,
                       ifelse(x < 144, 4, 5))))
}
y_switch <- function(x){
  ifelse(x < 145, 1,
         ifelse(x < 163, 2,
                ifelse(x < 178, 3,
                       ifelse(x < 197, 4, 5))))
}
df$Zone.X = as.vector(unlist(lapply(df$pitch.x, x_switch)))
df$Zone.Y = as.vector(unlist(lapply(df$pitch.y, y_switch)))

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