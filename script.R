##### Prepare Environment #####
pacman::p_load(tidyverse,glue,data.table,nflfastR,RcppRoll,httr,jsonlite,baseballr,lubridate,scraEP,qs)
setwd("~/Desktop/FunCode/Projects/PrizePicks/")

##### Set up API functions #####
### Sign up at https://prop-odds.com/ to get API key
api_key <- 'insert_key_here'
base_url <- 'https://api.prop-odds.com/'

# get list of games with odds available
get_games <- function(league,i_date=Sys.Date()) {
  url <- glue('{base_url}beta/games/{league}?date={i_date}&tz=America/New_York&api_key={api_key}')
  res <- GET(url)
  rbindlist(content(res)[['games']],T,T)
}

# get list of events for a game
get_markets <- function(game_id) {
  url <- glue('{base_url}beta/markets/{game_id}?api_key={api_key}')
  res <- GET(url)
  rbindlist(content(res)[['markets']],T,T)
}

# get list of odds for an event
get_odds <- function(game_id,event) {
  url <- glue('{base_url}beta/odds/{game_id}/{event}?api_key={api_key}')
  res <- GET(url)
  
  joy <- lapply(content(res)$sportsbooks,function(x){
    bookie <- x$bookie_key
    data <- rbindlist(x$market$outcomes,T,T)
    
    cbind(bookie=bookie,event=event,data)
  }) |> 
    rbindlist(T,T)
  
  joy
}

### Get all MLB pitcher strikeout data for yesterday
max_date <- Sys.Date()-1
min_date <- max_date
event <- 'pitcher_strikeout_over_under'

date_list <- seq(min_date,max_date,by='day')
odds_list <- list()

for(date_var in as.list(date_list)) {
  print(date_var)
  
  games <- get_games('mlb',date_var)
  game_ids <- games$game_id
  
  for(game_id_var in game_ids) {
    print(game_id_var)
    away_team <- games |> filter(game_id==game_id_var) |> pull(away_team)
    home_team <- games |> filter(game_id==game_id_var) |> pull(home_team)
    start_time <- games |> filter(game_id==game_id_var) |> pull(start_timestamp)
    
    tdf <- get_odds(game_id_var,event)
    tdf$game_id <- game_id_var
    tdf$away_team <- away_team
    tdf$home_team <- home_team
    tdf$start_time <- start_time
    
    odds_list <- append(odds_list,list(tdf))
  }
}

df_odds_raw <- odds_list |> 
  rbindlist(T,T)

df_odds <- df_odds_raw |> 
  mutate(across(
    .cols=c('start_time','timestamp'),
    .fns=~ymd_hms(.,tz='UTC')
  )) |> 
  mutate(type=case_when(
    str_detect(name,'Over') ~ 'over',
    str_detect(name,'Under') ~ 'under',
    T ~ 'other'
  ),
  date=as.Date(format(start_time,tz="America/Los_Angeles",usetz=TRUE))) |> 
  filter(type!='other',
         timestamp<=start_time) |>
  mutate(player_name=case_when(
    bookie %in% c('fanduel') ~ unaccent(str_extract(name,"\\w+\\s+\\w+")),
    bookie %in% c('pinnacle','barstool') ~ unaccent(str_extract(description,"\\w+\\s+\\w+")),
    bookie %in% c('draftkings') ~ unaccent(str_extract(name,"\\w+\\s+\\w+$")),
    bookie %in% c('betmgm') ~ unaccent(str_extract(str_extract(description,"(\\w+\\s+\\w+)\\s*\\("),"\\w+\\s+\\w+")),
    T ~ NA_character_
  )) |> 
  arrange(game_id,timestamp) |> 
  group_by(game_id,bookie,player_name,type) |> 
  slice(n()) |> 
  ungroup() |> 
  select(game_id,date,timestamp,bookie,event,player_name,type,handicap,odds) |> 
  mutate(prob=case_when(
    odds<0 ~ odds/(odds-100),
    odds>0 ~ 100/(odds+100),
    T ~ NA_real_
  )) |> 
  group_by(game_id,date,timestamp,bookie,event,player_name) |> 
  mutate(count=n(),
         implied_prob=prob/sum(prob)) |> 
  ungroup() |> 
  select(-prob) |> 
  filter(count==2)

# get actual results to compare
df_actual <- lapply(as.list(date_list),function(date_var){
  print(date_var)
  tdf <- bref_daily_pitcher(date_var,date_var) |> filter(!is.na(SO)) |> select(player_name=Name,IP,SO)
  tdf$date <- date_var
  
  tdf
}) |> rbindlist(T,T) |> 
  filter(!is.na(IP)) |> 
  mutate(player_name=unaccent(player_name))

# combine datasets
df <- df_odds |> 
  left_join(df_actual,by=c('player_name','date')) |> 
  mutate(outcome=case_when(
    handicap==SO ~ NA_real_,
    (type=='under' & handicap<SO) ~ 0,
    (type=='under' & handicap>SO) ~ 1,
    (type=='over' & handicap<SO) ~ 1,
    (type=='over' & handicap>SO) ~ 0,
    T ~ NA_real_
  )) |> 
  filter(!is.na(outcome)) |> 
  mutate(cat=cut(implied_prob,breaks=seq(0,1,by=.05)))

qsave(df,glue('data/mlb/pitcher_strikeouts/{max_date}.qs'))

df |> 
  filter(!is.na(outcome)) |> 
  group_by(bookie,type,cat) |> 
  summarize(count=n(),
            win_pct=mean(outcome)) |> 
  ungroup() |> View()


