#create a custom function to obtain a player's IBB totals by year
get_IBB <- function(player_id, year) {
  #subsets the data by the unique year id
  year_id <- subset(ncaa_season_id_lu, season == year, select = id)
  #subsets the data by the unique batting id
  batting_id <- subset(ncaa_season_id_lu, season == year, select = batting_id)
  #obtains a url for the batter to then read the data table of player's totals by year
  batting_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)
  #reads the data table from the url
  batting_payload <- xml2::read_html(batting_url)
  #data cleaning after extracting table  
  payload_df <- batting_payload %>%
    rvest::html_nodes('table') %>%
    .[3] %>%
    rvest::html_table(fill = T) %>%
    as.data.frame() %>%
    .[-1,]
  
  names(payload_df) <- payload_df[1,]
  
  payload_df <- payload_df[-1,]
  
  year_df = data.frame(Year = c("2012-13",
                                "2013-14",
                                "2014-15",
                                "2015-16",
                                "2016-17",
                                "2017-18",
                                "2018-19",
                                "2019-20"),
                       Season = c(2013,2014,2015,2016,2017,2018,2019,2020),
                       stringsAsFactors = F)
  
  #join data frame and year data frame to filter by year and pull the IBBs
  payload_df <- left_join(payload_df,year_df,by="Year")
  ibb <- payload_df %>% filter(Season == {{year}}) %>% pull(IBB)
  
  return(ibb)
}

#create a function called get_woba that calculates each player's wOBA accurately by year
get_woba <- function(df) {
  colnames <- c("wBB","BB","wHBP","HBP","w1B","X1B","w2B","X2B","w3B","X3B","wHR","HR","PA","IBB")
  if (!all(colnames %in% names(df))) warning("You must have the following variables in your dataset to calculate woBA: 'wBB','BB','wHBP','HBP','w1B','X1B','w2B','X2B','w3B','X3B','wHR','HR','PA','IBB'")
  df$wOBA <- round((((df$wBB * df$BB) + (df$wHBP * df$HBP) + (df$w1B * df$X1B) + (df$w2B * df$X2B) + 	(df$w3B * df$X3B) + (df$wHR * df$HR))/(df$PA-df$IBB)),3)
  return(df)
}

#write a function that gets the wRAA for each observation
get_wRAA <- function(wOBA,lgwOBA,Scale,PA) {
  wRAA = round(((wOBA-lgwOBA)/Scale) * PA,1)
}

