#adding in teams that were missed for whatever reason
library(rvest)
library(tidyverse)
library(janitor)
#Sports reference URL
url<-"https://www.sports-reference.com/cfb/schools/"

#College name masterset URL
html<-"https://www.sports-reference.com/cfb/schools/index.html"

college_table<-"table#schools"
college_href<-"table#schools a"

school_years <- html %>%
  read_html %>%
  html_nodes(college_table) %>%
  html_table() %>%
  reduce(rbind) %>%
  row_to_names(row_number=1) %>%
  clean_names() %>%
  filter(as.numeric(from)>1800)

school_names <- html %>%
  read_html %>%
  html_nodes(college_href) %>%
  html_attr(name="href")

#Creating substring of school url 
school_names<-data.frame(school_urls=school_names) %>%
  mutate(first_half=substr(school_urls, 1, 13)) %>%
  filter(first_half=="/cfb/schools/") %>%
  mutate(school_name=substr(school_urls, 14, nchar(school_urls)-1))
schools <- data.frame(school_names, school_years) %>%
  summarise(school=school_name, from=as.numeric(from), to=as.numeric(to))
rm(school_names, school_years)
#Team names to scrape

schools<-schools %>%
  filter(to>=1970)
#Filtering to more recent colleges
rushing_stats<-read_csv("rushing_stats.csv")
missing_schools<-anti_join(schools, rushing_stats, by="school") 
#getting rushing stats from schools missing from data
teams<-unlist(missing_schools$school)
get_rushing_stats <- function(season)tryCatch({
  Sys.sleep(5)
  team<-teams
  table <- "table#rushing_and_receiving"
  table_id <- paste(table, ' a') 
  html <- paste(url,team,"/",season, ".html",sep="")
  
  #grabbing table
  
  curl::curl(html)%>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node(table) %>%
    html_table() ->stats
  #grabbing unique player ids
  
  curl::curl(html) %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_nodes(table_id)%>%
    html_attr(name="href") %>%
    unlist() %>%
    as.character() ->  player_ids
  
  #extracting player id from player urls
  stats<-stats %>%
    row_to_names(row_number=1) %>%
    clean_names() %>%
    mutate(school=team, player_link=player_ids) %>%
    mutate(player_link=gsub("\\..*","", player_link ))
  print(season)
  stats %>%
    mutate(player_id=substr(player_link, 14, nchar(player_link)), season=season)
  

  
  
  
}, error=function(e) NULL)
for (teams in teams) {
  print(teams)
  to=as.numeric(merge(data.frame(teams), missing_schools, by.x="teams", by.y="school")[1,3])
  from=as.numeric(merge(data.frame(teams), missing_schools, by.x="teams", by.y="school")[1,2])
  
  from=ifelse(from>=1960, from, 1960)
  rushing_stats_new<-reduce(lapply(from:to, get_rushing_stats), rbind)
  
  
  rushing_stats<-rbind(rushing_stats, rushing_stats_new)
  closeAllConnections()
  Sys.sleep(15)
} 
write_csv(rushing_stats, "rushing_stats.csv")
rm(rushing_stats)
#doing the same for passing stats
passing_stats<-read_csv("passing_stats.csv")
missing_schools<-anti_join(schools, passing_stats, by="school") 
teams<-unlist(missing_schools$school)
get_passing_stats <- function(season) tryCatch({
  Sys.sleep(5)
  print(season)
  team<-teams
  passing_table <- "table#passing"
  table_id <- paste(passing_table, ' a') 
  html <- paste(url,team,"/",season, ".html",sep="")
  
  #grabbing table
  
  curl::curl(html)%>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node(passing_table) %>%
    html_table() -> passing_stats
  #grabbing unique player ids
  
  curl::curl(html) %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_nodes(table_id)%>%
    html_attr(name="href") %>%
    unlist() %>%
    as.character() ->  player_ids
  
  #extracting player id from player urls
  passing_stats<-passing_stats %>%
    row_to_names(row_number=1) %>%
    mutate(school=team, player_link=player_ids) %>%
    mutate(player_link=gsub("\\..*","", player_link ))
  
  passing_stats %>%
    mutate(player_id=substr(player_link, 14, nchar(player_link)), season=season)

}, error=function(e) NULL)
for (teams in teams) {
  print(teams)
  to=as.numeric(merge(data.frame(teams), missing_schools, by.x="teams", by.y="school")[1,3])
  from=as.numeric(merge(data.frame(teams), missing_schools, by.x="teams", by.y="school")[1,2])
  
  from=ifelse(from>=1960, from, 1960)
  passing_stats_new<-reduce(lapply(from:to, get_passing_stats), rbind)
  
  
  passing_stats<-rbind(passing_stats, passing_stats_new)
  closeAllConnections()
  Sys.sleep(5)
} 
write_csv(passing_stats,"passing_stats.csv")
#doing same for defensive stats
defense_stats<-read_csv("defense_stats.csv")
missing_schools<-anti_join(schools, defense_stats, by="school") 
teams<-unlist(missing_schools$school)
get_passing_stats <- function(season) tryCatch({
  Sys.sleep(5)
  print(season)
  team<-teams
  passing_table <- "table#passing"
  table_id <- paste(passing_table, ' a') 
  html <- paste(url,team,"/",season, ".html",sep="")
  
  #grabbing table
  
  curl::curl(html)%>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node(passing_table) %>%
    html_table() -> passing_stats
  #grabbing unique player ids
  
  curl::curl(html) %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_nodes(table_id)%>%
    html_attr(name="href") %>%
    unlist() %>%
    as.character() ->  player_ids
  
  #extracting player id from player urls
  passing_stats<-passing_stats %>%
    row_to_names(row_number=1) %>%
    mutate(school=team, player_link=player_ids) %>%
    mutate(player_link=gsub("\\..*","", player_link ))
  
  passing_stats %>%
    mutate(player_id=substr(player_link, 14, nchar(player_link)), season=season)
  
}, error=function(e) NULL)
for (teams in teams) {
  print(teams)
  to=as.numeric(merge(data.frame(teams), missing_schools, by.x="teams", by.y="school")[1,3])
  from=as.numeric(merge(data.frame(teams), missing_schools, by.x="teams", by.y="school")[1,2])
  
  from=ifelse(from>=1960, from, 1960)
  passing_stats_new<-reduce(lapply(from:to, get_passing_stats), rbind)
  
  
  passing_stats<-rbind(passing_stats, passing_stats_new)
  closeAllConnections()
  Sys.sleep(5)
} 
write_csv(passing_stats,"passing_stats.csv")

