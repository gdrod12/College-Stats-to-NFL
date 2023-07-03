library(tidyverse)
library(nflverse)
library(rvest)
library(janitor)

load_draft <- function(season){
  print(season)
 html<-paste("https://www.pro-football-reference.com/years/",season,"/draft.htm", sep="")
 Sys.sleep(5)
 print(season)
 table <- "table#drafts"
 x<-html %>%
   read_html() %>%
   html_nodes(table) %>%
   html_table() %>%
   data.frame() %>%
   row_to_names(row_number = 1) %>%
   clean_names() %>%
   #pre 2012 data doesn't include player_ids on players with null ages, post 2012 does
   filter(if (season<=2011) as.numeric(age)>0 else as.numeric(rnd)>0 )  %>%
   filter(as.numeric(rnd)>0)
 #the solo tackles column isn't included pre 1993, so might as well remove it since we dont need it
 x$solo<-NULL
 href<-"table#drafts a"
 ids <- html %>%
   read_html() %>%
   html_nodes(href) %>%
   html_attr(name="href") %>%
   data.frame(x=.) %>%
   filter(substr(x, 1, 9)=="/players/") %>%
   summarise(player_id=sub("\\..*", "", x)) %>%
   summarise(player_id=substr(player_id, 12, nchar(player_id))) %>%
   unlist()
x$player_id <- ids
x$x <- NULL
x
}

draft_data <- lapply(1980:2022, load_draft)
draft_data <- reduce(draft_data, rbind)
write_csv(draft_data, "draft_data.csv")
#getting rushing nfl data
rushers <- draft_data %>%
  filter(pos=="RB"|pos=="FB")
rusher_ids <- unlist(rushers$player_id)

get_rushers<-function(y)tryCatch({
player_id <- rusher_ids[y]
print(player_id)
player_first_character <- substr(player_id, 1, 1)
html<-paste("https://www.pro-football-reference.com/players/", player_first_character, "/",player_id,".htm", sep="")
table<-"table#rushing_and_receiving"
x<-html %>%
  read_html() %>%
  html_nodes(table) %>%
  html_table() %>%
  data.frame() %>%
  row_to_names(row_number=1) %>% 
  clean_names() %>%
  head(4) %>%
  group_by(pos) %>%
  summarise(av=sum(as.numeric(av, na.rm=T)), g=sum(as.numeric(g)), gs=sum(as.numeric(gs)))
x$player_id <- player_id
Sys.sleep(7)
x
}, error=function(e) NULL)
rushing_avs<-lapply(1:length(rusher_ids) ,get_rushers)
   
rushing_avs<-reduce(rushing_avs, rbind)
write_csv(rushing_avs, "rushing_avs.csv")
#Getting receiving nfl data
receivers <- draft_data %>%
  filter(pos=="TE"|pos=="WR")
receiver_ids <- unlist(receivers$player_id)
get_receivers<-function(y)tryCatch({
  player_id <- receiver_ids[y]
  print(player_id)
  player_first_character <- substr(player_id, 1, 1)
  html<-paste("https://www.pro-football-reference.com/players/", player_first_character, "/",player_id,".htm", sep="")
  table<-"table#receiving_and_rushing"
  x<-html %>%
    read_html() %>%
    html_nodes(table) %>%
    html_table() %>%
    data.frame() %>%
    row_to_names(row_number=1) %>% 
    clean_names() %>%
    head(4) %>%
    group_by(pos) %>%
    summarise(av=sum(as.numeric(av, na.rm=T)), g=sum(as.numeric(g)), gs=sum(as.numeric(gs)))
  x$player_id <- player_id
  Sys.sleep(7)
  x
}, error=function(e) NULL)
receiver_avs <- lapply(1:length(receiver_ids), get_receivers)

receiver_avs <- reduce(receiver_avs, rbind)
write_csv(receiver_avs, "receiver_avs.csv")
#getting passing nfl data
qbs <- draft_data %>%
  filter(pos=="QB")
passer_ids <- unlist(qbs$player_id)
get_qbs<-function(y)tryCatch({
  player_id <- passer_ids[y]
  print(player_id)
  player_first_character <- substr(player_id, 1, 1)
  html<-paste("https://www.pro-football-reference.com/players/", player_first_character, "/",player_id,".htm", sep="")
  table<-"table#passing"
  x<-html %>%
    read_html() %>%
    html_nodes(table) %>%
    html_table() %>%
    data.frame() %>%
    clean_names() %>%
    group_by(pos, year) %>%
    summarise(age, av=as.double(av), player_id, g=as.double(g), 
              gs=as.double(gs))
  x$player_id <- player_id
  Sys.sleep(4)
  x
}, error=function(e) NULL)
passer_avs <- lapply(1:length(passer_ids), get_qbs)
passer_avs<-passer_avs[!sapply(passer_avs, is.null)]
fix_data <- function(x){
  data.frame(passer_avs[x]) %>% 
    summarise(pos, year, age, av=as.double(av), player_id, g=as.double(g), 
            gs=as.double(gs))
}
passer_avs <- lapply(1:length(passer_avs), fix_data)
passer_avs<- reduce(passer_avs, rbind) 
passer_avs<-passer_avs %>%
  filter(!is.na(as.numeric(age)))
write_csv(passer_avs,"passer_avs.csv")





