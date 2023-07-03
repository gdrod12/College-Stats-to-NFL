library(tidyverse)
library(rvest)
library(janitor)
get_standings<-function(season){
print(season)
html<-paste("https://www.sports-reference.com/cfb/years/", season, "-standings.html", sep="")
standings_table<-"table#standings"
standings_href<-"table#standings a"
season_standings<-html %>%
  read_html %>%
  html_nodes(standings_table)%>%
  html_table() %>%
  reduce(rbind) %>%
  row_to_names(row_number=1) %>%
  clean_names() %>%
  filter(as.numeric(w)<100000) %>%
  mutate(season=season)
school_urls <- html %>%
  read_html %>%
  html_nodes(standings_href) %>%
  html_attr(name="href")
school_names<-data.frame(school_urls) %>%
  mutate(first_half=substr(school_urls, 1, 13)) %>%
  filter(first_half=="/cfb/schools/") %>%
  mutate(school_name=substr(school_urls, 14, nchar(school_urls)-1)) %>%
  mutate(school_name=sub("\\/.*", "", school_name))
season_standings$school_name <- school_names$school_name
rm(school_names)

closeAllConnections()
Sys.sleep(8)
season_standings %>%
  summarise(school_name, off, def, srs, sos, season)
}
#scraping year by year overall D1 standings
standings<-lapply(1960:2022, get_standings)
standings<-reduce(standings, rbind)

write_csv(standings, "standings.csv")
standings<-read_csv("standings.csv")
#doing this is much easier than converting everything to numeric
averages <- standings %>%
  group_by(season) %>%
  summarise(off_mean=mean(off, na.rm=T), def_mean=mean(def, na.rm=T))

standings<-merge(standings, averages, by=c("season")) %>%
  summarise(season, school_name, srs, sos, off_plus=100*off/off_mean, def_plus=100*def/def_mean)
write_csv(standings, "standings.csv")
