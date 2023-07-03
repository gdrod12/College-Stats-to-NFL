library(jsonlite)
library(tidyverse)
library(rvest)
html<-"https://www.fantasypros.com/nfl/stats/combine/?start=1987&end=1999"
combine_1987_1999<-read_html(html) %>%
  html_table() %>%
  data.frame() %>%
  summarise(pfr_player_name=Player,forty=X40.Yard.Dash, vertical=Vertical.Jump,
            broad_jump=Broad.Jump, shuttle=X20.Yard.Shuttle,
            bench=Bench.Press, college=School, season=Year,
            ht=NA, wt=NA, cone=NA)
#sauce gardner has a different id in these two datasets
nflreadr::load_combine(seasons=2022)
combine_2000_2023<-nflreadr::load_combine() %>%
  summarise(pfr_player_id=pfr_id, forty, vertical,
            broad_jump, shuttle, bench, college=school, season,
            ht, wt, cone) %>%
  filter(!is.na(pfr_player_id))

draft_data<-nflreadr::load_draft_picks() %>%
  mutate(pick_id=paste(season, round, pick, sep=""))
players<-fromJSON("player_dataframe.json") %>%
  summarise(season=as.double(draft_year),
            round=as.double(draft_round), pick=as.double(draft_position), 
            height, weight)
draft_data<-merge(draft_data, players,
      by=c("season",
           "round", "pick"), all.x=T) %>%
  filter(!duplicated(pick_id)) %>%
  mutate(height_inches=as.numeric(substr(height, 1, 1))*12+
           as.numeric(substr(height, 3, nchar(height))))
draft_data_pre2000<-draft_data %>%
  filter(season>=1987, season<2000) %>%
  mutate(name_id=paste(season, pfr_player_name))

duplicated_players<-draft_data_pre2000 %>%
  filter(duplicated(name_id, fromLast = T)|duplicated(name_id, fromLast = F))
players <- anti_join(draft_data_pre2000, duplicated_players)
more_duplicates <-merge(players, combine_1987_1999, 
                   by=c("season", "pfr_player_name"), all.x=T) %>%
  filter(duplicated(name_id, fromLast = T)|duplicated(name_id, fromLast = F)) %>%
  filter(substr(college.x,1,4)==substr(college.y,1,4)) %>%
  mutate(college=college.x)
more_duplicates$college.x <- NULL
more_duplicates$college.y <- NULL
players <- anti_join(players, more_duplicates)
players <-merge(players, combine_1987_1999, 
                        by=c("season", "pfr_player_name"), all.x=T) %>%
  mutate(college=college.x)
players$college.x <- NULL
players$college.y <- NULL
duplicated_players<-merge(duplicated_players, combine_1987_1999, 
                          by=c("season", "pfr_player_name", "college"), all.x=T)

draft_data_pre2000<-rbind(players, duplicated_players, more_duplicates) %>%
  mutate(ht_inches=as.numeric(substr(ht, 1, 1))*12+
           as.numeric(substr(ht, 3, nchar(ht)))) %>%
  summarise(season, pfr_player_name, round, pick, position, age, 
            height_inches=ifelse(is.na(height_inches),ht_inches, height_inches),
            weight=ifelse(is.na(weight), wt, weight), 
            forty, vertical, broad_jump,
            shuttle, bench,cone, college,
            pfr_player_id, cfb_player_id)
#merging post 2000 data
draft_data_post2000 <- draft_data %>%
  filter(season>=2000)
draft_data_post2000<-merge(draft_data_post2000, combine_2000_2022, by=c("pfr_player_id","season")) %>%
  mutate(college=college.x)
draft_data_post2000$college.x <- NULL
draft_data_post2000$college.y <- NULL
draft_data_post2000<-draft_data_post2000 %>%
  mutate(ht_inches=as.numeric(substr(ht, 1, 1))*12+
           as.numeric(substr(ht, 3, nchar(ht)))) %>%
  summarise(season, pfr_player_name, round, pick, position, age, 
            height_inches=ifelse(is.na(height_inches),ht_inches, height_inches),
            weight=ifelse(is.na(weight), wt, weight), 
            forty, vertical, broad_jump,
            shuttle, bench,cone, college,
            pfr_player_id, cfb_player_id)
draft_data<-rbind(draft_data_post2000, draft_data_pre2000) %>%
  filter(!is.na(pfr_player_id))
write_csv(draft_data,"draft_data.csv")
draft_data<-read_csv("draft_data.csv") %>%
  filter(!is.na(height_inches), !is.na(weight))

bench_model<-lm(bench~weight, data=draft_data)
forty_model1<-lm(forty~weight, data=draft_data)
forty_model2<-lm(forty~shuttle, data=draft_data)
vertical_model1 <- lm(vertical~broad_jump, data=draft_data)
vertical_model2 <- lm(vertical~weight+height_inches, data=draft_data)
draft_data <- draft_data %>%
  mutate(bench=ifelse(is.na(bench),
                      weight*bench_model$coefficients[2]+bench_model$coefficients[1],
                      bench),
         forty=ifelse(is.na(forty),
                      ifelse(is.na(shuttle),
                             weight*forty_model1$coefficients[2]+
                               forty_model1$coefficients[1],
                             shuttle*forty_model2$coefficients[2]+
                               forty_model2$coefficients[1]),
                      forty),
         vertical=ifelse(is.na(vertical),
                         ifelse(is.na(broad_jump),
                                height_inches*vertical_model2$coefficients[3]+
                                weight*vertical_model2$coefficients[2]+
                                  vertical_model2$coefficients[1],
                                broad_jump*vertical_model1$coefficients[2]+
                                  vertical_model1$coefficients[1]),
                         vertical))
broad_jump_model<-lm(broad_jump~vertical, data=draft_data)
shuttle_model<-lm(shuttle~forty, data=draft_data)
draft_data <- draft_data %>%
  mutate(broad_jump=ifelse(is.na(broad_jump),
                           vertical*broad_jump_model$coefficients[2]+
                             broad_jump_model$coefficients[1],
                           broad_jump),
         shuttle=ifelse(is.na(shuttle),
                           forty*shuttle_model$coefficients[2]+
                             shuttle_model$coefficients[1],
                           shuttle))
cone_model <- lm(cone~shuttle, data=draft_data)
draft_data <- draft_data %>%
  mutate(cone=ifelse(is.na(cone),
                           shuttle*cone_model$coefficients[2]+
                             cone_model$coefficients[1],
                           cone))
write_csv(draft_data, "draft_data_imputed.csv")
