library(tidyverse)

defense_stats<-read_csv("defense_stats.csv")
rushing_stats<-read_csv("rushing_stats.csv")
passing_stats<-read_csv("passing_stats.csv")
#getting era adjustments for rate stats
passing_adjustments <- passing_stats %>%
  group_by(season) %>%
  summarise(Cmp=sum(Cmp, na.rm=T), Att=sum(Att, na.rm=T), Yds=sum(Yds, na.rm=T), TD=sum(TD, na.rm=T), Int=sum(Int, na.rm=T)) %>%
  summarise(yards_att_adj=Yds/Att, cmp_pct_adj=Cmp/Att, td_pct_adj=TD/Att, int_pct_adj=Int/Att, season)

rushing_adjustments <- rushing_stats %>%
  group_by(season) %>%
  summarise(att=sum(att, na.rm=T), yds=sum(yds, na.rm=T), tds=sum(td, na.rm=T)) %>%
  summarise(rush_yards_att_adj=yds/att, rush_tds_att_adj=tds/att, season)
receiving_adjustments <- rushing_stats %>%
  group_by(season) %>%
  summarise(rec=sum(rec, na.rm=T), yds=sum(yds, na.rm=T), tds=sum(td_2, na.rm=T)) %>%
  summarise(yds_rec_adj=yds/rec, td_rec_adj=tds/rec, season)
#reading in standings to contextualize players
standings<-read_csv("standings.csv")
#creating passing stats table

passing_stats_new<-merge(merge(passing_stats %>%
                                 mutate(yards_att=Yds/Att, cmp_pct=Cmp/Att, td_pct=TD/Att, int_pct=Int/Att) %>%
                                 summarise(player_id, G, Att, yards_att, cmp_pct, td_pct, int_pct, school, season), 
                               passing_adjustments, 
                               by="season")%>%
                           summarise(season, player_id, school_name=school, G, att=Att, 
                                     yards_plus=(yards_att-yards_att_adj)*att,
                                     cmp_plus=(cmp_pct-cmp_pct_adj)*att,
                                     td_plus=(td_pct-td_pct_adj)*att,
                                     int_plus=(int_pct-int_pct_adj)*att), 
                         standings, 
                         by=c("season", "school_name"))
passing_stats_new$def_plus <- NULL


#creating rushing stats table
rushing_stats_new <- merge(merge(rushing_stats %>%
                                   mutate(yards_att=yds/att, tds_att=td/att ) %>%
                                   summarise(player_id, g, rush_att=att, yards_att, tds_att, school, season), 
                                 rushing_adjustments, by="season") %>%
                             summarise(season, player_id, school_name=school, g, rush_att,
                                       rush_yards_plus=(yards_att-rush_yards_att_adj)*rush_att,
                                       rush_tds_plus=(tds_att-rush_tds_att_adj)*rush_att), 
                           standings, 
                           by=c("season", "school_name"))
rushing_stats_new$def_plus <- NULL
#creating receiving stats table
receiving_stats_new <- merge(merge(rushing_stats %>%
                                     mutate(yards_rec=yds_2/rec, tds_rec=td_2/rec ) %>%
                                     summarise(player_id, g, rec, yards_rec, tds_rec, school, season), 
                                   receiving_adjustments, 
                                   by="season") %>%
                               summarise(season, player_id, school_name=school, g, rec,
                                         rec_yards_att_plus=100*yards_rec/yds_rec_adj,
                                         td_rec_att_plus=100*tds_rec/td_rec_adj),
                             standings, 
                             by=c("season", "school_name"))
receiving_stats_new$def_plus <- NULL

#creating_qb_data 
quarterbacks <- merge(passing_stats_new, 
                      rushing_stats_new,
                      by=c("season", "school_name", "player_id", "srs", "sos", "off_plus"), all.x=T)
quarterbacks$g <- quarterbacks$G
quarterbacks$G <- NULL
write_csv(quarterbacks, "quarterbacks.csv")

running_backs <- merge(rushing_stats_new, 
                       receiving_stats_new,
                       by=c("season", "school_name", "player_id", "srs",
                            "sos", "off_plus", "g"), 
                       all.x=T)
write_csv(running_backs, "running_backs.csv")
write_csv(receiving_stats_new, "receivers.csv")

