library(tidyverse)
library(nflreadr)
#quarterbacks <- merge(read_csv("quarterbacks.csv") %>%
                        #mutate(cfbref_id=player_id) %>%
#group_by(cfbref_id) %>%
#summarise(season=max(season)),
#read_csv("quarterbacks.csv") %>%
# mutate(cfbref_id=player_id),
#by=c("cfbref_id", "season"))
#quarterbacks$player_id <- NULL
combine_data<-read_csv("draft_data_imputed.csv")
quarterbacks<-read_csv("quarterbacks.csv") %>%
  mutate(cfbref_id=player_id)
quarterbacks$player_id <- NULL
quarterbacks[is.na(quarterbacks)] = 0
quarterbacks<-quarterbacks %>%
  mutate(srs_total=srs*G, sos_total=sos*G, off_plus_total=off_plus*G,
         yards_att_total=yards_att_plus*Att, cmp_pct_total=cmp_pct_plus*Att,
         td_att_total=td_pct_plus*Att, int_att_total=int_pct_plus*Att,
         rush_yards_att_total=rush_yards_att_plus*rush_att, 
         rush_tds_att_total=rush_tds_att_plus*rush_att) %>%
  group_by(cfbref_id) %>%
  summarise(G=sum(G), Att=sum(Att), rush_att=sum(rush_att), 
            srs=sum(srs_total)/sum(G),
            sos=sum(sos_total)/sum(G), 
            off_plus=sum(off_plus_total)/sum(G),
            yards_att_plus=sum(yards_att_total)/sum(Att),
            cmp_pct_plus=sum(cmp_pct_total)/sum(Att),
            td_pct_plus=sum(td_att_total)/sum(Att),
            int_pct_plus=sum(int_att_total)/sum(Att),
            rush_yards_att_plus=sum(rush_yards_att_total)/sum(rush_att),
            rush_tds_att_plus=sum(rush_tds_att_total)/sum(rush_att))
avs<-read_csv("passer_avs.csv") %>%
  filter(pos=="QB") %>%
  summarise(pfr_id=player_id, av)
avs <- merge(avs, combine_data, by.x="pfr_id", by.y="pfr_player_id") %>%
  summarise(pfr_id, av, season, pick, age, 
            height_inches, weight, forty, vertical, 
            broad_jump, shuttle, bench, cone,
            cfbref_id=cfb_player_id)
rm(combine_data)
quarterbacks <- merge(avs, quarterbacks, by="cfbref_id") %>%
  filter(av>=0) %>%
  filter(rush_att>0)

x_data <- quarterbacks[6:26]
x_stdv <- apply(x_data, 2, sd)
x_scaled <- scale(x_data,center=T, x_stdv)
y_data <- as.vector(quarterbacks[3])

get_correlations<-function(index) {
r_value=as.numeric(cor(x_data[index], y_data))
variable_names=as.vector(names(x_data[index]))
data<-data.frame(variable_names,
           r_value, 
           abs(r_value))
names(data) <- c("variable", "r", "r_abs_value")
data
}
correlations<-reduce(lapply(1:ncol(x_scaled), get_correlations), rbind)
weighted_x_scaled <- data.frame(matrix(nrow=nrow(x_scaled)))
for (index in 1:ncol(x_scaled)){
  column<-x_scaled[,index]*correlations[index,3]*100
  weighted_x_scaled<-cbind(weighted_x_scaled, column)
}

weighted_x_scaled[,1] <- NULL
colnames(weighted_x_scaled) <- colnames(x_scaled)
weighted_x_scaled <- data.frame(x_scaled)
weighted_x_scaled$sos<- NULL
project_qbs<-function(index){
print(quarterbacks[index,][1])
player_info <- quarterbacks[index,][1:5]
train<-as.matrix(weighted_x_scaled[-index,])
test<-as.matrix(weighted_x_scaled[index,])
k_value<- 30
model<-KODAMA::knn_Armadillo(Xtrain=train, Xtest=test, k=k_value)
knn_data<-data.frame()
for (player_index in 1:k_value){
id <- quarterbacks[model$nn_index[player_index],][1:5]
weight <- 1/model$distances[player_index]
new_data <- data.frame(id, weight)
knn_data <- rbind(knn_data, new_data)

}
comp_1 <- knn_data[1,1]
comp_2<-knn_data[2,1]
comp_3<-knn_data[3,1]
comp_4 <- knn_data[4,1]
comp_5<-knn_data[5,1]
comp_6<-knn_data[6,1]
comp_7 <- knn_data[7,1]
comp_8<-knn_data[8,1]
comp_9<-knn_data[9,1]
predicted_av<-as.numeric(knn_data %>%
  mutate(av_sum=av*weight) %>%
  summarise(predicted_av=sum(av_sum)/sum(weight)))
data.frame(player_info, predicted_av, comp_1, comp_2, comp_3,
           comp_4, comp_5, comp_6, comp_7, comp_8, comp_9)
}
qb_projections<-drop_na(reduce(lapply(1:nrow(quarterbacks), project_qbs), 
                               rbind))
cor(qb_projections$predicted_av, qb_projections$av)
lm(av~predicted_av, data=qb_projections)
ggplot(data=qb_projections, aes(x=pick, y=av)) +
  geom_point() +
  geom_smooth()
cor(qb_projections$pick,qb_projections$av)
cor(qb_projections$predicted_av,qb_projections$av)
write_csv(qb_projections, "quarterbacks_example.csv")
our_rankings <- qb_projections %>%
  arrange(-predicted_av) %>%
  summarise(cfbref_id, av, pick, our_rank=1:nrow(qb_projections)) %>%
  arrange(pick) %>%
  mutate(nfl_rank=1:nrow(qb_projections)) %>%
  arrange(-av) %>%
  mutate(actual_rank=1:nrow(qb_projections))

cor.test(our_rankings$our_rank, our_rankings$actual_rank, method="kendall")
cor.test(our_rankings$nfl_rank, our_rankings$actual_rank, method="kendall")
