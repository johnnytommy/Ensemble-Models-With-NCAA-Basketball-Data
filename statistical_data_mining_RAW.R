##R CODE Statistical Data Mining Final

#John Thomas, Sam Luxenberg, Ning Xie


 rm(list=ls())
 path1 <- 'C:/Users/johnt/Documents/GW- Spring 2019/Statistical Data Mining/Final/mens-machine-learning-competition-2019/Stage2DataFiles'
 path2 <- 'C:/Users/johnt/Documents/GW- Spring 2019/Statistical Data Mining/Final/mens-machine-learning-competition-2019/MasseyOrdinals_thru_2019_day_128'
 path3 <- 'C:/Users/johnt/Documents/GW- Spring 2019/Statistical Data Mining/Final/mens-machine-learning-competition-2019'



 library(Ckmeans.1d.dp)
 library(tidyverse)
 library(dplyr)
 library(randomForest)
 library(caret)
 library(ROCR)
 library(gridExtra)
 library(ggExtra)
 library(xgboost)



 teams <- read.csv(paste(path1,"Teams.csv",sep='/'), stringsAsFactors = FALSE)
 seeds <- read.csv(paste(path1,"NCAATourneySeeds.csv",sep='/'), stringsAsFactors = FALSE)
 tourney <- read.csv(paste(path1,"NCAATourneyDetailedResults.csv",sep='/'), stringsAsFactors = FALSE)
 regular <- read.csv(paste(path1,"RegularSeasonDetailedResults.csv", sep='/'), stringsAsFactors = FALSE)
 conference <- read.csv(paste(path1,"TeamConferences.csv", sep='/'), stringsAsFactors = FALSE)
 rank <- read.csv(paste(path2,"MasseyOrdinals_thru_2019_day_128.csv", sep='/'), stringsAsFactors=FALSE)



 mean_rank <- rank %>%
   group_by(Season, TeamID, RankingDayNum) %>%
   filter(Season >= 2003) %>%
   summarize(rank = mean(OrdinalRank))


#Use seeds from tournament games as a proxy for rank from regular season games
 seeds$region <- substring(seeds$Seed,1,1)
 seeds$rank <- as.numeric(substring(seeds$Seed,2,3))
 seeds_sep <- seeds %>%
   filter(Season >= 2003) %>%
   dplyr::select(Season,TeamID,region,rank)


#Create home, away, neutral location feature for losing teams
 regular$LLoc <- ifelse(regular$WLoc=='H', 'A',
                        ifelse(regular$WLoc=='N', 'N', 'H'))

#Join - conference
 regular_conf <- regular %>%
   left_join(conference,by=c("Season"="Season","WTeamID"="TeamID"))
 colnames(regular_conf)[length(names(regular_conf))] <- "Wconf"
 regular_conf <- regular_conf %>%
   left_join(conference,by=c("Season"="Season","LTeamID"="TeamID"))
 colnames(regular_conf)[length(names(regular_conf))] <- "Lconf"
 regular_conf$conf_diff <- ifelse(regular_conf$Wconf == regular_conf$Lconf, 0 ,1)



#Regular Season Winner Dataset
 regular_W <- regular_conf %>%
   filter(Season < 2019) %>%
   dplyr::select(Season, DayNum, WTeamID, WScore, LTeamID, LScore,WLoc, NumOT,
          Wconf, Lconf, conf_diff,
          WFGM, WFGA, WFGM3,
          WFGA3, WFTM, WFTA,
          WOR, WDR, WAst,
          WTO, WStl, WBlk, WPF) %>%
   mutate(Team1ID = WTeamID,
          Team2ID = LTeamID,
          Team1Score = WScore, #duplicates result feature
          Team2Score = LScore, #duplicates result feature
          conf1 = Wconf,
          conf2 = Lconf,
          Loc = WLoc,  #Game Location
          FGM = WFGM,  #Field Goals Made, too similar to points scored
          FGA = WFGA,  #Field Goals Attempted
          FGM3 = WFGM3,  #3-Point Field Goals Made
          FGA3 = WFGA3,  #3-Point Field Goals Attempted
          FG3_PCT = WFGM3 / WFGA3,
          FTM = WFTM,  #Free-Throws Made
          FTA = WFTA,  #Free-Throws Attempted
          FT_PCT = ifelse(WFTA==0, 0, WFTM / WFTA),
          OR = WOR,  #Offensive Rebounds
          DR = WDR,  #Defensive Rebounds
          Ast = WAst,  #Assists
          TO = WTO,  #Turnovers
          Ast_to_TO = ifelse(WTO==0, WAst / .1, WAst / WTO),  #Assist-to-Turnover Ratio
          Stl = WStl,  #Steals
          Blk = WBlk,  #Blocks
          PF = WPF  #Personal Fouls
          ) %>%
   dplyr::select(-c(WTeamID, WScore, Wconf, Lconf, LTeamID, LScore, WLoc,
             WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA,
             WOR, WDR, WAst, WTO, WStl, WBlk, WPF)) %>%
   mutate(tourney_game = 0,
          result = 1)


#Regular Season Winner Dataset Team Rankings

 #Team1 rankings
 regular_W_daily_rank <- regular_W %>%
    group_by(Season, Team1ID) %>%
    left_join(mean_rank, by=c('Season'='Season', 'Team1ID'='TeamID', 'DayNum'='RankingDayNum')) %>%
   fill(rank) %>%
   fill(rank, .direction='up') %>%
   ungroup()
 colnames(regular_W_daily_rank)[length(names(regular_W_daily_rank))] <- 'rank1'
 regular_W_daily_rank$rank1[is.na(regular_W_daily_rank$rank1)] <- 350

  #Team2 rankings
 regular_W_daily_rank <- regular_W_daily_rank %>%
   group_by(Season, Team2ID) %>%
   left_join(mean_rank, by=c('Season'='Season', 'Team2ID'='TeamID', 'DayNum'='RankingDayNum')) %>%
   fill(rank) %>%
   fill(rank, .direction='up') %>%
   ungroup()
 colnames(regular_W_daily_rank)[length(names(regular_W_daily_rank))] <- 'rank2'
 regular_W_daily_rank$rank2[is.na(regular_W_daily_rank$rank2)] <- 350



#Regular Season Loser Dataset
 regular_L <- regular_conf %>%
   filter(Season < 2019) %>%
   dplyr::select(Season, DayNum, WTeamID, WScore, LTeamID, LScore, LLoc, NumOT,
          Wconf, Lconf, conf_diff,
          LFGM, LFGA, LFGM3,
          LFGA3, LFTM, LFTA,
          LOR, LDR, LAst,
          LTO, LStl, LBlk, LPF) %>%
   mutate(Team1ID = LTeamID,
          Team2ID = WTeamID,
          Team1Score = LScore, #duplicates result feature
          Team2Score = WScore, #duplicates result feature
          conf1 = Lconf,
          conf2 = Wconf,
          Loc = LLoc,
          FGM = LFGM, #too similar to points scored
          FGA = LFGA,
          FGM3 = LFGM3,
          FGA3 = LFGA3,
          FG3_PCT = LFGM3 / LFGA3,
          FTM = LFTM,
          FTA = LFTA,
          FT_PCT = ifelse(LFTA==0, 0, LFTM / LFTA),
          OR = LOR,
          DR = LDR,
          Ast = LAst,
          TO = LTO,
          Ast_to_TO = ifelse(LTO==0, LAst / .1, LAst / LTO),
          Stl = LStl,
          Blk = LBlk,
          PF = LPF) %>%
   dplyr::select(-c(WTeamID, WScore, LTeamID, LScore, Lconf, Wconf, LLoc,
             LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA,
             LOR, LDR, LAst, LTO, LStl, LBlk, LPF)) %>%
   mutate(tourney_game = 0,
          result = 0)


#Regular Season Loser Team Rankings
  #Team1 rankings
 regular_L_daily_rank <- regular_L %>%
    group_by(Season, Team1ID) %>%
    left_join(mean_rank, by=c('Season'='Season', 'Team1ID'='TeamID', 'DayNum'='RankingDayNum')) %>%
   fill(rank) %>%
   fill(rank, .direction='up') %>%
   ungroup()
 colnames(regular_L_daily_rank)[length(names(regular_L_daily_rank))] <- 'rank1'
 regular_L_daily_rank$rank1[is.na(regular_L_daily_rank$rank1)] <- 350

  #Team2 rankings
 regular_L_daily_rank <- regular_L_daily_rank %>%
   group_by(Season, Team2ID) %>%
   left_join(mean_rank, by=c('Season'='Season', 'Team2ID'='TeamID', 'DayNum'='RankingDayNum')) %>%
   fill(rank) %>%
   fill(rank, .direction='up') %>%
   ungroup()
 colnames(regular_L_daily_rank)[length(names(regular_L_daily_rank))] <- 'rank2'
 regular_L_daily_rank$rank2[is.na(regular_L_daily_rank$rank2)] <- 350



#Combine winners and losers datasets
 regular_combined <- rbind(regular_W_daily_rank, regular_L_daily_rank)





#Make winners and losers tournament data
 tourney$LLoc <- ifelse(tourney$WLoc=='H', 'A',
                        ifelse(tourney$WLoc=='N', 'N', 'H'))

#Join - conference
 tourney_conf <- tourney %>%
   left_join(conference,by=c("Season"="Season","WTeamID"="TeamID"))
 colnames(tourney_conf)[length(names(tourney_conf))] <- "Wconf"
 tourney_conf <- tourney_conf %>%
   left_join(conference,by=c("Season"="Season","LTeamID"="TeamID"))
 colnames(tourney_conf)[length(names(tourney_conf))] <- "Lconf"
 tourney_conf$conf_diff <- ifelse(tourney_conf$Wconf == tourney_conf$Lconf, 0 ,1)


#Tournament Winner Dataset
 tourney_W <- tourney_conf %>%
   filter(Season < 2019) %>%
   dplyr::select(Season, DayNum, WTeamID, WScore, LTeamID, LScore,WLoc, NumOT,
          Wconf, Lconf, conf_diff,
          WFGM, WFGA, WFGM3,
          WFGA3, WFTM, WFTA,
          WOR, WDR, WAst,
          WTO, WStl, WBlk, WPF) %>%
   mutate(Team1ID = WTeamID,
          Team2ID = LTeamID,
          Team1Score = WScore, #duplicates result feature
          Team2Score = LScore, #duplicates result feature
          conf1 = Wconf,
          conf2 = Lconf,
          Loc = WLoc,  #Game Location
          FGM = WFGM,  #Field Goals Made, too similar to points scored
          FGA = WFGA,  #Field Goals Attempted
          FGM3 = WFGM3, # 3-Point Field Goals Made
          FGA3 = WFGA3, # 3-Point Field Goals Attempted
          FG3_PCT = WFGM3 / WFGA3,
          FTM = WFTM,  #Free-Throws Made
          FTA = WFTA, # Free-Throws Attempted
          FT_PCT = ifelse(WFTA==0, 0, WFTM / WFTA),
          OR = WOR,  #Offensive Rebounds
          DR = WDR,  #Defensive Rebounds
          Ast = WAst, # Assists
          TO = WTO,  #Turnovers
          Ast_to_TO = ifelse(WTO==0, WAst / .1, WAst / WTO),#  Assist-to-Turnover Ratio
          Stl = WStl, # Steals
          Blk = WBlk,#  Blocks
          PF = WPF  #Personal Fouls
          ) %>%
   dplyr::select(-c(WTeamID, WScore, LTeamID, LScore, Wconf, Lconf, WLoc,
             WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA,
             WOR, WDR, WAst, WTO, WStl, WBlk, WPF)) %>%
   mutate(tourney_game = 1,
          result = 1)


#Tournament Winner Dataset Team Rankings
#Team1 rankings
 tourney_W_daily_rank <- tourney_W %>%
   group_by(Season, Team1ID) %>%
   left_join(seeds_sep, by=c("Season"="Season", "Team1ID"="TeamID")) %>%
   ungroup() %>%
   dplyr::select(-c(region))
 colnames(tourney_W_daily_rank)[length(tourney_W_daily_rank)] <- 'rank1'

#Team2 rankings
 tourney_W_daily_rank <- tourney_W_daily_rank %>%
   group_by(Season, Team2ID) %>%
   left_join(seeds_sep, by=c("Season"="Season", "Team2ID"="TeamID")) %>%
   ungroup() %>%
   dplyr::select(-c(region))
 colnames(tourney_W_daily_rank)[length(tourney_W_daily_rank)] <- 'rank2'


#Tournament Loser Dataset

 tourney_L <- tourney_conf %>%
   filter(Season < 2019) %>%
   dplyr::select(Season, DayNum, WTeamID, WScore, LTeamID, LScore,LLoc, NumOT,
          Wconf, Lconf, conf_diff,
          LFGM, LFGA, LFGM3,
          LFGA3, LFTM, LFTA,
          LOR, LDR, LAst,
          LTO, LStl, LBlk, LPF) %>%
   mutate(Team1ID = LTeamID,
          Team2ID = WTeamID,
          Team1Score = LScore, #duplicates result feature
          Team2Score = WScore, #duplicates result feature
          conf1 = Lconf,
          conf2 = Wconf,
          Loc = LLoc,
          FGM = LFGM, #too similar to points scored
          FGA = LFGA,
          FGM3 = LFGM3,
          FGA3 = LFGA3,
          FG3_PCT = LFGM3 / LFGA3,
          FTM = LFTM,
          FTA = LFTA,
          FT_PCT = ifelse(LFTA==0, 0, LFTM / LFTA),
          OR = LOR,
          DR = LDR,
          Ast = LAst,
          TO = LTO,
          Ast_to_TO = ifelse(LTO==0, LAst / .1, LAst / LTO),
          Stl = LStl,
          Blk = LBlk,
          PF = LPF) %>%
   dplyr::select(-c(WTeamID, WScore, LTeamID, LScore, Lconf, Wconf, LLoc,
             LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA,
             LOR, LDR, LAst, LTO, LStl, LBlk, LPF)) %>%
   mutate(tourney_game = 1,
          result = 0)


#Tournament Loser Dataset Team Rankings

#Team1 rankings
 tourney_L_daily_rank <- tourney_L %>%
   group_by(Season, Team1ID) %>%
   left_join(seeds_sep, by=c("Season"="Season", "Team1ID"="TeamID")) %>%
   ungroup() %>%
   dplyr::select(-c(region))
 colnames(tourney_L_daily_rank)[length(tourney_L_daily_rank)] <- 'rank1'

#Team2 rankings
 tourney_L_daily_rank <- tourney_L_daily_rank %>%
   group_by(Season, Team2ID) %>%
   left_join(seeds_sep, by=c("Season"="Season", "Team2ID"="TeamID")) %>%
   ungroup() %>%
   dplyr::select(-c(region))
 colnames(tourney_L_daily_rank)[length(tourney_L_daily_rank)] <- 'rank2'


tourney_combined <- rbind(tourney_W_daily_rank, tourney_L_daily_rank)


#Split tournament data
 tourney_train <- tourney_combined %>%
   filter(Season <= 2016)
 tourney_test <- tourney_combined %>%
   filter(Season %in% c(2017, 2018))

#Create a training set
 train <- rbind(regular_combined, tourney_train)




#convert strings to factors

  train$Team1ID <- as.factor(train$Team1ID)
  train$Team2ID <- as.factor(train$Team2ID)
  levels(train$Team1ID) <- make.names(levels(train$Team1ID))
  levels(train$Team2ID) <- make.names(levels(train$Team2ID))
 
  tourney_test$Team1ID <- as.factor(tourney_test$Team1ID)
  tourney_test$Team2ID <- as.factor(tourney_test$Team2ID)
  levels(tourney_test$Team1ID) <- make.names(levels(tourney_test$Team1ID))
  levels(tourney_test$Team2ID) <- make.names(levels(tourney_test$Team2ID))

 train$conf1 <- as.factor(train$conf1)
 train$conf2 <- as.factor(train$conf2)
 tourney_test$conf1 <- as.factor(tourney_test$conf1)
 tourney_test$conf2 <- as.factor(tourney_test$conf2)
 levels(train$conf1) <- make.names(levels(train$conf1))
 levels(train$conf2) <- make.names(levels(train$conf2))
 levels(tourney_test$conf1) <- make.names(levels(tourney_test$conf1))
 levels(tourney_test$conf2) <- make.names(levels(tourney_test$conf2))
 train$Loc <- as.factor(train$Loc)
 tourney_test$Loc <- as.factor(tourney_test$Loc)

 train$result <- as.factor(train$result)
 tourney_test$result <- as.factor(tourney_test$result)
 levels(train$result) <- c('L', 'W')
 levels(tourney_test$result) <- c('L', 'W')



#Let's remove the team variables because as factors they force the algorithms to take a very long time.
 train <- train %>%
   dplyr::select(-c(Team1ID, Team2ID))
 tourney_test <- tourney_test %>%
   dplyr::select(-c(Team1ID, Team2ID))

#Let's create a model without any offensive features.
 train_def <- train %>%
   dplyr::select(-c("FGA", "FGM3", "FGA3", "FG3_PCT", "FTM", "FTA", "FT_PCT", "OR", "Ast", "TO", "Ast_to_TO"))
 tourney_test_def <- tourney_test %>%
   dplyr::select(-c("FGA", "FGM3", "FGA3", "FG3_PCT", "FTM", "FTA", "FT_PCT", "OR", "Ast", "TO", "Ast_to_TO"))

 train_def$Loc <- as.factor(train_def$Loc)
 tourney_test_def$Loc <- as.factor(tourney_test_def$Loc)
 train_def$result <- as.factor(train_def$result)
 tourney_test_def$result <- as.factor(tourney_test_def$result)
 levels(train_def$result) <- c('L', 'W')
 levels(tourney_test_def$result) <- c('L', 'W')




###Exploratory Data Analysis

#PCA
 train_for_pca <- train %>%
   dplyr::select(-c(Season, result))

 train_for_pca_matrix <- model.matrix(~., data=train_for_pca)
 train_for_pca_matrix <- train_for_pca_matrix[,-1]  remove intercept column
 train_pca <- prcomp(train_for_pca_matrix, scale=TRUE, center=TRUE)
 train_pca$sdev / (train_pca$sdev %>% sum)
 cumsum(train_pca$sdev / (train_pca$sdev %>% sum))


#Look at the strongest relations between different features and the first 3 principal components
 tail(train_pca$rotation[order(abs(train_pca$rotation[,1])),1:3], 30)

#1st component
 tail(train_pca$rotation[order(abs(train_pca$rotation[,2])),1:3], 30)

#2nd component
 tail(train_pca$rotation[order(abs(train_pca$rotation[,3])),1:3], 30)



#Visualizing PCA
 train_pca_df <- as.data.frame(train_pca$x)
 train_pca_df$result <- train$result


 train_pca_df %>% ggplot() +
   geom_point(mapping=aes(x=train_pca_df$PC1,
                          y=train_pca_df$PC2,
                          colour=factor(train_for_pca_matrix[,91],
                          labels=c('regular', 'tournament')))) +
   labs(color='Game Type')

 train_pca_df %>% ggplot() +
   geom_point(mapping=aes(x=train_pca_df$PC1,
                          y=train_pca_df$PC3,
                          colour=factor(train_for_pca_matrix[,91],
                          labels=c('regular', 'tournament')))) +
   labs(color='Game Type')


 
 train_pca_df %>% ggplot() +
   geom_point(mapping=aes(x=train_pca_df$PC2,
                          y=train_pca_df$PC3,
                          colour=train$Loc)) +
   labs(color='Game Type')




#Home-Court Advantage
 FTA1 <- train %>%
         ggplot() +
         geom_density(mapping=aes(x=FTA, fill=Loc), alpha=1/10) +
         labs(title = "FTA by Game Location", x= "Free Throw Attempted", y= "", fill = "Location")
 FTA2 <- train %>%
         ggplot() +
         geom_density(mapping=aes(x=FT_PCT, fill=Loc), alpha=1/10) +
         labs(title = "FT_PCT by Game Location",x = "Free Throw Percentage", fill = "Location")
 grid.arrange(FTA1,FTA2,ncol=2)


 data.frame(train$result,train$Loc) %>%
   ggplot()+
   geom_bar(mapping = aes(x = train$result, y = ..count.., fill = train$Loc ), position = position_dodge())+
   labs(title = "Relationship between Result and Location", x = "Result", y = "Times", fill = "location")


 train %>%
   ggplot() +
   geom_bar(mapping=aes(x=Blk, y=..prop.., fill=Loc), position=position_dodge()) +
   labs(title = "Blocks in Home and Away Games",x = "Blocks", y="Proportion", fill = "Location")




#Tournament Games Pressure

 train %>%
         ggplot() +
         geom_bar(mapping=aes(x=FTM, y=..prop.., fill=as.factor(tourney_game)), position=position_dodge()) +
         labs(title = "FTM and Game Type", x = "Free Throw Made", fill = "Game Type (Tournament = 1)")

 train %>%
         ggplot() +
         geom_density(mapping=aes(x=FT_PCT, fill=as.factor(tourney_game)), alpha=1/5) +
         labs(title = "FTM and Game Type", x = "Free Throw Percentage", fill = "Game Type (Tournament = 1)")



 train %>%
   ggplot() +
   geom_density(mapping=aes(x=FG3_PCT, fill=as.factor(tourney_game)), alpha=1/5) +
   labs(title = "3-Point Field Goals and Game Type", x = "3-Point Field Goal Percentage", fill = "Game Type (Tournament = 1)")


 train %>%
   ggplot() +
   geom_density(mapping=aes(x=log(Ast_to_TO), fill=as.factor(tourney_game)), alpha=1/5)+
   labs(title = "Assist-to-Turnover Ratio and Game Type", x = "Log of Assist-to-Turnover Ratio", fill = "Game Type (Tournament = 1)")


 train %>%
   ggplot() +
   geom_density(mapping=aes(x=PF, fill=as.factor(tourney_game)), alpha=1/5) +
   labs(title = "Personal Fouls and Game Type", x = "Personal Fouls", fill = "Game Type (Tournament = 1)")


 train %>%
   group_by(DayNum) %>%
   summarize(mean_Ast_to_TO = mean(Ast_to_TO)) %>%
     ggplot() +
     geom_line(mapping=aes(x=DayNum, y=mean_Ast_to_TO)) +
     labs(y = "Average Assist-to-Turnover Ratio", x = "Day of the Season")



###Individual Models

#Logistic Regression
 model1 <- glm(result ~ .,data=train,family="binomial")

 model1 %>%
   summary

 model1_pred_train <- model1 %>%
   predict(train, type="response") %>%
   prediction(labels=train$result)

 model1_pred_train_ensemble <- model1 %>%
   predict(train, type="response")


 model1_pred_test <- model1 %>%
   predict(tourney_test,type="response") %>%
   prediction(labels=tourney_test$result)


 model1_pred_test_ensemble <- model1 %>%
   predict(tourney_test,type="response")


performance(model1_pred_test,"auc")@y.values[[1]]

#Logistic Regression ROC Curve:
 logreg_plot <- performance(model1_pred_test,'tpr','fpr')
 plot(log_plot)



#LDA
 library(MASS)
 model2 <- lda(result~.,data = train)
 model2

 model2_pred_train <- model2 %>%
   predict(train) %>%
   (function(x) x$posterior[,2]) %>%
   prediction(labels=train$result)

 model2_pred_train_ensemble <- model2 %>%
   predict(train) %>%
   (function(x) x$posterior[,2])

 model2_pred_test <- model2 %>%
   predict(tourney_test) %>%
   (function(x) x$posterior[,2]) %>%
   prediction(labels=tourney_test$result)


 model2_pred_test_ensemble <- model2 %>%
   predict(tourney_test) %>%
   (function(x) x$posterior[,2])




#LDA AUC
performance(model2_pred_test,"auc")@y.values[[1]]

#LDA ROC Curve:
 lda_plot <- performance(model2_pred_test,'tpr','fpr')
 plot(lda_plot)




#QDA
 model3 <- qda(result~.,data = train)
 model3

 model3_pred_train <- model3 %>%
   predict(train) %>%
   (function(x) x$posterior[,2]) %>%
   prediction(labels=train$result)

 model3_pred_test <- model3 %>%
   predict(tourney_test) %>%
   (function(x) x$posterior[,2]) %>%
   prediction(labels=tourney_test$result)





#QDA AUC: 
performance(model3_pred_test,"auc")@y.values[[1]]

#QDA ROC Curve:
 qda_plot <- performance(model3_pred_test,'tpr','fpr')
 plot(qda_plot)



#Decision Trees

 library(tree)
 library(dplyr)

 train_tree <- train %>%
   dplyr::select(-c("conf1","conf2"))

 test_tourney_tree <- tourney_test %>%
   dplyr::select(-c("conf1","conf2"))

 set.seed(111)
 model.tree <- tree(result~., data= train_tree)
 tree.prune <- prune.misclass(model.tree, best =3)

 tree.pred.train <- prediction(predictions = as.numeric(predict(tree.prune, train_tree, type = 'class')),
 labels = as.numeric(train_tree$result))
 model4_pred_train <- tree.pred.train

 performance(tree.pred.train, 'auc')@y.values[[1]]



#Get test set predictions for ensembling
      tree.pred.test <- prediction(predictions=as.numeric(predict(tree.prune, test_tourney_tree, type='class')),
                            labels=as.numeric(test_tourney_tree$result))
 model4_pred_test <- tree.pred.test





 #Decision Tree AUC: 
   rperformance(model4_pred_test, 'auc')@y.values[[1]]

 #Decision Tree ROC Curve:
 tree_plot <- performance(model4_pred_test,'tpr','fpr')
 plot(tree_plot)




#XGBoost
  #####Note XGBoost can't handle factor variables with only one level
 
 train_for_xgboost <- train %>%
   dplyr::select(-c(conf1, conf2, tourney_game, Loc, result))
 train_for_xgboost_matrix <- model.matrix(~., data=train_for_xgboost)[,-1]

 train_def_for_xgboost <- train_def %>%
   dplyr::select(-c(conf1, conf2, tourney_game, Loc, result))
 train_def_for_xgboost_matrix <- model.matrix(~., data=train_def_for_xgboost)[,-1]

 test_for_xgboost <- tourney_test %>%
   dplyr::select(-c(conf1, conf2, tourney_game, Loc, result))
 test_for_xgboost_matrix <- model.matrix(~., data=test_for_xgboost)[,-1]

 test_def_for_xgboost <- tourney_test_def %>%
   dplyr::select(-c(conf1, conf2, tourney_game, Loc, result))
 test_def_for_xgboost_matrix <- model.matrix(~., data=test_def_for_xgboost)[,-1]

 d_train <- train_for_xgboost_matrix %>%
   xgb.DMatrix(label=(as.integer(train$result)-1))
 d_train_def <- train_def_for_xgboost_matrix %>%
   xgb.DMatrix(label=(as.integer(train_def$result)-1))


 d_test <- test_for_xgboost_matrix %>%
   xgb.DMatrix(label=(as.integer(tourney_test$result)-1))
 d_test_def <- test_def_for_xgboost_matrix %>%
   xgb.DMatrix(label=(as.integer(tourney_test_def$result)-1))



 
 tune_grid <- expand.grid(nrounds = 150, c(50, 100, 150, 250, 300),
                    max_depth = 15,  c(2,4,6,10,15,20),
                    eta = 0.1,
                    gamma = 1,
                    colsample_bytree = .7,
                    min_child_weight = 50,
                    subsample = .3)  c(.3, .5, .7, 1)

 tune_control <- caret::trainControl(method='cv', number=3)
 xgb_tune <- caret::train(x=train_for_xgboost_matrix,
                          y=train$result,
                          trControl=tune_control,
                          tuneGrid=tune_grid,
                          method='xgbTree'
                          )



 
 depth <- 15
 shrinkage <- .1
 gamma <- 1
 colSample <- .7
 minChildWeight <- 50
 nrounds <- 150
 subSample <- .3
 set.seed(876)
 model5_xgb <- xgboost(params=list(max_depth=depth,
                                  eta=shrinkage,
                                  gamma=gamma,
                                  colsample_by_tree=colSample,
                                  min_child_weight=minChildWeight,
                                  subsample=subSample),
                      data=d_train,
                      nrounds=150,
                      objective='binary:logistic',
                      eval_metric='auc')



 
  #XGBoost Model for Defenseive variables
 set.seed(564)
 model5_xgb_def <- xgboost(params=list(max_depth=depth,
                                  eta=shrinkage,
                                  gamma=gamma,
                                  colsample_by_tree=colSample,
                                  min_child_weight=minChildWeight,
                                  subsample=subSample),
                      data=d_train_def,
                      nrounds=150,
                      objective='binary:logistic',
                      eval_metric='auc')



 
 model5_pred_train <- predict(model5_xgb, d_train) %>%
   prediction(labels=train$result)
 model5_pred_test <- predict(model5_xgb, d_test) %>%
   prediction(labels=tourney_test$result)


 model5_pred_train_ensemble <- predict(model5_xgb, d_train)
 model5_pred_test_ensemble <- predict(model5_xgb, d_test)


#XGBoost AUC: 
performance(model5_pred_test,"auc")@y.values[[1]]

#XGBoost ROC Curve:
 xgb_plot <- performance(model5_pred_test, 'tpr', 'fpr')
 plot(xgb_plot)


 
 model5_xgb_def_pred_test <- predict(model5_xgb_def, d_test_def) %>%
   prediction(labels=tourney_test_def$result)


 #XGBoost Defense AUC: 
performance(model5_xgb_def_pred_test, "auc")@y.values[[1]]

# XGBoost Defense ROC Curve:

 xgb_def_plot <- performance(model5_xgb_def_pred_test, 'tpr', 'fpr')
 plot(xgb_def_plot)


 
#XGBoost Feature Importance
 feature_importance <- xgb.importance(colnames(train), model=model5_xgb)
 xgb.ggplot.importance(feature_importance)




#Two More XGBoost Models for Ensembling
 
#Let's run 2 more XGBoost Models to ensemble in the next step.

 set.seed(32)
 model6_xgb <- xgboost(params=list(max_depth=depth,
                                  eta=shrinkage,
                                  gamma=gamma,
                                  colsample_by_tree=colSample,
                                  min_child_weight=minChildWeight,
                                  subsample=subSample),
                      data=d_train,
                      nrounds=150,
                      objective='binary:logistic',
                      eval_metric='auc')

 model6_pred_train <- predict(model6_xgb, d_train) %>%
   prediction(labels=train$result)
 model6_pred_test <- predict(model6_xgb, d_test) %>%
   prediction(labels=tourney_test$result)

 model6_pred_train_ensemble <- predict(model6_xgb, d_train)
 model6_pred_test_ensemble <- predict(model6_xgb, d_test)

 set.seed(123)
 model7_xgb <- xgboost(params=list(max_depth=depth,
                                  eta=shrinkage,
                                  gamma=gamma,
                                  colsample_by_tree=colSample,
                                  min_child_weight=minChildWeight,
                                  subsample=subSample),
                      data=d_train,
                      nrounds=150,
                      objective='binary:logistic',
                      eval_metric='auc')

 model7_pred_train <- predict(model7_xgb, d_train) %>%
   prediction(labels=train$result)
 model7_pred_test <- predict(model7_xgb, d_test) %>%
   prediction(labels=tourney_test$result)

 model7_pred_train_ensemble <- predict(model7_xgb, d_train)
 model7_pred_test_ensemble <- predict(model7_xgb, d_test)



 #XGBoost Model 2 AUC: 
   performance(model6_pred_test, "auc")@y.values[[1]]

#XGBoost Model 2 ROC Curve:
model6_xgb__plot <- performance(model6_pred_test, 'tpr', 'fpr')
 plot(model6_xgb_plot)


 #XGBoost Model 3 AUC: 
 performance(model7_pred_test, "auc")@y.values[[1]]

# XGBoost Model 3 ROC Curve:
 model7_xgb__plot <- performance(model7_pred_test, 'tpr', 'fpr')
 plot(model7_xgb_plot)


##Ensemble Models
 #Exclude QDA and Decision Tree

 new_train <- data.frame(result = train$result,
                         model1_pred = model1_pred_train_ensemble,
                         model2_pred = model2_pred_train_ensemble,
                         model5_pred = model5_pred_train_ensemble,
                         model6_pred = model6_pred_train_ensemble,
                         model7_pred = model7_pred_train_ensemble)

 new_test <- data.frame(result = tourney_test$result,
                        model1_pred = model1_pred_test_ensemble,
                        model2_pred = model2_pred_test_ensemble,
                        model5_pred = model5_pred_test_ensemble,
                        model6_pred = model6_pred_test_ensemble,
                        model7_pred = model7_pred_test_ensemble)


#Random Forest Stack Model
 set.seed(42)
 modelrf_stack <- randomForest(result~., data=new_train, mtry=3, ntree=200)
 modelrf_stack_pred_train <- predict(modelrf_stack, newdata=new_train, type='prob')[,2] %>%
   prediction(labels=new_train$result)


 
#Random Forest Train AUC
 performance(modelrf_stack_pred_train, 'auc')@y.values[[1]]


 
 modelrf_stack_pred_test <- predict(modelrf_stack, newdata=new_test, type='prob')[,2] %>%
   prediction(labels=new_test$result)



 #Random Forest Ensemble Model AUC: 
 performance(modelrf_stack_pred_test, 'auc')@y.values[[1]]

 #Random Forest Ensemble Model ROC Curve:
 modelrf_stack_plot <- performance(modelrf_stack_pred_test, 'tpr', 'fpr')
 plot(modelrf_stack_plot)




 
#XGBoost Stack Model
 d_new_train <- new_train %>%
   dplyr::select(-result) %>%
   as.matrix %>%
   xgb.DMatrix(label=(as.integer(new_train$result)-1))

 d_new_test <- new_test %>%
   dplyr::select(-result) %>%
   as.matrix %>%
   xgb.DMatrix(label=(as.integer(new_test$result)-1))


 
 set.seed(128)
 model_xgb_stack <- xgboost(params=list(max_depth=depth,
                                        eta=shrinkage,
                                        gamma=gamma,
                                        colsample_bytree=colSample,
                                        min_child_weight=minChildWeight,
                                        subsample=subSample),
                            data=d_new_train,
                            nrounds=150,
                            objective="binary:logistic",
                            eval_metric="auc")



 
#XGBoost Train AUC
 model_xgb_stack_pred_train <- predict(model_xgb_stack, d_new_train) %>%
   prediction(labels=new_train$result)
 performance(model_xgb_stack_pred_train, 'auc')@y.values[[1]]


 
#XGBoost Test AUC
 model_xgb_stack_pred_test <- predict(model_xgb_stack, d_new_test) %>%
   prediction(labels=new_test$result)
 performance(model_xgb_stack_pred_test, 'auc')@y.values[[1]]



#XGBoost Ensemble Model AUC: 
 performance(model_xgb_stack_pred_test, 'auc')@y.values[[1]]

# XGBoost Ensemble Model ROC Curve:
 model_xgb_stack_plot <- performance(model_xgb_stack_pred_test, 'tpr', 'fpr')
 plot(model_xgb_stack_plot)



 
#Logistic Regression Stack Model
 model_logreg_stack <- glm(result~., data=new_train, family='binomial')
 model_logreg_stack %>% summary


 
 model_logreg_stack_pred_train <- model_logreg_stack %>%
   predict(new_train, type='response') %>%
   prediction(labels=new_train$result)
 performance(model_logreg_stack_pred_train, 'auc')@y.values[[1]]


 
 model_logreg_stack_pred_test <- model_logreg_stack %>%
   predict(new_test, type='response') %>%
   prediction(labels=new_test$result)
 performance(model_logreg_stack_pred_test, 'auc')@y.values[[1]]


 #Logistic Regression Ensemble Model AUC: 
 performance(model_logreg_stack_pred_test, 'auc')@y.values[[1]]

# Logistic Regression Ensemble Model ROC Curve:
 model_logreg_stack_plot <- performance(model_logreg_stack_pred_test, 'tpr', 'fpr')
 plot(model_logreg_stack_plot)


