########### 
#### Code for Publication
####
#### Mark Colley, Annika Stampf, William Fischer, and Enrico Rukzio. 2023. 
#### Effects of 3D Displays on Mental Workload, Situation Awareness, Trust, and Performance Assessment in Automated Vehicles. 
#### In Proceedings of the 22nd International Conference on Mobile and Ubiquitous Multimedia (MUM '23). 
#### Association for Computing Machinery, New York, NY, USA, 128–138. https://doi.org/10.1145/3626705.3627786
####
#### ATTENTION: Add directory "plot" for figures to save properly


library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))



library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")

library(stargazer)
library(reporttools)
library(xtable)
# library(equatiomatic)
# library(DataEditR)

# Bayes Analysis
library(BayesFactor)



main_df <- readxl::read_xlsx(path = "3d-comp-vehicle-main.xlsx", sheet = "Results")
main_df <- as.data.frame(main_df)
names(main_df)





main_df$UserID <- as.factor(main_df$UserID)
main_df$ConditionID <- as.factor(main_df$ConditionID)




# SART
main_df$Demand <- main_df$SART1 + main_df$SART2 + main_df$SART3
main_df$Supply <- main_df$SART4 + main_df$SART5 + main_df$SART6 + main_df$SART7
main_df$Understanding <- main_df$SART8 + main_df$SART9 + main_df$SART10

main_df$SA <- main_df$Understanding - (main_df$Demand - main_df$Supply)


main_df$overallTiATrust <- rowSums(main_df[, c("TiA_Trust1", "TiA_Trust2")]) / 2.0

# Calculate relevant trust scores:
# tiau 2 und 4 inverse
main_df$TiA_U2 <- 6 - main_df$TiA_U2
main_df$TiA_U4 <- 6 - main_df$TiA_U4
main_df$overallTiAUnderstanding <- rowSums(main_df[, c("TiA_U1", "TiA_U2", "TiA_U3", "TiA_U4")]) / 4.0


main_df$ps_score <- rowSums(main_df[, c("perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4")]) / 4.0


labels_xlab <- c("LookingGlass" = "LookingGlass", "LumePad" = "LumePad", "2D" = "2D")



vars <- main_df[,c('TLX1', 'ps_score', 'Understanding', 'Demand', 'Supply', 'SA', 'overallTiATrust', 'overallTiAUnderstanding', 'drivingstyle', 'longitudinal', 'lateral', 'unsafeJudgement', 'reacted_appropriate', 'performbetter', 'recognized_vehicle', 'predict_paths', 'clear_av_will_do', 'recognize_road_users', 'perceive_distances_better', 'perceive_surroundings_better', 'certain_drive_to')]
group <- main_df[,c('ConditionID')]

#DataEditR::data_edit(main_df)

# see https://stackoverflow.com/questions/25389683/obtaining-separate-summary-statistics-by-categorical-variable-with-stargazer-pac

## display default statistics, only use a subset of observations, grouped analysis
tableContinuous(vars = vars, group = group, prec = 2, cap = "Table of scores.", lab = "tab:_descr_stat",  stats = c("min", "q1", "median", "mean", "q3", "max",  "s", "iqr"))





########### TLX
# not sig.
#ggwithinstats(data = main_df, x = ConditionID, y = TLX1, type = "np",  xlab = labels_xlab)
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "TLX1", ylab = "Mental Workload", xlabels = labels_xlab)

BayesFactor::anovaBF(TLX1 ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


########### Trust
# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "overallTiAUnderstanding", ylab = "Understanding", xlabels = labels_xlab)

anovaBF(overallTiAUnderstanding ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "overallTiATrust", ylab = "Trust in Automation", xlabels = labels_xlab)

anovaBF(overallTiATrust ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()



###### SART ######

# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "SA", ylab = "Situation Awareness", xlabels = labels_xlab)

anovaBF(SA ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig. - almost
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "Demand", ylab = "Demand", xlabels = labels_xlab)

anovaBF(Demand ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "Supply", ylab = "Supply", xlabels = labels_xlab)

anovaBF(Supply ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "Understanding", ylab = "Understanding", xlabels = labels_xlab)


anovaBF(Understanding ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()




############# Perceived Safety ####
# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "ps_score", ylab = "Perceived Safety", xlabels = labels_xlab)


anovaBF(ps_score ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()





###### Recognitions ####


# sig. no post-hoc
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "recognized_vehicle", ylab = "Recognized Vehicles", xlabels = labels_xlab)

anovaBF(recognized_vehicle ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "predict_paths", ylab = "Predict Vehicle Path", xlabels = labels_xlab)

anovaBF(predict_paths ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "clear_av_will_do", ylab = "Clarity next action", xlabels = labels_xlab)

anovaBF(clear_av_will_do ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "recognize_road_users", ylab = "recognize_road_users", xlabels = labels_xlab)

anovaBF(recognize_road_users ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "perceive_distances_better", ylab = "perceive_distances_better", xlabels = labels_xlab)

anovaBF(perceive_distances_better ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()





# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "perceive_surroundings_better", ylab = "perceive_surroundings_better", xlabels = labels_xlab)

anovaBF(perceive_surroundings_better ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "certain_drive_to", ylab = "certain_drive_to", xlabels = labels_xlab)

anovaBF(certain_drive_to ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()




##### Driving Style ####

# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "drivingstyle", ylab = "Driving Style", xlabels = labels_xlab)

anovaBF(drivingstyle ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "longitudinal", ylab = "Longitudinal Guidance", xlabels = labels_xlab)

anovaBF(longitudinal ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "lateral", ylab = "Lateral Guidance", xlabels = labels_xlab)

anovaBF(lateral ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()





##### STS-AD #####
# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "unsafeJudgement", ylab = "Unsafe Judgement", xlabels = labels_xlab)

anovaBF(unsafeJudgement ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()




# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "reacted_appropriate", ylab = "AV reacted appropriately", xlabels = labels_xlab)

anovaBF(reacted_appropriate ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()


# not sig.
ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "performbetter", ylab = "I would have performed better", xlabels = labels_xlab)

anovaBF(performbetter ~ ConditionID + UserID, data = main_df, whichRandom="UserID")  |> bayesfactor_models() |> report()

















#### Ranking ####
library(tidyr)

rank_df <- readxl::read_xlsx(path = "3d-comp-vehicle-final.xlsx", sheet = "final")
rank_df <- as.data.frame(rank_df)
names(rank_df)


ranking <- NULL
ranking <- rank_df[,16:18] #only the rankings
ranking

# Landmark
ranking$twoD <-  which(apply(ranking[ ,c(1:3)], 1, function(x) grepl("2D", x)) ,arr.ind=TRUE)[,1]

# 
ranking$LookingGlass <-  which(apply(ranking[ ,c(1:3)], 1, function(x) grepl("LookingGlass", x)) ,arr.ind=TRUE)[,1]

# 
ranking$LumePad <-  which(apply(ranking[ ,c(1:3)], 1, function(x) grepl("LumePad", x)) ,arr.ind=TRUE)[,1]


ranking_number <- ranking[,c(4:6)]
names(ranking_number)

data_long <- gather(ranking_number, key = ConditionID, value = rank, factor_key=TRUE)
data_long

rank_x_lab <- c("twoD" = "2D", "LookingGlass" = "LookingGlass", "LumePad" = "LumePad")


ggwithinstatsWithPriorNormalityCheckAsterisk(data = data_long, x = "ConditionID", y = "rank", ylab = "Rank (lower is better)", xlabels = rank_x_lab)
ggsave("plots/rank_devices.pdf", width = pdfwidth + 5, height = pdfheight + 2, device = cairo_pdf)










