
rm(list=ls(all=TRUE))

library("xtable")
library("ggplot2")

options(scipen=999)

data <- readRDS("/Users/Ang/Desktop/Research/Counterfactual slopes/Data/data_cleaned_mare_hs.rds")
colMeans(is.na(data))

data$attendance23 <- as.numeric(data$attendance23)-1
data$HS20 <- as.numeric(data$HS20)-1

Y="attendance23"
D="HS20"
G="parental_income_log"
X <- c("parental_income_log","gender","medu","parental_presence",
       "n_sib","urban","edu_exp","AFQT","age","friend_edu_exp","rotter_score","rosenberg_irt_score",
       "sig_other_expec","foreign_lang",
       "SMSA","mother_seperate","fm_foreign_born",
       "region","m_work","race")
data=data[,c(Y,D,X)]

data <- na.omit(data)

#############################################################################################################################################

set.seed(1)

#tune grid for neural networks
tune_grid <- expand.grid(size = c(1, 3, 5), decay = c(0.1, 0.25, 0.5, 0.75, 1))
#############################################################################################################################################

##### phi1 (relationship between Y1 and G)

## estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

## treatment model
data[,D] <- as.factor(data[,D])

message <- utils::capture.output( DgivenX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(X, collapse="+"), sep="~")), data=data[sample1,], method="nnet",
                                                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))
message <- utils::capture.output( DgivenX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(X, collapse="+"), sep="~")), data=data[sample2,], method="nnet",
                                                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenX.Pred <- rep(NA, nrow(data))

DgivenX.Pred[sample2] <- stats::predict(DgivenX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenX.Pred[sample1] <- stats::predict(DgivenX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

## outcome regression model
data[,Y] <- as.factor(data[,Y])

message <- utils::capture.output( YgivenDX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(c(D,X), collapse="+"), sep="~")), data=data[sample1,], method="nnet",
                                                                         preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))
message <- utils::capture.output( YgivenDX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(c(D,X), collapse="+"), sep="~")), data=data[sample2,], method="nnet",
                                                                         preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))

data[,Y] <- as.numeric(data[,Y])-1

# outcome predictions
YgivenX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 1

YgivenX.Pred_D1[sample2] <- stats::predict(YgivenDX.Model.sample1, newdata = pred_data[sample2,], type="prob")[,2]
YgivenX.Pred_D1[sample1] <- stats::predict(YgivenDX.Model.sample2, newdata = pred_data[sample1,], type="prob")[,2]

## The "IPO" (individual potential outcome) function
# For each d value, we have IPO(d,g)=\frac{\one(D=d)}{\pi(d,X)}[Y-\mu(d,X)]+\mu(d,X)
# We stabilize the weight by dividing the sample average of estimated weights

IPO_D1 <- data[,D]/DgivenX.Pred/mean(data[,D]/DgivenX.Pred)*(data[,Y]-YgivenX.Pred_D1) + YgivenX.Pred_D1
data$IPO_D1 <- IPO_D1


### For the purpose of getting tau1, get YgivenX.Pred_D1 and DgivenX.Pred without cross-fitting
YgivenX.Pred_D1_ncf <- rep(NA, nrow(data))
YgivenX.Pred_D1_ncf[sample1] <- stats::predict(YgivenDX.Model.sample1, newdata = pred_data[sample1,], type="prob")[,2]
YgivenX.Pred_D1_ncf[sample2] <- stats::predict(YgivenDX.Model.sample2, newdata = pred_data[sample2,], type="prob")[,2]

DgivenX.Pred_ncf <- rep(NA, nrow(data))
DgivenX.Pred_ncf[sample1] <- stats::predict(DgivenX.Model.sample1, newdata = data[sample1,], type="prob")[,2]
DgivenX.Pred_ncf[sample2] <- stats::predict(DgivenX.Model.sample2, newdata = data[sample2,], type="prob")[,2]

data$IPO_D1_ncf <- data[,D]/DgivenX.Pred_ncf/mean(data[,D]/DgivenX.Pred_ncf)*(data[,Y]-YgivenX.Pred_D1_ncf) + YgivenX.Pred_D1_ncf

message <- utils::capture.output( tau1_model.sample1 <- caret::train(stats::as.formula(paste("IPO_D1_ncf", G, sep="~")), data=data[sample1,], method="nnet",
                                                                         preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=TRUE ))
message <- utils::capture.output( tau1_model.sample2 <- caret::train(stats::as.formula(paste("IPO_D1_ncf", G, sep="~")), data=data[sample2,], method="nnet",
                                                                     preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=TRUE ))

tau1 <- rep(NA, nrow(data))
tau1[sample2] <- stats::predict(tau1_model.sample1, newdata = pred_data[sample2,])
tau1[sample1] <- stats::predict(tau1_model.sample2, newdata = pred_data[sample1,])

head(tau1[order(tau1,decreasing=TRUE)],5)
head(tau1[order(tau1,decreasing=FALSE)],5)

phi1 <- cov((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)), data[,G])/var(data[,G])

## Standard error
eif1 <- ((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)) - mean((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1

se <- function(x) {sqrt( mean(x^2)/nrow(data) )}
se1 <- se(eif1)


##### phi0 (relationship between D and G)
data[,D] <- as.factor(data[,D])

message <- utils::capture.output( DgivenG.Model.sample1 <- caret::train(stats::as.formula(paste(D, G, sep="~")), data=data[sample1,], method="nnet",
                                                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))
message <- utils::capture.output( DgivenG.Model.sample2 <- caret::train(stats::as.formula(paste(D, G, sep="~")), data=data[sample2,], method="nnet",
                                                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenG.Pred <- rep(NA, nrow(data))

DgivenG.Pred[sample2] <- stats::predict(DgivenG.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenG.Pred[sample1] <- stats::predict(DgivenG.Model.sample2, newdata = data[sample1,], type="prob")[,2]

sum(DgivenG.Pred==0)

phi0 <- cov((data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred)), data[,G])/var(data[,G])

eif0 <- ((data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred)) - mean((data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi0

se0 <- se(eif0)

##### phi1_desc (relationship between Y and G conditional on D=1)
# outcome regression model
data[,Y] <- as.factor(data[,Y])

message <- utils::capture.output( YgivenDG.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(c(D,G), collapse="+"), sep="~")), data=data[sample1,], method="nnet",
                                                                         preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))
message <- utils::capture.output( YgivenDG.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(c(D,G), collapse="+"), sep="~")), data=data[sample2,], method="nnet",
                                                                         preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), tuneGrid=tune_grid, linout=FALSE ))

data[,Y] <- as.numeric(data[,Y])-1

# outcome predictions
YgivenG.Pred_D1 <- rep(NA, nrow(data))

YgivenG.Pred_D1[sample2] <- stats::predict(YgivenDG.Model.sample1, newdata = pred_data[sample2,], type="prob")[,2]
YgivenG.Pred_D1[sample1] <- stats::predict(YgivenDG.Model.sample2, newdata = pred_data[sample1,], type="prob")[,2]

head(YgivenG.Pred_D1[order(YgivenG.Pred_D1,decreasing=TRUE)],5)

IPO_D1_desc <- data[,D]/DgivenG.Pred/mean(data[,D]/DgivenG.Pred)*(data[,Y]-YgivenG.Pred_D1) + YgivenG.Pred_D1

phi1_desc <- cov((IPO_D1_desc-YgivenG.Pred_D1)/(YgivenG.Pred_D1*(1-YgivenG.Pred_D1)) + log(YgivenG.Pred_D1/(1-YgivenG.Pred_D1)), data[,G])/var(data[,G])

eif1_desc <- ((IPO_D1_desc-YgivenG.Pred_D1)/(YgivenG.Pred_D1*(1-YgivenG.Pred_D1)) + log(YgivenG.Pred_D1/(1-YgivenG.Pred_D1)) - mean((IPO_D1_desc-YgivenG.Pred_D1)/(YgivenG.Pred_D1*(1-YgivenG.Pred_D1)) + log(YgivenG.Pred_D1/(1-YgivenG.Pred_D1))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1_desc

se1_desc <- se(eif1_desc)

##### Differences
diff <- phi0-phi1
diff_desc <- phi0-phi1_desc

se_diff <- se(eif0-eif1)
se_diff_desc <- se(eif0-eif1_desc)

##### Output
detail <- data.frame(log_odds1=(IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)), 
                     log_odds1_desc=(IPO_D1_desc-YgivenG.Pred_D1)/(YgivenG.Pred_D1*(1-YgivenG.Pred_D1)) + log(YgivenG.Pred_D1/(1-YgivenG.Pred_D1)), 
                     log_odds0=(data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred)),
                     G=data[,G], 
                     Y=data[,Y], 
                     D=data[,D])

overall <- data.frame(item=c("diff_desc","diff","phi0","phi1_desc","phi1"),
                      point=c(diff_desc, diff, phi0, phi1_desc,phi1),
                      se=c(se_diff_desc, se_diff, se0, se1_desc, se1))
overall$CI_lower <- overall$point - qnorm(0.975)*overall$se
overall$CI_higher <- overall$point + qnorm(0.975)*overall$se
overall$pvalues <- 1-abs(pnorm(overall$point/overall$se)-0.5)*2

overall

plot_smooth <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="loess", data=detail, aes(x=G, y=log_odds1, color='Y1 | G'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", data=detail, aes(x=G, y=log_odds1_desc, color='Y | G, D=1'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", data=detail, aes(x=G, y=log_odds0, color='D | G'), se=FALSE, span=1.5) +
  scale_color_manual(breaks=c("Y1 | G", "Y | G, D=1", "D | G"),
                     values=c("Y1 | G"="green", "Y | G, D=1"="purple", "D | G"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("Transition log odds") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))

plot_smooth

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_HS_smooth",".jpg", sep=""), plot_smooth, width=6, height=3.5)

plot_linear <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds1, color='Y_1 | G'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds1_desc, color='Y | G, D=1'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds0, color='D | G'), se=FALSE) +
  scale_color_manual(breaks=c("Y_1 | G", "Y | G, D=1", "D | G"),
                     values=c("Y_1 | G"="green", "Y | G, D=1"="purple", "D | G"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("Transition log odds") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))

plot_linear

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_HS",".jpg", sep=""), plot_linear, width=6, height=3.5)


