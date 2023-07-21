
rm(list=ls(all=TRUE))

library("xtable")
library("ggplot2")

options(scipen=999)

data <- readRDS("/Users/Ang/Desktop/Research/Counterfactual covariances/Data/data_cleaned_mare_hs.rds")
colMeans(is.na(data))

data$gender <- as.numeric(data$gender)-1

data$parental_presence <- as.numeric(data$parental_presence)-1

data$n_sib <- as.numeric(data$n_sib)-1

data$urban <- as.numeric(data$urban)-1

data$edu_exp <- as.numeric(data$edu_exp)-2
data$edu_exp[data$edu_exp==-1] <- 0

data$age <- as.numeric(data$age)-1

data$friend_edu_exp <- as.numeric(data$friend_edu_exp)-2
data$friend_edu_exp[data$friend_edu_exp==-1] <- 0

data$sig_other_exp1 <- NA
data$sig_other_exp1[data$sig_other_expec==1 & !is.na(data$sig_other_expec)] <- 1
data$sig_other_exp1[data$sig_other_expec!=1 & !is.na(data$sig_other_expec)] <- 0
data$sig_other_exp2 <- NA
data$sig_other_exp2[data$sig_other_expec==2 & !is.na(data$sig_other_expec)] <- 1
data$sig_other_exp2[data$sig_other_expec!=2 & !is.na(data$sig_other_expec)] <- 0
data$sig_other_exp3 <- NA
data$sig_other_exp3[data$sig_other_expec==3 & !is.na(data$sig_other_expec)] <- 1
data$sig_other_exp3[data$sig_other_expec!=3 & !is.na(data$sig_other_expec)] <- 0
data$sig_other_exp4 <- NA
data$sig_other_exp4[data$sig_other_expec==4 & !is.na(data$sig_other_expec)] <- 1
data$sig_other_exp4[data$sig_other_expec!=4 & !is.na(data$sig_other_expec)] <- 0

data$foreign_lang <- as.numeric(data$foreign_lang)-1

data$SMSA1 <- NA
data$SMSA1[data$SMSA==0 & !is.na(data$SMSA)] <- 1
data$SMSA1[data$SMSA!=0 & !is.na(data$SMSA)] <- 0
data$SMSA2 <- NA
data$SMSA2[data$SMSA==1 & !is.na(data$SMSA)] <- 1
data$SMSA2[data$SMSA!=1 & !is.na(data$SMSA)] <- 0
data$SMSA3 <- NA
data$SMSA3[data$SMSA==2 & !is.na(data$SMSA)] <- 1
data$SMSA3[data$SMSA!=2 & !is.na(data$SMSA)] <- 0
data$SMSA4 <- NA
data$SMSA4[data$SMSA==3 & !is.na(data$SMSA)] <- 1
data$SMSA4[data$SMSA!=3 & !is.na(data$SMSA)] <- 0

data$mother_seperate <- as.numeric(data$mother_seperate)-1

#data$school_satis1 <- NA
#data$school_satis1[(data$school_satisfaction==1 | data$school_satisfaction==2) & !is.na(data$school_satisfaction)] <- 1
#data$school_satis1[data$school_satisfaction!=1 & data$school_satisfaction!=2 & !is.na(data$school_satisfaction)] <- 0
#data$school_satis2 <- NA
#data$school_satis2[data$school_satisfaction==3 & !is.na(data$school_satisfaction)] <- 1
#data$school_satis2[data$school_satisfaction!=3 & !is.na(data$school_satisfaction)] <- 0
#data$school_satis3 <- NA
#data$school_satis3[data$school_satisfaction==4 & !is.na(data$school_satisfaction)] <- 1
#data$school_satis3[data$school_satisfaction!=4 & !is.na(data$school_satisfaction)] <- 0

data$fm_foreign_born <- NA
data$fm_foreign_born[(data$f_foreign_born==2 | data$m_foreign_born==2) & !is.na(data$f_foreign_born) & !is.na(data$m_foreign_born)] <- 1
data$fm_foreign_born[(data$f_foreign_born==1 | data$m_foreign_born==1) & !is.na(data$f_foreign_born) & !is.na(data$m_foreign_born)] <- 0

data$region1 <- NA
data$region1[data$region==1 & !is.na(data$region)] <- 1
data$region1[data$region!=1 & !is.na(data$region)] <- 0
data$region2 <- NA
data$region2[data$region==2 & !is.na(data$region)] <- 1
data$region2[data$region!=2 & !is.na(data$region)] <- 0
data$region3 <- NA
data$region3[data$region==3 & !is.na(data$region)] <- 1
data$region3[data$region!=3 & !is.na(data$region)] <- 0
data$region4 <- NA
data$region4[data$region==4 & !is.na(data$region)] <- 1
data$region4[data$region!=4 & !is.na(data$region)] <- 0

data$m_work <- as.numeric(data$m_work)-1

data$race1 <- NA
data$race1[data$race=="Other" & !is.na(data$race)] <- 1
data$race1[data$race!="Other" & !is.na(data$race)] <- 0
data$race2 <- NA
data$race2[data$race=="Black" & !is.na(data$race)] <- 1
data$race2[data$race!="Black" & !is.na(data$race)] <- 0
data$race3 <- NA
data$race3[data$race=="Hispanic" & !is.na(data$race)] <- 1
data$race3[data$race!="Hispanic" & !is.na(data$race)] <- 0

data$attendance29 <- as.numeric(data$attendance29)-1

data$G_sqr <- data$parental_income_log^2

Y="attendance29"
D="HS20"
G="parental_income_log"
G_v=c("parental_income_log","G_sqr")
X <- c("parental_income_log","G_sqr","gender","medu","parental_presence",
       "n_sib","urban","edu_exp","AFQT","age","friend_edu_exp","rotter_score","rosenberg_irt_score",
       "sig_other_exp1","sig_other_exp2","sig_other_exp3","sig_other_exp4","foreign_lang",
       "SMSA1","SMSA2","SMSA3","SMSA4","mother_seperate","fm_foreign_born",
       "region1","region2","region3","region4","m_work","race1","race2","race3")
data=data[,c(Y,D,X)]

data <- na.omit(data)

#############################################################################################################################################


#############################################################################################################################################

##### phi1 (relationship between Y1 and G)

## treatment model
DgivenX.Model <- stats::glm(stats::as.formula(paste(D, paste(X, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenX.Pred <- stats::predict(DgivenX.Model, newdata=data, type="response")

## outcome model (fitted on the subsample D=1, otherwise the logit model doesn't work well)
YgivenDX.Model <- stats::glm(stats::as.formula(paste(Y, paste(X, collapse="+"), sep="~")), data=data[data[,D]==1,], family=stats::binomial(link="logit"))

# outcome predictions
YgivenX.Pred_D1 <- stats::predict(YgivenDX.Model, newdata=data, type="response")

## The "IPO" (individual potential outcome) function
# For each d value, we have IPO(d,g)=\frac{\one(D=d)}{\pi(d,X)}[Y-\mu(d,X)]+\mu(d,X)
# We stabilize the weight by dividing the sample average of estimated weights

IPO_D1 <- data[,D]/DgivenX.Pred/mean(data[,D]/DgivenX.Pred)*(data[,Y]-YgivenX.Pred_D1) + YgivenX.Pred_D1
data$IPO_D1 <- IPO_D1

tau1 <- stats::predict(stats::lm(as.formula(paste("IPO_D1", paste(G_v, collapse="+"), sep="~")), data=data), newdata=data, type="response")
  
phi1 <- cov((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)), data[,G])/var(data[,G])
# theoretically, (IPO_D1-tau1)/(tau1*(1-tau1)) does not need to appear, and naturally it doesn't matter empirically either

## Standard error
eif1 <- ((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)) - mean((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1

se <- function(x) {sqrt( mean(x^2)/nrow(data) )}
se1 <- se(eif1)


##### phi0 (relationship between D and G)
DgivenG.Pred <- stats::predict(stats::glm(stats::as.formula(paste(D, paste(G_v, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit")), type="response")

phi0 <- cov((data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred)), data[,G])/var(data[,G])

eif0 <- ((data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred)) - mean((data[,D]-DgivenG.Pred)/(DgivenG.Pred*(1-DgivenG.Pred)) + log(DgivenG.Pred/(1-DgivenG.Pred))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi0

se0 <- se(eif0)

##### phi1_desc (relationship between Y and G conditional on D=1)

## outcome regression model
YgivenDG.Model <- stats::glm(stats::as.formula(paste(Y, paste(G_v, collapse="+"), sep="~")), data=data[data[,D]==1,], family=stats::binomial(link="logit"))

# outcome predictions
YgivenG.Pred_D1 <- stats::predict(YgivenDG.Model, newdata=data, type="response")

IPO_D1_desc <- data[,D]/DgivenG.Pred/mean(data[,D]/DgivenG.Pred)*(data[,Y]-YgivenG.Pred_D1) + YgivenG.Pred_D1

tau1_desc <- stats::predict(stats::glm(stats::as.formula(paste(Y, paste(G_v, collapse="+"), sep="~")), data=data[data[,D]==1,], family=stats::binomial(link="logit")), newdata=data, type="response")

phi1_desc <- cov((IPO_D1_desc-tau1_desc)/(tau1_desc*(1-tau1_desc)) + log(tau1_desc/(1-tau1_desc)), data[,G])/var(data[,G])

eif1_desc <- ((IPO_D1_desc-tau1_desc)/(tau1_desc*(1-tau1_desc)) + log(tau1_desc/(1-tau1_desc)) - mean((IPO_D1_desc-tau1_desc)/(tau1_desc*(1-tau1_desc)) + log(tau1_desc/(1-tau1_desc))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1_desc

se1_desc <- se(eif1_desc)

##### Differences
diff <- phi1-phi0
diff_desc <- phi1_desc-phi0

se_diff <- se(eif1-eif0)
se_diff_desc <- se(eif1_desc-eif0)

##### Output
detail <- data.frame(log_odds1=log(tau1/(1-tau1)), log_odds1_desc=log(tau1_desc/(1-tau1_desc)), log_odds0=log(DgivenG.Pred/(1-DgivenG.Pred)), G=data[,G], Y=data[,Y], D=data[,D])

overall <- data.frame(item=c("phi1_desc","phi1","phi0","diff_desc","diff"),
                      point=c(phi1_desc,phi1,phi0,diff_desc,diff),
                      se=c(se1_desc,se1,se0,se_diff_desc,se_diff))
overall$CI_lower <- overall$point - qnorm(0.975)*overall$se
overall$CI_higher <- overall$point + qnorm(0.975)*overall$se
overall$pvalues <- 1-abs(pnorm(overall$point/overall$se)-0.5)*2

overall

save(detail, file="/Users/Ang/Desktop/Research/Counterfactual covariances/ST_HS_parametric_detail.RData")

parametric_plot <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(data=detail, aes(x=G, y=log_odds1, color='Y1 | G'), se=FALSE, span=0.1) +
  geom_smooth(data=detail, aes(x=G, y=log_odds1_desc, color='Y | G, D=1'), se=FALSE, span=0.1) +
  geom_smooth(data=detail, aes(x=G, y=log_odds0, color='D | G'), se=FALSE, span=0.1) +
  scale_color_manual(breaks=c("Y1 | G", "Y | G, D=1", "D | G"),
                     values=c("Y1 | G"="purple", "Y | G, D=1"="brown", "D | G"="green")) +
  theme(legend.title=element_blank()) +
  ylab("log adult family income") +
  xlab("log adolescence family income") +
  ggtitle("Great equalizer thesis") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))

parametric_plot

