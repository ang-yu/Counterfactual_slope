
rm(list=ls(all=TRUE))

library("xtable")
library("ggplot2")

options(scipen=999)

data <- readRDS("/Users/Ang/Desktop/Research/Counterfactual slopes/Data/data_cleaned_mare_gradaute_conditional.rds")
colMeans(is.na(data))

data$attendance23 <- as.numeric(data$attendance23)-1
data$completion29 <- as.numeric(data$completion29)-1
data$grad34 <- as.numeric(data$grad34)-1

table(data$completion29[data$parental_income_log<log(5000)])   # very low parental income is associated with very low probability (4.5%) of college completion29
# note that at very high parental income, the distribution of treatment is much more balanced
data <- data[data$parental_income_log>=log(5000),]  # sample size down by 245

data$G_sqr <- data$parental_income_log^2

Y="grad34"
D="completion29"
P="attendance23"
G="parental_income_log"
G_v=c("parental_income_log","G_sqr")
X <- c("parental_income_log","G_sqr","gender","medu","parental_presence",
       "n_sib","urban","edu_exp","AFQT","age","friend_edu_exp","rotter_score","rosenberg_irt_score",
       "sig_other_expec","foreign_lang",
       "SMSA","mother_seperate","fm_foreign_born",
       "region","m_work","race")
data=data[,c(Y,D,P,X)]

data <- na.omit(data)

#############################################################################################################################################


#############################################################################################################################################

##### phi1 (relationship between Y1 and G)

## treatment model
DgivenPX.Model <- stats::glm(stats::as.formula(paste(D, paste(X, collapse="+"), sep="~")), data=data[data[,P]==1,], family=stats::binomial(link="logit"))

# treatment predictions
DgivenX.Pred_P1 <- stats::predict(DgivenPX.Model, newdata=data, type="response")

## outcome model (fitted on the subsample D=1, otherwise the logit model doesn't work well)
YgivenDX.Model <- stats::glm(stats::as.formula(paste(Y, paste(X, collapse="+"), sep="~")), data=data[data[,D]==1,], family=stats::binomial(link="logit"))

# outcome predictions
YgivenX.Pred_D1 <- stats::predict(YgivenDX.Model, newdata=data, type="response")

## The "IPO" (individual potential outcome) function
# For each d value, we have IPO(d,g)=\frac{\one(D=d)}{\pi(d,X)}[Y-\mu(d,X)]+\mu(d,X)
# We stabilize the weight by dividing the sample average of estimated weights

IPO_D1 <- data[,D]/DgivenX.Pred_P1/mean(data[,D]/DgivenX.Pred_P1)*(data[,Y]-YgivenX.Pred_D1) + YgivenX.Pred_D1
data$IPO_D1 <- IPO_D1

tau1 <- stats::predict(stats::lm(as.formula(paste("IPO_D1", paste(G_v, collapse="+"), sep="~")), data=data[data[,P]==1,]), newdata=data, type="response")
head(tau1[order(tau1,decreasing=TRUE)],5)
head(tau1[order(tau1,decreasing=FALSE)],5)

phi1 <- cov((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)), data[,G])/var(data[,G])

## Standard error
eif1 <- ((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1)) - mean((IPO_D1-tau1)/(tau1*(1-tau1)) + log(tau1/(1-tau1))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1

se <- function(x) {sqrt( mean(x^2)/nrow(data) )}
se1 <- se(eif1)


##### phi0 (relationship between D and G)
PgivenG.Pred <- stats::predict(stats::glm(stats::as.formula(paste(P, paste(G_v, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit")), type="response")

DgivenPG.Model <- stats::glm(stats::as.formula(paste(D, paste(G_v, collapse="+"), sep="~")), data=data[data[,P]==1,], family=stats::binomial(link="logit"))

DgivenG.Pred_P1 <- stats::predict(DgivenPG.Model, newdata=data, type="response")

IPO_P1 <- data[,D]/PgivenG.Pred/mean(data[,D]/PgivenG.Pred)*(data[,Y]-DgivenG.Pred_P1) + DgivenG.Pred_P1


phi0 <- cov((IPO_P1-DgivenG.Pred_P1)/(DgivenG.Pred_P1*(1-DgivenG.Pred_P1)) + log(DgivenG.Pred_P1/(1-DgivenG.Pred_P1)), data[,G])/var(data[,G])

eif0 <- ((IPO_P1-DgivenG.Pred_P1)/(DgivenG.Pred_P1*(1-DgivenG.Pred_P1)) + log(DgivenG.Pred_P1/(1-DgivenG.Pred_P1)) - mean((IPO_P1-DgivenG.Pred_P1)/(DgivenG.Pred_P1*(1-DgivenG.Pred_P1)) + log(DgivenG.Pred_P1/(1-DgivenG.Pred_P1))))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi0

se0 <- se(eif0)

##### phi1_desc (relationship between Y and G conditional on D=1)
# treatment predictions
DgivenG.Pred <- stats::predict(stats::glm(stats::as.formula(paste(D, paste(G_v, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit")), type="response")

## outcome regression model
YgivenDG.Model <- stats::glm(stats::as.formula(paste(Y, paste(G_v, collapse="+"), sep="~")), data=data[data[,D]==1,], family=stats::binomial(link="logit"))

# outcome predictions
YgivenG.Pred_D1 <- stats::predict(YgivenDG.Model, newdata=data, type="response")

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
                     log_odds0=(IPO_P1-DgivenG.Pred_P1)/(DgivenG.Pred_P1*(1-DgivenG.Pred_P1)) + log(DgivenG.Pred_P1/(1-DgivenG.Pred_P1)),
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
  geom_smooth(method="loess", data=detail, aes(x=G, y=log_odds0, color='D | G, P=1'), se=FALSE, span=1.5) +
  scale_color_manual(breaks=c("Y1 | G", "Y | G, D=1", "D | G, P=1"),
                     values=c("Y1 | G"="green", "Y | G, D=1"="purple", "D | G, P=1"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("Transition log odds") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))

plot_smooth

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_completion_smooth_conditional_parametric",".jpg", sep=""), plot_smooth, width=6, height=3.5)

plot_linear <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds1, color='Y_1 | G, P=1'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds1_desc, color='Y | G, D=1'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds0, color='D | G, P=1'), se=FALSE) +
  scale_color_manual(breaks=c("Y_1 | G, P=1", "Y | G, D=1", "D | G, P=1"),
                     values=c("Y_1 | G, P=1"="green", "Y | G, D=1"="purple", "D | G, P=1"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("Transition log odds") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))

plot_linear

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_completion_conditional_parametric",".jpg", sep=""), plot_linear, width=6, height=3.5)


