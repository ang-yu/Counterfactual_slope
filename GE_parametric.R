
rm(list=ls(all=TRUE))

library("xtable")
library("ggplot2")

options(scipen=999)

data <- readRDS("/Users/Ang/Desktop/Research/Counterfactual slopes/Data/data_cleaned_ge.rds")
colMeans(is.na(data))

table(data$completion[data$parental_income_log<log(5000)])   # very low parental income is associated with very low probability (4.5%) of college completion
# note that at very high parental income, the distribution of treatment is much more balanced
data <- data[data$parental_income_log>=log(5000),]  # sample size down by 245

data$completion <- as.numeric(data$completion)-1

data$G_sqr <- data$parental_income_log^2

Y="adult_income_log"
D="completion"
G="parental_income_log"
G_v=c("parental_income_log","G_sqr")
X <- c("parental_income_log","G_sqr","gender","medu","parental_presence",
       "n_sib","urban","edu_exp","AFQT","age","friend_edu_exp","rotter_score","rosenberg_irt_score",
       "sig_other_expec","foreign_lang",
       "SMSA","mother_seperate","fm_foreign_born",
       "region","m_work","race")
data=data[,c(Y,D,X)]

data <- na.omit(data)

#############################################################################################################################################


#############################################################################################################################################

##### For the selection-free test

## treatment model
DgivenX.Model <- stats::glm(stats::as.formula(paste(D, paste(X, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenX.Pred <- stats::predict(DgivenX.Model, newdata=data, type="response")

## outcome regression model
YgivenDX.Model <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,X,sep="*"),collapse="+"), sep="~")), data=data)

# outcome predictions
YgivenX.Pred_D0 <- YgivenX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0
YgivenX.Pred_D0 <- stats::predict(YgivenDX.Model, newdata=pred_data)

pred_data <- data
pred_data[,D] <- 1
YgivenX.Pred_D1 <- stats::predict(YgivenDX.Model, newdata=pred_data)

## The "IPO" (individual potential outcome) function
# For each d value, we have IPO(d,g)=\frac{\one(D=d)}{\pi(d,X)}[Y-\mu(d,X)]+\mu(d,X)
# We stabilize the weight by dividing the sample average of estimated weights

IPO_D0 <- (1-data[,D])/(1-DgivenX.Pred)/mean((1-data[,D])/(1-DgivenX.Pred))*(data[,Y]-YgivenX.Pred_D0) + YgivenX.Pred_D0
IPO_D1 <- data[,D]/DgivenX.Pred/mean(data[,D]/DgivenX.Pred)*(data[,Y]-YgivenX.Pred_D1) + YgivenX.Pred_D1

## Point estimates for \xi_{\text{linear}}(d) 
phi0 <- cov(IPO_D0, data[,G])/var(data[,G])
phi1 <- cov(IPO_D1, data[,G])/var(data[,G])

diff <- phi0-phi1

## Standard error estimates
eif0 <- (IPO_D0-mean(IPO_D0))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi0
eif1 <- (IPO_D1-mean(IPO_D1))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1

se <- function(x) {sqrt( mean(x^2)/nrow(data) )}
se0 <- se(eif0)
se1 <- se(eif1)
se_diff <- se(eif0-eif1)


##### For the descriptive test
## treatment model
DgivenG.Model <- stats::glm(stats::as.formula(paste(D, paste(G_v, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenG.Pred <- stats::predict(DgivenG.Model, newdata=data, type="response")

## outcome regression model
YgivenDG.Model <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,G_v,sep="*"),collapse="+"), sep="~")), data=data)

# outcome predictions
YgivenG.Pred_D0 <- YgivenG.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0
YgivenG.Pred_D0 <- stats::predict(YgivenDG.Model, newdata=pred_data)

pred_data <- data
pred_data[,D] <- 1
YgivenG.Pred_D1 <- stats::predict(YgivenDG.Model, newdata=pred_data)

## The "IPO" (individual potential outcome) function
IPO_D0_desc <- (1-data[,D])/(1-DgivenG.Pred)/mean((1-data[,D])/(1-DgivenG.Pred))*(data[,Y]-YgivenG.Pred_D0) + YgivenG.Pred_D0
IPO_D1_desc <- data[,D]/DgivenG.Pred/mean(data[,D]/DgivenG.Pred)*(data[,Y]-YgivenG.Pred_D1) + YgivenG.Pred_D1

## Point estimates
phi0_desc <- cov(IPO_D0_desc, data[,G])/var(data[,G])
phi1_desc <- cov(IPO_D1_desc, data[,G])/var(data[,G])

diff_desc <- phi0_desc-phi1_desc

## Standard error estimates
eif0_desc <- (IPO_D0_desc-mean(IPO_D0_desc))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi0_desc
eif1_desc <- (IPO_D1_desc-mean(IPO_D1_desc))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1_desc

se0_desc <- se(eif0_desc)
se1_desc <- se(eif1_desc)
se_diff_desc <- se(eif0_desc-eif1_desc)

detail <- data.frame(IPO_D0=IPO_D0, IPO_D1=IPO_D1, IPO_D0_desc=IPO_D0_desc, IPO_D1_desc=IPO_D1_desc, G=data[,G], Y=data[,Y], D=data[,D])

overall <- data.frame(item=c("diff_desc","diff","phi1_desc","phi0_desc","phi1","phi0"),
                      point=c(diff_desc,diff,phi1_desc,phi0_desc,phi1,phi0),
                      se=c(se_diff_desc,se_diff,se1_desc,se0_desc,se1,se0))
overall$CI_lower <- overall$point - qnorm(0.975)*overall$se
overall$CI_higher <- overall$point + qnorm(0.975)*overall$se
overall$pvalues <- 1-abs(pnorm(overall$point/overall$se)-0.5)*2

overall

plot_smooth <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="loess", aes(x=G, y=IPO_D1_desc, color='Y | G, D=1', linetype='Y | G, D=1'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", aes(x=G, y=IPO_D0_desc, color='Y | G, D=0', linetype='Y | G, D=0'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", aes(x=G, y=IPO_D1, color='Y_1 | G', linetype='Y_1 | G'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", aes(x=G, y=IPO_D0, color='Y_0 | G', linetype='Y_0 | G'), se=FALSE, span=1.5) +
  scale_color_manual(name="Legend", 
                     breaks=c("Y | G, D=1", "Y | G, D=0", "Y_1 | G", "Y_0 | G"),
                     values=c("Y | G, D=1"="purple", "Y | G, D=0"="brown", "Y_1 | G"="green", "Y_0 | G"="red")) +
  scale_linetype_manual(name="Legend",
                        breaks=c("Y | G, D=1", "Y | G, D=0", "Y_1 | G", "Y_0 | G"),
                        values=c("Y | G, D=1"="solid", "Y | G, D=0"="dashed", "Y_1 | G"="dotted", "Y_0 | G"="dotdash")) +
  theme(legend.title=element_blank()) +
  ylab("Log adult family income") +
  xlab("Log adolescence family income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))

plot_smooth

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/GE_smooth",".jpg", sep=""), plot_smooth, width=6, height=3.5)

plot_linear <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=IPO_D1_desc, color='Y | G, D=1', linetype='Y | G, D=1'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=IPO_D0_desc, color='Y | G, D=0', linetype='Y | G, D=0'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=IPO_D1, color='Y_1 | G', linetype='Y_1 | G'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=IPO_D0, color='Y_0 | G', linetype='Y_0 | G'), se=FALSE) +
  scale_color_manual(name="Legend", 
                     breaks=c("Y | G, D=1", "Y | G, D=0", "Y_1 | G", "Y_0 | G"),
                     values=c("Y | G, D=1"="purple", "Y | G, D=0"="brown", "Y_1 | G"="green", "Y_0 | G"="red")) +
  scale_linetype_manual(name="Legend",
                        breaks=c("Y | G, D=1", "Y | G, D=0", "Y_1 | G", "Y_0 | G"),
                        values=c("Y | G, D=1"="solid", "Y | G, D=0"="dashed", "Y_1 | G"="dotted", "Y_0 | G"="dotdash")) +
  theme(legend.title=element_blank()) +
  ylab("Log income") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))

plot_linear

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/GE",".jpg", sep=""), plot_linear, width=6, height=3.5)







