
rm(list=ls(all=TRUE))

library("xtable")
library("ggplot2")

options(scipen=999)

data <- readRDS("/Users/Ang/Desktop/Research/Counterfactual slopes/Data/data_cleaned_mare_attendance.rds")
colMeans(is.na(data))

data$completion29 <- as.numeric(data$completion29)-1
data$attendance23 <- as.numeric(data$attendance23)-1

data$G_sqr <- data$parental_income_log^2

Y="completion29"
D="attendance23"
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

phi1 <- cov(IPO_D1, data[,G])/var(data[,G])

## Standard error
eif1 <- (IPO_D1-mean(IPO_D1))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1

se <- function(x) {sqrt( mean(x^2)/nrow(data) )}
se1 <- se(eif1)


##### phi0 (relationship between D and G)
phi0 <- cov(data[,D], data[,G])/var(data[,G])

eif0 <- (data[,D]-mean(data[,D]))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi0

se0 <- se(eif0)

##### phi1_desc (relationship between Y and G conditional on D=1)

## treatment model
DgivenG.Model <- stats::glm(stats::as.formula(paste(D, paste(G_v, collapse="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenG.Pred <- stats::predict(DgivenG.Model, newdata=data, type="response")

## outcome regression model
YgivenDG.Model <- stats::lm(stats::as.formula(paste(Y, paste(G_v,collapse="+"), sep="~")), data=data[data[,D]==1,])

# outcome predictions
YgivenG.Pred_D1 <- stats::predict(YgivenDG.Model, newdata=data)

IPO_D1_desc <- data[,D]/DgivenG.Pred/mean(data[,D]/DgivenG.Pred)*(data[,Y]-YgivenG.Pred_D1) + YgivenG.Pred_D1

phi1_desc <- cov(IPO_D1_desc, data[,G])/var(data[,G])

eif1_desc <- (IPO_D1_desc-mean(IPO_D1_desc))*(data[,G]-mean(data[,G]))/var(data[,G]) - (data[,G]^2-2*data[,G]*mean(data[,G])+mean(data[,G])^2)/var(data[,G])*phi1_desc

se1_desc <- se(eif1_desc)

##### Differences
diff <- phi0-phi1
diff_desc <- phi0-phi1_desc

se_diff <- se(eif0-eif1)
se_diff_desc <- se(eif0-eif1_desc)

##### Output
detail <- data.frame(D=data[,D], IPO_D1=IPO_D1, IPO_D1_desc=IPO_D1_desc,
                     G=data[,G], 
                     Y=data[,Y])

summary(lm(IPO_D1 ~ G, data=detail))$r.squared  # 0.042
summary(lm(IPO_D1_desc ~ G, data=detail))$r.squared  # 0.039
summary(lm(D ~ G, data=detail))$r.squared  # 0.077

overall <- data.frame(item=c("diff_desc","diff","phi0","phi1_desc","phi1"),
                      point=c(diff_desc, diff, phi0, phi1_desc,phi1),
                      se=c(se_diff_desc, se_diff, se0, se1_desc, se1))
overall$CI_lower <- overall$point - qnorm(0.975)*overall$se
overall$CI_higher <- overall$point + qnorm(0.975)*overall$se
overall$pvalues <- 1-abs(pnorm(overall$point/overall$se)-0.5)*2

overall

plot_smooth <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="loess", data=detail, aes(x=G, y=IPO_D1, color='Y1 | G'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", data=detail, aes(x=G, y=IPO_D1_desc, color='Y | G, D=1'), se=FALSE, span=1.5) +
  geom_smooth(method="loess", data=detail, aes(x=G, y=D, color='D | G'), se=FALSE, span=1.5) +
  scale_color_manual(breaks=c("Y1 | G", "Y | G, D=1", "D | G"),
                     values=c("Y1 | G"="green", "Y | G, D=1"="purple", "D | G"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("Transition log odds") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))

plot_smooth

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_attendance_smooth_linear_parametric",".jpg", sep=""), plot_smooth, width=6, height=3.5)

plot_linear <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=IPO_D1, color='Y_1 | G'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=IPO_D1_desc, color='Y | G, D=1'), se=FALSE) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=D, color='D | G'), se=FALSE) +
  scale_color_manual(breaks=c("Y_1 | G", "Y | G, D=1", "D | G"),
                     values=c("Y_1 | G"="green", "Y | G, D=1"="purple", "D | G"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("Transition log odds") +
  xlab("Log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))

plot_linear

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_attendance_linear_parametric",".jpg", sep=""), plot_linear, width=6, height=3.5)

