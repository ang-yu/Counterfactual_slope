
rm(list=ls(all=TRUE))

library("xtable")
library("ggplot2")

options(scipen=999)

data <- readRDS("/Users/Ang/Desktop/Research/Counterfactual slopes/Data/data_cleaned_mare_all.rds")
colMeans(is.na(data))

data$HS20 <- as.numeric(data$HS20)-1
data$attendance23 <- as.numeric(data$attendance23)-1
data$completion29 <- as.numeric(data$completion29)-1
data$grad34 <- as.numeric(data$grad34)-1

data$G_sqr <- data$parental_income_log^2

G="parental_income_log"
G_v=c("parental_income_log","G_sqr")
X <- c("parental_income_log","G_sqr","gender","medu","parental_presence",
       "n_sib","urban","edu_exp","AFQT","age","friend_edu_exp","rotter_score","rosenberg_irt_score",
       "sig_other_expec","foreign_lang",
       "SMSA","mother_seperate","fm_foreign_born",
       "region","m_work","race")
data=data[,c("HS20","attendance23","completion29","grad34",X)]

data <- na.omit(data)  # From 5650 to 3073

table(data$HS20, data$attendance23)
table(data$attendance23, data$completion29)
table(data$completion29, data$grad34)

data$attendance23 <- 

#############################################################################################################################################

Y="attendance23"
D="HS20"

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

head(tau1[order(tau1,decreasing=TRUE)],5)
# There are 2 individuals for whom the estimated tau1 is greater than 1.
# We recode their tau1 to be 0.97, which still makes them have the largest tau1 in the data.
tau1[tau1>1] <- 0.97

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

# save(detail, file="/Users/Ang/Desktop/Research/Counterfactual slopes/ST_HS_parametric_detail.RData")

plot_smooth <- ggplot(detail, aes(x=G, y=Y)) +
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

plot_smooth

plot_linear <- ggplot(detail, aes(x=G, y=Y)) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds1, color='Y_1 | G'), se=FALSE, span=0.1) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds1_desc, color='Y | G, D=1'), se=FALSE, span=0.1) +
  geom_smooth(method="lm", data=detail, aes(x=G, y=log_odds0, color='D | G'), se=FALSE, span=0.1) +
  scale_color_manual(breaks=c("Y_1 | G", "Y | G, D=1", "D | G"),
                     values=c("Y_1 | G"="green", "Y | G, D=1"="purple", "D | G"="brown")) +
  theme(legend.title=element_blank()) +
  ylab("transition log odds") +
  xlab("log parental income") +
  theme(plot.title=element_text(size=17)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))

plot_linear

ggsave(paste("/Users/Ang/Desktop/Research/Counterfactual slopes/ST_HS",".jpg", sep=""), plot_linear, width=6, height=3.5)


