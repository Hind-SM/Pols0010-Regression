ge19 <- read.csv("~/Documents/pols_data/ge2019.csv")
aps19 <- read.csv("~/Documents/pols_data/aps19.csv")
ge19aps19 <- merge(ge19, aps19,by="ons_id") # merge two data frames by ons_id

#calculate percentage of votes that were conservatives
ge19aps19$con.per <- ge19aps19$con/ge19aps19$valid_votes

ge19aps19$england<-ge19aps19$country_name #create a new variable called country that replicates the country_name variable
library(dplyr) #load the dplyr package to run the recode function

ge19aps19$england<-recode(ge19aps19$england, Scotland = "Other", Wales="Other")# collapse Scotland and Wales into one level and England into another
library(forcats) #load the forcats package to run fct_drop function to drop levels
ge19aps19$england<-fct_drop(ge19aps19$england)
table(ge19aps19$england)

fit_2 <- lm(con.per ~ england, data=ge19aps19)
summary(fit_2)
## 
## Call:
## lm(formula = con.per ~ england, data = ge19aps19)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.46762 -0.11393  0.02827  0.12504  0.29961 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.467625   0.006709   69.71   <2e-16 ***
## englandOther -0.180101   0.016950  -10.62   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1549 on 630 degrees of freedom
## Multiple R-squared:  0.152,  Adjusted R-squared:  0.1506 
## F-statistic: 112.9 on 1 and 630 DF,  p-value: < 2.2e-16

#linear regression with one binary categorical predictor is like two sample t test
mean(ge19aps19$con.per[ge19aps19$england == "England"], na.rm=TRUE)
#binary regression with one explanatory variable, coefficient is mean between 2 levels in
#your factor variable, between the averages in your 2 groups
mean(ge19aps19$con.per[ge19aps19$england == "England"], na.rm=TRUE) - mean(ge19aps19$con.per[ge19aps19$england == "Other"], na.rm=TRUE)
levels(ge19aps19$region_name)

fit_3 <- lm(con.per ~ relevel(region_name, "South East"), data=ge19aps19)
summary(fit_3)

fit_4 <- lm(con.per ~ ecoactive+england, data=ge19aps19)
summary(fit_4)

reduced = lm(con.per ~ ecoactive+england, data=ge19aps19) # Reduced model
full = lm(con.per ~ ecoactive+england+aged50over, data=ge19aps19) # Full Model

anova(reduced, full)# Compare the models
library(MASS)

step.model <- lm(con.per ~ ecoactive+england+aged50over+british+employed+finance+admin+whiteukborn, data=ge19aps19)
summary(step.model)
step <- stepAIC(step.model, direction="both")
final.model <- lm(con.per ~ england+aged50over+british+employed+finance+admin+whiteukborn, data=ge19aps19)

ge19aps19_sm <- subset(ge19aps19, select=c(con.per, aged50over,british,employed,finance,admin,whiteukborn))
cor(ge19aps19_sm, use="complete.obs")
pairs(ge19aps19_sm)
library(GGally)
ggpairs(ge19aps19_sm)
library(car)
vif(final.model)
1/(vif(final.model))

