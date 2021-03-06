setwd("C:/Users/rache/Downloads")

# evidence from the paper suggests that the greatest improvement is seen at the 4 month check
# after which suervision decreases, possibly explaining the demise of mobility improvement

# Main Question:
# Does stage of PD at the start of the trial predict level of improvement between 0 and 4 months?

# Improvement is measured in reduction in 2 meter walk time and in elevation in 6 meter
# walk distance. Both are variables centered on mobility and specifically walking

library(ggplot2)
library(tableone)
park <- read.csv(file="Parkinsons.csv", sep=",")
park$Group <- as.factor(park$Group)

## Vector of variables to summarize
myVars <- c("Age", "YearsDx", "Gender", "HYstage0")
## Vector of categorical variables that need transformation
catVars <- c("Gender", "HYstage0")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = park, factorVars = catVars)
tab2

# make change variables
park$sixdif <- park$SixMn_Wk4-park$SixMn_Wk0 # negative if improvement
park$secdif <- park$FiveM_Tm4-park$FiveM_Tm0 # negative if improvement

# initial test
test <- lm(sixdif ~ Gender + Group + LEDD4 + Group*LEDD4, data=park)

# bootstrap function that reports bootstrapped r squared values
boot_r2 <- function(formula, dat){
  fun_lm <- function(formula, dat){
    summary(lm(formula, data=dat[sample(1:nrow(dat), nrow(dat), replace=TRUE),]))$r.squared
  }
  sapply(1:1000, function(x) fun_lm(formula,dat))
}

# bootstrap r squared for change in 6 minute distance
six_r2 <- as.data.frame(boot_r2(sixdif ~ HYstage0 + Group + Gender + LEDD4 + Group*SixMn_Wk4, park))
colnames(six_r2) <- c("rsq")
six_crude <- as.data.frame(boot_r2(sixdif ~ Group + Gender + LEDD4 + Group*SixMn_Wk4, park))
colnames(six_crude) <- c("rsq")
# assign model name and bind into 1 dataframe
six_r2$model <- 'Stage Adjusted'
six_crude$model <- 'Crude'
six_r2_full <- rbind(six_r2, six_crude)

# overlapping density plots
ggplot(six_r2_full, aes(rsq, fill = model)) + geom_density(alpha = 0.2) +
  ggtitle('R-Squared Values for Change in 6 Minute Distance') + ylab('Frequency') + xlab('R-Squared')
      # enormous overlap indicates that stage at diagnosis may not improve
      # variation predicted by model


# bootstrap r squared for change in 2 meter time
sec_r2 <- as.data.frame(boot_r2(secdif ~ HYstage0 + Group + Gender + LEDD4 + Group*SixMn_Wk4, park))
colnames(sec_r2) <- c("rsq")
sec_crude <- as.data.frame(boot_r2(secdif ~ Group + Gender + LEDD4 + Group*SixMn_Wk4, park))
colnames(sec_crude) <- c("rsq")
# assign model name and bind into 1 dataframe
sec_r2$model <- 'Stage Adjusted'
sec_crude$model <- 'Crude'
sec_r2_full <- rbind(sec_r2, sec_crude)

# plot overlapping histograms
ggplot(sec_r2_full, aes(rsq, fill = model)) + geom_density(alpha = 0.2) +
  ggtitle('R-Squared Values for Change in 5 Meter Time') + ylab('Frequency') + xlab('R-Squared')

# crude linear model
lm_six_crude <- lm(park$sixdif ~ Gender + Group + LEDD4 + Group*LEDD4, data = park) # group 6 significant
lm_sec_crude <- lm(park$secdif ~ Gender + Group + LEDD4 + Group*LEDD4, data = park) # nothing significant

#stage at week 0
lm_sixminute <- lm(park$sixdif ~ HYstage0 + Gender + Group + LEDD4 + Group*LEDD4, data = park) # group 6 significant
lm_seconds <- lm(park$secdif ~ HYstage0 + Gender + Group + LEDD4 + Group*LEDD4, data = park) # not significant

# Partial F tests:
six_anova <- anova(lm_six_crude, lm_sixminute) # not significant
sec_anova <- anova(lm_sec_crude, lm_seconds) # not significant

# plots
park$improvement_six <- ifelse(park$sixdif > 0,"None", "Improved")
table()

ggplot (park, aes(x = HYstage0, y = sixdif, color = Group)) + geom_point(size=1.5)

library(pander)
pander(table(park$HYstage0, park$Group))
