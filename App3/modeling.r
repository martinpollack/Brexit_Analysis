

logitData <- read.csv("logitData.csv")
logitData <- logitData[,-1]
colnames(logitData)

# model with no added variables
model.univariate <- glm(NewLeaveVote ~ WeightedAIS, data=logitData, family=binomial(link="logit"))
summary(model.univariate)

# model with all added variables, no interactions
model.all.var.no.interact <- glm(NewLeaveVote ~ ., data=logitData, family=binomial(link="logit"))
summary(model.all.var.no.interact)

# adding all variables gives significant improvements
test.all.var <- anova(model.univariate, model.all.var.no.interact)
1 - pchisq(test.all.var$Deviance[2], test.all.var$Df[2])

# model with all added variables, all interactions
model.all.var.all.interact <- glm(NewLeaveVote ~ .^2, data=logitData, family=binomial(link="logit"))
summary(model.all.var.all.interact)

# adding all interactions gives significant improvements
test.all.var.all.interact <- anova(model.all.var.no.interact, model.all.var.all.interact)
1 - pchisq(test.all.var.all.interact$Deviance[2], test.all.var.all.interact$Df[2])
