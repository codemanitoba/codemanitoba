#Load data set
#data set was imported from Excel via the import data set panel in environment

#Estimating the Model with LS
data.regg <- lm(income ~ elderly + infant_death, data = data)
data.regg
summary(data.regg)

#saving residuals
resids <- data.regg$residuals
resids

#Orthogonality
cov(data$elderly,resids)
cov(data$infant_death,resids)

#LS resids sum to zero
sum.resids <- sum(resids)
sum.resids

#Number 5: regression passes through the sample mean of data
#We need ybar = Beta0hat + Beta1hatXelderlybar + Beta2hatXinfant_deathbar
ybar <- mean(data$income)
ybar

right.side <- coefficients(data.regg)[1] + coefficients(data.regg)[2] * mean(data$elderly) + coefficients(data.regg)[3] * mean(data$infant_death) 
left.side <- ybar

left <- round(left.side,2)
right <- round(right.side,2)

left == right

#Number 6: residuals values are invariant to a non-singular linear transformation
#is infant_Death = 2 X elderly?
vec <- (data$elderly) * 2
vec

data_1 <- data
data_1$elderly2 <- vec

data.regg2 <- lm(income ~ elderly + elderly2, data =data_1)
data.regg2
summary(data.regg2)

(data_1$infant_death) == (data_1$elderly2)

#number 7: LS estimator for B2
data.regg3 = lm(elderly ~  infant_death, data = data_1)
summary(data.regg3)
resid1 <- data.regg3$residuals

data.regg4 = lm(income ~  elderly, data = data_1)
summary(data.regg4)
resid2 <- data.regg4$residuals

B2 = lm(resid2 ~ resid1-1)
B2







