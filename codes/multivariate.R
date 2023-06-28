library('carData')
library('car')
library(readxl)
twins_test_data <- read_excel("E:/Users/ASUS/Documents/term8/multivariate analysis/multivariate project/twins_test_data.xlsx")
attach(twins_test_data)

#prepare data
faed = as.factor(faed)
faminc = as.factor(faminc)
sex = as.factor(sex)
zygosity = as.factor(zygosity)
moed = as.factor(moed)
twins_test_data$sex = sex
twins_test_data$faed = faed
twins_test_data$faminc = faminc
twins_test_data$moed = moed
twins_test_data$zygosity = zygosity
summary(twins_test_data)

#correlation analysis
math = rbind(Math1,Math2)
english = rbind(English1,English2)
socsci = rbind(SocSci1,SocSci2)
vocab = rbind(Vocab1,Vocab2)
natsci = rbind(NatSci1,NatSci2)
scores.data = matrix(c(math,english,socsci,vocab,natsci),ncol = 5,dimnames = list(c(),c("math","english","socsci","vocab","natsci")))
scores.data.cor = as.data.frame(na.omit(scores.data))
cor(scores.data.cor)
heatmap(cor(scores.data.cor))
scatterplotMatrix(scores.data,smooth = FALSE,ellipse = TRUE)

#regression model
L1=lm(cbind(Vocab1,Vocab2,SocSci1,SocSci2)~.,data=twins_test_data)
summary(L1)
F1=L1$fitted.values
R1=L1$residuals
par(mfrow=c(2,2))
plot(F1[,1],R1[,1])
plot(F1[,2],R1[,2])
plot(F1[,3],R1[,3])
plot(F1[,4],R1[,4])
qqnorm(R1)
shapiro.test(R1)
aoutcorr=acf(R1)

#tests
Anova(L1,test.statistic = "Wilks")
L2=update(L1,.~.-moed-zygosity)
Anova(L2,test.statistic = "Wilks")
anova(L1,L2,test = "Hotelling-Lawley")
linearHypothesis(L1,c("Math1=0","Math2=0"))

#PCA
library("usethis")
library("devtools")
library("ggplot2")
library("factoextra")
continuous_independent_variables = twins_test_data[,c(-1,-2,-3,-4,-5,-8,-10,-13,-15)]
dim(continuous_independent_variables)
pca = princomp(continuous_independent_variables,cor = FALSE,scores = TRUE)
summary(pca)
loadings_pca = as.data.frame.matrix(pca$loading) #coefficients of variables
loadings_pca
head(pca$scores)
fviz_eig(pca)
plot(pca$scores[,1],pca$scores[,2],
     xlab = "comp1", ylab = "comp2", col = "pink")
abline(h=0, col = "blue")
abline(v=0, col = "blue")
fviz_pca_ind(pca, col.ind = "cos2",repel = TRUE)
fviz_pca_biplot(pca, repel = TRUE, col.var = "green", col.ind = "blue")

#FACTOR ANALYSIS
fa1 = factanal(continuous_independent_variables, 3, scores = "regression",
               rotation = "none", cor = "pearson")
fa1
par(mfrow=c(1,2))
plot(loadings(fa1)[,1], loadings(fa1)[,2], pch=16, xlab="factor 1",
     ylab="factor 2", col="red")
abline(h=0, col="blue")
abline(v=0, col="blue")
plot(loadings(fa1)[,1], loadings(fa1)[,3], pch=16, xlab="factor 1",
     ylab="factor 3", col="green")
abline(h=0, col="blue")
abline(v=0, col="blue")

fa2 = factanal(continuous_independent_variables, 3, scores = "Bartlett",
               cor = "pearson")
fa2
par(mfrow=c(1,2))
plot(loadings(fa2)[,1], loadings(fa2)[,2], pch=16, xlab="factor 1",
     ylab="factor 2", col="red")
abline(h=0, col="blue")
abline(v=0, col="blue")
plot(loadings(fa2)[,1], loadings(fa2)[,3], pch=16, xlab="factor 1",
     ylab="factor 3", col="green")
abline(h=0, col="blue")
abline(v=0, col="blue")

fa3 = factanal(continuous_independent_variables, 3, scores = "regression",
               rotation = "varimax", cor = "pearson")
fa3
par(mfrow=c(1,2))
plot(loadings(fa3)[,1], loadings(fa3)[,2], pch=16, xlab="factor 1",
     ylab="factor 2", col="red")
abline(h=0, col="blue")
abline(v=0, col="blue")
plot(loadings(fa3)[,1], loadings(fa3)[,3], pch=16, xlab="factor 1",
     ylab="factor 3", col="green")
abline(h=0, col="blue")
abline(v=0, col="blue")

#FACTOR ANALYSIS PART 2
library(ggplot2)
library(psych)
library(hrbrthemes)
fa4 = fa(continuous_independent_variables, nfactors  = 6, scores = "regression",
               rotate = "varimax", covar = TRUE) #use variance covariance matrix
fa4
n_factors4 = length(fa4$e.values)
scree4 = data.frame(factor_n = as.factor(1:n_factors4),
                   eigen_values = fa4$e.values)
ggplot(scree4, aes(x = factor_n, y = eigen_values, group = 1))+
  geom_line( color="blue") +
  geom_point(shape=22, color="blue", fill="blue", size=6) +
  theme_ipsum() +
  xlab("number of factors") + 
  ylab("initial eigen values") +
  labs(title = "scree plot",
       subtitle = "based on the unreduced correlation matrix")
fa5 = fa(continuous_independent_variables, nfactors  = 6, scores = "regression",
         rotate = "none", covar = TRUE)
fa5
n_factors5 = length(fa5$e.values)
scree5 = data.frame(factor_n = as.factor(1:n_factors5),
                   eigen_values = fa5$e.values)
ggplot(scree5, aes(x = factor_n, y = eigen_values, group = 1))+
  geom_line( color="pink") +
  geom_point(shape=24, color="pink", fill="pink", size=6) +
  theme_ipsum() +
  xlab("number of factors") + 
  ylab("initial eigen values") +
  labs(title = "scree plot")
fa6 = fa(continuous_independent_variables, nfactors  = 6, scores = "regression",
         rotate = "varimax")
fa6
n_factors6 = length(fa6$e.values)
scree6 = data.frame(factor_n = as.factor(1:n_factors6),
                   eigen_values = fa6$e.values)
ggplot(scree6, aes(x = factor_n, y = eigen_values, group = 1)) +
  geom_line(color="green") +
  geom_point(shape=21, color="green", fill="#69b3a2", size=6) +
  theme_ipsum() +
  xlab("number of factors") + 
  ylab("initial eigen values") +
  labs(title = "scree plot")

#CCA
library(splines)
library(Matrix)
library(fds)
library(rainbow)
library(MASS)
library(pcaPP)
library(RCurl)
library(deSolve)
library(lattice)
library(graphics)
library(fda)
library(viridis)
library(viridisLite)
library(base)
library(spam)
library(fields)
library(CCA)
library(permute)
library(vegan)
head(continuous_independent_variables)
x1 = continuous_independent_variables[,1:3]
x2 = continuous_independent_variables[,4:6]
cc1 = cancor(x1,x2)
cc2 = cc(x1,x2)
correl = matcor(x1,x2)
img.matcor(correl,type = 1)
img.matcor(correl,type = 2)
cc1$cor
cc2$cor
par(mfrow=c(1,2))
barplot(cc1$cor, main = "CCA WITH CANCOR FUNCTION", col = "pink")
barplot(cc2$cor, main = "CCA WITH CC FUNCTION", col = "blue")
cc1$xcoef
cc2$xcoef
cc1$ycoef
cc2$ycoef
cc3 = cca(x1,x2)
plot(cc3, scaling = 1)
plt.cc(cc2, var.label = TRUE, ind.names = continuous_independent_variables)

# LDA (Linear Discriminant Analysis)
library(MASS)
m1 = lda(faminc~., data = twins_test_data)
m1
library(ggplot2)
pp = predict(m1)
dd = data.frame(LD1 = pp$x[,1], LD2 = pp$x[,2])
ggplot(data = dd, aes(x = dd$LD1, y = dd$LD2, col = faminc)) +
  geom_point(aes(shape = factor(twins_test_data$faminc), size = 1), data = dd) +
  scale_color_manual(values = c("red","blue","green", "orange","purple",
                                "black","pink"))
