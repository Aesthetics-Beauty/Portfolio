library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(viridis)
library(dplyr)




LCA_data <- read.csv("D:/STUDIE/PSYCH/JAAR 3/HONOURS MUSICOLOGY/Portfolio/LCA_data.csv", sep=";")

data <- as.matrix(LCA_data)

#Creating LCAs
source(file = "D:/STUDIE/PSYCH/JAAR 3/LATENT VARIABLE MODELS/lca.r")

set.seed(1)
model3=lca(data,3)
#model1=lca(data,1)
model2=lca(data,2)

#model4=lca(data,4)
#model5=lca(data,5)
#model6=lca(data,6)

summary(model3)

#BICs
model1$bic 
model2$bic
model3$bic
model4$bic
model5$bic
model6$bic

#Best is a 3-class model, judging by absolute fit

p.cond <- round(model3$p,3)

p_class <- matrix(round(model3$classprob, 3), 3, 1)

p <- cbind(p_class, p.cond)


Classes <- predict.lca(model3, data)

GOLD.MSI <- read.csv("D:/STUDIE/PSYCH/JAAR 3/HONOURS MUSICOLOGY/Portfolio/GOLD-MSI.csv", sep=";")


Gold_class <- cbind(GOLD.MSI, Classes)
Gold_class$Classes <- factor(Gold_class$Classes, ordered = TRUE)
Gold_class$GOLD.MSI.SCORE <- as.numeric(Gold_class$GOLD.MSI.SCORE)

Gold_class_1 <- subset(Gold_class, Classes == 1)
Gold_class_2 <- subset(Gold_class, Classes == 2)
Gold_class_3 <- subset(Gold_class, Classes == 3)

ggplot(Gold_class, aes(GOLD.MSI.SCORE, fill = Classes)) +
  geom_histogram(bins = 30) +
  facet_wrap(. ~ Classes)


mean(Gold_class_1$GOLD.MSI.SCORE)
mean(Gold_class_2$GOLD.MSI.SCORE)
mean(Gold_class_3$GOLD.MSI.SCORE)



#running anova for GOLD

anova <- aov(GOLD.MSI.SCORE ~ Classes, data = Gold_class)

summary(Gold_class_1)

ggplot(Gold_class, aes(x = Classes, y = GOLD.MSI.SCORE, col = Classes)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.4)


#posthoc
summary(anova)
Gold_score <- Gold_class$GOLD.MSI.SCORE
Classes <- Gold_class$Classes
pairwise.t.test(Gold_score, Classes, p.adj = "bonf", pool.sd = TRUE)


#boxplot
#running ANOVA for age
AGE <- read.csv("D:/STUDIE/PSYCH/JAAR 3/HONOURS MUSICOLOGY/Portfolio/AGE.csv", sep=";")
Age_class <- cbind(AGE, Classes)
Age_class$Classes <- as.factor(Age_class$Classes)

ggplot(Age_class, aes(x = Classes, y = AGE, col = Classes)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.4)

+ 
  labs(x = "proportion pcors", y= " ", fill="",title=" ") +
  theme(plot.title=element_text(hjust=-0.07,vjust=2,size=30))+
  theme(axis.text=element_text(size=15),axis.title = element_text(size=20)) +
  theme(legend.text=element_text(size=20), legend.position="bottom")




#running ANOVA for age
AGE <- read.csv("D:/STUDIE/PSYCH/JAAR 3/HONOURS MUSICOLOGY/Portfolio/AGE.csv", sep=";")
Age_class <- cbind(AGE, Classes)
Age_class$Classes <- as.factor(Age_class$Classes)

ggplot(Age_class, aes(x = Classes, y = AGE, col = Classes)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.4)








rownames(p) <- c("Class prop", "Item 1")

write.csv(p, file = "p.csv")

p



#With equilinear restrictions
restr2 <- matrix(c(rep(1,15),rep(-1,15)), 2, 15, byrow = T)

model2_restr = lca(data,2, restr = restr2)
summary(model2)

df <- 31 - 2

model2$logl
model2_restr$logl

-model2_restr$logl--model2$logl

LRT <- -2 * (-model2_restr$logl--model2$logl)
pchisq(LRT, df, low=F)


#chisquare is non-significant, meaning that both models fit the data equally well, which means we could select the constrained model 










#Checking the BICS:
Table_BIC <- matrix(round(c(model1$bic, model2$bic, model3$bic, model4$bic, model5$bic),2), 1, 5)
colnames(Table_BIC) = c("1 Class", "2 Class", "3 Class", "4 Class", "5 Class")
rownames(Table_BIC) = "BIC"

#Making the grob for BICs
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = viridis(5, alpha = 0.5) , col=NA),
            fg_params=list(fontface=1)),
  colhead=list(fg_params=list(col="navyblue", fontface=1L, cex = 1)),
  rowhead=list(fg_params=list(col="navyblue", fontface=2L, cex = 1)),
  base_size = 15)



grid.draw(tableGrob(Table_BIC, theme=tt3))


#Making the table

#Three class model is the best model (BIC 1850)


predicted_classes <- predict.lca(model2, data)



hist(predicted_classes)



p

write.csv()





