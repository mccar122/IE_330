###Problem 1###
MSf = 330.4716
Fstat = 4.45
DFt = 31
DFf = 4-1
DFe = 31-3
pval = pf(Fstat,DFf,DFe, lower.tail = FALSE)
MSe = MSf / Fstat
SSf = MSf * DFf
SSe = MSe * DFe
SSt = SSf + SSe

###Problem 2###
vals <-c(2.6,2.7,3,3.2,3.8,4.6,3.6,4.2,4.2,4.6,4.9,5,2.9,3.4,3.5,4.1,4.6,5.1)
groups <- c(rep("125", 6), rep("160", 6), rep("200", 6))
df<-data.frame(vals, groups)
result_anova <- aov(vals ~ groups, data = df)
summary(result_anova)
library(agricolae)
fisher <- LSD.test(result_anova,"groups"))

###Problem 3###
vals_3 <-c(14.8, 14.8, 14.7, 14.8, 14.9, 14.6, 15.0, 14.9, 14.8, 14.7, 12.7, 11.6,
         12.4, 12.7, 12.1, 14.2, 14.4, 14.4, 12.2, 11.7)
groups_3 <- c(rep("1", 5), rep("2", 5), rep("3", 5), rep("4",5))
df_3<-data.frame(vals_3, groups_3)
result_anova_3 <- aov(vals_3 ~ groups_3, data = df_3)
summary(result_anova_3)
library(agricolae)
fisher_3 <- LSD.test(result_anova_3,"groups_3")

###Problem 4###
vals_4 <- c(8.2, 8, 8.2, 7.9, 8.1, 8, 8.3, 8.4, 8.3, 8.2, 8.3, 8.1, 8.9, 8.7, 8.9,
          8.4, 8.3, 8.5, 8.5, 8.7, 8.7, 8.7, 8.8, 8.8, 8.8, 9.1, 9.0, 8.7, 8.9, 
          8.5, 8.6, 8.5, 8.6, 8.7, 8.8, 8.8)
groups_4 <- c(rep("-1", 6), rep("-0.75", 6), rep("-0.5", 6), rep("0",6), rep("0.5",6), rep("1",6))
df_4<-data.frame(vals_4, groups_4)
dim(vals_4) <- c(6,6)
colnames(vals_4)<- c("-1", "-0.75", "-0.5", "0", "0.5", "1")
boxplot(vals_4)
val_avg_4 <- colMeans(vals_4)
plot(val_avg_4)
result_anova_4 <- aov(vals_4 ~ groups_4, data = df_4)
summary(result_anova_4)
library(agricolae)
fisher_4 <- LSD.test(result_anova_4,"groups_4"))
