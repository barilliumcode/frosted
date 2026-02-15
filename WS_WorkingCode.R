setwd("~/Desktop/Thesis/Code")

#--------------------------------------------------------------------------------

# Packages used
install.packages("ggplot2") #graphing
install.packages("ggpubr") #graphing
install.packages("car") #levene's test
install.packages("FSA") #dunn test
install.packages("epitools") #odds ratio
library(ggplot2)
library(ggpubr)
library(car)
library(FSA)
library(epitools)


#--------------------------------------------------------------------------------
# Does the chance of survival differ between feeding groups?

surv<-read.csv("WS_R_survival.csv")

# Finding the percent survival 
survivalTable<-table(surv$Survive, surv$Treatment)
survivalTable
# if(surv$Survive = 0) then the larva did not survive to pupation
# if(surv$Survive = 1) then the larva survived to pupation
# Total number of egg hatch = 296
71+103+74+48
# Hatches in sundial feeding group = 145
# Hatches in large leaf feeding group = 151
# Survived in sundial feeding group = 74
# Survived in large leaf feeding group = 48
74/145 # Sundial survival percentage: 51.03%
48/151 #Large Leaf survival percentage: 31.79%
# Odds of survival differed between feeding groups (OR=2.13, X2 = 10.2282, df = 1, p < 0.05)
51.03/31.79 #Sundial feeders 1.6x more likely to survive to pupation

# Odds Ratio = 2.139063
oddsratio(survivalTable, method = "wald")$measure[-1] 

# X2 contingency test for survival to pupation
# Observed Frequencies
df_OF<-as.data.frame(matrix(nrow=3, ncol=4))
names(df_OF)<-c(" ","L. perennis Fed", "L. polyphyllus Fed", "Total")
df_OF[1,1]<- "Pupated"
df_OF[2,1]<- "Died"
df_OF[3,1]<-"Total"
df_OF[1,2:4]<-c(74, 48, 122)
df_OF[2,2:4]<-c(71, 103, 190)
df_OF[3,2:4]<-c(145, 151, 296)
head(df_OF)
# Expected Frequencies
df_EF<-as.data.frame(matrix(nrow=3, ncol=4))
names(df_EF)<-c(" ","L. perennis Fed", "L. polyphyllus Fed", "Total")
df_EF[1,1]<- "Pupated"
df_EF[2,1]<- "Died"
df_EF[3,1]<-"Total"
df_EF[1,2:4]<-c(59.76, 62.24, 122)
df_EF[2,2:4]<-c(85.24, 88.76, 190)
df_EF[3,2:4]<-c(145, 151, 296)
head(df_EF)
#X2 value = 11.309, df = 1, p < 0.01

#Remove environment clutter. 
rm(df_OF)
rm(df_EF)


#--------------------------------------------------------------------------------
# Does the chance of eclosure differ between feeding groups?

# Finding the percent eclosure 
# The remaining 2025 pupae may eclose in the Spring of 2025, so only looking at 2024 data
subsetEclose<- surv[ which(surv$Year == 2024 & surv$Survive == 1),]
ecloseTable<-table(subsetEclose$Eclose, subsetEclose$Treatment)
ecloseTable
# if(surv$Eclose = 0) then the pupa did not eclose
# if(surv$Eclose = 1) then the larva survived to pupation
# Total number of pupae = 88
16+2+20+50
# Pupae in sundial feeding group = 52
# Pupae in large leaf feeding group = 36
# Eclosures in sundial feeding group = 50
# Eclosures in large leaf feeding group = 20
50/52 # Sundial eclosure percentage: 96.15385%
20/36 # Large Leaf eclosure percentage: 55.55556%
# Eclosure success differed between feeding groups (OR = 20.00, X2 = 21.5493, df = 1, p < 0.01)

# Odds Ratio = 20.00000
oddsratio(ecloseTable, method = "wald")$measure[-1] 

# X2 contingency test for survival to pupation
# Observed Frequencies
df_OF<-as.data.frame(matrix(nrow=3, ncol=4))
names(df_OF)<-c(" ","L. perennis Fed", "L. polyphyllus Fed", "Total")
df_OF[1,1]<- "Eclosed"
df_OF[2,1]<- "Didn't Eclose"
df_OF[3,1]<-"Total"
df_OF[1,2:4]<-c(50, 20, 70)
df_OF[2,2:4]<-c(2, 16, 18)
df_OF[3,2:4]<-c(52, 36, 88)
head(df_OF)
# Expected Frequencies
df_EF<-as.data.frame(matrix(nrow=3, ncol=4))
names(df_EF)<-c(" ","L. perennis Fed", "L. polyphyllus Fed", "Total")
df_EF[1,1]<- "Eclosed"
df_EF[2,1]<- "Didn't Eclose"
df_EF[3,1]<-"Total"
df_EF[1,2:4]<-c(41.364, 28.636, 70)
df_EF[2,2:4]<-c(10.636, 7.364, 18)
df_EF[3,2:4]<-c(52, 36, 88)
head(df_EF)
#X2 value = 21.5493, df = 1, p < 0.01

#Graph both mosaic plots together
par(mfrow=c(1,2))
mosaicplot(t(survivalTable), col=c("#4f5f29", "#9bbb59"), cex.axis=1,
           xlab="Feeding Group", ylab="Proportion Survived", 
           main="Survival Mosaic Plot")
mosaicplot(t(ecloseTable), col=c("#4f5f29", "#9bbb59"), cex.axis=1,
           xlab="Feeding Group", ylab="Proportion Eclosed", 
           main="Eclosure Mosaic Plot")


#Remove environment clutter. 
rm(df_OF)
rm(df_EF)
rm(ecloseTable)
rm(survivalTable)
rm(subsetEclose)
rm(surv)


#--------------------------------------------------------------------------------
# Do pupal weights differ between feeding groups?

full<-read.csv("WS_R_FullLarvae.csv")

# Checking assumptions of a 2 way t-test. 
subsetS<- full[ which(full$Treatment == 'Lupinus perennis'),]
shapiro.test(subsetS$Weight..g.) #p>0.05 normal dist
subsetL<- full[ which(full$Treatment == 'Lupinus polyphyllus'),]
shapiro.test(subsetL$Weight..g.) #p<0.05 skewed dist
# Assumptions not met. Move to Wilcoxon test. 
# Testing assumption of equal variance. 
leveneTest(Weight..g. ~ Treatment, data=full) #p>0.05 equal variance
# Wilcoxon Test. 
wilcox.test(Weight..g. ~ Treatment, data=full)
# Pupal weights differ between feeding groups (W=324, p=2.803x10^-14).
# Graphing pupal weight data. 
ggplot(full, aes(x=Treatment, y=Weight..g., fill=Treatment))+
  geom_boxplot(show.legend=FALSE)+theme_classic()+
  scale_fill_manual(values=c("#4f5f29", "#9bbb59"))+
  xlab("Feeding Group")+ylab("Pupal Weight (g)")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="#CCC", fill="#CCC")
# Result:
# Median weight for sundial lupine feeders is 0.115970 g
# Median weight for large leaf lupine feeders is 0.097355 g
aggregate(full$Weight..g., list(Treatment = full$Treatment),median)
aggregate(full$Weight..g., list(Treatment = full$Treatment),IQR)

#-----------------------------------------------------------median()#--------------------------------------------------------------------------------
# Does the number of days from hatch until pupation differ between feeding groups?

regress<-read.csv("WS_R_length_regression.csv")

# Checking assumptions of 2 way t-test. 
subsetS<- full[ which(full$Treatment == 'Lupinus perennis'),]
shapiro.test(subsetS$Pupated..days.) #p<0.05 skewed dist
subsetL<- full[ which(full$Treatment == 'Lupinus polyphyllus'),] 
shapiro.test(subsetL$Pupated..days.) #p<0.05 skewed dist
# Assumptions not met. Move to Wilcoxon. 
# Testing assumption of equal variance. 
leveneTest(Pupated..days. ~ Treatment, data=full) #p<0.05 unequal variance - possibly from outlier in large leaf group that extends the range of data for days to pupation?
# Wilcoxon Test. 
wilcox.test(Pupated..days. ~ Treatment, data=full)
# Number of days until pupation differ between feeding groups (W=3277.5, p=2.868x10^-15). 


# Linear model. 
rlm<-lm(Length..mm.~Treatment*Age..days.,data=regress)
summary(rlm)
hist(resid(rlm)) #normal
qqnorm(resid(rlm)) #normal 

# Plot the model with boxplot comparing days to pupation. 
par(mfrow=c(1,2))
ggplot(full, aes(x=Treatment, y=Pupated..days., fill=Treatment))+
  geom_boxplot(show.legend=FALSE)+theme_classic()+
  scale_fill_manual(values=c("#4f5f29", "#9bbb59"))+
  xlab("Feeding Group")+ylab("Days to Pupation")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="#CCC", fill="#CCC")
ggplot(regress, aes(x=Age..days., y=Length..mm., color=Treatment)) +
  geom_point() + 
  geom_smooth(show.legend=FALSE, method=lm, aes(fill=Treatment)) + theme_classic()+
  scale_fill_manual(values=c("#4f5f29", "#9bbb59"))+
  scale_colour_manual(name="", values = c("Lupinus perennis"="#4f5f29", "Lupinus polyphyllus"="#9bbb59"))+
  xlab("Age (days)")+ylab("Larval Length (mm)")
# Result:
# Median number of days to pupation for sundial lupine feeders is 32 days
# Median number of days to pupation for large-leaf lupine feeders is 39 days
aggregate(full$Pupated..days., list(Treatment = full$Treatment),median)
aggregate(full$Pupated..days., list(Treatment = full$Treatment),IQR)

# Remove environment clutter. 
rm(subsetS)
rm(subsetL)
rm(regress)
rm(rlm)
rm(full)


#--------------------------------------------------------------------------------
# Do frosted elfins preferentially lay eggs on one lupine over the other?

laydata <-read.csv("WS_R_laydata.csv")

# Checking assumptions of 2 way t-test. 
shapiro.test(laydata$Number.Eggs) #p<0.05, skewed dist
# Assumptions not met. Move to Wilcoxon. 
# Testing assumption of equal variance. 
leveneTest(Number.Eggs ~ Location, data=laydata) #p>0.05, equal variance
# Wilcoxon Test. 
wilcox.test(laydata$Number.Eggs ~ laydata$Location)
# # More eggs laid on sundial lupine than large leaf lupine (W=216, p=0.005702). 
#Graphing. 
ggboxplot(laydata, x="Location", y="Number.Eggs",
          order = c("Lupinus perennis", "Lupinus polyphyllus"),
          ylab = "Number of Eggs", xlab = "Lay Loci",
          color = "Location", palette = c("#4f5f29", "#9bbb59"))

#Remove environment clutter. 
rm(laydata)


#--------------------------------------------------------------------------------