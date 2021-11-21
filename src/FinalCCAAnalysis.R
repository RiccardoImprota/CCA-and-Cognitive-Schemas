
#=======================================================================

# Cognitive schema studies with WVS
# Riccardo Improta
# Università degli Studi di Trento

#=======================================================================

#
# Data Preparation and cleaning
#


df <-EVS_WVS_Joint_v2_0[,c("cntry_AN","A008",
                           "A165",
                           "E018",
                           "E025",
                           "F063",
                           "F118",
                           "F120",
                           "G006",
                           "X025R",
                           "X001",
                           "X003R",
                           "X047_WVS7")]

rm(EVS_WVS_Joint_v2_0)

df$Culture_Zone <- "undefined"

df[df$cntry_AN %in% c("DK","SE","FI","DE","IS","NL","NO","CH"),]$Culture_Zone <- "Protestant Europe"
df[df$cntry_AN %in% c("AM","BA","BG","BY","GE","GR","ME","RO","RU","RS","UA","MK"),]$Culture_Zone <- "Orthodox Europe"
df[df$cntry_AN %in% c("AL","AZ","BD","ET","ID","IR","IQ","KZ","JO","KG","LB","NG","PK","ZW","TJ","TN","TR","EG"),]$Culture_Zone <- "African-Islamic"
df[df$cntry_AN %in% c("IN","MM","CY","MY","SG","VN","TH"),]$Culture_Zone <- "West/South Asia"
df[df$cntry_AN %in% c("AD","AT","HR","CZ","FR","HU","IT","PL","PT","SK","SI","ES","EE","LT"),]$Culture_Zone <- "Catholic Europe"
df[df$cntry_AN %in% c("BO","AR","BR","CL","CO","EC","GT","MX","NI","PE","PH","PR"),]$Culture_Zone <- "Latin America"
df[df$cntry_AN %in% c("CN","TW","HK","JP","KR","MO"),]$Culture_Zone <- "Confucian"
df[df$cntry_AN %in% c("AU","CA","NZ","GB","US"),]$Culture_Zone <- "English Speaking"

df$Culture_Zone=as.factor(df$Culture_Zone)
df <- subset( df, select = -c(cntry_AN))

library(dplyr)
df=filter(df, !(A008 <0))
df=filter(df, !(A165 <0))
df=filter(df, !(E018 <0))
df=filter(df, !(E025 <0))
df=filter(df, !(F063 <0))
df=filter(df, !(F118 <0))
df=filter(df, !(F120 <0))
df=filter(df, !(G006 <0))
df=filter(df, !(X001 <0))
df=filter(df, !(X047_WVS7 <0))
df=filter(df, !(X025R <0))
df=filter(df, !(X003R <0))
df=filter(df, !(A008 ==10))
df=filter(df, !(A165 ==10))
df=filter(df, !(E018 ==10))
df=filter(df, !(E025 ==10))
df=filter(df, !(F063 ==10))
df=filter(df, !(F118 ==10))
df=filter(df, !(F120 ==10))
df=filter(df, !(G006 ==10))


df<-df %>% 
  rename(
    "Feeling_of_happiness"= "A008",
    "Most_people_can_be_trusted"="A165",
    "Respect_for_authority"="E018",
    "Political_action_signing_a_petition"="E025",
    "Importance_of_God"="F063",
    "Justifiable_Homosexuality"="F118",
    "Justifiable_Abortion"="F120",
    "Proud_of_nationality"="G006",
    "Level_of_education"="X025R",
    "Sex"="X001",
    "Age"="X003R",
    "Wealth"="X047_WVS7",
  )

library(expss)
df = apply_labels(df,
                  "Feeling_of_happiness"= "A008",
                  "Most_people_can_be_trusted"="A165",
                  "Respect_for_authority"="E018",
                  "Political_action_signing_a_petition"="E025",
                  "Importance_of_God"="F063",
                  "Justifiable_Homosexuality"="F118",
                  "Justifiable_Abortion"="F120",
                  "Proud_of_nationality"="G006",
                  "Level_of_education"="X025R",
                  "Sex"="X001",
                  "Age"="X003R",
                  "Wealth"="X047_WVS7"
)







#=======================================================================

#
# CCA analysis
#



China <- df[which(df$Culture_Zone=="Confucian" ),]
Anglo <- df[which(df$Culture_Zone=="English Speaking" ),]

Total2<- rbind(China,Anglo)
library(corclass)
Res2<-cca(Total2[, which(names(Total2) %in% c("Feeling_of_happiness",
                                              "Most_people_can_be_trusted",
                                              "Respect_for_authority",
                                              "Political_action_signing_a_petition",
                                              "Importance_of_God",
                                              "Justifiable_Homosexuality",
                                              "Justifiable_Abortion",
                                              "Proud_of_nationality"))],
          filter.significance = TRUE,filter.value = 0.25,zero.action="ownclass",verbose=TRUE)



#=====
# Optional plots
plot(Res2,1,main='Module #1')
plot(Res2,3,main='Module #2')
plot(Res2,4,main='Module #3')





#=======================================================================

#
# Random Forests for variable importance
#


Total2$CCAmodule <- Res2$membership
Total2$CCAmodule = as.factor(Total2$CCAmodule)

# The CCA contains a small degenerate class, the number 2 (only five elements)
# I will therefore exclude elements that appear in that class.
Total2 <- Total2[Total2$CCAmodule != 2,]
Total2$CCAmodule[which(Total2$CCAmodule==3)]<-2
Total2$CCAmodule[which(Total2$CCAmodule==4)]<-3
Total2$CCAmodule = as.factor(Total2$CCAmodule)
Total2$CCAmodule <-droplevels(Total2$CCAmodule)

# Random Forest
library(randomForest)
set.seed(71)
rf <-randomForest(CCAmodule~.,data=Total2,mtry=5,importance=TRUE, ntree=500)


#optional plot
par(xpd = FALSE)
impToPlot <- importance(rf, scale=FALSE)
dotchart(sort(impToPlot[,1]),xlab="Mean Decrease Accuracy",main="Variable Importance for CCA allocation",sub="")



#=======================================================================
#
# Plots
#
par(xpd = FALSE)

#Module Graphs
plot(Res2,1,main='CCA: Module #1',bw=TRUE,LAYOUT = layout.fruchterman.reingold)
plot(Res2,3,main='CCA: Module #2',bw=TRUE,LAYOUT = layout.fruchterman.reingold)
plot(Res2,4,main='CCA: Module #3',bw=TRUE)

#CClass Distribution by Culture Zone
library(ggplot2)
ggplot(Total2) +ggtitle("CClass Distribution by Culture Zone")+ geom_bar(aes(x = CCAmodule, y = ..prop.., group = Culture_Zone))+ facet_wrap(~ Culture_Zone)+theme(plot.title = element_text(hjust = 0.5))



# Variable Importance
par(xpd = FALSE)
impToPlot <- importance(rf, scale=FALSE)
dotchart(sort(impToPlot[,1]),xlab="Mean Decrease Accuracy",main="Variable Importance for CClasses",sub="")


#====================================================================
#
# Summary Data
#

print(mean(Total2$Importance_of_God[which(Total2$CCAmodule==1)]))
print(mean(Total2$Importance_of_God[which(Total2$CCAmodule==2)]))
print(mean(Total2$Importance_of_God[which(Total2$CCAmodule==3)]))
print(mean(Total2$Importance_of_God[which(Total2$Culture_Zone=="Confucian")]))
print(mean(Total2$Importance_of_God[which(Total2$Culture_Zone=="English Speaking")]))


print(mean(Total2$Justifiable_Homosexuality[which(Total2$CCAmodule==1)]))
print(mean(Total2$Justifiable_Homosexuality[which(Total2$CCAmodule==2)]))
print(mean(Total2$Justifiable_Homosexuality[which(Total2$CCAmodule==3)]))
print(mean(Total2$Justifiable_Homosexuality[which(Total2$Culture_Zone=="Confucian")]))
print(mean(Total2$Justifiable_Homosexuality[which(Total2$Culture_Zone=="English Speaking")]))


print(mean(Total2$Political_action_signing_a_petition[which(Total2$CCAmodule==1)]))
print(mean(Total2$Political_action_signing_a_petition[which(Total2$CCAmodule==2)]))
print(mean(Total2$Political_action_signing_a_petition[which(Total2$CCAmodule==3)]))
print(mean(Total2$Political_action_signing_a_petition[which(Total2$Culture_Zone=="Confucian")]))
print(mean(Total2$Political_action_signing_a_petition[which(Total2$Culture_Zone=="English Speaking")]))

sum(Total2$CCAmodule==1 & Total2$Culture_Zone=="Confucian")
sum(Total2$CCAmodule==1 & Total2$Culture_Zone=="English Speaking")
sum(Total2$CCAmodule==2 & Total2$Culture_Zone=="Confucian")
sum(Total2$CCAmodule==2 & Total2$Culture_Zone=="English Speaking")
