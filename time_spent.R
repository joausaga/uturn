###
# In this script we aim to answer the following question:
# Q: Does the amount of time spent on female/male side in 
# condition A (the female ratio in Presence 12) affect the sense 
# of presence and embodiment values (variables in Presence12 file)
###

# load libraries
library(dplyr)

# read data
presence = read.csv("./data/Presence12.csv",header=T, sep=",", stringsAsFactors=F)

# remove textural and NA columns
presence = select(presence, -X, -Q4.2, -Q4.3, -Q4.4, -X.1, -Q5.1, -X.3)

# select data related with condition A
presence = filter(presence, Condition=='A')

# print n
nrow(presence)

# analyze correlation between amount of FemaleRatio and sense of presence
cor(presence$FemaleRatio,presence$PP1,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$PP2,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$PP3,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$PP4,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$PP5,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$SP1,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$SP2,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$SP3,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$SP4,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$SP5,method='spearman',use='complete.obs')


# analyze correlation between amount of FemaleRatio and embodiment
cor(presence$FemaleRatio,presence$E1,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E2,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E3,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E4,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E5,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E6,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E7,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E8,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E9,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E10,method='spearman',use='complete.obs')   # cor: -0.47
cor(presence$FemaleRatio,presence$E11,method='spearman',use='complete.obs')  
cor(presence$FemaleRatio,presence$E12,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E13,method='spearman',use='complete.obs')
cor(presence$FemaleRatio,presence$E14,method='spearman',use='complete.obs')
# only E10 shows to be somehow highly correlated with the amount of FemaleRatio

# check correlation between E10 and the amount of FemaleRatio
cor.test(presence$FemaleRatio,presence$E10,method='spearman',use='complete.obs', exact=F)
# S = 2604.7, p-value = 0.02703
# rho -0.4707408 