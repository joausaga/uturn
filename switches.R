###
# In this script we aim to answer the following question:
# Q: Does the amount of switches (Switches in Presence12 file) from one
# side to another in condition A affect the sense of presence and embodiment 
# values (variables in Presence12 file)?
###



# load libraries
library(dplyr)

# read data
presence = read.csv("./data/Presence12.csv",header=T, sep=",", stringsAsFactors=F)

# remove textural and NA columns
presence = select(presence, -X, -Q4.2, -Q4.3, -Q4.4, -X.1, -Q5.1, -X.3)

# select data related with condition A
presence = filter(presence, Condition=='A')

# analyze correlation between amount of switches and sense of presence
cor(presence$Switches,presence$PP1,method='spearman',use='complete.obs')
cor(presence$Switches,presence$PP2,method='spearman',use='complete.obs')
cor(presence$Switches,presence$PP3,method='spearman',use='complete.obs')
cor(presence$Switches,presence$PP4,method='spearman',use='complete.obs')
cor(presence$Switches,presence$PP5,method='spearman',use='complete.obs')
cor(presence$Switches,presence$SP1,method='spearman',use='complete.obs')
cor(presence$Switches,presence$SP2,method='spearman',use='complete.obs')
cor(presence$Switches,presence$SP3,method='spearman',use='complete.obs')
cor(presence$Switches,presence$SP4,method='spearman',use='complete.obs')
cor(presence$Switches,presence$SP5,method='spearman',use='complete.obs')
# the amount of switches is not associated with the sense of presence

# analyze correlation between amount of switches and embodiment
cor(presence$Switches,presence$E1,method='spearman',use='complete.obs')  # cor: -0.61
cor(presence$Switches,presence$E2,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E3,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E4,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E5,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E6,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E7,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E8,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E9,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E10,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E11,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E12,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E13,method='spearman',use='complete.obs')
cor(presence$Switches,presence$E14,method='spearman',use='complete.obs')
# only E1 shows to be highly correlated with the amount of switches

# check correlation between E1 and the amount of switches
cor.test(presence$Switches,presence$E1,method='spearman',use='complete.obs', exact=F)
# S = 2851.3, p-value = 0.002574
# rho  -0.6100193 