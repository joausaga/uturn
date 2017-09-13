###
# In this script we aim to answer the following question:
# Q: Does the speed of moving the gaze (Speed in Presence12) 
# in A and B affect the sense of presence and embodiment values 
# (variables in Presence12 file)?
###

# load libraries
library(dplyr)

# read data
presence = read.csv("./data/Presence12.csv",header=T, sep=",", stringsAsFactors=F)

# remove textural and NA columns
presence = select(presence, -X, -Q4.2, -Q4.3, -Q4.4, -X.1, -Q5.1, -X.3)

##
# condition A
#
presence_A = filter(presence, Condition=='A')  # n= 25

# analyze correlation between amount of Speed and sense of presence
cor(presence_A$Speed,presence_A$PP1,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$PP2,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$PP3,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$PP4,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$PP5,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$SP1,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$SP2,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$SP3,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$SP4,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$SP5,method='spearman',use='complete.obs')
# no correlation

# analyze correlation between amount of Speed and embodiment
cor(presence_A$Speed,presence_A$E1,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E2,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E3,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E4,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E5,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E6,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E7,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E8,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E9,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E10,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E11,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E12,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E13,method='spearman',use='complete.obs')
cor(presence_A$Speed,presence_A$E14,method='spearman',use='complete.obs')


##
# condition B
#
presence_B = filter(presence, Condition=='B')   # n= 22

# analyze correlation between amount of Speed and sense of presence
cor(presence_B$Speed,presence_B$PP1,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$PP2,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$PP3,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$PP4,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$PP5,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$SP1,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$SP2,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$SP3,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$SP4,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$SP5,method='spearman',use='complete.obs')
# no correlation

# analyze correlation between amount of Speed and embodiment
cor(presence_B$Speed,presence_B$E1,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E2,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E3,method='spearman',use='complete.obs')  # cor: -0.52
cor(presence_B$Speed,presence_B$E4,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E5,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E6,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E7,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E8,method='spearman',use='complete.obs')  # cor: -0.42
cor(presence_B$Speed,presence_B$E9,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E10,method='spearman',use='complete.obs')  
cor(presence_B$Speed,presence_B$E11,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E12,method='spearman',use='complete.obs')
cor(presence_B$Speed,presence_B$E13,method='spearman',use='complete.obs') # cor: 0.48 (n=1)
cor(presence_B$Speed,presence_B$E14,method='spearman',use='complete.obs')
# E3 shows to be highly correlated with the amount of Speed (there is only one record for E13)

# check correlation between E3 and the amount of Speed
cor.test(presence_B$Speed,presence_B$E3,method='spearman', use='complete.obs', exact=F)
# S = 853.63, p-value = 0.0448
# rho -0.5243376