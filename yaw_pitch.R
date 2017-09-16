###
# In this script we analyze the correlation between 
# yaw and pitch and sense of presence and embodiment
###

# load libraries
library(dplyr)

find_correlation = function(data, vars_x, vars_y, cor_threshold, alpha) {
  found_correlation = F
  for (i in vars_x) {
    x = data[!is.na(data[,i]),i]
    for (j in vars_y) {
      # y = data[, j]
      y = data[!is.na(data[,j]),j]
      if (length(y) == length(x)) {
        r = cor.test(x, y, method='spearman', use='complete.obs', exact=F)
        if (r$estimate > cor_threshold && r$p.value < alpha) {
          print(paste('high and significant correlation (r=', round(r$estimate,2), 
                      ', p-value=', round(r$p.value,5), ') between ', 
                      colnames(data)[i], ' and ', colnames(data[j]), sep=''))
          found_correlation = T
        } 
      }
    }
  }
  if (!found_correlation) {
    print('The variables are not correlated')
  }
}

# read data
presence = read.csv("./data/Presence12.csv",header=T, sep=",", stringsAsFactors=F)
data_condition_a = read.csv("./data/uturn_data_condition_a.csv",header=T, sep=",", stringsAsFactors=F)
data_condition_b = read.csv("./data/uturn_data_condition_b.csv",header=T, sep=",", stringsAsFactors=F)

# remove textural and NA columns
presence = select(presence, -X, -Q4.2, -Q4.3, -Q4.4, -X.1, -Q5.1, -X.3)

# idx dependent vars
idx_sp_em = c(5:24,26:29)

##
# condition A
#
presence_A = filter(presence, Condition=='A')  # n= 25

# merge data
m_df = merge(presence_A, data_condition_a, by.x='User', by.y='User')  # n= 22

# idx independent vars
idx_yaw_pitch = c(42:51)

# analyze correlation
find_correlation(m_df, idx_sp_em, idx_yaw_pitch, 0.50, 0.05) # no correlation

##
# condition B
#
presence_B = filter(presence, Condition=='B')

# merge data
m_df = merge(presence_B, data_condition_b, by.x='User', by.y='User')  # n= 18

# idx independent vars
idx_yaw_pitch = c(40:49)

# analyze correlation
find_correlation(m_df, idx_sp_em, idx_yaw_pitch, 0.50, 0.05) # no correlation
