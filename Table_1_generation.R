# Code that generates the results of Table 1 for the paper:
#Orestis Papakyriakopoulos and Ellen Goodmann. 2022. The Impact of Twitter Labels on Misinformation Spread and User Engagement: Lessons from Trump’s Election Tweets. 
#In Proceedings of the ACM Web Conference 2022 (WWW '22). Association for Computing Machinery, New York, NY, USA, 2541–2551. DOI:https://doi.org/10.1145/3485447.3512126

# read tweets of Donald Trump
df <- read.csv("trump_tweets_labeled.csv")

#create misinformation variable
df$misinformation <- 0
df[df$fraud>0 | df$won>0 | df$ballots>0, c("misinformation")] <- 1

# descriptive statistics for labeled and unlabeled tweets by type
table(df$flagged[df$misinformation==1])
table(df$flagged)
table(df$flagged[df$fraud==1])
table(df$flagged[df$won==1])
table(df$flagged[df$ballots==1])

# median values for labeled and unlabeled tweets by type
apply(df[df$flagged=="Y",c('favorites','replies', 'retweets','quote_tweets')],2,median)
apply(df[df$flagged=="N",c('favorites','replies', 'retweets','quote_tweets')],2,median)

apply(df[(df$flagged=="Y") & (df$fraud==1),c('favorites','replies', 'retweets','quote_tweets')],2,median)
apply(df[(df$flagged=="N") & (df$fraud==1),c('favorites','replies', 'retweets','quote_tweets')],2,function(x) median(x, na.rm = T))

apply(df[(df$flagged=="Y") & (df$won==1),c('favorites','replies', 'retweets','quote_tweets')],2,median)
apply(df[(df$flagged=="N") & (df$won==1),c('favorites','replies', 'retweets','quote_tweets')],2,median)

apply(df[(df$flagged=="Y") & (df$ballots==1),c('favorites','replies', 'retweets','quote_tweets')],2,median)
apply(df[(df$flagged=="N") & (df$ballots==1),c('favorites','replies', 'retweets','quote_tweets')],2,median)



# Mann-Whitney U tests for each type of user interaction and warning label category
# flagged vs not flagged - all
for (type in c('favorites','replies', 'retweets','quote_tweets')){
print(wilcox.test(as.vector(df[df$flagged=="Y",type]),as.vector(df[df$flagged=="N",type]), paired = F))
}

# flagged vs not flagged - "fraud statements"
for (type in c('favorites','replies', 'retweets','quote_tweets')){
  print(wilcox.test(as.vector(df[df$flagged=="Y" & (df$won==1),type]),as.vector(df[df$flagged=="N" & (df$fraud==1),type]), paired = F))
}

# flagged vs not flagged - "won the election statements"
for (type in c('favorites','replies', 'retweets','quote_tweets')){
  print(wilcox.test(as.vector(df[df$flagged=="Y" & (df$won==1),type]),as.vector(df[df$flagged=="N" & (df$won==1),type]), paired = F))
}

# flagged vs not flagged - "mailing ballots statements"
for (type in c('favorites','replies', 'retweets','quote_tweets')){
  print(type)
  print(wilcox.test(as.vector(df[df$flagged=="Y" & (df$ballots==1),type]),as.vector(df[df$flagged=="N" & (df$ballots==1),type]), paired = F))
}

