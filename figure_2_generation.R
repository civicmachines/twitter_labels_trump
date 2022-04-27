# Code that generates the results of Figure 2 for the paper:
#Orestis Papakyriakopoulos and Ellen Goodmann. 2022. The Impact of Twitter Labels on Misinformation Spread and User Engagement: Lessons from Trump’s Election Tweets. 
#In Proceedings of the ACM Web Conference 2022 (WWW '22). Association for Computing Machinery, New York, NY, USA, 2541–2551. DOI:https://doi.org/10.1145/3485447.3512126


library(AER)
library(rstan)
library(rstanarm)
library(vcd)
library(sjPlot)
library(ggplot2)
library(ggpubr)
library(MASS)
library(scales)

options(scipen = 999)

# read data
df <- read.csv("trump_tweets_labeled.csv")
# format date for models
df$date <- as.numeric(as.Date(df$date))
df$date <- df$date - min(df$date)

# create aggregate dummies for flag_type & overlap_type
df$flag_type <- "no_flag"
df$flag_type[df$Contextual==1] <- "Contextual"
df$flag_type[df$Veracity==1] <- "Veracity"
df$flag_type <- factor(df$flag_type, levels = c("no_flag", "Veracity", "Contextual"))
df$overlap_type <- "no_overlap"
df$overlap_type[df$Lingual.overlap==1] <- "Lingual"
df$overlap_type[df$Content.overlap==1] <- "Content"
df$overlap_type <- factor(df$overlap_type, levels = c("no_overlap", "Content", "Lingual"))
df$Rebuttal <- factor(df$Rebuttal, levels = c("0", "1", "2"))
#

# run logistic regression models for favorites, retweets, replies & quote tweets
model1 <- rlm( log2(favorites) ~ flag_type + overlap_type + Rebuttal + date + won+ fraud + ballots , data = df )
coeftest(model1, vcov = vcovHAC)
#
model2 <- rlm( log2(retweets) ~    flag_type + overlap_type + Rebuttal + date + won+ fraud + ballots, data = df )
coeftest(model2, vcov = vcovHAC)
#
model3 <- rlm( log(replies)  ~ flag_type + overlap_type + Rebuttal + date + won+ fraud + ballots, data = df  )
coeftest(model3, vcov = vcovHAC)
#
model4 <- rlm( log(quote_tweets) ~ flag_type  + overlap_type + Rebuttal + date + won+ fraud + ballots, data = df   )
coeftest(model4, vcov = vcovHAC)

#### edit the standard error function - add vcovHAC instead of vcovHC in line 33
trace(plot_model, edit=TRUE)

# create single model plots
s1 <- plot_model(model1, type = "est", ci.lvl=0.95,
                 robust = T, show.data = T, vline.color = "gray", p.style='asterisk',  axis.labels = c("Ballot related tweets","Fraud related tweets","Election winner related tweets","Date","Strong rebuttal","Moderate rebuttal","Topical overlap","Linguistic overlap","Contextual label","Veracity label")) + theme_minimal()  + ggtitle('favorites') + ylab("User engagement percentual change")  +
     scale_color_manual(values = c('orange', 'black')) +  scale_y_continuous(labels = trans_format("exp",  label_number(suffix = "%", scale = 100)))

s1
s2 <- plot_model(model2, type = "est",ci.lvl=0.95,
                 robust = T, show.data = T, vline.color = "gray", p.style='asterisk', trasform ="exp", axis.labels = c("Ballot related tweets","Fraud related tweets","Election winner related tweets","Date","Strong rebuttal","Moderate rebuttal","Topical overlap","Linguistic overlap","Contextual label","Veracity label")) + ggtitle('retweets') + theme_minimal()  +   ylab("User engagement change (in thousands)")  +
  scale_color_manual(values = c('orange', 'black')) +  scale_y_continuous(labels = trans_format("exp",  label_number(suffix = "%", scale = 100)))

s2
s3 <- plot_model(model3, type = "est",robust = T, show.data = T, vline.color = "gray", p.style='asterisk',  axis.labels = c("Ballot related tweets","Fraud related tweets","Election winner related tweets","Date","Strong rebuttal","Moderate rebuttal","Topical overlap","Linguistic overlap","Contextual label","Veracity label")) + ggtitle('replies') + theme_minimal()  +   ylab("User engagement change (in thousands)")  +
  scale_color_manual(values = c('orange', 'black')) +  scale_y_continuous(labels = trans_format("exp",  label_number(suffix = "%", scale = 100)))

s3

s4 <- plot_model(model4, type = "est",robust = T, show.data = T, vline.color = "gray", p.style='asterisk', trasform ="exp", axis.labels = c("Ballot related tweets","Fraud related tweets","Election winner related tweets","Date","Strong rebuttal","Moderate rebuttal","Topical overlap","Linguistic overlap","Contextual label","Veracity label")) + ggtitle('quote tweets') + theme_minimal()  +   ylab("User engagement change (in thousands)") +
  scale_color_manual(values = c('orange', 'black'))+  scale_y_continuous(labels = trans_format("exp",  label_number(suffix = "%", scale = 100)))
s4

# generate general plot
figure <- ggarrange(s1+theme(axis.title.x=element_blank()) , s2+theme(axis.title.x=element_blank()) + 
                              theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    axis.title.y = element_blank(),
                                    plot.margin = margin(r = 1) ) , s3+theme(axis.title.x=element_blank()) + 
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank(),
                                            plot.margin = margin(r = 1) ) , s4+theme(axis.title.x=element_blank()) + 
                                              theme(axis.text.y = element_blank(),
                                                    axis.ticks.y = element_blank(),
                                                    axis.title.y = element_blank(),
                                                    plot.margin = margin(r = 1) ) ,
                    ncol = 4, nrow = 1, common.legend = T, widths = c(1.3, 1,1, 1), align = "h") 
annotate_figure(figure, top=textGrob("Effect of label prevalence and type on Trump tweets' user engagement", gp = gpar(cex = 1.3)), bottom = textGrob("User engagement change compared to baseline (100%)", gp = gpar(cex = 1)))
