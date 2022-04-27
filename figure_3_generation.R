# Code that generates the results of Figure 3 for the paper:
#Orestis Papakyriakopoulos and Ellen Goodmann. 2022. The Impact of Twitter Labels on Misinformation Spread and User Engagement: Lessons from Trump’s Election Tweets. 
#In Proceedings of the ACM Web Conference 2022 (WWW '22). Association for Computing Machinery, New York, NY, USA, 2541–2551. DOI:https://doi.org/10.1145/3485447.3512126

library(betareg)
library(sjPlot)
library(nnet)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)
library(betareg)

# add type ="pearson" in summary (line 6)
trace(insight:::get_statistic.betareg, edit=TRUE)
trace(parameters:::standard_error.betareg, edit=TRUE)
trace(parameters:::p_value.betareg, edit=TRUE)
# Replace line 33 with 'vcov.fun <- "vcovHAC"'
trace(plot_model, edit=TRUE)
# add type ="pearson" in type (line 5)
trace(betareg:::residuals.betareg, edit=TRUE)

#saveRDS(df_total, "comments_toxicity.RDS")
df_total <- readRDS('comments_toxicity_figure_3.RDS')
df_fraud_class <- readRDS('comments_fraud_figure_3.RDS')

# run beta regression for toxicity in comments
tox_reg <- betareg(toxicity ~ flag_type + overlap_type + Rebuttal  + date +   orientation +  Freq + won + fraud + ballots, data = df_total)
summary(tox_reg, type='pearson')


# run multinomial regression for fraud-related comments
multinom_model <-nnet::multinom(labels ~ flag_type + overlap_type + Rebuttal  + date +     orientation + Freq + won + fraud + ballots , data = df_fraud_class)

summary(multinom_model, type='pearson')
z <- summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# generate final figure
figure_toxicity<- plot_model(tox_reg, type = "est", vline.color = "gray", axis.labels = c("Ballot related tweets","Fraud related tweets","Election winner related tweets","Posting Frequency","Political orientation","Date","Strong rebuttal","Moderate rebuttal","Topical overlap","Linguistic overlap","Contextual label","Veracity label")) + theme_minimal()  +   ylab("change in odds ratio")  +
  scale_color_manual(values = c('orange', 'black'))+    ylim(0.8, 1.2)  + ggtitle("            Toxicity")

figure_fraud<- plot_model(multinom_model, type = "est", vline.color = "gray", axis.labels = c("Ballot related tweets","Fraud related tweets","Election winner related tweets","Posting Frequency","Political orientation","Date","Strong rebuttal","Moderate rebuttal","Topical overlap","Linguistic overlap","Contextual label","Veracity label")) + theme_minimal()  +   ylab("change in odds ratio")  +
  scale_color_manual(values = c('orange', 'black')) +    ylim(0.6, 1.5) + ggtitle("Rejecting fraud theory          Supporting fraud theory")   + theme(plot.title = element_text(size = 9))

figure <- ggarrange( figure_fraud+ rremove("xlab") + theme(
                         strip.background = element_blank(),
                         strip.text.x = element_blank()
                       ), ggplot() + theme_void(), figure_toxicity+ rremove("xlab")  + theme(axis.text.y = element_blank())  + theme(plot.title = element_text(size = 9)) ,
                    ncol =3, nrow = 1, common.legend = T, align = "hv",  widths = c(2, -0.05,1))

annotate_figure(figure, top=textGrob("Relation between label properties and user comments", gp = gpar(cex = 1), hjust = 0.3), bottom = textGrob("Change in odds generating a reply",gp = gpar(cex = 0.9), hjust = 0.2))

