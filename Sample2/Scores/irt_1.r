library(datasets)
pacman::p_load(rio,shiny,mirt)
learn_data <- import('learn_scores.csv')
d <- learn_data[,3:ncol(learn_data)]
head(d)
irt_model <- mirt(d,
                  1,
                  itemtype = '2PL'
)
for (i in 1:length(d)) {
  ItemPlot  <- itemfit(irt_model, 
                       group.bins  =  9, 
                       empirical.plot  =  i, 
                       empirical.CI  =  .95,
                       method  = 'ML')
  print(ItemPlot)
}
learn_scores <- as.vector(fscores(irt_model))
learn_scores
summary(irt_model)
learn_data_params <- coef(irt_model,simplify = TRUE)
write.csv((learn_data_params),file='learn_item_params.csv')

# repeating the same for cognitive items
cog_data <- import('cog_scores.csv')
d <- cog_data[,3:ncol(cog_data)]
head(d)
irt_model <- mirt(d,
                  1,
                  itemtype = '2PL'
)
for (i in 1:length(d)) {
  ItemPlot  <- itemfit(irt_model, 
                       group.bins  =  9, 
                       empirical.plot  =  i, 
                       empirical.CI  =  .95,
                       method  = 'ML')
  print(ItemPlot)
}


cog_scores <- as.vector(fscores(irt_model))
cog_scores
summary(irt_model)
cog_data_params <- coef(irt_model,simplify = TRUE)
write.csv((cog_data_params),file='cog_item_params.csv')


scores <- data.frame(learn_scores, cog_scores)
write.csv((scores),file='learncog_ability.csv')
