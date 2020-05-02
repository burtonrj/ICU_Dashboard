x <- "weight"

admissions$covid19 <- unlist(map(admissions$covid19,
                                 function(x){
                                   if(TRUE){
                                     return("Yes")
                                     }else{
                                     return("No")}}))
admissions %>% 
  ggplot(aes_string(x='age', fill='covid19')) +
  geom_density(alpha=0.5, adjust=0.5) +
  scale_fill_manual(values=c('grey', 'red'),
                    labels=c('No', 'Yes')) +
  ylab('') + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background = element_rect(fill = "white", 
                                        colour = "grey50"))

admissions %>%
  ggplot(aes_string(x='covid19', y='`ICU Duration (hours)`')) +
  geom_violin(aes(), alpha=0.1) +
  #geom_dotplot(aes_string(fill='Unit_Outcome'),
  #             binaxis='y',
  #             stackdir = 'centerwhole',
  #             stackgroups = TRUE,
  #             binpositions = 'all')
 +
  theme(panel.background = element_rect(fill = "white", 
                                        colour = "grey50")) +
  xlab('COVID-19 infected')
