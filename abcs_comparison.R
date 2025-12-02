library(tidyverse)
a1 <- vroom::vroom("https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/abcs/standard/data.csv.gz")

agg1 <- a1 %>%
  mutate(period = if_else(time <'2000-01-01' ,0,
                     if_else(  time >='2000-01-01' & time < '2005-01-01',1,
                               if_else(      time >='2005-01-01' & time < '2010-01-01',2,
                                             if_else(      time >='2010-01-01' & time < '2015-01-01',3,
                                                           if_else(      time >='2015-01-01' & time < '2020-01-01',4,
                                                                         if_else(     time >='2020-01-01' & time < '2024-01-01',5, NA_real_
                          ))))))
         )%>%
  group_by(period, geography, serotype) %>%
  summarize(N_IPD = sum(N_IPD)) %>%
  pivot_wider(id_cols=c(period, serotype) , values_from = N_IPD, names_from=geography ) %>%
  rename(ABCs = '00',
         CA = '06',
         CO = '08',
         CT = '09',
         GA = '13',
         MD = '24',
         MN = '27',
         NM ='35',
         NY = '36',
         OR = '41',
         TN ='47'
         ) %>%
  ungroup() %>%
  mutate( period = factor(period, levels = 0:5, labels = c('1998-1999','2000-2004', '2005-2009', '2010-2014', '2015-2019', '2020-2023'))
  )

agg1 %>%
  dplyr::select(-period, -serotype) %>%
  as.matrix() %>%
  cor()

cor(as.matrix(agg1[,'ABCs','CA','CO',"CT",'GA','MD','MN','NM','NY','OR', 'TN']))


ggplot(agg1) + 
  geom_point(aes(x=CT, y=ABCs, color=period, group=period)) + 
  #facet_wrap(~period)+
  theme_classic() +
  ggtitle('Serotype patterns in Connecticut match the national patterns')
