

library(tidyverse)
library(png)
library(lubridate)
library('patchwork')
library(cowplot)
library(ggplot2)
#ibrary(mapview)
library(sf)

library(dplyr)
library(babynames)
library(viridis)
#library(hrbrthemes)
library(plotly)



#install.packages('sf')
#install.packages('mapview')
#install.packages('sMART_ER')

atp<- read_csv('D:\\smart\\monthly_reports\\2026\\q1_026\\all_observations_for_r_000017.csv',show_col_types = FALSE)
atp

#view(atp)
atp4<- read_csv('D:\\smart\\monthly_reports\\2026\\q1_026\\patrol_per_setor_for_r_000018.csv',show_col_types = FALSE)
atp4
#view(atp4)
#replacing col spaces with underscore

dn <- atp                 # Duplicate data
colnames(dn) <- gsub(' ', '_', colnames(dn))
dn      

dr<- atp4                 # Duplicate data
colnames(dr) <- gsub(' ', '_', colnames(dr))
dr
#view(dr)

# Print updated data
#view(dn)

# Splitting date col

atp3<-separate(dn, 'Waypoint_Date', c('day','month','year'), sep = ' ')
#view(atp3)
#converting month to a factor to aid set the levels in the desired order
atp2<-atp3|>
# mutate(month=factor(month, levels=month.name))
 #mutate(Month = factor(month, levels = month.name[month.name %in% unique(month)]))
  mutate(month=fct_relevel (month,'Jan','Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
#view(atp2)

#atp2
#arrange(match(months, month.name))

atp6<-separate(dr, 'patrol_end_date', c('month','year'), sep = '-')
#
atp5<-atp6|>
  # mutate(month=factor(month, levels=month.name))
  #mutate(Month = factor(month, levels = month.name[month.name %in% unique(month)]))
  mutate(month=fct_relevel (month,'Jan','Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
#view(atp5)

#atp5
#arrange(match(months, month.name))

#view(atp5)




#---------------------------------CHARTS----------------

#patrols, Patrol hrs, patrol km, violations, turtle poaching, mangrove logging, illegal fishing
#illegal fishing, legal encounters, illegal encounters

#======================foot patrols for q1 2026 
patrl_ft_q4<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type %in% c('Foot','Vehicle') & year =='26' & month %in% c('Jan','Feb','Mar'))%>%
  summarize(across(where(is.numeric), list(sum = sum)))%>%
  tidyr::pivot_longer(cols = where(is.numeric),
               names_sep = '_sum',
               names_to  = c('component', 'n_value'))%>%
  ggplot(aes(x=component, y=value, fill=component)) +
  geom_bar(stat='identity',fill = '#332288')+
  #scale_fill_manual(values = c('#D4B068', '#808000','#454B1B','red','blue'),limits = c('24', '23','22'))+
  geom_text(aes(label=round(value, 0)), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  ggtitle('Foot Patrols') + 
  xlab(' ') + 
  ylab('Sum') +
  theme(axis.text.x = element_text(color = 'grey20', size = 10, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
        axis.text.y = element_text(color = 'grey20', size = 10, angle = 0, hjust = 0.5, vjust = 0.5, face = 'plain'),  
        axis.title.x = element_text(color = '#808080', size = 12, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
        axis.title.y = element_text(color = '#808080', size = 12, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
        plot.title= element_text(color = 'grey20', size = 14, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
        strip.text = element_text(size=18))
#facet_wrap(facets = 'year')
#+coord_flip()
#patrl_ft_q4


#========================================foot patrols for year 
 months_to_keep_2026 <- c("Jan", "Feb", "Mar")
 patrl_ft_v1<- atp5 %>%
   mutate(
    year = as.factor(year),
    month = as.factor(month),
    Number_of_Patrols = as.numeric(as.character(Number_of_Patrols))
   ) %>%
  # 1. Fill all gaps with 0
  complete(year, month, fill = list(Number_of_Patrols = 0)) %>%
  # 2. Specifically remove future months for 2026 only
  filter(!(year == "26" & !month %in% months_to_keep_2026))
#view(patrl_ft_v1)
# Plotting

  patrl_ft<-patrl_ft_v1%>%
  drop_na(Number_of_Patrols)%>%
  filter(type %in% c('Foot','Vehicle')& year %in% c('26','25','24','23','22','21'))%>%
  #filter(type=='Foot'& year =='25' & month %in% c('Jan','Feb','Mar'))%>%
  group_by(month,year)%>%
  #view(patrl_ft)
  summarise(tot= sum((Number_of_Patrols), na.rm = TRUE),.groups="drop")%>%
  
  ggplot(aes(x=month,y=tot,color=year, group = year))+

  # geom_histogram(month())
  
  geom_point(size=2.5)+
  geom_line(linewidth = 2)+ 
  scale_color_manual(values = c('#117733','#999933','#ddcc77','#332288','#cc6677', '#aa4499'),limits = c('26','25','24', '23','22','21'))+
 ggtitle('Foot Patrols') + 
  geom_text(aes(label = sprintf('%1.0f', tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Patrols') +
  guides(col= guide_legend(title= 'year'))+
  theme(legend.title = element_text(color = 'grey20', size = 16),legend.text = element_text(color = 'grey20', size = 14))+
  theme(axis.text.x = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
        axis.text.y = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0, vjust = 0, face = 'plain'),  
        axis.title.x = element_text(color = '#808080', size = 16, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
        axis.title.y = element_text(color = '#808080', size = 16, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
        plot.title= element_text(color = 'grey20', size = 20, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
        strip.text = element_text(size=18))
#facet_wrap(facets = 'Age')
#coord_flip()
#patrl_ft
  
  
  
  
  
#  ===============fill areA
  patrl_ft_v1%>%
    drop_na(Number_of_Patrols)%>%
    filter(type %in% c('Foot','Vehicle')& year %in% c('26','25','24','23','22','21'))%>%
    #filter(type=='Foot'& year =='25' & month %in% c('Jan','Feb','Mar'))%>%
    group_by(month,year)%>%
    #view(patrl_ft)
    summarise(tot= sum((Number_of_Patrols), na.rm = TRUE),.groups="drop")%>%
    
    ggplot(aes(x=month,y=tot,color=year,fill=year, group = year))+
    
    # geom_area adds the shading. 
    # alpha = 0.3 makes it transparent to see overlapping years.
    geom_area(alpha = 0.3, position = "identity") +
    
    geom_point(size=2.5)+
    geom_line(linewidth = 2)+ 
    scale_color_manual(values = c('#117733','#999933','#ddcc77','#332288','#cc6677', '#aa4499'),limits = c('26','25','24', '23','22','21'))+
    #adding scale fill to match yeares custom colours
    scale_fill_manual(values = c('#117733','#999933','#ddcc77','#332288','#cc6677', '#aa4499'),
                      limits = c('26','25','24', '23','22','21')) +
    ggtitle('Foot Patrols') + 
    geom_text(aes(label = sprintf('%1.0f', tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
    xlab('Month') + 
    ylab('Number of Patrols') +
    guides(col= guide_legend(title= 'year'))+
    theme(legend.title = element_text(color = 'grey20', size = 16),legend.text = element_text(color = 'grey20', size = 14))+
    theme(axis.text.x = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
          axis.text.y = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0, vjust = 0, face = 'plain'),  
          axis.title.x = element_text(color = '#808080', size = 16, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
          axis.title.y = element_text(color = '#808080', size = 16, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
          plot.title= element_text(color = 'grey20', size = 20, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
          strip.text = element_text(size=18)) 

#=============================boat patrols for q1 2026

patrl_bt_q1<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type %in% c('Boat') & year =='26' & month %in% c('Jan','Feb','Mar'))%>%
  summarize(across(where(is.numeric), list(sum = sum)))%>%
  tidyr::pivot_longer(cols = where(is.numeric),
                      names_sep = '_sum',
                      names_to  = c('component', 'n_value'))%>%
  ggplot(aes(x=component, y=value, fill=component)) +
  geom_bar(stat='identity',fill = '#332288')+
  #scale_fill_manual(values = c('#D4B068', '#808000','#454B1B','red','blue'),limits = c('25','24', '23','22'))+
  geom_text(aes(label=round(value, 0)), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  ggtitle('Boat Patrols') + 
  xlab(' ') + 
  ylab('Sum') +
  theme(axis.text.x = element_text(color = 'grey20', size = 10, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
        axis.text.y = element_text(color = 'grey20', size = 10, angle = 0, hjust = 0.5, vjust = 0.5, face = 'plain'),  
        axis.title.x = element_text(color = '#808080', size = 12, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
        axis.title.y = element_text(color = '#808080', size = 12, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
        plot.title= element_text(color = 'grey20', size = 14, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
        strip.text = element_text(size=18))
#facet_wrap(facets = 'year')
#+coord_flip()
#patrl_bt_q4


#==========================================boat patrols for year 
patrl_bt<- patrl_ft_v1 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Boat'& year %in% c('26','25','24','23','22'))%>%
  #filter(type=='Foot'& year =='25' & month %in% c('Jan','Feb','Mar'))%>%
  group_by(month,year)%>%
  summarise(tot= sum((Number_of_Patrols), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color=as.factor(year), group = year))+
  geom_point(size=2)+
  geom_line(linewidth = 1.6)+ 
  scale_color_manual(values = c('#117733','#999933','#ddcc77','#332288','#cc6677', '#aa4499'),limits = c('26','25','24', '23','22'))+
  ggtitle('Boat Patrols') + 
  geom_text(aes(label = sprintf('%1.0f', tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Patrols') +
  guides(col= guide_legend(title= 'year'))+
  theme(legend.title = element_text(color = 'grey20', size = 16),legend.text = element_text(color = 'grey20', size = 14))+
  theme(axis.text.x = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
        axis.text.y = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0, vjust = 0, face = 'plain'),  
        axis.title.x = element_text(color = '#808080', size = 16, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
        axis.title.y = element_text(color = '#808080', size = 16, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
        plot.title= element_text(color = 'grey20', size = 20, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
        strip.text = element_text(size=18))
#facet_wrap(facets = 'Age')
#coord_flip()
#patrl_bt
#patrl_bt_q4

-------

# resource usersline

usersl<-atp2 %>%
  drop_na(User_Type)%>%
  filter(Category_0=='Resource use' & year %in% c('2026','2025','2024','2023','2022','2021'))%>%
  group_by(month,year)%>%
  summarise(tot = sum((Sum), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = year, group = year))+
  geom_point(size=2)+
  geom_line(linewidth = 1.6)+ 
  scale_color_manual(values = c('#117733','#999933','#ddcc77','#332288','#cc6677', '#aa4499'),limits = c('2026','2025','2024', '2023','2022','2021'))+
  #geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 3) +
  #geom_text(aes(label = ifelse(year=='25',tot,'')), vjust = -0.5, hjust = 1,size=5,show.legend=F) +
  #aes(label=ifelse(player=="player_a",paste0(100*fg_perc,"%")
  ggtitle('Resource Users') + 
  xlab('Month') + 
  ylab('Number of Resource Users') +
  theme((legend.position='right'))+
  theme(legend.title = element_text(color = 'grey20', size = 16),legend.text = element_text(color = 'grey20'))+
  theme(axis.text.x = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
        axis.text.y = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0, vjust = 0, face = 'plain'),  
        axis.title.x = element_text(color = '#808080', size = 16, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
        axis.title.y = element_text(color = '#808080', size = 16, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
        plot.title= element_text(color = 'grey20', size = 20, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
        strip.text = element_text(size=18))
#facet_wrap(facets = 'year')
# coord_flip()
#usersl

--------
#action taken
#===prepping
  
#case_evid_action<-atp2 %>%
#drop_na(Action_taken)%>%
#filter(year %in% c('2026','2025','2024','2023','2022','2021' ) & !Action_taken %in% c('None','Other'))%>%
#group_by(Action_taken,month,year,.drop = FALSE)%>%
#count(Action_taken,.drop = FALSE)
 
              #  summarise(counts = n())
  
#view(case_evid_action)
  
#write_csv(case_evid_action, 'D:\\smart\\monthly_reports\\2026\\q1_026\\caxn.csv')
  
#=======using the data
caxn<- read_csv('D:\\smart\\monthly_reports\\2026\\q1_026\\caxn.csv',show_col_types = FALSE)
  
caxn1<-caxn|>
mutate(month=fct_relevel (month,'Jan','Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
#view(caxn1)

#---------plotting
  
  case_evid_action<-caxn1 %>%
    filter(year %in% c('2026','2025','2024'))%>%

  ggplot(aes(x=month,y=n,color=as.factor(year)))+
  geom_point(size=1.2)+
  geom_line(linewidth = 1, aes(group = year))+ 
  scale_color_manual(values = c('#117733','#999933','#ddcc77','#332288','#cc6677', '#aa4499'),
                    # limits = c('2026','2025','2024', '2023','2022','2021'), name = 'Year')+
                    limits = c('2026','2025','2024'), name = 'Year')+
    geom_text(aes(label = n),show.legend=F, vjust = 0.5, hjust = 2, size = 3) +
  ggtitle('Action Taken') + 
  xlab('Month') + 
  ylab('Sum') +
  theme((legend.position='right'), size = 5)+
  theme(legend.title = element_text(color = 'grey20', size = 16),legend.text = element_text(color = 'grey20', size = 14))+
  theme(axis.text.x = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
        axis.text.y = element_text(color = 'grey20', size = 16, angle = 0, hjust = 0, vjust = 0, face = 'plain'),  
        axis.title.x = element_text(color = '#808080', size = 16, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
        axis.title.y = element_text(color = '#808080', size = 16, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
        plot.title= element_text(color = 'grey20', size = 20, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
        strip.text = element_text(size=18))+
facet_wrap(~ Action_taken, ncol=1)
# coord_flip()
#case_evid_action

#Plotting only 2024-2026
  
  # Define the months we want to keep for 2026
  months_to_keep_2026 <- c("Jan", "Feb", "Mar") 
  
  caxn1_updated <- caxn1 %>%
    mutate(
      year = as.factor(year),
      month = as.factor(month),
      Action_taken = as.factor(Action_taken)
    ) %>%
    filter(year %in% c('2026', '2025', '2024')) %>%
    # 1. Fill all gaps with 0
    complete(year, month, Action_taken, fill = list(n = 0)) %>%
    # 2. Specifically remove future months for 2026 only
    filter(!(year == "2026" & !month %in% months_to_keep_2026))
  
  # Plotting
  ggplot(caxn1_updated, aes(x = month, y = n, color = year)) +
    geom_point(size = 1.2) +
    geom_line(linewidth = 1, aes(group = year)) + 
    scale_color_manual(
      values = c('2026' = '#117733', '2025' = '#999933', '2024' = '#ddcc77'),
      name = 'Year'
    ) +
    geom_text(aes(label = n), show.legend = FALSE, vjust = -1, size = 3) +
    facet_wrap(~ Action_taken, ncol = 1) +
    labs(title = 'Action Taken', x = 'Month', y = 'Sum') +
    theme_minimal() +
    theme(
      legend.position = 'right',
      legend.title = element_text(color = 'grey20', size = 16),
      legend.text = element_text(color = 'grey20', size = 14),
      axis.text.x = element_text(color = 'grey20', size = 14),
      axis.text.y = element_text(color = 'grey20', size = 14),  
      plot.title = element_text(color = 'grey20', size = 20, hjust = 0.5, face = 'italic'),
      strip.text = element_text(size = 18, face = "bold")
    )
  
  
#=-===========================action taken per violation
  # illegal gears, mangrove logging, turtle poaching
  
evid_n_action<-atp2 %>%
 # drop_na(Action_taken)%>%
     filter(year %in% c('2026') & month %in% c('Jan','Feb','Mar')& !Action_taken %in% c('None','Other')
          # & Observation %in% c('Undersized net', 'Monofilament','Spear gun','Mosquito net')
           & Category_0 %in% c('Mangrove logging','Poached turtles sum')
            )%>%
  group_by(Action_taken,year,.drop = FALSE)%>%
  count(Action_taken,.drop = FALSE)
#%>%
    # summarise(counts = n())%>%
  view(evid_n_action)
  
  
  
#=============================================turtle poaching


#for poached turtle,mangrove logging,illegal gears
#illegal gears

antp_ig1<-select(atp2, month, year, Category_0,Observation,Sum,Sum_of_Evidence) %>%
  drop_na(Observation)%>%
  filter(Category_0=='Resource use'& Observation 
         %in% c('Undersized net', 'Monofilament',
                'Spear gun','Mosquito net')  
    # & year %in% c('24','23','22'))%>%
   & year =='2026' & month %in% c('Jan','Feb','Mar'))%>%
  #group_by(year)%>%
  summarise(illegal_gears = sum((Sum_of_Evidence), na.rm = TRUE)) %>%
  summarize(across(where(is.numeric), list(sum = sum)))%>%
  tidyr::pivot_longer(cols = where(is.numeric),
                      names_sep = '_sum',
                      names_to  = c('component', 'sum'))

#view(antp_ig1)

#illegal logging

antp_illg2<-select(atp2, month, year, Category_0,Observation,Sum) %>%
 # drop_na(Observation)%>%
 # filter(Category_0=='Mangrove logging' & year %in% c('24','23','22'))%>%
  filter(Category_0 %in% c('Mangrove logging','Poached turtles sum') & year =='2026' & month %in% c('Jan','Feb','Mar'))%>%
group_by(Category_0)%>%
  summarise(tot = sum((Sum), na.rm = TRUE))

#view(antp_illg2)
#view(antp_ig1)
  
# Rename columns in antp_ig1 to match antp_illg2
antp_ig1_renamed <- antp_ig1 %>%
  rename(Category_0=component, tot=value)

#view(antp_ig1_renamed)

# Append rows from df2 into df1
combined_df1 <- bind_rows(antp_illg2, antp_ig1_renamed)  # Alternative: combined_df <- rbind(antp_illg2, antp_ig1_renamed)

# View the result
#print(combined_df1)

combined_df<-combined_df1 %>% 
  mutate(violations = replace(Category_0, Category_0 =='Poached turtles sum' , 'Poached turtles' ))



# assigning the second column name to a new name tot-number
colnames(combined_df)[2] <- 'number'

#view(combined_df)

violns<-select(combined_df, c('violations','number'))


#bar
#violations_q4<- violns %>%
 # drop_na(number)%>%
 # ggplot(aes(x=violations, y=number)) +
#  geom_bar(stat='identity',fill = '#191970')+
 # geom_text(aes(label=number,), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  #ggtitle('Violations') + 
 # xlab('Violation Type') + 
#  ylab('Sum') +
 # theme(axis.text.x = element_text(color = 'grey20', size = 10, angle = 0, hjust = 0.5, vjust = 0, face = 'plain'),
 #       axis.text.y = element_text(color = 'grey20', size = 10, angle = 0, hjust = 0.5, vjust = 0.5, face = 'plain'),  
  #      axis.title.x = element_text(color = '#808080', size = 12, angle = 0, hjust = .5, vjust = 0, face = 'plain'),
  #      axis.title.y = element_text(color = '#808080', size = 12, angle = 90, hjust = .5, vjust = .5, face = 'plain'),
  #      plot.title= element_text(color = 'grey20', size = 14, angle = 0, hjust = .5, vjust = .5, face = 'italic'),
 #       strip.text = element_text(size=18))
#facet_wrap(facets = 'year')
#+coord_flip()
#violations_q4


#view(violations)

#pie
rh_cols <- c('#999933','#ddcc77','#332288','#cc6677', '#aa4499')

violations_q2<- violns %>%
  drop_na(number)%>%
 # ggplot(aes(x=Observation_Category_0, y=tot)) +
  ggplot(aes(x = 2, number, fill = violations)) +
  #scale_x_discrete(drop=FALSE)+
  geom_bar(stat='identity',width=1) +
  coord_polar('y', start=0)+
  geom_text(aes(label=number,), size = 8,colour='white',position = position_stack(vjust = .5))+
  scale_fill_manual(values = rh_cols) +
 theme_void() +
  #xlim(0.5, 2.5)+
  guides(fill=guide_legend(title='Violations'))+
  ggtitle('Violations')+
  theme(title= element_text(color = 'grey20', size = 14, angle = 0, hjust = 0.5,face = 'italic'),
        strip.text = element_text(size=18),plot.title = element_text(hjust = 0.5))
#facet_wrap(facets = 'year')
#+coord_flip()
#violations_q2

#plot hotspot trends
hotspot<-atp2 %>%
  drop_na(Area) %>%
  filter(year =='2026')%>%
  #filter(Observation_Category_0 != 'meeting with informer' & Resource_Use_Type != 'Legal use' & year =='2025')%>%
  #group_by(Observation_Category_0)%>%
 # summarise(tot = sum((Sum), na.rm = TRUE))


view(hotspot)



# Turn it interactive
#ilg_fish_g <- ggplotly(ilg_fish_g, tooltip='text')
#ilg_fish_g


# single plots
plot_grid(patrl_ft_q4,nrow = 1 ,labels = c('A1'))
#  630*560 #q1_foot_patrols_details
plot_grid(patrl_ft,nrow = 1 ,labels = c('A2'))
#1750*625 # foot_patrols
plot_grid(patrl_bt_q1,nrow = 1 ,labels = c('A3'))
#  630*560 #q1_boat_patrols_details
plot_grid(patrl_bt,nrow = 1 ,labels = c('A4'))
#  1750*625#boat_parols
plot_grid(violations_q2,nrow = 1 ,labels = c('A5'))
#  720*550#violations
plot_grid(usersl,nrow = 1 ,labels = c('A6'))
#  1750*625 #resource_users
plot_grid(case_evid_action,nrow = 1 ,labels = c('A7'))
# width=1272&height=1278# action_taken


#exporting dataframes to cvs

#write_csv_arrow(cars, 'cars.csv')

#jpeg(file = “D:\\smart\\monthly_reports\\2024\\monthly_06_024\\outputs_q2\\out_2\\test.jpeg”)

#plot(x,y)
#dev.off()





#bulk plots
#jpeg('hist_gpa_sat.jpg')
#par(mfrow=c(2,1))
#(ilg_fish_g)
#(mang_lc)

#dev.off()



#
