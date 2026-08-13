# Trading genetics in moba
# Fartein Ask Torvik, 2023 & 2026
#-------------------------------------------------------------------------------
library(tidyverse)
library(vroom)
library(fixest)
library(broom)
#-------------------------------------------------------------------------------
d <- vroom('trading_dataset_20231102.csv')

# scale variables
d$eapgsresid <- as.numeric(scale(d$eapgsresid))
d$deppgsresid <- as.numeric(scale(d$deppgsresid))
d$bmipgsresid <- as.numeric(scale(d$bmipgsresid))
d$heightpgsresid <- as.numeric(scale(d$heightpgsresid))
d$smokingpgsresid <- as.numeric(scale(d$smokingpgsresid))
var(d$eapgsresid, na.rm=T)

# parity. truncated at 6.
table(d$parity)
#d$parity[d$parity>=6] = 6
table(d$parity)

# family size truncated at 6.
table(d$famsize)
#d$famsize[d$famsize>=6] = 6
table(d$famsize)

# height in cm
table(d$height)

# bmi in kg/m2
table(round(d$bmi))

# age; average of parents, in years
cor(d$fatherage, d$motherage, use='c')
d$parentalage = (d$fatherage + d$motherage)/ 2
table(d$parentalage)

# edulevel
table(d$edulevel)
d <- d %>% mutate(university = if_else(edulevel >= 6, 1, 0))
d <- d %>% mutate(eduyears = recode(edulevel, 
                           `1` = 7, 
                           `2` = 9, 
                           `3` = 10.5, 
                           `4` = 11, 
                           `5` = 12, 
                           `6` = 15, 
                           `7` = 17, 
                           `8` = 20))

# add spouse PGS to all observations/individuals (introduce dependency)
dd <- d %>% 
  select(PREGID, role, eapgsresid, deppgsresid, bmipgsresid, heightpgsresid, smokingpgsresid) %>% 
  mutate(role = recode(role, 'M'='F','F'='M')) %>% 
  rename(eapgsresid_p = eapgsresid) %>% 
  rename(deppgsresid_p = deppgsresid) %>% 
  rename(bmipgsresid_p = bmipgsresid) %>% 
  rename(heightpgsresid_p = heightpgsresid) %>% 
  rename(smokingpgsresid_p = smokingpgsresid) %>% 
  right_join(d)

dim(dd)

dd$incomez = dd$income30z
dd$incomedec = dd$income30dec
cor(dd$incomez, dd$income30z, use='c')

dd <- dd %>% filter(
  !is.na(eapgsresid_p),
  !is.na(parity),
  !is.na(university),
  !is.na(incomez),
  !is.na(height),
  !is.na(bmi),
  !is.na(eapgsresid),
  !is.na(parentalage),
  !is.na(famsize),
  !is.na(byear),
  !is.na(bmonth)
)
dim(dd)
dd %>% count(famsize)
dd <- dd %>% filter(famsize %in% 2:6)
dim(dd)

dd <- dd %>% mutate(incomez = ifelse(incomez >= 10,10,incomez) )
sum(dd$incomez>=10,na.rm=T)

#-------------------------------------------------------------------------------
# regressions
#-------------------------------------------------------------------------------

# results table 1
formulas_for_table1 <- list(
  university = university~parity+eapgsresid+parentalage |    factor(famsize)+factor(byear)+factor(bmonth),
  income = incomez~parity+eapgsresid+parentalage        |    factor(famsize)+factor(byear)+factor(bmonth),
  height= height~parity+eapgsresid+parentalage          |    factor(famsize)+factor(byear)+factor(bmonth),
  bmi = bmi~parity+eapgsresid+parentalage               |    factor(famsize)+factor(byear)+factor(bmonth),
  scl = scl~parity+eapgsresid+parentalage               |    factor(famsize)+factor(byear)+factor(bmonth)
) # note: 'factor' not needed

results_for_table1 = lapply(formulas_for_table1, feols, dd, cluster='PREGID')
lapply(results_for_table1, summary)

# results for table 2
formulas_for_table2 <- list(
  column1 = eapgsresid_p~parity | factor(famsize),
  column2 = eapgsresid_p~parity + eapgsresid | factor(bmonth) + factor(famsize)+factor(byear),
  column3 = eapgsresid_p~parity+eapgsresid+parentalage | factor(bmonth) + factor(famsize)+factor(byear)
)

results_for_table2 = lapply(formulas_for_table2, feols, dd, cluster='PREGID')
lapply(results_for_table2, summary)


# results for table 3
formulas_for_table3 <- list(
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3 = lapply(formulas_for_table3, feols, dd, cluster='PREGID')
lapply(results_for_table3, summary)



# Request 7: no birth year or month control. results for table 3
formulas_for_table3_nobirthadjust <- list(
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)
)
results_for_table3_nobirthadjust = lapply(formulas_for_table3_nobirthadjust, feols, dd, cluster='PREGID')
lapply(results_for_table3_nobirthadjust, summary)



# Response to Request #1, 2026-06-29. Adding PGI controls
formulas_for_table3pgicontrols <- list(
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+deppgsresid+bmipgsresid+heightpgsresid+smokingpgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+deppgsresid+bmipgsresid+heightpgsresid+smokingpgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+deppgsresid+bmipgsresid+heightpgsresid+smokingpgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+deppgsresid+bmipgsresid+heightpgsresid+smokingpgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+deppgsresid+bmipgsresid+heightpgsresid+smokingpgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+deppgsresid+bmipgsresid+heightpgsresid+smokingpgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3pgicontrols = lapply(formulas_for_table3pgicontrols, feols, dd, cluster='PREGID')
lapply(results_for_table3pgicontrols, summary)
# Describing PGI differences by parity
formulas_balancedescriptive <- list(
  pgi_ea_self = eapgsresid ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_dep_self = deppgsresid ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_bmi_self = bmipgsresid ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_height_self = heightpgsresid ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_smoking_self = smokingpgsresid ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),

  pgi_ea_partner = eapgsresid_p ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_dep_partner = deppgsresid_p ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_bmi_partner = bmipgsresid_p ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_height_partner = heightpgsresid_p ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth),
  pgi_smoking_partner = smokingpgsresid_p ~ factor(parity)  | factor(famsize)+factor(byear)+factor(bmonth)
  
)
balancedescriptives = lapply(formulas_balancedescriptive, feols, dd, cluster='PREGID')
lapply(balancedescriptives, summary)





# results for table 3 with dummies for birth order
formulas_for_table3dummy <- list(
  column1 = eapgsresid_p ~ factor(parity) +                                      eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ factor(parity) + scl +                                eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ factor(parity) +       university +        height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ factor(parity) +                   incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ factor(parity) +       university +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ factor(parity) + scl + university +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3dummy = lapply(formulas_for_table3dummy, feols, dd, cluster='PREGID')
lapply(results_for_table3dummy, summary)


# results for table 3 with dummies for birth order stratified by family size
famsizestratified <- list(
  famsize2 = dd %>% filter(famsize==2) %>% feols(eapgsresid_p ~ factor(parity) + eapgsresid+parentalage | factor(byear)+factor(bmonth), cluster='PREGID'),
  famsize3 = dd %>% filter(famsize==3) %>% feols(eapgsresid_p ~ factor(parity) + eapgsresid+parentalage | factor(byear)+factor(bmonth), cluster='PREGID'),
  famsize4 = dd %>% filter(famsize==4) %>% feols(eapgsresid_p ~ factor(parity) + eapgsresid+parentalage | factor(byear)+factor(bmonth), cluster='PREGID'),
  famsize5 = dd %>% filter(famsize==5) %>% feols(eapgsresid_p ~ factor(parity) + eapgsresid+parentalage | factor(byear)+factor(bmonth), cluster='PREGID'),
  famsize6 = dd %>% filter(famsize==6) %>% feols(eapgsresid_p ~ factor(parity) + eapgsresid+parentalage | factor(byear)+factor(bmonth), cluster='PREGID')
  )
lapply(famsizestratified, summary)


# results for table 3 eduyears
formulas_for_table3eduyears <- list(
  column1 = eapgsresid_p ~ parity +                                    eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                              eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       eduyears +        height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                 incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       eduyears +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + eduyears +incomez+height+bmi+eapgsresid+parentalage | factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3eduyears = lapply(formulas_for_table3eduyears, feols, dd, cluster='PREGID')
lapply(results_for_table3eduyears, summary)

# results for table 5
results_for_table5male          = lapply(formulas_for_table3[c(1:6)], feols, dd %>% filter(male==0), cluster='PREGID')
results_for_table5female        = lapply(formulas_for_table3[c(1:6)], feols, dd %>% filter(male==1), cluster='PREGID')
names(results_for_table5male)   = paste0(names(results_for_table5male),'male')
names(results_for_table5female) = paste0(names(results_for_table5female),'female')
results_for_table5=c(results_for_table5male, results_for_table5female)
lapply(results_for_table5, summary)

# results for figure 4:
figure4university <- dd %>% 
  group_by(sex, university) %>%
  summarise(mean = mean(eapgsresid_p, na.rm = TRUE),
            lower = t.test(eapgsresid_p, conf.level = 0.95)$conf.int[1],
            upper = t.test(eapgsresid_p, conf.level = 0.95)$conf.int[2])
figure4incomedec <- dd %>% 
  group_by(sex, incomedec) %>%
  summarise(mean = mean(eapgsresid_p, na.rm = TRUE),
            lower = t.test(eapgsresid_p, conf.level = 0.95)$conf.int[1],
            upper = t.test(eapgsresid_p, conf.level = 0.95)$conf.int[2])



# export results
export_table1_glance = lapply(results_for_table1, broom::glance)
export_table2_glance = lapply(results_for_table2, glance)
export_table3_glance = lapply(results_for_table3, glance)
export_table3pgicontrols_glance = lapply(results_for_table3pgicontrols, glance)
export_table3eduyears_glance = lapply(results_for_table3eduyears, glance)
export_table3dummy_glance  = lapply(results_for_table3dummy, glance)
export_table3_nobirthadjust_glance = lapply(results_for_table3_nobirthadjust, glance)
export_table5_glance = lapply(results_for_table5, glance)
balancedescriptives_glance = lapply(balancedescriptives, glance)
famsizestratified_glance  = lapply(famsizestratified, glance)



export_table1_tidyse = lapply(results_for_table1, function(x) tidy(x, vcov = ~ PREGID))
export_table2_tidyse = lapply(results_for_table2, function(x) tidy(x, vcov = ~ PREGID))
export_table3_tidyse = lapply(results_for_table3, function(x) tidy(x, vcov = ~ PREGID))
export_table3pgicontrols_tidyse = lapply(results_for_table3pgicontrols, function(x) tidy(x, vcov = ~ PREGID))
export_table3eduyears_tidyse = lapply(results_for_table3eduyears, function(x) tidy(x, vcov = ~ PREGID))
export_table3dummy_tidyse = lapply(results_for_table3dummy, function(x) tidy(x, vcov = ~ PREGID))
export_table3_nobirthadjust_tidyse = lapply(results_for_table3_nobirthadjust, function(x) tidy(x, vcov = ~ PREGID))
export_table5_tidyse = lapply(results_for_table5, function(x) tidy(x, vcov = ~ PREGID))
balancedescriptives_tidyse = lapply(balancedescriptives, function(x) tidy(x, vcov = ~ PREGID))
famsizestratified_tidyse = lapply(famsizestratified, function(x) tidy(x, vcov = ~ PREGID))



export_table1_tidyse_error = lapply(results_for_table1, function(x) tidy(x, se='cluster'))
export_table2_tidyse_error = lapply(results_for_table2, function(x) tidy(x, se='cluster'))
export_table3_tidyse_error = lapply(results_for_table3, function(x) tidy(x, se='cluster'))
export_table3pgicontrols_tidyse_error = lapply(results_for_table3pgicontrols, function(x) tidy(x, se='cluster'))
export_table3eduyears_tidyse_error = lapply(results_for_table3eduyears, function(x) tidy(x, se='cluster'))
export_table3dummy_tidyse_error = lapply(results_for_table3dummy, function(x) tidy(x, se='cluster'))
export_table3_nobirthadjust_tidyse_error = lapply(results_for_table3_nobirthadjust, function(x) tidy(x, se='cluster'))
export_table5_tidyse_error = lapply(results_for_table5, function(x) tidy(x, se='cluster'))
balancedescriptives_tidyse_error = lapply(balancedescriptives, function(x) tidy(x, se='cluster'))
famsizestratified_tidyse_error = lapply(famsizestratified, function(x) tidy(x, se='cluster'))




  

save(  export_table1_glance,export_table2_glance,export_table3_glance,export_table5_glance,
  export_table1_tidyse,export_table2_tidyse,export_table3_tidyse,export_table5_tidyse, 
  export_table3dummy_glance,export_table3eduyears_glance,
  export_table3dummy_tidyse,export_table3eduyears_tidyse,
  figure4university,figure4incomedec,
  
  #added 2026-06
  export_table3pgicontrols_glance,export_table3pgicontrols_tidyse,balancedescriptives_glance,balancedescriptives_tidyse, export_table3dummy_tidyse,
  famsizestratified_glance, famsizestratified_tidyse, 
  export_table3_nobirthadjust_glance, export_table3_nobirthadjust_tidyse, 
  
  file = 'tradinggenetics_moba_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_revised_2026_v02.Rdata', verbose=T)




# extra checks
summary(lm(dd$eapgsresid~factor(dd$famsize)+factor(dd$parity)))
summary(lm(dd$heightpgsresid~factor(dd$famsize)+factor(dd$parity)))
summary(lm(dd$bmipgsresid~factor(dd$famsize)+factor(dd$parity)))

