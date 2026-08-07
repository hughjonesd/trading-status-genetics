# Trading genetics in moba
# Fartein Ask Torvik, 2023-2026

library(tidyverse)
library(vroom)
library(haven)
library(lme4)
library(fixest)

#-------------------------------------------------------------------------------
# fiddling with the data, from regression script
d <- vroom('trading_dataset_20230705.csv')
d <- vroom('trading_dataset_20231102.csv')

# scale variables
d$eapgsresid <- as.numeric(scale(d$eapgsresid))
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
  select(PREGID, role, eapgsresid, eduyears, university, incomez) %>% 
  mutate(role = recode(role, 'M'='F','F'='M')) %>% 
  rename(eapgsresid_p = eapgsresid) %>% 
  rename(eduyears_p = eduyears) %>% 
  rename(university_p = university) %>% 
  rename(incomez_p = incomez) %>% 
  right_join(d)

dim(dd)

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
# RESHAPE TO SIBLING DATA
#-------------------------------------------------------------------------------

#if (!exists('pop')) pop = vroom("N:/durable/data/registers/original/csv/w19_0634_faste_oppl_ut.csv")
if (!exists('pop')) pop = vroom('N:/durable/data/registers/SSB/01_data/data_v6.1/CORE/csv/POPULATION_FASTE_OPPLYSNINGER_reduced.csv')

popshort <- pop %>% 
  filter(w19_0634_lnr %in% dd$w19_0634_lnr) %>% 
  rename(mor_lnr = lopenr_mor) %>% 
  rename(far_lnr = lopenr_far) %>% 
  select(w19_0634_lnr,mor_lnr,far_lnr)

popshort2 <- popshort %>% 
  filter(!is.na(mor_lnr)) %>% 
  filter(!is.na(far_lnr)) %>% 
  mutate(famid = paste0(mor_lnr,far_lnr))
popshort2

popshort3 <- popshort2 %>% 
  group_by(famid) %>%
  mutate(groupsize = n()) %>% 
  ungroup()
popshort3
popshort3 %>% count(groupsize)
nrow(popshort3)

set.seed(1)
popshort4 <- popshort3 %>% 
  arrange(sample(nrow(.))) %>% 
  group_by(famid) %>%
  mutate(sibnum = row_number()) %>% 
  ungroup()
popshort4
popshort4 %>% count(groupsize)
popshort4 %>% count(sibnum)
nrow(popshort4)

dwide0 <- dd %>% left_join(popshort4) 

count(dwide0, sibnum)

dwide1 <- dwide0 %>% 
  mutate(sibnum = ifelse(is.na(sibnum),1,sibnum))
dwide1 %>% count(sibnum)
dwide1 %>% count(groupsize)

#dwide1 <- dwide1 %>% 
#  select(w19_0634_lnr, sibnum, famid)

dwide1$famid[is.na(dwide1$famid)] <- paste0('miss',1:sum(is.na(dwide1$famid)))

count(dwide1, sibnum)
count(dwide1, is.na(famid))

dwide2 <- dwide1 %>% 
  filter(sibnum %in% 1:2) %>% 
  pivot_wider(
    names_from = sibnum,
    values_from = -famid,
    names_sep='_'
  )
dwide2
names(dwide2)
count(dwide2, groupsize_1, groupsize_2)
count(dwide2, is.na(famid))

# only individuals with siblings 
dwide3 <- dwide2 %>% filter(groupsize_1 >= 2) 
count(dwide3,groupsize_1)
sibdat = dwide3





#reverse coded
dwide2x <- dwide1 %>% 
  filter(sibnum %in% 1:2) %>% 
  mutate(sibnum2 = 2-sibnum+1) %>% 
  pivot_wider(
    names_from = sibnum2,
    values_from = -famid,
    names_sep='_'
  )
dwide3x <- dwide2x %>% filter(groupsize_1 >= 2) 
sibdatx = dwide3x

sibdatdouble = bind_rows(sibdat, sibdatx)

# det er noe kluss med dataene, det er mange som er sib2, men som ikke har noen sib 1 ?? 
# se om det har noe med missing på gorupsize å gjøre.

#-------------------------------------------------------------------------------

# doing some preliminary test regressions in these data:

summary(lm(eapgsresid_p_1 ~ parity_1 + eapgsresid_1 + parentalage_1 + factor(famsize_1) + byear_1, data=sibdat))
summary(lm(eapgsresid_p_2 ~ parity_2 + eapgsresid_2 + parentalage_2 + factor(famsize_2) + byear_2, data=sibdat))

siblong <- dwide0 %>% 
  filter(groupsize >= 2) %>% 
  group_by(famid) %>% 
  mutate(eapgsresid_mean = mean(eapgsresid)) %>% 
  mutate(eapgsresid_p_mean = mean(eapgsresid_p)) %>% 
  mutate(parity_mean = mean(parity)) %>% 
  mutate(eduyears_mean = mean(eduyears)) %>% 
  mutate(byear_mean = mean(byear)) %>% 
  mutate(parentalage_mean = mean(parentalage)) %>% 
  mutate(sex_mean = mean(sex)) %>% 
  identity()

#-------------------------------------------------------------------------------
# the one i sent david by email in 11 august 2023:
rrr= feols (eduyears_p ~ eapgsresid + eapgsresid_p + parity + sex + byear +1|famid, data=siblong)
summary(rrr)

#-------------------------------------------------------------------------------
# the ones david requested by email 07.09.2023:

smod1a  = feols(university_p ~ eapgsresid | famid + byear + bmonth, data = siblong)
smod1ax = feols(university_p ~ eapgsresid + eapgsresid_p | famid + byear + bmonth, data = siblong)
summary(smod1a )
summary(smod1ax)

smod1b  = feols(eduyears_p   ~ eapgsresid | famid + byear + bmonth, data = siblong)
smod1bx = feols(eduyears_p   ~ eapgsresid + eapgsresid_p | famid + byear + bmonth, data = siblong)
summary(smod1b )
summary(smod1bx)

smod2   = feols(incomez_p    ~ eapgsresid | famid + byear + bmonth, data = siblong)
smod2x  = feols(incomez_p    ~ eapgsresid + eapgsresid_p | famid + byear + bmonth, data = siblong)
summary(smod2  )
summary(smod2x )

#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x))
export_results_for_table3withinfamily_tidyse = lapply(, function(x) tidy(x))

export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



#-------------------------------------------------------------------------------
siblong <- siblong %>% ungroup()

# added june 2026
# we have a within sibship analysis

# results for table 3 WITHIN FAMILY NO AGE ADJUSTMENT
formulas_for_table3withinfamily <- list(
  column0 = eapgsresid_p ~ parity                                                   | famid + factor(famsize),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid | famid + factor(famsize),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid | famid + factor(famsize),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid | famid + factor(famsize),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid | famid + factor(famsize),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid | famid + factor(famsize)
)
results_for_table3withinfamily = lapply(formulas_for_table3withinfamily, feols, siblong)
lapply(results_for_table3withinfamily, summary)





# results for table 3 WITHIN FAMILY AGE ADJUSTED
formulas_for_table3withinfamilyageadjusted <- list(
  column0 = eapgsresid_p ~ parity +                                                 parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column1 = eapgsresid_p ~ parity +                                      eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  columnx = eapgsresid_p ~ parity + scl +                                eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column2 = eapgsresid_p ~ parity +       university +        height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column3 = eapgsresid_p ~ parity +                   incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column4 = eapgsresid_p ~ parity +       university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth),
  column5 = eapgsresid_p ~ parity + scl + university +incomez+height+bmi+eapgsresid+parentalage | famid + factor(famsize)+factor(byear)+factor(bmonth)
)
results_for_table3withinfamilyageadjusted = lapply(formulas_for_table3withinfamilyageadjusted, feols, siblong)
lapply(results_for_table3withinfamilyageadjusted, summary)



# export
export_results_for_table3withinfamily_glance = lapply(results_for_table3withinfamily, glance)
export_results_for_table3withinfamily_tidyse = lapply(results_for_table3withinfamily, function(x) tidy(x, se='cluster'))
export_results_for_table3withinfamilyageadjusted_glance = lapply(results_for_table3withinfamilyageadjusted, glance)
export_results_for_table3withinfamilyageadjusted_tidyse = lapply(results_for_table3withinfamilyageadjusted, function(x) tidy(x, se='cluster'))

# save objects
sibresults_2023 = list(rrr,smod1a,smod1ax,smod1b,smod1bx,smod2,smod2x)
sibresults_2023_glance = lapply(sibresults_2023, glance)
sibresults_2023_tidyse = lapply(sibresults_2023, function(x) tidy(x, se='cluster'))





save(
  export_results_for_table3withinfamily_glance,
  export_results_for_table3withinfamily_tidyse,
  export_results_for_table3withinfamilyageadjusted_glance,
  export_results_for_table3withinfamilyageadjusted_tidyse,
  sibresults_2023_glance,
  sibresults_2023_tidyse,
  file = 'tradinggenetics_moba_sibs_revised_2026_v02.Rdata')

load ('tradinggenetics_moba_sibs_revised_2026_v02.Rdata', verbose=T)



