# for mini-analysis example in letter to editor @ AJOG 
# regarding article https://pubmed.ncbi.nlm.nih.gov/35283088/

library(tidyverse)
library(readxl)
library(gee)

# ------------------------- set variables to use ---------------------------- #

var_lb <- "ND_IntentRetLB"
var_denom <- "ND_NumIntentRet"

# ---------------------------------------------------------------------------- #
# -------------------------- WRANGLE DATA ----------------------------------- #
# ---------------------------------------------------------------------------- #

mandated_states <- c("Connecticut", "Illinois", "Maryland", "Massachusetts"
                     , "New Jersey", "Rhode Island")

clinics18_00 <- read_excel("data/FINAL-2018-clinic-table-dataset.xlsx"
                        , sheet = "Clinic Table Data Records") %>%
  select(clinic_name = CurrentClinicName1, clinic_state = CurrentClinicState
          , starts_with(c(var_lb, var_denom)))

# * indicates a number >0 but <5
# for some clinics with >0 but <5 live births, format for LB column is "* / #"
# or for 0 live births is "0 / #"
# or for >0 but <5 live births and >0 but <5 retrievals "* / *" 
check1 <- clinics18_00 %>% count(across(paste0(var_denom,5)))
check2 <- clinics18_00 %>% count(across(paste0(var_lb,5)))
check3 <- clinics18_00 %>% count(across(paste0(var_lb,4)))

clinics18_0 <- clinics18_00 %>%
  pivot_longer(cols=-c(clinic_name, clinic_state), names_to="var", values_to="value") %>%
  separate(value, into=c("temp1","temp2"), sep="/", remove=FALSE) %>%
  mutate(temp3 = case_when(# set * / * to missing
                          temp1=="*" & temp2=="*" ~ NA_real_
                          # set 0 / * to missing since don't have denominator 
                          # can't include in analysis
                          , temp2=="*" ~ NA_real_
                          # set # / # to correct %
                          , str_detect(value, "/") & temp1 != "*" & temp2 != "*" ~ parse_number(temp1)/parse_number(temp2)
                          # if reported as %, get number
                          , str_detect(value, "%") ~ parse_number(value)/100)
         , value_use = case_when(str_detect(var, var_denom) ~ parse_number(value)
                                , str_detect(var, var_lb) ~ temp3)
         , agegrp_num = parse_number(var)
         , agegrp = case_when(agegrp_num==1 ~ "<35"
                              , agegrp_num==2 ~ "35-37"
                              , agegrp_num==3 ~ "38-40"
                              , agegrp_num==4 ~ "41-42"
                              , agegrp_num==5 ~ "43+")
         , var2 = str_replace_all(var, "[:digit:]" ,"")
         , mandate = ifelse(clinic_state %in% toupper(mandated_states), yes=1, no=0)) %>%
  select(clinic_name, clinic_state, mandate, agegrp, var2, value_use) %>%
  pivot_wider(id_cols=c(clinic_name, clinic_state, mandate, agegrp)
              , names_from="var2", values_from="value_use")
       
check4 <- clinics18_0 %>% count(mandate, clinic_state)
check5 <- clinics18_0 %>% filter(is.na(!!rlang::sym(var_denom)) | !!rlang::sym(var_denom)==0)         
check6 <- clinics18_0 %>% filter(is.na(!!rlang::sym(var_lb)))
# how many known intended retrievals with unknown LB?
check7 <- clinics18_0 %>% filter(is.na(!!rlang::sym(var_lb)) &
                                !is.na(!!rlang::sym(var_denom))) %>%
  group_by(agegrp) %>%
  summarize(sum(!!rlang::sym(var_denom)))

clinics18 <- clinics18_0 %>% 
  mutate(numLB = round(!!rlang::sym(var_denom)*!!rlang::sym(var_lb),0)
         , numNoLB = !!rlang::sym(var_denom) - numLB) %>%
  # exclude clinics where % LB among intended retrievals is missing
  filter(!is.na(!!rlang::sym(var_lb)))

check8 <- clinics18 %>% filter(is.na(!!rlang::sym(var_denom)) | !!rlang::sym(var_denom)==0)         
check9 <- clinics18 %>% filter(is.na(!!rlang::sym(var_lb)))
check10 <- clinics18 %>% group_by(agegrp) %>% summarize(n=n())


# ---------------------------------------------------------------------------- #
# --------------- CHI-SQ ANALYSIS IGNORING CLUSTERING ------------------------ #
# ---------------------------------------------------------------------------- #

# --------------------------- overall ---------------------------------------- #

clinics18_forchisq_overall <- clinics18 %>%
  group_by(mandate) %>%
  summarize(totLB = sum(numLB, na.rm=TRUE)
            , totNoLB = sum(numNoLB, na.rm=TRUE)) %>%
  select(totLB, totNoLB)

clinics18_forchisq_overall
chisq_all <- chisq.test(clinics18_forchisq_overall)
chisq_all

# --------------------------- by age group ----------------------------------- #

clinics18_forchisq <- clinics18 %>% 
  group_by(agegrp, mandate) %>% 
  summarize(totLB = sum(numLB, na.rm=TRUE)
            , totNoLB = sum(numNoLB, na.rm=TRUE)
            , totDenom = sum(!!rlang::sym(var_denom), na.rm=TRUE)) %>%
  mutate(percLB = totLB/totDenom) 

clinics18_forchisq

clinics18_forchisq_agegrp1 <- clinics18_forchisq %>% 
  filter(agegrp=="<35") %>% 
  ungroup() %>% 
  select(totLB, totNoLB)

clinics18_forchisq_agegrp1
clinics18_forchisq %>% filter(agegrp=="<35") %>% select(mandate, percLB)
chisq1 <- chisq.test(clinics18_forchisq_agegrp1)
chisq1

clinics18_forchisq_agegrp2 <- clinics18_forchisq %>% 
  filter(agegrp=="35-37") %>% 
  ungroup() %>% 
  select(totLB, totNoLB)

clinics18_forchisq_agegrp2
clinics18_forchisq %>% filter(agegrp=="35-37") %>% select(mandate, percLB)
chisq2 <- chisq.test(clinics18_forchisq_agegrp2)
chisq2

clinics18_forchisq_agegrp3 <- clinics18_forchisq %>% 
  filter(agegrp=="38-40") %>% 
  ungroup() %>% 
  select(totLB, totNoLB)

clinics18_forchisq_agegrp3
clinics18_forchisq %>% filter(agegrp=="38-40") %>% select(mandate, percLB)
chisq3 <- chisq.test(clinics18_forchisq_agegrp3)
chisq3

clinics18_forchisq_agegrp4 <- clinics18_forchisq %>% 
  filter(agegrp=="41-42") %>% 
  ungroup() %>% 
  select(totLB, totNoLB)

clinics18_forchisq_agegrp4
clinics18_forchisq %>% filter(agegrp=="41-42") %>% select(mandate, percLB)
chisq4 <- chisq.test(clinics18_forchisq_agegrp4)
chisq4 

clinics18_forchisq_agegrp5 <- clinics18_forchisq %>% 
  filter(agegrp=="43+") %>% 
  ungroup() %>% 
  select(totLB, totNoLB)

clinics18_forchisq_agegrp5 
clinics18_forchisq %>% filter(agegrp=="43+") %>% select(mandate, percLB)
chisq5 <- chisq.test(clinics18_forchisq_agegrp5)
chisq5

# ---------------------------------------------------------------------------- #
# --------------- GEE ANALYSIS ACCOUNTING FOR CLUSTERING --------------------- #
# ---------------------------------------------------------------------------- #

check11 <- clinics18 %>% filter(agegrp=="<35" & !!rlang::sym(var_lb) < 0.2)

# variability in live birth rates across clinics, by mandate
ggplot(data=clinics18, aes(x=!!rlang::sym(var_lb), color=as.factor(mandate))) +
  geom_boxplot() +
  facet_wrap(~agegrp) +
  scale_color_manual(labels = c("Clinics in states without comprehensive mandates"
                                , "Clinics in states with comprehensive mandates")
                     , values = c("#756bb1", "#feb24c")) +
  labs(color="", y="", x="Live birth rate per intended retrieval"
       , title="Live birth rate per intended retrieval"
       , subtitle="CDC, 2018") +
  theme(axis.text.y=element_blank()
        , axis.ticks.y=element_blank()
        , legend.position = "top")
  
ggsave("fig1.png", height=8, width=8, units="in")

# % clinics omitted due to low number of retrievals
clinics18_0 %>% group_by(agegrp) %>% summarize(sum(is.na(!!rlang::sym(var_denom)))/n())

clinics18_forgee <- clinics18 %>%
  select(clinic_name, clinic_state, mandate, agegrp, numLB, numNoLB) %>%
  pivot_longer(cols=c(numLB, numNoLB), names_to="LB0", values_to="freq") %>%
  mutate(LB = ifelse(LB0=="numLB", yes=1, no=0)
         , clinic_name = as.factor(clinic_name)) %>%
  filter(!is.na(freq)) %>%
  uncount(freq) %>%
  arrange(clinic_name)

# check gee data numbers/percentages match chisq data numbers/percentages. good, yes.
mosaic::tally(LB ~ mandate + agegrp, data=clinics18_forgee)
mosaic::tally(LB ~ mandate + agegrp, data=clinics18_forgee, format="percent")

# --------------------------- overall ---------------------------------------- #

# run on cluster (needed more memory)
# mod_overall <- gee(LB ~ mandate, data=clinics18_forgee
#                     , family = "binomial", id=clinic_name, corstr = "exchangeable")
# 
# #summary(mod_overall)
# summary(mod_overall)$coeff
# names(mod_overall)
# mod_overall$working.correlation[1,2]
# (1-pnorm(q=abs(summary(mod_overall)$coeff["mandate", "Robust z"])))*2

# --------------------------- by age group ----------------------------------- #

mod1 <- gee(LB ~ mandate, data=filter(clinics18_forgee, agegrp=="<35")
              , family = "binomial", id=clinic_name, corstr = "exchangeable")

mod2 <- gee(LB ~ mandate, data=filter(clinics18_forgee, agegrp=="35-37")
            , family = "binomial", id=clinic_name, corstr = "exchangeable")

summary(mod2)$coeff

mod3 <- gee(LB ~ mandate, data=filter(clinics18_forgee, agegrp=="38-40")
            , family = "binomial", id=clinic_name, corstr = "exchangeable")

summary(mod3)$coeff

mod4 <- gee(LB ~ mandate, data=filter(clinics18_forgee, agegrp=="41-42")
            , family = "binomial", id=clinic_name, corstr = "exchangeable")

summary(mod4)$coeff

mod5 <- gee(LB ~ mandate, data=filter(clinics18_forgee, agegrp=="43+")
            , family = "binomial", id=clinic_name, corstr = "exchangeable")

summary(mod5)$coeff


# ---------------------------------------------------------------------------- #
# ---------------       GATHER INFO INTO TABLE           --------------------- #
# ---------------------------------------------------------------------------- #

p_vals_chisq <- c(chisq_all$p.value, chisq1$p.value, chisq2$p.value
                  , chisq3$p.value, chisq4$p.value, chisq5$p.value)

p_vals_gee <- c(0.009/2 # run on cluster
                , 1-pnorm(q=abs(summary(mod1)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod2)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod3)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod4)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod5)$coeff["mandate", "Robust z"])))*2

wc_gee <- c(mod1$working.correlation[1,2]
            , mod2$working.correlation[1,2]
            , mod3$working.correlation[1,2]
            , mod4$working.correlation[1,2]
            , mod5$working.correlation[1,2])


results <- data.frame(agegrp = c("Overall", "<35", "35-37", "38-40", "41-42", "43+")
                     , pval_cs = p_vals_chisq
                     , pval_gee = p_vals_gee) %>%
  pivot_longer(cols=c("pval_cs", "pval_gee"), names_to="what", values_to="value") %>%
  mutate(value = case_when(value < 0.0001 ~ "< 0.0001"
                           , value < 0.001 ~ as.character(round(value,4))
                           , value < 0.01 ~ as.character(round(value,3))
                           , TRUE ~ as.character(round(value,2)))) %>%
  bind_rows(clinics18_forchisq_overall %>%
              mutate(value=paste0(round((totLB/(totLB+totNoLB))*100,1),"%")
                     , what=ifelse(row_number()==2, yes="Comprehensive mandate"
                                 , no="Noncomprehensive mandate")
                     , agegrp="Overall") %>%
              select(what, agegrp, value)) %>%
  bind_rows(clinics18_forchisq %>% 
              mutate(value=paste0(round(percLB*100,1),"%")
                     , what=ifelse(mandate==1, yes="Comprehensive mandate"
                                 , no="Noncomprehensive mandate")) %>%
              select(agegrp, what, value)) %>%
  pivot_wider(id_cols="what", names_from="agegrp", values_from="value") %>%
  mutate(what = case_when(what == "pval_cs" ~ "P value from Chi-square test"
                          , what == "pval_gee" ~ "P value from GEE"
                          , TRUE ~ what)
         , order = case_when(str_detect(what, "Comp") ~ 1
                            , str_detect(what, "Chi") ~ 3
                            , str_detect(what, "GEE") ~ 4
                            , TRUE ~ 2)) %>%
  arrange(order)


results %>%
  select(-order) %>%
  kableExtra::kable(format = "html")#, caption = "Live birth rate per intended retrieval") #%>%
 # kableExtra::add_header_above(c(" " = 1, "Age group (years)" = 5))
         

