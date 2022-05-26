# for mini-analysis example in letter to editor @ AJOG 
# regarding article https://pubmed.ncbi.nlm.nih.gov/35283088/

library(tidyverse)
library(readxl)
library(gee)

# ---------------------------------------------------------------------------- #
# -------------------------- WRANGLE DATA ----------------------------------- #
# ---------------------------------------------------------------------------- #

mandated_states <- c("Connecticut", "Illinois", "Maryland", "Massachusetts"
                     , "New Jersey", "Rhode Island")

clinics18_0 <- read_excel("data/FINAL-2018-clinic-table-dataset.xlsx"
                        , sheet = "Clinic Table Data Records") %>%
  select(clinic_name = CurrentClinicName1, clinic_state = CurrentClinicState
          , starts_with(c("ND_NumIntentRet", "ND_NumRetrieve", "ND_RetrieveLB")))

# * indicates a number >0 but <5
# for some clinics with >0 but <5 live births, format for LB column is "* / #"
# or for 0 live births is "0 / #"
# or for >0 but <5 live births and >0 but <5 retrievals "* / *" 
check1 <- clinics18_0 %>% count(ND_NumIntentRet5)
check2 <- clinics18_0 %>% count(ND_NumRetrieve5)
check3 <- clinics18_0 %>% count(ND_RetrieveLB5)

clinics18 <- clinics18_0 %>%
  mutate(across(starts_with("ND_NumIntentRet")
                , ~ifelse(.x=="*", yes=2, no=parse_number(.x))
                , .names = "n_{.col}")
        , across(starts_with("ND_NumRetrieve")
                 , ~ifelse(.x=="*", yes=2, no=parse_number(.x))
                 , .names = "n_{.col}")                    
       , across(starts_with("ND_RetrieveLB")
                 , ~case_when(str_detect(.x, "0 / ") ~ 0
                              , str_detect(.x, "//* / ") ~ NA_real_
                              , str_detect(.x, "%") ~ parse_number(.x)/100)
                , .names = "n_{.col}")
        , mandate = ifelse(clinic_state %in% toupper(mandated_states), yes=1, no=0)) 

check4 <- clinics18 %>% count(ND_NumIntentRet5, n_ND_NumIntentRet5)
check5 <-  clinics18 %>% count(ND_NumRetrieve5, n_ND_NumRetrieve5)
check6 <- clinics18 %>% count(ND_RetrieveLB5, n_ND_RetrieveLB5)
check7 <- clinics18 %>% count(mandate, clinic_state)

clinics18_long <- clinics18 %>% 
  select(-starts_with("ND_")) %>%
  pivot_longer(cols=starts_with(c("n_ND_")), names_to = "var0", values_to="value") %>%
  mutate(var = case_when(str_detect(var0, "NumIntentRet") ~ "NumIntentRet"
                         , str_detect(var0, "NumRetrieve") ~ "NumRetrieve"
                         , TRUE ~ "RetrieveLB")
         , agegrp_num = parse_number(var0)
         , agegrp = case_when(agegrp_num==1 ~ "<35"
                              , agegrp_num==2 ~ "35-37"
                              , agegrp_num==3 ~ "38-40"
                              , agegrp_num==4 ~ "41-42"
                              , agegrp_num==5 ~ "43+")) %>%
  pivot_wider(id_cols=c(clinic_name, clinic_state, mandate, agegrp)
              , names_from="var", values_from="value") %>%
  mutate(numLB_intent = round(NumIntentRet*RetrieveLB,0)
         , numLB_ret = round(NumRetrieve*RetrieveLB,0))
         
mosaic::favstats(NumRetrieve ~ agegrp, data=clinics18_long)
mosaic::favstats(NumIntentRet ~ agegrp, data=clinics18_long)
# among 43+ age group, 19% (72/(313+72)) of the clinics missing LB rate
# among 41-42 age group, 33% (139/(277+139)) of the clinics missing LB rate
mosaic::favstats(RetrieveLB ~ agegrp, data=filter(clinics18_long, NumRetrieve>0))

# ---------------------------------------------------------------------------- #
# --------------- CHI-SQ ANALYSIS IGNORING CLUSTERING ------------------------ #
# ---------------------------------------------------------------------------- #

# live birth rates by mandate, ignoring clustering by clinic
clinics18_long %>%
  group_by(agegrp, mandate) %>%
  summarize(totLB_ret = sum(numLB_ret, na.rm=TRUE) 
            , totRet = sum(NumRetrieve, na.rm=TRUE)
            , totLB_intent = sum(numLB_intent, na.rm=TRUE)
            , totIntent = sum(NumIntentRet, na.rm=TRUE)) %>%
  mutate(LBrate_ret = totLB_ret / totRet
         , LBrate_intent = totLB_intent / totIntent)

# live birth rates per retrieval and per intended retrieval are so similar/the same
# just focus on per retrieval

clinics18_forchisq <- clinics18_long %>% 
  group_by(agegrp, mandate) %>% 
  summarize(totLB_ret = sum(numLB_ret, na.rm=TRUE)
            , totRet = sum(NumRetrieve, na.rm=TRUE)) %>%
  mutate(totNoLB_ret = totRet - totLB_ret
         , percLB = totLB_ret/totRet) 

clinics18_forchisq_agegrp1 <- clinics18_forchisq %>% 
  filter(agegrp=="<35") %>% 
  ungroup() %>% 
  select(totLB_ret, totNoLB_ret)

clinics18_forchisq_agegrp1
clinics18_forchisq %>% filter(agegrp=="<35") %>% select(mandate, percLB)
chisq1 <- chisq.test(clinics18_forchisq_agegrp1)
chisq1

clinics18_forchisq_agegrp2 <- clinics18_forchisq %>% 
  filter(agegrp=="35-37") %>% 
  ungroup() %>% 
  select(totLB_ret, totNoLB_ret)

clinics18_forchisq_agegrp2
clinics18_forchisq %>% filter(agegrp=="35-37") %>% select(mandate, percLB)
chisq2 <- chisq.test(clinics18_forchisq_agegrp2)
chisq2

clinics18_forchisq_agegrp3 <- clinics18_forchisq %>% 
  filter(agegrp=="38-40") %>% 
  ungroup() %>% 
  select(totLB_ret, totNoLB_ret)

clinics18_forchisq_agegrp3
clinics18_forchisq %>% filter(agegrp=="38-40") %>% select(mandate, percLB)
chisq3 <- chisq.test(clinics18_forchisq_agegrp3)
chisq3

clinics18_forchisq_agegrp4 <- clinics18_forchisq %>% 
  filter(agegrp=="41-42") %>% 
  ungroup() %>% 
  select(totLB_ret, totNoLB_ret)

clinics18_forchisq_agegrp4
clinics18_forchisq %>% filter(agegrp=="41-42") %>% select(mandate, percLB)
chisq4 <- chisq.test(clinics18_forchisq_agegrp4)
chisq4 

clinics18_forchisq_agegrp5 <- clinics18_forchisq %>% 
  filter(agegrp=="43+") %>% 
  ungroup() %>% 
  select(totLB_ret, totNoLB_ret)

clinics18_forchisq_agegrp5 
clinics18_forchisq %>% filter(agegrp=="43+") %>% select(mandate, percLB)
chisq5 <- chisq.test(clinics18_forchisq_agegrp5)
chisq5

# ---------------------------------------------------------------------------- #
# --------------- GEE ANALYSIS ACCOUNTING FOR CLUSTERING --------------------- #
# ---------------------------------------------------------------------------- #

# variability in live birth rates across clinics, by mandate
ggplot(data=clinics18_long, aes(x=RetrieveLB, color=as.factor(mandate))) +
  geom_boxplot() +
  facet_wrap(~agegrp) +
  scale_color_manual(labels = c("Clinics in states without comprehensive mandates"
                                , "Clinics in states with comprehensive mandates")
                     , values = c("#756bb1", "#feb24c")) +
  labs(color="", y="", x="Live birth rate per retrieval"
       , title="Live birth rate per retrieval"
       , subtitle="CDC, 2018") +
  theme(axis.text.y=element_blank()
        , axis.ticks.y=element_blank()
        , legend.position = "top")
  
ggsave("fig1.png", height=8, width=8, units="in")

clinics18_forgee <- clinics18_long %>%
  mutate(numNoLB_ret = NumRetrieve - numLB_ret) %>%
  select(clinic_name, clinic_state, mandate, agegrp, numLB_ret, numNoLB_ret) %>%
  pivot_longer(cols=c(numLB_ret, numNoLB_ret), names_to="LB_ret0", values_to="freq") %>%
  mutate(LB_ret = ifelse(LB_ret0=="numLB_ret", yes=1, no=0)) %>%
  filter(!is.na(freq)) %>%
  uncount(freq) %>%
  arrange(clinic_name)

mod1 <- gee(LB_ret ~ mandate, data=filter(clinics18_forgee, agegrp=="<35")
                 , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")
summary(mod1)
names(mod1)
mod1$working.correlation[1,2]
#1-pnorm(q=1.96)
1-pnorm(q=abs(summary(mod1)$coeff["mandate", "Robust z"]))

mod2 <- gee(LB_ret ~ mandate, data=filter(clinics18_forgee, agegrp=="35-37")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod2)

mod3 <- gee(LB_ret ~ mandate, data=filter(clinics18_forgee, agegrp=="38-40")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod3)

mod4 <- gee(LB_ret ~ mandate, data=filter(clinics18_forgee, agegrp=="41-42")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod4)

mod5 <- gee(LB_ret ~ mandate, data=filter(clinics18_forgee, agegrp=="43+")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod5)


# ---------------------------------------------------------------------------- #
# ---------------       GATHER INFO INTO TABLE           --------------------- #
# ---------------------------------------------------------------------------- #

p_vals_chisq <- c(chisq1$p.value, chisq2$p.value, chisq3$p.value, chisq4$p.value
                  , chisq5$p.value)

p_vals_gee <- c(1-pnorm(q=abs(summary(mod1)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod2)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod3)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod4)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod5)$coeff["mandate", "Robust z"])))

wc_gee <- c(mod1$working.correlation[1,2]
            , mod2$working.correlation[1,2]
            , mod3$working.correlation[1,2]
            , mod4$working.correlation[1,2]
            , mod5$working.correlation[1,2])


results <- data.frame(agegrp = c("<35", "35-37", "38-40", "41-42", "43+")
                     , pval_cs = p_vals_chisq
                     , pval_gee = p_vals_gee) %>%
  pivot_longer(cols=c("pval_cs", "pval_gee"), names_to="what", values_to="value") %>%
  mutate(value = case_when(value < 0.0001 ~ "< 0.0001"
                           , value < 0.001 ~ as.character(round(value,4))
                           , value < 0.01 ~ as.character(round(value,3))
                           , TRUE ~ as.character(round(value,2)))) %>%
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
  rename(``=what) %>%
  kableExtra::kable(format = "html", caption = "Live birth rate per retrieval") %>%
  kableExtra::add_header_above(c(" " = 1, "Age group (years)" = 5))
         

