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
          , starts_with(c("ND_NumIntentRet", "ND_IntentRetLB", "ND_NumRetrieve", "ND_RetrieveLB")))

# * indicates a number >0 but <5
# for some clinics with >0 but <5 live births, format for LB column is "* / #"
# or for 0 live births is "0 / #"
# or for >0 but <5 live births and >0 but <5 retrievals "* / *" 
check1 <- clinics18_0 %>% count(ND_NumIntentRet5)
check2 <- clinics18_0 %>% count(ND_NumRetrieve5)
check3 <- clinics18_0 %>% count(ND_RetrieveLB5)

clinics18 <- clinics18_0 %>%
  mutate(across(starts_with(c("ND_NumIntentRet", "ND_NumRetrieve"))
                , ~ifelse(.x=="*", yes=NA_real_, no=parse_number(.x))
                , .names = "n_{.col}")
       , across(starts_with(c("ND_IntentRetLB", "ND_RetrieveLB"))
                # had originally set 0 / * to 0% LB, but will be excluded from 
                # analysis anyways because missing denominator so set to missing here
                 , ~case_when(str_detect(.x, "0 / ") ~ NA_real_
                              , str_detect(.x, "//* / ") ~ NA_real_
                              , str_detect(.x, "%") ~ parse_number(.x)/100)
                , .names = "n_{.col}")
        , mandate = ifelse(clinic_state %in% toupper(mandated_states), yes=1, no=0)) 

check4 <- clinics18 %>% count(ND_NumIntentRet5, n_ND_NumIntentRet5)
check5 <-  clinics18 %>% count(ND_NumRetrieve5, n_ND_NumRetrieve5)
check6 <- clinics18 %>% count(ND_RetrieveLB5, n_ND_RetrieveLB5)
check7 <- clinics18 %>% count(mandate, clinic_state)

clinics18_long0 <- clinics18 %>% 
  select(-starts_with("ND_")) %>%
  pivot_longer(cols=starts_with(c("n_ND_")), names_to = "var0", values_to="value") %>%
  mutate(var = str_replace_all(var0, "n_ND_|[:digit:]" ,"")
         , agegrp_num = parse_number(var0)
         , agegrp = case_when(agegrp_num==1 ~ "<35"
                              , agegrp_num==2 ~ "35-37"
                              , agegrp_num==3 ~ "38-40"
                              , agegrp_num==4 ~ "41-42"
                              , agegrp_num==5 ~ "43+")) %>%
  pivot_wider(id_cols=c(clinic_name, clinic_state, mandate, agegrp)
              , names_from="var", values_from="value") %>%
  mutate(numLB_intent = round(NumIntentRet*IntentRetLB,0)
         , numLB_ret = round(NumRetrieve*RetrieveLB,0)
         , numNoLB_intent = NumIntentRet - numLB_intent) 

clinics18_long <- clinics18_long0 %>%
  # exclude clinics where % LB among intended retrievals is missing
  filter(!is.na(IntentRetLB))

check8 <- clinics18_long %>% filter(is.na(NumIntentRet))         
check9 <- clinics18_long %>% filter(is.na(IntentRetLB))

mosaic::favstats(NumRetrieve ~ agegrp, data=clinics18_long)
mosaic::favstats(NumIntentRet ~ agegrp, data=clinics18_long)
# among 43+ age group, ~58% (162/(162+116)) of the clinics missing LB rate
# among 41-42 age group, ~50% (179/(179+175)) of the clinics missing LB rate
# all of these clinics missing LB rate had > 1 and < 5 retrievals - OR - 
# 0 live births and < 20 retrievals
mosaic::favstats(IntentRetLB ~ agegrp, data=filter(clinics18_long0, NumIntentRet>0))

# ---------------------------------------------------------------------------- #
# --------------- CHI-SQ ANALYSIS IGNORING CLUSTERING ------------------------ #
# ---------------------------------------------------------------------------- #

# live birth rates per intended retrieval are more similar to numbers in article
# just focus on per intended retrieval
clinics18_long %>%
  group_by(agegrp, mandate) %>%
  summarize(totLB_ret = sum(numLB_ret, na.rm=TRUE) 
            , totRet = sum(NumRetrieve, na.rm=TRUE)
            , totLB_intent = sum(numLB_intent, na.rm=TRUE)
            , totIntent = sum(NumIntentRet, na.rm=TRUE)) %>%
  mutate(LBrate_ret = totLB_ret / totRet
         , LBrate_intent = totLB_intent / totIntent)

clinics18_forchisq <- clinics18_long %>% 
  group_by(agegrp, mandate) %>% 
  summarize(totLB_intent = sum(numLB_intent, na.rm=TRUE)
            , totNoLB_intent = sum(numNoLB_intent, na.rm=TRUE)
            , totIntentRet = sum(NumIntentRet, na.rm=TRUE)) %>%
  mutate(totNoLB_intent2 = totIntentRet - totLB_intent
         , percLB_intent = totLB_intent/totIntentRet) 

clinics18_forchisq_agegrp1 <- clinics18_forchisq %>% 
  filter(agegrp=="<35") %>% 
  ungroup() %>% 
  select(totLB_intent, totNoLB_intent)

clinics18_forchisq_agegrp1
clinics18_forchisq %>% filter(agegrp=="<35") %>% select(mandate, percLB_intent)
chisq1 <- chisq.test(clinics18_forchisq_agegrp1)
chisq1

clinics18_forchisq_agegrp2 <- clinics18_forchisq %>% 
  filter(agegrp=="35-37") %>% 
  ungroup() %>% 
  select(totLB_intent, totNoLB_intent)

clinics18_forchisq_agegrp2
clinics18_forchisq %>% filter(agegrp=="35-37") %>% select(mandate, percLB_intent)
chisq2 <- chisq.test(clinics18_forchisq_agegrp2)
chisq2

clinics18_forchisq_agegrp3 <- clinics18_forchisq %>% 
  filter(agegrp=="38-40") %>% 
  ungroup() %>% 
  select(totLB_intent, totNoLB_intent)

clinics18_forchisq_agegrp3
clinics18_forchisq %>% filter(agegrp=="38-40") %>% select(mandate, percLB_intent)
chisq3 <- chisq.test(clinics18_forchisq_agegrp3)
chisq3

clinics18_forchisq_agegrp4 <- clinics18_forchisq %>% 
  filter(agegrp=="41-42") %>% 
  ungroup() %>% 
  select(totLB_intent, totNoLB_intent)

clinics18_forchisq_agegrp4
clinics18_forchisq %>% filter(agegrp=="41-42") %>% select(mandate, percLB_intent)
chisq4 <- chisq.test(clinics18_forchisq_agegrp4)
chisq4 

clinics18_forchisq_agegrp5 <- clinics18_forchisq %>% 
  filter(agegrp=="43+") %>% 
  ungroup() %>% 
  select(totLB_intent, totNoLB_intent)

clinics18_forchisq_agegrp5 
clinics18_forchisq %>% filter(agegrp=="43+") %>% select(mandate, percLB_intent)
chisq5 <- chisq.test(clinics18_forchisq_agegrp5)
chisq5

# ---------------------------------------------------------------------------- #
# --------------- GEE ANALYSIS ACCOUNTING FOR CLUSTERING --------------------- #
# ---------------------------------------------------------------------------- #

check10 <- clinics18_long %>% filter(agegrp=="<35" & IntentRetLB < 0.2)

# variability in live birth rates across clinics, by mandate
ggplot(data=clinics18_long, aes(x=IntentRetLB, color=as.factor(mandate))) +
  geom_boxplot() +
  facet_wrap(~agegrp) +
  scale_color_manual(labels = c("Clinics in states without comprehensive mandates"
                                , "Clinics in states with comprehensive mandates")
                     , values = c("#756bb1", "#feb24c")) +
  labs(color="", y="", x="Live birth rate per retrieval"
       , title="Live birth rate per intended retrieval"
       , subtitle="CDC, 2018") +
  theme(axis.text.y=element_blank()
        , axis.ticks.y=element_blank()
        , legend.position = "top")
  
ggsave("fig1.png", height=8, width=8, units="in")

# % of clinics omitted due to low number of retrievals
clinics18_long %>% group_by(agegrp) %>% summarize(sum(is.na(IntentRetLB))/n())

clinics18_forgee <- clinics18_long %>%
  mutate(numNoLB_intent = NumIntentRet - numLB_intent) %>%
  select(clinic_name, clinic_state, mandate, agegrp, numLB_intent, numNoLB_intent) %>%
  pivot_longer(cols=c(numLB_intent, numNoLB_intent), names_to="LB_intent0", values_to="freq") %>%
  mutate(LB_intent = ifelse(LB_intent0=="numLB_intent", yes=1, no=0)) %>%
  filter(!is.na(freq)) %>%
  uncount(freq) %>%
  arrange(clinic_name)

# check gee data numbers/percentages match chisq data numbers/percentages. good, yes.
mosaic::tally(LB_intent ~ mandate + agegrp, data=clinics18_forgee)
mosaic::tally(LB_intent ~ mandate + agegrp, data=clinics18_forgee, format="percent")

mod1 <- gee(LB_intent ~ mandate, data=filter(clinics18_forgee, agegrp=="<35")
                 , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")
summary(mod1)
summary(mod1)$coeff
names(mod1)
mod1$working.correlation[1,2]
#1-pnorm(q=1.96)
(1-pnorm(q=abs(summary(mod1)$coeff["mandate", "Robust z"])))*2

mod2 <- gee(LB_intent ~ mandate, data=filter(clinics18_forgee, agegrp=="35-37")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod2)$coeff

mod3 <- gee(LB_intent ~ mandate, data=filter(clinics18_forgee, agegrp=="38-40")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod3)$coeff

mod4 <- gee(LB_intent ~ mandate, data=filter(clinics18_forgee, agegrp=="41-42")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod4)$coeff

mod5 <- gee(LB_intent ~ mandate, data=filter(clinics18_forgee, agegrp=="43+")
            , family = "binomial", id=as.factor(clinic_name), corstr = "exchangeable")

summary(mod5)$coeff


# ---------------------------------------------------------------------------- #
# ---------------       GATHER INFO INTO TABLE           --------------------- #
# ---------------------------------------------------------------------------- #

p_vals_chisq <- c(chisq1$p.value, chisq2$p.value, chisq3$p.value, chisq4$p.value
                  , chisq5$p.value)

p_vals_gee <- c(1-pnorm(q=abs(summary(mod1)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod2)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod3)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod4)$coeff["mandate", "Robust z"]))
                , 1-pnorm(q=abs(summary(mod5)$coeff["mandate", "Robust z"])))*2

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
              mutate(value=paste0(round(percLB_intent*100,1),"%")
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
  kableExtra::kable(format = "html", caption = "Live birth rate per intended retrieval") #%>%
 # kableExtra::add_header_above(c(" " = 1, "Age group (years)" = 5))
         

