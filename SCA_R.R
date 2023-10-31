#Load packages
library(gtools)
library(tidyverse)
library(readxl)
library(psych)
library(corrplot)
library(RColorBrewer)
library(corrtable)
library(car)
library(lmtest)
library(sandwich)
library(MASS)
library(flextable)
library(officer)
library(reshape2)
library(data.table)

#Load SCA data

# DATA WRANGLING ---------------------------------------------------------------
#Create social inference scores
SCAdata <- SCAdata %>%
  mutate(sincere = do_2_1_cor + think_2_1_cor + feel_2_1_cor + say_2_1_cor +
           do_2_3_cor + think_2_3_cor + feel_2_3_cor + say_2_3_cor +
           do_2_6_cor + think_2_6_cor + feel_2_6_cor + say_2_6_cor +
           do_2_8_cor + think_2_8_cor + feel_2_8_cor + say_2_8_cor)


SCAdata <- SCAdata %>%
  mutate(sarcasm = do_2_2_cor + think_2_2_cor + feel_2_2_cor + say_2_2_cor +
           do_2_4_cor + think_2_4_cor + feel_2_4_cor + say_2_4_cor +
           do_2_5_cor + think_2_5_cor + feel_2_5_cor + say_2_5_cor +
           do_2_7_cor + think_2_7_cor + feel_2_7_cor + say_2_7_cor +
           do_2_9_cor + think_2_9_cor + feel_2_9_cor + say_2_9_cor)

SCAdata <- SCAdata %>%
  mutate(lies = do_3_1_cor + think_3_1_cor + feel_3_1_cor + say_3_1_cor +
           do_3_3_cor + think_3_3_cor + feel_3_3_cor + say_3_3_cor +
           do_3_6_cor + think_3_6_cor + feel_3_6_cor + say_3_6_cor +
           do_3_7_cor + think_3_7_cor + feel_3_7_cor + say_3_7_cor)

SCAdata <- SCAdata %>%
  mutate(Esarcasm = do_3_2_cor + think_3_2_cor + feel_3_2_cor + say_3_2_cor +
           do_3_4_cor + think_3_4_cor + feel_3_4_cor + say_3_4_cor +
           do_3_5_cor + think_3_5_cor + feel_3_5_cor + say_3_5_cor +
           do_3_8_cor + think_3_8_cor + feel_3_8_cor + say_3_8_cor +
           do_3_9_cor + think_3_9_cor + feel_3_9_cor + say_3_9_cor)


#Total education
SCAdata <- SCAdata %>%
  mutate(education = primary_schooling + secondary_schooling + tertiary_edu)

#Total do
SCAdata <- SCAdata %>%
  mutate(do = tot_do_2_cal + tot_do_3_cal)

#Total think
SCAdata <- SCAdata %>%
  mutate(think = tot_think_2_calc + tot_think_3_calc)

#Total feel
SCAdata <- SCAdata %>%
  mutate(feel = tot_feel_2_calc +tot_feel_3_calc)

#Total hearing
SCAdata <- SCAdata %>%
  mutate(hearing = hhie_total_calc_a + hhie_total_calc_b)

#Total loneliness 
SCAdata <- SCAdata %>%
  mutate(loneliness = lsns6_total_a + lsns6_total_b)

#Total depression
SCAdata <- SCAdata %>%
  mutate(depression = gds_total_a + gds_total_b)

#Total empathy
SCAdata <- SCAdata %>%
  mutate(empathy = eq_total_a + eq_total_b)

#Rename id
SCAdata <- SCAdata %>%
  rename(id = record_id)

#Rename happy
SCAdata <- SCAdata %>%
  rename(happy = t_1_hap)

#Rename neutral
SCAdata <- SCAdata %>%
  rename(neutral = t_1_neu)

#Rename sad
SCAdata <- SCAdata %>%
  rename(sad = t_1_sad)

#Rename angry
SCAdata <- SCAdata %>%
  rename(angry = t_1_ang)

#Rename anxious
SCAdata <- SCAdata %>%
  rename(anxious = t_1_anx)

#Rename revolted
SCAdata <- SCAdata %>%
  rename(revolted = t_1_rev)

#Rename RME 
SCAdata <- SCAdata %>%
  rename(RME = rme_tot)

#Rename cognition
SCAdata <- SCAdata %>%
  rename(cognition = ace_total)

#Rename testing mode
SCAdata <- SCAdata %>%
  rename(testingmode = testing_covid)

#Create total emotion
SCAdata <- SCAdata %>%
  mutate(emotion = happy + neutral + sad + angry + anxious + revolted)

#Select newly created variables
df <- SCAdata %>%
  dplyr::select(c("id", "age", "sex", "education", "happy", "neutral", "sad", "angry", "emotion",
                  "anxious", "revolted", "do", "think", "feel", "RME", "cognition", 
                  "hearing", "loneliness", "depression", "empathy", "sarcasm", "sincere", "lies", "Esarcasm", "testingmode"))

#Change sex from numeric to categorical
df$sex <- as.factor(df$sex)

#Change testingmode from numerical to categorical 
df$testingmode <- as.factor(df$testingmode)


# DESCRIPTIVES -----------------------------------------------------

#Total sample descriptives
psych::describe(df)

#total proportion of males to females
prop.table(table(df$sex))
prop.table(table(df$testingmode))

#Create age groups
agebreaks <- c(50, 60, 70, 80, 90)
agelabels <- c("50-59", "60-69", "70-79", "80-89")

df <- df %>%
  mutate(agegroup = cut(age, breaks = agebreaks, labels = agelabels, right = FALSE))

#Create dataframes
socialvariables <- c("happy", "neutral", "sad", "angry","anxious", "revolted", "emotion", "think", "do", "feel", "RME", "sarcasm", 
                     "sincere", "lies", "Esarcasm", "empathy", "education", "hearing", "loneliness", "depression", "cognition")

socialdescriptives  <- df %>%
  group_by(agegroup) %>%
  summarize_at(
    vars(all_of(socialvariables)),
    list(
      mean = ~ mean(., na.rm = TRUE),
      #median = ~ median(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      n = ~ sum(!is.na(.))
    )
  )


socialdescriptives <- socialdescriptives %>%
  pivot_longer(
    cols = -agegroup,
    names_to = c("variable", ".value"),
    names_sep = "_"
  )

#Calculate sex proportions
sex <- df %>%
  group_by(agegroup) %>%
  summarize(
    sex_count = table(sex)
  )

#Calculate testingmode proportions
testingmode <- df %>%
  group_by(agegroup) %>%
  summarize(
    sex_count = table(testingmode)
  )


# CORRELATIONS ------------------------------------------------------------

#Correlation df
dfCor <- data.frame(df$happy, df$neutral, df$sad, df$angry, df$anxious, df$revolted, df$emotion, 
                    df$think, df$do, df$feel, df$RME, df$sarcasm, df$sincere, df$lies, df$Esarcasm,  df$empathy)
#Spearman correlation
spearman <- corr.test(dfCor, method = "spearman", use = "complete.obs")$r
spearmanp <- corr.test(dfCor, method = "spearman", use = "complete.obs")$p

#Rename columns and rows for heatmap figure
colnames(spearman)<- c("Happy", "Neutral", "Sad", "Angry", "Anxious", "Revolted", "Emotion total",  
                       "First-order cognitive", "Second-order cognitive", "Affective (TASIT)", "Affective (RME)",
                       "Sarcasm", "Sincere", "Lies", "Enriched sarcasm", "Emotional empathy") 
rownames(spearman)<- c("Happy", "Neutral", "Sad", "Angry", "Anxious", "Revolted", "Emotion total",  
                       "First-order cognitive", "Second-order cognitive", "Affective (TASIT)", "Affective (RME)",
                       "Sarcasm", "Sincere", "Lies", "Enriched sarcasm", "Emotional empathy") 
colnames(spearmanp)<- c("Happy", "Neutral", "Sad", "Angry", "Anxious", "Revolted", "Emotion total",  
                        "First-order cognitive", "Second-order cognitive", "Affective (TASIT)", "Affective (RME)",
                        "Sarcasm", "Sincere", "Lies", "Enriched sarcasm", "Emotional empathy") 
rownames(spearmanp)<- c("Happy", "Neutral", "Sad", "Angry", "Anxious", "Revolted", "Emotion total",  
                        "First-order cognitive", "Second-order cognitive", "Affective (TASIT)", "Affective (RME)",
                        "Sarcasm", "Sincere", "Lies", "Enriched sarcasm", "Emotional empathy") 

#Create heatmap figure
tiff(filename = "correlation.tiff", compression = "lzw", width = 7, height = 6, units = "in", res = 300)

corrplot(spearman, col=brewer.pal(n=8, name="RdBu"), cl.lim = c(-1, 1), tl.col = "black", tl.srt = 60, cl.ratio = 0.4, 
         p.mat = spearmanp, sig.level = 0.017, insig = "pch", pch.col = "gray49", pch.cex = 2, 
         method = "color", tl.cex = 1.2, cl.cex = 1.1, addgrid.col = "black", diag = FALSE, type = 'lower')
dev.off()


#Create excel table
save_correlation_matrix(dfCor, type = "spearman", digits = 2, use = 'lower', replace_diagonal = TRUE,
                        filename = "corrtable.csv")


# MULTIPLE REGRESSIONS ----------------------------------------------------

#Happy
multihappy <- lm(happy ~ age + sex + education + cognition + hearing + 
                   loneliness + depression, data = df)
summary(multihappy)

#Neutral 
multineutral <- lm(neutral  ~ age + sex + education + cognition + hearing + 
                     loneliness + depression, data = df)
summary(multineutral)

#Sad
multisad <- lm(sad  ~ age + sex + education + cognition + hearing + 
                 loneliness + depression, data = df)
summary(multisad)

#Angry
multiangry <- lm(angry  ~ age + sex + education + cognition + hearing + 
                   loneliness + depression, data = df)
summary(multiangry)

#Anxious
multianxious <- lm(anxious  ~ age + sex + education + cognition + hearing + 
                     loneliness + depression, data = df)
summary(multianxious)

#Revolted
multirevolted <- lm(revolted  ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = df)
summary(multirevolted)

#emotion
multiemotion <- lm(emotion  ~ age + sex + education + cognition + hearing + 
                     loneliness + depression, data = df)
summary(multiemotion)

#Do
multido <- lm(do  ~ age + sex + education + cognition + hearing + 
                loneliness + depression, data = df)
summary(multido)

#think
multithink <- lm(think  ~ age + sex + education + cognition + hearing + 
                   loneliness + depression, data = df)
summary(multithink)

#feel
multifeel <- lm(feel  ~ age + sex + education + cognition + hearing + 
                  loneliness + depression, data = df)
summary(multifeel)

#sarcasm
multisarcasm <- lm(sarcasm ~ age + sex + education + cognition + hearing + 
                     loneliness + depression, data = df)
summary(multisarcasm)

#sincere
multisincere <- lm(sincere ~ age + sex + education + cognition + hearing + 
                     loneliness + depression, data = df)
summary(multisincere)

#lies
multilies <- lm(lies ~ age + sex + education + cognition + hearing + 
                  loneliness + depression, data = df)
summary(multilies)

#Esarcasm
multiEsarcasm <- lm(Esarcasm ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = df)
summary(multiEsarcasm)


#RME
multiRME <- lm(RME ~  age + sex + education + cognition + hearing + 
                 loneliness + depression, data = df)
summary(multiRME)

#empathy
multiempathy <- lm(empathy  ~ age + sex + education + cognition + hearing + 
                     loneliness + depression, data = df)
summary(multiempathy)


# ASSUMPTION CHECKS -------------------------------------------------------

#Linearity 
#happy
plot(multihappy, 1)

#neutral
plot(multineutral, 1)

#sad
plot(multisad, 1)

#angry
plot(multiangry, 1)

#anxious
plot(multianxious, 1)

#revolted
plot(multirevolted, 1)

#emotion
plot(multiemotion, 1)

#do
plot(multido, 1)

#think
plot(multithink, 1)

#feel
plot(multifeel, 1)

#sarcasm
plot(multisarcasm, 1)

#sincere
plot(multisincere, 1)

#lies
plot(multilies, 1)

#Esarcasm
plot(multiEsarcasm, 1)

#RME
plot(multiRME, 1)

#empathy
plot(multiempathy, 1)

#Normality
#happy
sresidmultihappy <- studres(multihappy)
shapiro.test(sresidmultihappy) #Not normal

#neutral
sresidmultineutral <- studres(multineutral)
shapiro.test(sresidmultineutral) #Not normal

#sad
sresidmultisad <- studres(multisad)
shapiro.test(sresidmultisad) #Not normal

#angry
sresidmultiangry <- studres(multiangry)
shapiro.test(sresidmultiangry) #Not normal

#anxious
sresidmultianxious <- studres(multianxious)
shapiro.test(sresidmultianxious) #Not normal

#revolted
sresidmultirevolted <- studres(multirevolted)
shapiro.test(sresidmultirevolted) #Not normal

#emotion
sresidmultiemotion <- studres(multiemotion)
shapiro.test(sresidmultiemotion) #Not normal

#do
sresidmultido <- studres(multido)
shapiro.test(sresidmultido)

#think
sresidmultithink <- studres(multithink)
shapiro.test(sresidmultithink) #Not normal

#feel
sresidmultifeel <- studres(multifeel)
shapiro.test(sresidmultifeel)

#sarcasm
sresidmultisarcasm <- studres(multisarcasm)
shapiro.test(sresidmultisarcasm) #Not normal

#sincere
sresidmultisincere <- studres(multisincere)
shapiro.test(sresidmultisincere) #Not normal

#lies
sresidmultilies <- studres(multilies)
shapiro.test(sresidmultilies) 

#Esarcasm
sresidmultiEsarcasm <- studres(multiEsarcasm)
shapiro.test(sresidmultiEsarcasm) #Not normal

#RME
sresidmultiRME <- studres(multiRME)
shapiro.test(sresidmultiRME) #Not normal

#empathy
sresidmultiempathy <- studres(multiempathy)
shapiro.test(sresidmultiempathy)


#Homoscedasticity
#happy
ncvTest(multihappy)

#neutral
ncvTest(multineutral)

#sad
ncvTest(multisad) #heteroscedastic

#angry
ncvTest(multiangry) #heteroscedastic

#anxious
ncvTest(multianxious) #heteroscedastic

#revolted
ncvTest(multirevolted)

#emotion
ncvTest(multiemotion)

#do
ncvTest(multido) #heteroscedastic

#think
ncvTest(multithink)

#feel
ncvTest(multifeel)

#sarcasm
ncvTest(multisarcasm)

#sincere
ncvTest(multisincere)

#lies
ncvTest(multilies)

#Esarcasm
ncvTest(multiEsarcasm)

#RME
ncvTest(multiRME)

#empathy
ncvTest(multiempathy)

# ROBUST REGRESSIONS ------------------------------------------------------

#happy
happyEst <- coeftest(multihappy, vcov. = vcovHC(multihappy, type = "HC1"))
happyCI <- coefci(multihappy, vcov. = vcovHC(multihappy, type = "HC1"))

#neutral
neutralEst <- coeftest(multineutral, vcov. = vcovHC(multineutral, type = "HC1"))
neutralCI <- coefci(multineutral, vcov. = vcovHC(multineutral, type = "HC1"))

#sad
sadEst <- coeftest(multisad, vcov. = vcovHC(multisad, type = "HC1"))
sadCI <- coefci(multisad, vcov. = vcovHC(multisad, type = "HC1"))

#angry
angryEst <- coeftest(multiangry, vcov. = vcovHC(multiangry, type = "HC1"))
angryCI <- coefci(multiangry, vcov. = vcovHC(multiangry, type = "HC1"))

#anxious
anxiousEst <- coeftest(multianxious, vcov. = vcovHC(multianxious, type = "HC1"))
anxiousCI <- coefci(multianxious, vcov. = vcovHC(multianxious, type = "HC1"))

#revolted
revoltedEst <- coeftest(multirevolted, vcov. = vcovHC(multirevolted, type = "HC1"))
revoltedCI <- coefci(multirevolted, vcov. = vcovHC(multirevolted, type = "HC1"))

#emotion
emotionEst <- coeftest(multiemotion, vcov. = vcovHC(multiemotion, type = "HC1"))
emotionCI <- coefci(multiemotion, vcov. = vcovHC(multiemotion, type = "HC1"))

#do
doEst <- coeftest(multido, vcov. = vcovHC(multido, type = "HC1"))
doCI <- coefci(multido, vcov. = vcovHC(multido, type = "HC1"))

#think
thinkEst <- coeftest(multithink, vcov. = vcovHC(multithink, type = "HC1"))
thinkCI <- coefci(multithink, vcov. = vcovHC(multithink, type = "HC1"))

#feel
feelEst <- coeftest(multifeel, vcov. = vcovHC(multifeel, type = "HC1"))
feelCI <- coefci(multifeel, vcov. = vcovHC(multifeel, type = "HC1"))

#sarcasm
sarcasmEst <- coeftest(multisarcasm, vcov. = vcovHC(multisarcasm, type = "HC1"))
sarcasmCI <- coefci(multisarcasm, vcov. = vcovHC(multisarcasm, type = "HC1"))

#sincere
sincereEst <- coeftest(multisincere, vcov. = vcovHC(multisincere, type = "HC1"))
sincereCI <- coefci(multisincere, vcov. = vcovHC(multisincere, type = "HC1"))

#lies
liesEst <- coeftest(multilies, vcov. = vcovHC(multilies, type = "HC1"))
liesCI <- coefci(multilies, vcov. = vcovHC(multilies, type = "HC1"))

#Esarcasm
EsarcasmEst <- coeftest(multiEsarcasm, vcov. = vcovHC(multiEsarcasm, type = "HC1"))
EsarcasmCI <- coefci(multiEsarcasm, vcov. = vcovHC(multiEsarcasm, type = "HC1"))

#RME
RMEEst <- coeftest(multiRME, vcov. = vcovHC(multiRME, type = "HC1"))
RMECI <- coefci(multiRME, vcov. = vcovHC(multiRME, type = "HC1"))

#empathy
empathyEst <- coeftest(multiempathy, vcov. = vcovHC(multiempathy, type = "HC1"))
empathyCI <- coefci(multiempathy, vcov. = vcovHC(multiempathy, type = "HC1"))

# STANDARDISED MODELS ------------------------------------------------------

#Standardise dataset
dfselect <- df %>%
  dplyr::select(-id,-sex, -testingmode, -agegroup)

dfselect1 <- df %>%
  dplyr::select(id,sex,testingmode)

dfselect <- lapply(dfselect, scale)

dfstandard1 <- cbind(dfselect, dfselect1)

#Rerun regression models using the standardised dataset 
#happy
multihappyS <- lm(happy ~ age + sex + education + cognition + hearing + 
                    loneliness + depression, data = dfstandard1)
happyBS <- coeftest(multihappyS, vcov. = vcovHC(multihappyS, type = "HC1"))

#neutral
multineutralS <- lm(neutral ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = dfstandard1)
neutralBS <- coeftest(multineutralS, vcov. = vcovHC(multineutralS, type = "HC1"))

#sad
multisadS <- lm(sad ~ age + sex + education + cognition + hearing + 
                  loneliness + depression, data = dfstandard1)
sadBS <- coeftest(multisadS, vcov. = vcovHC(multisadS, type = "HC1"))

#angry
multiangryS <- lm(angry ~ age + sex + education + cognition + hearing + 
                    loneliness + depression, data = dfstandard1)
angryBS <- coeftest(multiangryS, vcov. = vcovHC(multiangryS, type = "HC1"))

#anxious
multianxiousS <- lm(anxious ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = dfstandard1)
anxiousBS <- coeftest(multianxiousS, vcov. = vcovHC(multianxiousS, type = "HC1"))

#revolted
multirevoltedS <- lm(revolted ~ age + sex + education + cognition + hearing + 
                       loneliness + depression, data = dfstandard1)
revoltedBS <- coeftest(multirevoltedS, vcov. = vcovHC(multirevoltedS, type = "HC1"))

#emotion
multiemotionS <- lm(emotion ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = dfstandard1)
emotionBS <- coeftest(multiemotionS, vcov. = vcovHC(multiemotionS, type = "HC1"))


#do
multidoS <- lm(do ~ age + sex + education + cognition + hearing + 
                 loneliness + depression, data = dfstandard1)
doBS <- coeftest(multidoS, vcov. = vcovHC(multidoS, type = "HC1"))

#think
multithinkS <- lm(think ~ age + sex + education + cognition + hearing + 
                    loneliness + depression, data = dfstandard1)
thinkBS <- coeftest(multithinkS, vcov. = vcovHC(multithinkS, type = "HC1"))

#feel
multifeelS <- lm(feel ~ age + sex + education + cognition + hearing + 
                   loneliness + depression, data = dfstandard1)
feelBS <- coeftest(multifeelS, vcov. = vcovHC(multifeelS, type = "HC1"))

#sarcasm
multisarcasmS <- lm(sarcasm ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = dfstandard1)
sarcasmBS <- coeftest(multisarcasmS, vcov. = vcovHC(multisarcasmS, type = "HC1"))

#sincere
multisincereS <- lm(sincere ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = dfstandard1)
sincereBS <- coeftest(multisincereS, vcov. = vcovHC(multisincereS, type = "HC1"))

#lies
multiliesS <- lm(lies ~ age + sex + education + cognition + hearing + 
                   loneliness + depression, data = dfstandard1)
liesBS <- coeftest(multiliesS, vcov. = vcovHC(multiliesS, type = "HC1"))

#Esarcasm
multiEsarcasmS <- lm(Esarcasm ~ age + sex + education + cognition + hearing + 
                       loneliness + depression, data = dfstandard1)
EsarcasmBS <- coeftest(multiEsarcasmS, vcov. = vcovHC(multiEsarcasmS, type = "HC1"))


#RME
multiRMES <- lm(RME ~ age + sex + education + cognition + hearing + 
                  loneliness + depression, data = dfstandard1)
RMEBS <- coeftest(multiRMES, vcov. = vcovHC(multiRMES, type = "HC1"))

#empathy
multiempathyS <- lm(empathy ~ age + sex + education + cognition + hearing + 
                      loneliness + depression, data = dfstandard1)
empathyBS <- coeftest(multiempathyS, vcov. = vcovHC(multiempathyS, type = "HC1"))

# MODEL TABLE AND FIGURE -----------------------------------------------------
#First need to create separate data frames for each regression model 
#happy
happyregression <- data.frame(Outcome = c("Happy"),
                              Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                              R2 = c(summary(multihappy)$r.squared),
                              B = c(happyEst[2:8,1]),
                              BS = c(happyBS[2:8,1]), 
                              CILower = c(happyCI[2:8,1]),
                              CIUpper = c(happyCI[2:8,2]),
                              p = c(happyEst[2:8,4]))

#neutral
neutralregression <- data.frame(Outcome = c("Neutral"),
                                Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                R2 = c(summary(multineutral)$r.squared),
                                B = c(neutralEst[2:8,1]),
                                BS = c(neutralBS[2:8,1]), 
                                CILower = c(neutralCI[2:8,1]),
                                CIUpper = c(neutralCI[2:8,2]),
                                p = c(neutralEst[2:8,4]))
#sad
sadregression <- data.frame(Outcome = c("Sad"),
                            Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                            R2 = c(summary(multisad)$r.squared),
                            B = c(sadEst[2:8,1]),
                            BS = c(sadBS[2:8,1]), 
                            CILower = c(sadCI[2:8,1]),
                            CIUpper = c(sadCI[2:8,2]),
                            p = c(sadEst[2:8,4]))
#angry
angryregression <- data.frame(Outcome = c("Angry"),
                              Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                              R2 = c(summary(multiangry)$r.squared),
                              B = c(angryEst[2:8,1]),
                              BS = c(angryBS[2:8,1]), 
                              CILower = c(angryCI[2:8,1]),
                              CIUpper = c(angryCI[2:8,2]),
                              p = c(angryEst[2:8,4]))
#anxious
anxiousregression <- data.frame(Outcome = c("Anxious"),
                                Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                R2 = c(summary(multianxious)$r.squared),
                                B = c(anxiousEst[2:8,1]),
                                BS = c(anxiousBS[2:8,1]), 
                                CILower = c(anxiousCI[2:8,1]),
                                CIUpper = c(anxiousCI[2:8,2]),
                                p = c(anxiousEst[2:8,4]))
#revolted
revoltedregression <- data.frame(Outcome = c("Revolted"),
                                 Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                 R2 = c(summary(multirevolted)$r.squared),
                                 B = c(revoltedEst[2:8,1]),
                                 BS = c(revoltedBS[2:8,1]), 
                                 CILower = c(revoltedCI[2:8,1]),
                                 CIUpper = c(revoltedCI[2:8,2]),
                                 p = c(revoltedEst[2:8,4]))
#emotion
emotionregression <- data.frame(Outcome = c("Emotion"),
                                Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                R2 = c(summary(multiemotion)$r.squared),
                                B = c(emotionEst[2:8,1]),
                                BS = c(emotionBS[2:8,1]), 
                                CILower = c(emotionCI[2:8,1]),
                                CIUpper = c(emotionCI[2:8,2]),
                                p = c(emotionEst[2:8,4]))

#do
doregression <- data.frame(Outcome = c("Do"),
                           Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                           R2 = c(summary(multido)$r.squared),
                           B = c(doEst[2:8,1]),
                           BS = c(doBS[2:8,1]), 
                           CILower = c(doCI[2:8,1]),
                           CIUpper = c(doCI[2:8,2]),
                           p = c(doEst[2:8,4]))
#think
thinkregression <- data.frame(Outcome = c("Think"),
                              Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                              R2 = c(summary(multithink)$r.squared),
                              B = c(thinkEst[2:8,1]),
                              BS = c(thinkBS[2:8,1]), 
                              CILower = c(thinkCI[2:8,1]),
                              CIUpper = c(thinkCI[2:8,2]),
                              p = c(thinkEst[2:8,4]))
#feel
feelregression <- data.frame(Outcome = c("Feel"),
                             Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                             R2 = c(summary(multifeel)$r.squared),
                             B = c(feelEst[2:8,1]),
                             BS = c(feelBS[2:8,1]), 
                             CILower = c(feelCI[2:8,1]),
                             CIUpper = c(feelCI[2:8,2]),
                             p = c(feelEst[2:8,4]))

#happy
happyregression <- data.frame(Outcome = c("Happy"),
                              Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                              R2 = c(summary(multihappy)$r.squared),
                              B = c(happyEst[2:8,1]),
                              BS = c(happyBS[2:8,1]), 
                              CILower = c(happyCI[2:8,1]),
                              CIUpper = c(happyCI[2:8,2]),
                              p = c(happyEst[2:8,4]))

#sarcasm
sarcasmregression <- data.frame(Outcome = c("sarcasm"),
                                Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                R2 = c(summary(multisarcasm)$r.squared),
                                B = c(sarcasmEst[2:8,1]),
                                BS = c(sarcasmBS[2:8,1]), 
                                CILower = c(sarcasmCI[2:8,1]),
                                CIUpper = c(sarcasmCI[2:8,2]),
                                p = c(sarcasmEst[2:8,4]))

#sincere
sincereregression <- data.frame(Outcome = c("sincere"),
                                Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                R2 = c(summary(multisincere)$r.squared),
                                B = c(sincereEst[2:8,1]),
                                BS = c(sincereBS[2:8,1]), 
                                CILower = c(sincereCI[2:8,1]),
                                CIUpper = c(sincereCI[2:8,2]),
                                p = c(sincereEst[2:8,4]))

#lies
liesregression <- data.frame(Outcome = c("lies"),
                             Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                             R2 = c(summary(multilies)$r.squared),
                             B = c(liesEst[2:8,1]),
                             BS = c(liesBS[2:8,1]), 
                             CILower = c(liesCI[2:8,1]),
                             CIUpper = c(liesCI[2:8,2]),
                             p = c(liesEst[2:8,4]))

#Esarcasm
Esarcasmregression <- data.frame(Outcome = c("Esarcasm"),
                                 Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                 R2 = c(summary(multiEsarcasm)$r.squared),
                                 B = c(EsarcasmEst[2:8,1]),
                                 BS = c(EsarcasmBS[2:8,1]), 
                                 CILower = c(EsarcasmCI[2:8,1]),
                                 CIUpper = c(EsarcasmCI[2:8,2]),
                                 p = c(EsarcasmEst[2:8,4]))
#RME
RMEregression <- data.frame(Outcome = c("RME"),
                            Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                            R2 = c(summary(multiRME)$r.squared),
                            B = c(RMEEst[2:8,1]),
                            BS = c(RMEBS[2:8,1]), 
                            CILower = c(RMECI[2:8,1]),
                            CIUpper = c(RMECI[2:8,2]),
                            p = c(RMEEst[2:8,4]))
#empathy
empathyregression <- data.frame(Outcome = c("Empathy"),
                                Variable = c("Age", "Sex", "Education", "Cognition", "Hearing", "Loneliness", "Depression"), 
                                R2 = c(summary(multiempathy)$r.squared),
                                B = c(empathyEst[2:8,1]),
                                BS = c(empathyBS[2:8,1]), 
                                CILower = c(empathyCI[2:8,1]),
                                CIUpper = c(empathyCI[2:8,2]),
                                p = c(empathyEst[2:8,4]))

#Combine all regression data frames together
combinedregressions <- rbind(happyregression, neutralregression, sadregression,
                             angryregression, anxiousregression, revoltedregression, emotionregression, thinkregression,
                             doregression, feelregression, RMEregression, sarcasmregression, sincereregression, liesregression,
                             Esarcasmregression, empathyregression)


#Create flexable object
ft <- flextable(data = combinedregressions %>%
                  mutate(p = sprintf("%.3g", p))) %>%
  theme_apa() %>%
  autofit()

#Create a temp file
tmp <- tempfile(fileext = ".docx")

#Create docx file
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Regressiontable.docx")

#Create flexable object
ft <- flextable(data = combinedregressions %>%
                  mutate(p = formatC(p, digits = 3, format = "fg"))) %>%
  theme_apa() %>%
  autofit()

# Heatmap ----------------------------------------------------------

#create beta (standardised) dataframe 

#beta for empathy and sex is greater than one so will need to make separate vector for this
empathy <- c(-.058, 1, -.030, .046, .069, .180, .084)

b.heatmap <- data.frame(happyBS[2:8,1], neutralBS[2:8,1], sadBS[2:8,1], angryBS[2:8,1], anxiousBS[2:8,1], revoltedBS[2:8,1], emotionBS[2:8,1], 
                        thinkBS[2:8,1], doBS[2:8,1], feelBS[2:8,1], RMEBS[2:8,1], sarcasmBS[2:8,1], sincereBS[2:8,1], liesBS[2:8,1],
                        EsarcasmBS[2:8,1], empathy)
b.heatmap <- as.matrix(b.heatmap)
b.heatmap <- t(b.heatmap)
colnames(b.heatmap)<- c("Age", "Sex", "Education", "Cognition", "Hearing", "Social engagement", "Depression") 
rownames(b.heatmap)<- c("Happy", "Neutral", "Sad", "Angry", "Anxious", "Revolted", "Emotion Total", "First-order cognitive", 
                        "Second-order cognitive", "Affective (TASIT)", "Affective (RME)", "Sarcasm", "Sincere", "Lies", 
                        "Enriched Sarcasm", "Emotional empathy")
b.heatmap <- round(b.heatmap,3)

#Create p value dataframe
p.heatmap <- data.frame(happyEst[2:8,4], neutralBS[2:8,4], sadBS[2:8,4], angryBS[2:8,4], anxiousBS[2:8,4], revoltedBS[2:8,4], emotionBS[2:8,4], 
                        thinkBS[2:8,4], doBS[2:8,4], feelBS[2:8,4], RMEBS[2:8,4], sarcasmBS[2:8,4], sincereBS[2:8,4], liesBS[2:8,4],
                        EsarcasmBS[2:8,4], empathyBS[2:8,4])
p.heatmap <- as.matrix(p.heatmap)
p.heatmap <- t(p.heatmap)
colnames(p.heatmap)<- c("Age", "Sex", "Education", "Cognition", "Hearing", "Social engagement", "Depression") 
rownames(p.heatmap)<- c("Happy", "Neutral", "Sad", "Angry", "Anxious", "Revolted", "Emotion Total", "First-order cognitive", 
                        "Second-order cognitive", "Affective (TASIT)", "Affective (RME)", "Sarcasm", "Sincere", "Lies", 
                        "Enriched Sarcasm", "Emotional empathy")
p.heatmap <- round(p.heatmap,3)

#Create heatmap figure 
tiff(filename = "heatmap.tiff", compression = "lzw", width = 7, height = 9, units = "in", res = 300)

heatmapfig <- corrplot(b.heatmap, col=brewer.pal(n=8, name="RdBu"), cl.lim = c(-1, 1), tl.col = "black", tl.srt = 60, cl.ratio = 0.4, 
                       p.mat = p.heatmap, sig.level = 0.017, insig = "pch", pch.col = "gray49", pch.cex = 2, 
                       method = "color", tl.cex = 1.2, cl.cex = 1.1, addgrid.col = "black")
dev.off()

# BETA GRAPHS -----------------------------------------------------------

#create predicted values for lines and CI
ages <- 40:90
ages <- rep(ages, 4)

#Create separate data frames for each domain
happydf <- data.frame(age = df$age, happy = df$happy)
neutraldf <- data.frame(age = df$age, neutral = df$neutral)
saddf <- data.frame(age = df$age, sad = df$sad)
angrydf <- data.frame(age = df$age, angry = df$angry)
anxiousdf <- data.frame(age = df$age, anxious = df$anxious)
revolteddf <- data.frame(age = df$age, revolted = df$revolted)
emotiondf <- data.frame(age = df$age, emotion = df$emotion)
dodf <- data.frame(age = df$age, do = df$do)
thinkdf <- data.frame(age = df$age, think = df$think)
feeldf <- data.frame(age = df$age, feel = df$feel)
sarcasmdf <- data.frame(age = df$age, sarcasm = df$sarcasm)
sinceredf <- data.frame(age = df$age, sincere = df$sincere)
liesdf <- data.frame(age = df$age, lies = df$lies)
Esarcasmdf <- data.frame(age = df$age, Esarcasm = df$Esarcasm)
RMEdf <- data.frame(age = df$age, RME = df$RME)
empathydf <- data.frame(age = df$age, empathy = df$empathy)

#Convert into long format
happydf <- melt(setDT(happydf), id.vars = c("age"), variable.name = "domain")
neutraldf <- melt(setDT(neutraldf), id.vars = c("age"), variable.name = "domain")
saddf <- melt(setDT(saddf), id.vars = c("age"), variable.name = "domain")
angrydf <- melt(setDT(angrydf), id.vars = c("age"), variable.name = "domain")
anxiousdf <- melt(setDT(anxiousdf), id.vars = c("age"), variable.name = "domain")
revolteddf <- melt(setDT(revolteddf), id.vars = c("age"), variable.name = "domain")
emotiondf <- melt(setDT(emotiondf), id.vars = c("age"), variable.name = "domain")
dodf <- melt(setDT(dodf), id.vars = c("age"), variable.name = "domain")
thinkdf <- melt(setDT(thinkdf), id.vars = c("age"), variable.name = "domain")
feeldf <- melt(setDT(feeldf), id.vars = c("age"), variable.name = "domain")
sarcasmdf <- melt(setDT(sarcasmdf), id.vars = c("age"), variable.name = "domain")
sinceredf <- melt(setDT(sinceredf), id.vars = c("age"), variable.name = "domain")
liesdf <- melt(setDT(liesdf), id.vars = c("age"), variable.name = "domain")
Esarcasmdf <- melt(setDT(Esarcasmdf), id.vars = c("age"), variable.name = "domain")
RMEdf <- melt(setDT(RMEdf), id.vars = c("age"), variable.name = "domain")
empathydf <- melt(setDT(empathydf), id.vars = c("age"), variable.name = "domain")

#Create label for beta values
happybeta <- data.frame(domain = unique(happydf$domain), labels = paste("B =", round(happyEst[2,1],3)), face = c("plain"))
neutralbeta <- data.frame(domain = unique(neutraldf$domain), labels = paste("B =", round(neutralEst[2,1],3)), face = c("plain"))
sadbeta <- data.frame(domain = unique(saddf$domain), labels = paste("B =", round(sadEst[2,1],3)), face = c("plain"))
angrybeta <- data.frame(domain = unique(angrydf$domain), labels = paste("B =", round(angryEst[2,1],3)), face = c("plain"))
anxiousbeta <- data.frame(domain = unique(anxiousdf$domain), labels = paste("B =", round(anxiousEst[2,1],3)), face = c("plain"))
revoltedbeta <- data.frame(domain = unique(revolteddf$domain), labels = paste("B =", round(revoltedEst[2,1],3)), face = c("plain"))
emotionbeta <- data.frame(domain = unique(emotiondf$domain), labels = paste("B =", round(emotionEst[2,1],3)), face = c("plain"))
dobeta <- data.frame(domain = unique(dodf$domain), labels = paste("B =", round(doEst[2,1],3)), face = c("plain"))
thinkbeta <- data.frame(domain = unique(thinkdf$domain), labels = paste("B =", round(thinkEst[2,1],3)), face = c("plain"))
feelbeta <- data.frame(domain = unique(feeldf$domain), labels = paste("B =", round(feelEst[2,1],3)), face = c("plain"))
sarcasmbeta <- data.frame(domain = unique(sarcasmdf$domain), labels = paste("B =", round(sarcasmEst[2,1],3)), face = c("plain"))
sincerebeta <- data.frame(domain = unique(sinceredf$domain), labels = paste("B =", round(sincereEst[2,1],3)), face = c("plain"))
liesbeta <- data.frame(domain = unique(liesdf$domain), labels = paste("B =", round(liesEst[2,1],3)), face = c("plain"))
Esarcasmbeta <- data.frame(domain = unique(Esarcasmdf$domain), labels = paste("B =", round(EsarcasmEst[2,1],3)), face = c("plain"))
RMEbeta <- data.frame(domain = unique(RMEdf$domain), labels = paste("B =", round(RMEEst[2,1],3)), face = c("plain"))
empathybeta <- data.frame(domain = unique(empathydf$domain), labels = paste("B =", round(empathyEst[2,1],3)), face = c("plain"))


#Find age only intercept value
linearhappy <- lm(happy ~ age, data = df)
linearneutral <- lm(neutral ~ age, data = df)
linearsad <- lm(sad ~ age, data = df)
linearangry <- lm(angry ~ age, data = df)
linearanxious <- lm(anxious ~ age, data = df)
linearrevolted <- lm(revolted ~ age, data = df)
linearemotion <- lm(emotion ~ age, data = df)
lineardo <- lm(do ~ age, data = df)
linearthink <- lm(think ~ age, data = df)
linearfeel <- lm(feel ~ age, data = df)
linearsarcasm <- lm(sarcasm ~ age, data = df)
linearsincere <- lm(sincere ~ age, data = df)
linearlies <- lm(lies ~ age, data = df)
linearEsarcasm <- lm(Esarcasm ~ age, data = df)
linearRME <- lm(RME ~ age, data = df)
linearempathy <- lm(empathy ~ age, data = df)


##Create predicted values for regression lines
happyvalues <- ages[1:204] * happyEst[2,1] + summary(linearhappy)$coefficients[1,1]
neutralvalues <- ages[1:204] * neutralEst[2,1] + summary(linearneutral)$coefficients[1,1]
sadvalues <- ages[1:204] * sadEst[2,1] + summary(linearsad)$coefficients[1,1]
angryvalues <- ages[1:204] * angryEst[2,1] + summary(linearangry)$coefficients[1,1]
anxiousvalues <- ages[1:204] * anxiousEst[2,1] + summary(linearanxious)$coefficients[1,1]
revoltedvalues <- ages[1:204] * revoltedEst[2,1] + summary(linearrevolted)$coefficients[1,1]
emotionvalues <- ages[1:204] * emotionEst[2,1] + summary(linearemotion)$coefficients[1,1]
dovalues <- ages[1:204] * doEst[2,1] + summary(lineardo)$coefficients[1,1]
thinkvalues <- ages[1:204] * thinkEst[2,1] + summary(linearthink)$coefficients[1,1]
feelvalues <- ages[1:204] * feelEst[2,1] + summary(linearfeel)$coefficients[1,1]
sarcasmvalues <- ages[1:204] * sarcasmEst[2,1] + summary(linearsarcasm)$coefficients[1,1]
sincerevalues <- ages[1:204] * sincereEst[2,1] + summary(linearsincere)$coefficients[1,1]
liesvalues <- ages[1:204] * liesEst[2,1] + summary(linearlies)$coefficients[1,1]
Esarcasmvalues <- ages[1:204] * EsarcasmEst[2,1] + summary(linearEsarcasm)$coefficients[1,1]
RMEvalues <- ages[1:204] * RMEEst[2,1] + summary(linearRME)$coefficients[1,1]
empathyvalues <- ages[1:204] * empathyEst[2,1] + summary(linearempathy)$coefficients[1,1]


#Create minimum values for error shading
happymin <- ages[1:204] * happyCI[2,1] + summary(linearhappy)$coefficients[1,1]
neutralmin <- ages[1:204] * neutralCI[2,1] + summary(linearneutral)$coefficients[1,1]
sadmin <- ages[1:204] * sadCI[2,1] + summary(linearsad)$coefficients[1,1]
angrymin <- ages[1:204] * angryCI[2,1] + summary(linearangry)$coefficients[1,1]
anxiousmin <- ages[1:204] * anxiousCI[2,1] + summary(linearanxious)$coefficients[1,1]
revoltedmin <- ages[1:204] * revoltedCI[2,1] + summary(linearrevolted)$coefficients[1,1]
emotionmin <- ages[1:204] * emotionCI[2,1] + summary(linearemotion)$coefficients[1,1]
domin <- ages[1:204] * doCI[2,1] + summary(lineardo)$coefficients[1,1]
thinkmin <- ages[1:204] * thinkCI[2,1] + summary(linearthink)$coefficients[1,1]
feelmin <- ages[1:204] * feelCI[2,1] + summary(linearfeel)$coefficients[1,1]
sarcasmmin <- ages[1:204] * sarcasmCI[2,1] + summary(linearsarcasm)$coefficients[1,1]
sinceremin <- ages[1:204] * sincereCI[2,1] + summary(linearsincere)$coefficients[1,1]
liesmin <- ages[1:204] * liesCI[2,1] + summary(linearlies)$coefficients[1,1]
Esarcasmmin <- ages[1:204] * EsarcasmCI[2,1] + summary(linearEsarcasm)$coefficients[1,1]
RMEmin <- ages[1:204] * RMECI[2,1] + summary(linearRME)$coefficients[1,1]
empathymin <- ages[1:204] * empathyCI[2,1] + summary(linearempathy)$coefficients[1,1]


#Create maximum values for error shading
happymax <- ages[1:204] * happyCI[2,2] + summary(linearhappy)$coefficients[1,1]
neutralmax <- ages[1:204] * neutralCI[2,2] + summary(linearneutral)$coefficients[1,1]
sadmax <- ages[1:204] * sadCI[2,2] + summary(linearsad)$coefficients[1,1]
angrymax <- ages[1:204] * angryCI[2,2] + summary(linearangry)$coefficients[1,1]
anxiousmax <- ages[1:204] * anxiousCI[2,2] + summary(linearanxious)$coefficients[1,1]
revoltedmax <- ages[1:204] * revoltedCI[2,2] + summary(linearrevolted)$coefficients[1,1]
emotionmax <- ages[1:204] * emotionCI[2,2] + summary(linearemotion)$coefficients[1,1]
domax <- ages[1:204] * doCI[2,2] + summary(lineardo)$coefficients[1,1]
thinkmax <- ages[1:204] * thinkCI[2,2] + summary(linearthink)$coefficients[1,1]
feelmax <- ages[1:204] * feelCI[2,2] + summary(linearfeel)$coefficients[1,1]
sarcasmmax <- ages[1:204] * sarcasmCI[2,2] + summary(linearsarcasm)$coefficients[1,1]
sinceremax <- ages[1:204] * sincereCI[2,2] + summary(linearsincere)$coefficients[1,1]
liesmax <- ages[1:204] * liesCI[2,2] + summary(linearlies)$coefficients[1,1]
Esarcasmmax <- ages[1:204] * EsarcasmCI[2,2] + summary(linearEsarcasm)$coefficients[1,1]
RMEmax <- ages[1:204] * RMECI[2,2] + summary(linearRME)$coefficients[1,1]
empathymax <- ages[1:204] * empathyCI[2,2] + summary(linearempathy)$coefficients[1,1]


#Create domain variable
happydomain <- factor(c(rep("Happy", 204)))
neutraldomain <- factor(c(rep("neutral", 204)))
saddomain <- factor(c(rep("sad", 204)))
angrydomain <- factor(c(rep("angry", 204)))
anxiousdomain <- factor(c(rep("anxious", 204)))
revolteddomain <- factor(c(rep("revolted", 204)))
emotiondomain <- factor(c(rep("emotion", 204)))
dodomain <- factor(c(rep("do", 204)))
thinkdomain <- factor(c(rep("think", 204)))
feeldomain <- factor(c(rep("feel", 204)))
sarcasmdomain <- factor(c(rep("sarcasm", 204)))
sinceredomain <- factor(c(rep("sincere", 204)))
liesdomain <- factor(c(rep("lies", 204)))
Esarcasmdomain <- factor(c(rep("Esarcasm", 204)))
RMEdomain <- factor(c(rep("RME", 204)))
empathydomain <- factor(c(rep("empathy", 204)))

#Create line data 
happyline <- data.frame(ages, happydomain, happyvalues, happymin, happymax)
neutralline <- data.frame(ages, neutraldomain, neutralvalues, neutralmin, neutralmax)
sadline <- data.frame(ages, saddomain, sadvalues, sadmin, sadmax)
angryline <- data.frame(ages, angrydomain, angryvalues, angrymin, angrymax)
anxiousline <- data.frame(ages, anxiousdomain, anxiousvalues, anxiousmin, anxiousmax)
revoltedline <- data.frame(ages, revolteddomain, revoltedvalues, revoltedmin, revoltedmax)
emotionline <- data.frame(ages, emotiondomain, emotionvalues, emotionmin, emotionmax)
doline <- data.frame(ages, dodomain, dovalues, domin, domax)
thinkline <- data.frame(ages, thinkdomain, thinkvalues, thinkmin, thinkmax)
feelline <- data.frame(ages, feeldomain, feelvalues, feelmin, feelmax)
sarcasmline <- data.frame(ages, sarcasmdomain, sarcasmvalues, sarcasmmin, sarcasmmax)
sincereline <- data.frame(ages, sinceredomain, sincerevalues, sinceremin, sinceremax)
liesline <- data.frame(ages, liesdomain, liesvalues, liesmin, liesmax)
Esarcasmline <- data.frame(ages, Esarcasmdomain, Esarcasmvalues, Esarcasmmin, Esarcasmmax)
RMEline <- data.frame(ages, RMEdomain, RMEvalues, RMEmin, RMEmax)
empathyline <- data.frame(ages, empathydomain, empathyvalues, empathymin, empathymax)


#Create figures 
#Happy
tiff(filename = "happy.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(happydf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = happyline, aes(x = ages, y = happyvalues), col = "Red", size = 1) + 
  geom_ribbon(data = happyline, aes(x = ages, ymin = happymin, ymax = happymax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,4) +
  ggtitle("Happy") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = happybeta, aes(label = labels, x = 71, y = 4, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#neutral
tiff(filename = "neutral.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(neutraldf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = neutralline, aes(x = ages, y = neutralvalues), col = "Red", size = 1) + 
  geom_ribbon(data = neutralline, aes(x = ages, ymin = neutralmin, ymax = neutralmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,4) +
  ggtitle("Neutral") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = neutralbeta, aes(label = labels, x = 71, y = 4, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#sad
tiff(filename = "sad.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(saddf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = sadline, aes(x = ages, y = sadvalues), col = "Red", size = 1) + 
  geom_ribbon(data = sadline, aes(x = ages, ymin = sadmin, ymax = sadmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,4) +
  ggtitle("Sad") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = sadbeta, aes(label = labels, x = 71, y = 4, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#angry
tiff(filename = "angry.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(angrydf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = angryline, aes(x = ages, y = angryvalues), col = "Red", size = 1) + 
  geom_ribbon(data = angryline, aes(x = ages, ymin = angrymin, ymax = angrymax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,4) +
  ggtitle("Angry") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = angrybeta, aes(label = labels, x = 71, y = 4, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#anxious
tiff(filename = "anxious.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(anxiousdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = anxiousline, aes(x = ages, y = anxiousvalues), col = "Red", size = 1) + 
  geom_ribbon(data = anxiousline, aes(x = ages, ymin = anxiousmin, ymax = anxiousmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,4) +
  ggtitle("Anxious") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = anxiousbeta, aes(label = labels, x = 71, y = 4, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#revolted
tiff(filename = "revolted.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(revolteddf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = revoltedline, aes(x = ages, y = revoltedvalues), col = "Red", size = 1) + 
  geom_ribbon(data = revoltedline, aes(x = ages, ymin = revoltedmin, ymax = revoltedmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,4) +
  ggtitle("Revolted") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = revoltedbeta, aes(label = labels, x = 71, y = 4, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#emotion
tiff(filename = "emotion.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(emotiondf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = emotionline, aes(x = ages, y = emotionvalues), col = "Red", size = 1) + 
  geom_ribbon(data = emotionline, aes(x = ages, ymin = emotionmin, ymax = emotionmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,10) +
  ggtitle("Emotion Total") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = emotionbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#do
tiff(filename = "do.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(dodf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = doline, aes(x = ages, y = dovalues), col = "Red", size = 1) + 
  geom_ribbon(data = doline, aes(x = ages, ymin = domin, ymax = domax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,20) +
  ggtitle("Second-order cognitive ToM") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = dobeta, aes(label = labels, x = 71, y = 20, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#think
tiff(filename = "think.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(thinkdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = thinkline, aes(x = ages, y = thinkvalues), col = "Red", size = 1) + 
  geom_ribbon(data = thinkline, aes(x = ages, ymin = thinkmin, ymax = thinkmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,20) +
  ggtitle("First-order cognitive ToM") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = thinkbeta, aes(label = labels, x = 71, y = 20, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#feel
tiff(filename = "feel.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(feeldf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = feelline, aes(x = ages, y = feelvalues), col = "Red", size = 1) + 
  geom_ribbon(data = feelline, aes(x = ages, ymin = feelmin, ymax = feelmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,20) +
  ggtitle("Affective ToM (TASIT)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = feelbeta, aes(label = labels, x = 71, y = 20, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#sarcasm
tiff(filename = "sarcasm.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(sarcasmdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = sarcasmline, aes(x = ages, y = sarcasmvalues), col = "Red", size = 1) + 
  geom_ribbon(data = sarcasmline, aes(x = ages, ymin = sarcasmmin, ymax = sarcasmmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,22) +
  ggtitle("Sarcasm") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = sarcasmbeta, aes(label = labels, x = 71, y = 21, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#sincere
tiff(filename = "sincere.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(sinceredf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = sincereline, aes(x = ages, y = sincerevalues), col = "Red", size = 1) + 
  geom_ribbon(data = sincereline, aes(x = ages, ymin = sinceremin, ymax = sinceremax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,22) +
  ggtitle("Sincere") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = sincerebeta, aes(label = labels, x = 71, y = 20, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#lies
tiff(filename = "lies.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(liesdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = liesline, aes(x = ages, y = liesvalues), col = "Red", size = 1) + 
  geom_ribbon(data = liesline, aes(x = ages, ymin = liesmin, ymax = liesmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,22) +
  ggtitle("Lies") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = liesbeta, aes(label = labels, x = 71, y = 20, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#Esarcasm
tiff(filename = "Esarcasm.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(Esarcasmdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = Esarcasmline, aes(x = ages, y = Esarcasmvalues), col = "Red", size = 1) + 
  geom_ribbon(data = Esarcasmline, aes(x = ages, ymin = Esarcasmmin, ymax = Esarcasmmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,22) +
  ggtitle("Enriched Sarcasm") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = Esarcasmbeta, aes(label = labels, x = 71, y = 22, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#RME
tiff(filename = "RME.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(RMEdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = RMEline, aes(x = ages, y = RMEvalues), col = "Red", size = 1) + 
  geom_ribbon(data = RMEline, aes(x = ages, ymin = RMEmin, ymax = RMEmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(10,35) +
  ggtitle("Affective ToM (RME)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = RMEbeta, aes(label = labels, x = 71, y = 35, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#empathy
tiff(filename = "empathy.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(empathydf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = empathyline, aes(x = ages, y = empathyvalues), col = "Red", size = 1) + 
  geom_ribbon(data = empathyline, aes(x = ages, ymin = empathymin, ymax = empathymax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,25) +
  ggtitle("Emotional Empathy") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = empathybeta, aes(label = labels, x = 71, y = 25, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Create figures 
#sarcasm
tiff(filename = "sarcasm.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(sarcasmdf, aes(x = age, y = value)) + 
  geom_jitter() + 
  geom_line(data = sarcasmline, aes(x = ages, y = sarcasmvalues), col = "Red", size = 1) + 
  geom_ribbon(data = sarcasmline, aes(x = ages, ymin = sarcasmmin, ymax = sarcasmmax), inherit.aes = FALSE, alpha = 0.017) +
  xlab("Age") +
  ylab("Value") +
  ylim(0,22) +
  ggtitle("Sarcasm") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = sarcasmbeta, aes(label = labels, x = 71, y = 22, hjust = 0, vjust = 1, fontface = face))
dev.off()


# MINIMAL MODEL ---------------------------------------------------

#happy
minhappy <- lm(happy ~ age + sex, df)
summary(minhappy)

#neutral
minneutral <- lm(neutral ~ age + sex, df)
summary(minneutral)

#sad
minsad <- lm(sad ~ age + sex, df)
summary(minsad)

#angry
minangry <- lm(angry ~ age + sex, df)
summary(minangry)

#anxious
minanxious <- lm(anxious ~ age + sex, df)
summary(minanxious)

#revolted
minrevolted <- lm(revolted ~ age + sex, df)
summary(minrevolted)

#emotion
minemotion <- lm(emotion ~ age + sex, df)
summary(minemotion)

#do
mindo <- lm(do ~ age + sex, df)
summary(mindo)

#think
minthink <- lm(think ~ age + sex, df)
summary(minthink)

#feel
minfeel <- lm(feel ~ age + sex, df)
summary(minfeel)

#sarcasm
minsarcasm <- lm(sarcasm ~ age + sex, df)
summary(minsarcasm)

#sincere
minsincere <- lm(sincere ~ age + sex, df)
summary(minsincere)

#lies
minlies <- lm(lies ~ age + sex, df)
summary(minlies)

#Esarcasm
minEsarcasm <- lm(Esarcasm ~ age + sex, df)
summary(minEsarcasm)

#RME
minRME <- lm(RME ~ age + sex, df)
summary(minRME)

#empathy
minempathy <- lm(empathy ~ age + sex, df)
summary(minempathy)

# Normality --------------------------------------------

#Happy
sresidminhappy <- studres(minhappy)
shapiro.test(sresidminhappy) #Not normal

#neutral
sresidminneutral <- studres(minneutral)
shapiro.test(sresidminneutral) #Not normal

#sad
sresidminsad <- studres(minsad)
shapiro.test(sresidminsad) #Not normal

#angry
sresidminangry <- studres(minangry)
shapiro.test(sresidminangry) #Not normal

#anxious
sresidminanxious <- studres(minanxious)
shapiro.test(sresidminanxious) #Not normal

#revolted
sresidminrevolted <- studres(minrevolted)
shapiro.test(sresidminrevolted) #Not normal

#emotion
sresidminemotion <- studres(minemotion)
shapiro.test(sresidminemotion) 

#do
sresidmindo <- studres(mindo)
shapiro.test(sresidmindo) #Not normal

#think
sresidminthink <- studres(minthink)
shapiro.test(sresidminthink) #Not normal

#feel
sresidminfeel <- studres(minfeel)
shapiro.test(sresidminfeel) 

#sarcasm
sresidminsarcasm <- studres(minsarcasm)
shapiro.test(sresidminsarcasm) #Not normal

#sincere
sresidminsincere <- studres(minsincere)
shapiro.test(sresidminsincere) #Not normal

#lies
sresidminlies <- studres(minlies)
shapiro.test(sresidminlies)

#Esarcasm
sresidminEsarcasm <- studres(minEsarcasm)
shapiro.test(sresidminEsarcasm) #Not normal

#RME
sresidminRME <- studres(minRME)
shapiro.test(sresidminRME) #Not normal

#empathy
sresidminempathy <- studres(minempathy)
shapiro.test(sresidminempathy)


# Homoscedasticity -----------------------------------------
#happy
ncvTest(minhappy)

#neutral
ncvTest(minneutral)

#sad
ncvTest(minsad) #homoscedastic

#angry
ncvTest(minangry) #homoscedastic

#anxious
ncvTest(minanxious)

#revolted
ncvTest(minrevolted)

#emotion
ncvTest(minemotion)

#do
ncvTest(mindo)

#think
ncvTest(minthink)

#feel
ncvTest(minfeel)

#sarcasm
ncvTest(minsarcasm)

#sincere
ncvTest(minsincere)

#lies
ncvTest(minlies)

#Esarcasm
ncvTest(minEsarcasm)

#RME
ncvTest(minRME) #homoscedastic

#empathy
ncvTest(minempathy)


# Robust models  ------------------------------------------------------

#happy
happyminEst <- coeftest(minhappy, vcov. = vcovHC(minhappy, type = "HC1"))
happyminCI <- coefci(minhappy, vcov. = vcovHC(minhappy, type = "HC1"))

#neutral
neutralminEst <- coeftest(minneutral, vcov. = vcovHC(minneutral, type = "HC1"))
neutralminCI <- coefci(minneutral, vcov. = vcovHC(minneutral, type = "HC1"))

#sad
sadminEst <- coeftest(minsad, vcov. = vcovHC(minsad, type = "HC1"))
sadminCI <- coefci(minsad, vcov. = vcovHC(minsad, type = "HC1"))

#angry
angryminEst <- coeftest(minangry, vcov. = vcovHC(minangry, type = "HC1"))
angryminCI <- coefci(minangry, vcov. = vcovHC(minangry, type = "HC1"))

#anxious
anxiousminEst <- coeftest(minanxious, vcov. = vcovHC(minanxious, type = "HC1"))
anxiousminCI <- coefci(minanxious, vcov. = vcovHC(minanxious, type = "HC1"))

#revolted
revoltedminEst <- coeftest(minrevolted, vcov. = vcovHC(minrevolted, type = "HC1"))
revoltedminCI <- coefci(minrevolted, vcov. = vcovHC(minrevolted, type = "HC1"))

#emotion
emotionminEst <- coeftest(minemotion, vcov. = vcovHC(minemotion, type = "HC1"))
emotionminCI <- coefci(minemotion, vcov. = vcovHC(minemotion, type = "HC1"))

#do
dominEst <- coeftest(mindo, vcov. = vcovHC(mindo, type = "HC1"))
dominCI <- coefci(mindo, vcov. = vcovHC(mindo, type = "HC1"))

#think
thinkminEst <- coeftest(minthink, vcov. = vcovHC(minthink, type = "HC1"))
thinkminCI <- coefci(minthink, vcov. = vcovHC(minthink, type = "HC1"))

#feel
feelminEst <- coeftest(minfeel, vcov. = vcovHC(minfeel, type = "HC1"))
feelminCI <- coefci(minfeel, vcov. = vcovHC(minfeel, type = "HC1"))

#sarcasm
sarcasmminEst <- coeftest(minsarcasm, vcov. = vcovHC(minsarcasm, type = "HC1"))
sarcasmminCI <- coefci(minsarcasm, vcov. = vcovHC(minsarcasm, type = "HC1"))

#sincere
sincereminEst <- coeftest(minsincere, vcov. = vcovHC(minsincere, type = "HC1"))
sincereminCI <- coefci(minsincere, vcov. = vcovHC(minsincere, type = "HC1"))

#lies
liesminEst <- coeftest(minlies, vcov. = vcovHC(minlies, type = "HC1"))
liesminCI <- coefci(minlies, vcov. = vcovHC(minlies, type = "HC1"))

#Esarcasm
EsarcasmminEst <- coeftest(minEsarcasm, vcov. = vcovHC(minEsarcasm, type = "HC1"))
EsarcasmminCI <- coefci(minEsarcasm, vcov. = vcovHC(minEsarcasm, type = "HC1"))

#RME
RMEminEst <- coeftest(minRME, vcov. = vcovHC(minRME, type = "HC1"))
RMEminCI <- coefci(minRME, vcov. = vcovHC(minRME, type = "HC1"))

#empathy
empathyminEst <- coeftest(minempathy, vcov. = vcovHC(minempathy, type = "HC1"))
empathyminCI <- coefci(minempathy, vcov. = vcovHC(minempathy, type = "HC1"))


# Standardised betas ---------------------------------------------

#happy
minhappyS <- lm(happy ~ age + sex, data = dfstandard1)
minhappyBS <- coeftest(minhappyS , vcov. = vcovHC(minhappyS, type = "HC1"))

#neutral
minneutralS <- lm(neutral ~ age + sex, data = dfstandard1)
minneutralBS <- coeftest(minneutralS , vcov. = vcovHC(minneutralS, type = "HC1"))

#sad
minsadS <- lm(sad ~ age + sex, data = dfstandard1)
minsadBS <- coeftest(minsadS , vcov. = vcovHC(minsadS, type = "HC1"))

#angry
minangryS <- lm(angry ~ age + sex, data = dfstandard1)
minangryBS <- coeftest(minangryS , vcov. = vcovHC(minangryS, type = "HC1"))

#anxious
minanxiousS <- lm(anxious ~ age + sex, data = dfstandard1)
minanxiousBS <- coeftest(minanxiousS , vcov. = vcovHC(minanxiousS, type = "HC1"))

#revolted
minrevoltedS <- lm(revolted ~ age + sex, data = dfstandard1)
minrevoltedBS <- coeftest(minrevoltedS , vcov. = vcovHC(minrevoltedS, type = "HC1"))

#emotion
minemotionS <- lm(emotion ~ age + sex, data = dfstandard1)
minemotionBS <- coeftest(minemotionS , vcov. = vcovHC(minemotionS, type = "HC1"))

#do
mindoS <- lm(do ~ age + sex, data = dfstandard1)
mindoBS <- coeftest(mindoS , vcov. = vcovHC(mindoS, type = "HC1"))

#think
minthinkS <- lm(think ~ age + sex, data = dfstandard1)
minthinkBS <- coeftest(minthinkS , vcov. = vcovHC(minthinkS, type = "HC1"))

#feel
minfeelS <- lm(feel ~ age + sex, data = dfstandard1)
minfeelBS <- coeftest(minfeelS , vcov. = vcovHC(minfeelS, type = "HC1"))

#sarcasm
minsarcasmS <- lm(sarcasm ~ age + sex, data = dfstandard1)
minsarcasmBS <- coeftest(minsarcasmS , vcov. = vcovHC(minsarcasmS, type = "HC1"))

#sincere
minsincereS <- lm(sincere ~ age + sex, data = dfstandard1)
minsincereBS <- coeftest(minsincereS , vcov. = vcovHC(minsincereS, type = "HC1"))

#lies
minliesS <- lm(lies ~ age + sex, data = dfstandard1)
minliesBS <- coeftest(minliesS , vcov. = vcovHC(minliesS, type = "HC1"))

#Esarcasm
minEsarcasmS <- lm(Esarcasm ~ age + sex, data = dfstandard1)
minEsarcasmBS <- coeftest(minEsarcasmS , vcov. = vcovHC(minEsarcasmS, type = "HC1"))

#RME
minRMES <- lm(RME ~ age + sex, data = dfstandard1)
minRMEBS <- coeftest(minRMES , vcov. = vcovHC(minRMES, type = "HC1"))

#empathy
minempathyS <- lm(empathy ~ age + sex, data = dfstandard1)
minempathyBS <- coeftest(minempathyS , vcov. = vcovHC(minempathyS, type = "HC1"))


# Table ----------------------------------------------------------

#data frames for each regression model 
#happy
happyminregression <- data.frame(Outcome = c("Happy"),
                                 Variable = c("Age", "Sex"), 
                                 R2 = c(summary(minhappy)$r.squared),
                                 B = c(happyminEst[2:3,1]),
                                 BS = c(minhappyBS[2:3,1]), 
                                 CILower = c(happyminCI[2:3,1]),
                                 CIUpper = c(happyminCI[2:3,2]),
                                 p = c(happyminEst[2:3,4]))

#neutral
neutralminregression <- data.frame(Outcome = c("neutral"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minneutral)$r.squared),
                                   B = c(neutralminEst[2:3,1]),
                                   BS = c(minneutralBS[2:3,1]), 
                                   CILower = c(neutralminCI[2:3,1]),
                                   CIUpper = c(neutralminCI[2:3,2]),
                                   p = c(neutralminEst[2:3,4]))

#sad
sadminregression <- data.frame(Outcome = c("sad"),
                               Variable = c("Age", "Sex"), 
                               R2 = c(summary(minsad)$r.squared),
                               B = c(sadminEst[2:3,1]),
                               BS = c(minsadBS[2:3,1]), 
                               CILower = c(sadminCI[2:3,1]),
                               CIUpper = c(sadminCI[2:3,2]),
                               p = c(sadminEst[2:3,4]))

#angry
angryminregression <- data.frame(Outcome = c("angry"),
                                 Variable = c("Age", "Sex"), 
                                 R2 = c(summary(minangry)$r.squared),
                                 B = c(angryminEst[2:3,1]),
                                 BS = c(minangryBS[2:3,1]), 
                                 CILower = c(angryminCI[2:3,1]),
                                 CIUpper = c(angryminCI[2:3,2]),
                                 p = c(angryminEst[2:3,4]))

#anxious
anxiousminregression <- data.frame(Outcome = c("anxious"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minanxious)$r.squared),
                                   B = c(anxiousminEst[2:3,1]),
                                   BS = c(minanxiousBS[2:3,1]), 
                                   CILower = c(anxiousminCI[2:3,1]),
                                   CIUpper = c(anxiousminCI[2:3,2]),
                                   p = c(anxiousminEst[2:3,4]))

#revolted
revoltedminregression <- data.frame(Outcome = c("revolted"),
                                    Variable = c("Age", "Sex"), 
                                    R2 = c(summary(minrevolted)$r.squared),
                                    B = c(revoltedminEst[2:3,1]),
                                    BS = c(minrevoltedBS[2:3,1]), 
                                    CILower = c(revoltedminCI[2:3,1]),
                                    CIUpper = c(revoltedminCI[2:3,2]),
                                    p = c(revoltedminEst[2:3,4]))

#emotion
emotionminregression <- data.frame(Outcome = c("emotion"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minemotion)$r.squared),
                                   B = c(emotionminEst[2:3,1]),
                                   BS = c(minemotionBS[2:3,1]), 
                                   CILower = c(emotionminCI[2:3,1]),
                                   CIUpper = c(emotionminCI[2:3,2]),
                                   p = c(emotionminEst[2:3,4]))

#do
dominregression <- data.frame(Outcome = c("do"),
                              Variable = c("Age", "Sex"), 
                              R2 = c(summary(mindo)$r.squared),
                              B = c(dominEst[2:3,1]),
                              BS = c(mindoBS[2:3,1]), 
                              CILower = c(dominCI[2:3,1]),
                              CIUpper = c(dominCI[2:3,2]),
                              p = c(dominEst[2:3,4]))

#think
thinkminregression <- data.frame(Outcome = c("think"),
                                 Variable = c("Age", "Sex"), 
                                 R2 = c(summary(minthink)$r.squared),
                                 B = c(thinkminEst[2:3,1]),
                                 BS = c(minthinkBS[2:3,1]), 
                                 CILower = c(thinkminCI[2:3,1]),
                                 CIUpper = c(thinkminCI[2:3,2]),
                                 p = c(thinkminEst[2:3,4]))

#feel
feelminregression <- data.frame(Outcome = c("feel"),
                                Variable = c("Age", "Sex"), 
                                R2 = c(summary(minfeel)$r.squared),
                                B = c(feelminEst[2:3,1]),
                                BS = c(minfeelBS[2:3,1]), 
                                CILower = c(feelminCI[2:3,1]),
                                CIUpper = c(feelminCI[2:3,2]),
                                p = c(feelminEst[2:3,4]))

#sarcasm
sarcasmminregression <- data.frame(Outcome = c("sarcasm"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minsarcasm)$r.squared),
                                   B = c(sarcasmminEst[2:3,1]),
                                   BS = c(minsarcasmBS[2:3,1]), 
                                   CILower = c(sarcasmminCI[2:3,1]),
                                   CIUpper = c(sarcasmminCI[2:3,2]),
                                   p = c(sarcasmminEst[2:3,4]))

#sincere
sincereminregression <- data.frame(Outcome = c("sincere"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minsincere)$r.squared),
                                   B = c(sincereminEst[2:3,1]),
                                   BS = c(minsincereBS[2:3,1]), 
                                   CILower = c(sincereminCI[2:3,1]),
                                   CIUpper = c(sincereminCI[2:3,2]),
                                   p = c(sincereminEst[2:3,4]))

#lies
liesminregression <- data.frame(Outcome = c("lies"),
                                Variable = c("Age", "Sex"), 
                                R2 = c(summary(minlies)$r.squared),
                                B = c(liesminEst[2:3,1]),
                                BS = c(minliesBS[2:3,1]), 
                                CILower = c(liesminCI[2:3,1]),
                                CIUpper = c(liesminCI[2:3,2]),
                                p = c(liesminEst[2:3,4]))

#Esarcasm
Esarcasmminregression <- data.frame(Outcome = c("Esarcasm"),
                                    Variable = c("Age", "Sex"), 
                                    R2 = c(summary(minEsarcasm)$r.squared),
                                    B = c(EsarcasmminEst[2:3,1]),
                                    BS = c(minEsarcasmBS[2:3,1]), 
                                    CILower = c(EsarcasmminCI[2:3,1]),
                                    CIUpper = c(EsarcasmminCI[2:3,2]),
                                    p = c(EsarcasmminEst[2:3,4]))

#RME
RMEminregression <- data.frame(Outcome = c("RME"),
                               Variable = c("Age", "Sex"), 
                               R2 = c(summary(minRME)$r.squared),
                               B = c(RMEminEst[2:3,1]),
                               BS = c(minRMEBS[2:3,1]), 
                               CILower = c(RMEminCI[2:3,1]),
                               CIUpper = c(RMEminCI[2:3,2]),
                               p = c(RMEminEst[2:3,4]))

#empathy
empathyminregression <- data.frame(Outcome = c("empathy"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minempathy)$r.squared),
                                   B = c(empathyminEst[2:3,1]),
                                   BS = c(minempathyBS[2:3,1]), 
                                   CILower = c(empathyminCI[2:3,1]),
                                   CIUpper = c(empathyminCI[2:3,2]),
                                   p = c(empathyminEst[2:3,4]))


#Combine all regression data frames together
combinedminregressions <- rbind(happyminregression, neutralminregression, sadminregression, angryminregression, 
                                anxiousminregression, revoltedminregression, emotionminregression, thinkminregression, 
                                dominregression, feelminregression, RMEminregression, sarcasmminregression, sincereminregression,
                                liesminregression, Esarcasmminregression, empathyminregression)

#Create flexable object
ftmin <- flextable(data = combinedminregressions %>%
                     mutate(p = sprintf("%.3g", p))) %>%
  theme_apa() %>%
  autofit()

#Create a temp file
tmp <- tempfile(fileext = ".docx")

#Create docx file
read_docx() %>%
  body_add_flextable(ftmin) %>%
  print(target = "mintable.docx")

#Create flexable object
ftmin <- flextable(data = combinedminregressions %>%
                     mutate(p = formatC(p, digits = 3, format = "fg"))) %>%
  theme_apa() %>%
  autofit()


# TESTING MODE MODEL ----------------------------------------------

# Testing mode regressions --------------------------------------------

#happy 
happytesting <- lm(happy ~ age + testingmode, data = df)
happytestingEst <- coeftest(happytesting, vcov. = vcovHC(happytesting,  type = "HC1"))
happytestingCI <- coefci(happytesting, vcov. = vcovHC(happytesting, type = "HC1"))

#neutral
neutraltesting <- lm(neutral ~ age + testingmode, data = df)
neutraltestingEst <- coeftest(neutraltesting, vcov. = vcovHC(neutraltesting,  type = "HC1"))
neutraltestingCI <- coefci(neutraltesting, vcov. = vcovHC(neutraltesting, type = "HC1"))

#sad 
sadtesting <- lm(sad ~ age + testingmode, data = df)
sadtestingEst <- coeftest(sadtesting, vcov. = vcovHC(sadtesting,  type = "HC1"))
sadtestingCI <- coefci(sadtesting, vcov. = vcovHC(sadtesting, type = "HC1"))

#angry 
angrytesting <- lm(angry ~ age + testingmode, data = df)
angrytestingEst <- coeftest(angrytesting, vcov. = vcovHC(angrytesting,  type = "HC1"))
angrytestingCI <- coefci(angrytesting, vcov. = vcovHC(angrytesting, type = "HC1"))

#anxious 
anxioustesting <- lm(anxious ~ age + testingmode, data = df)
anxioustestingEst <- coeftest(anxioustesting, vcov. = vcovHC(anxioustesting,  type = "HC1"))
anxioustestingCI <- coefci(anxioustesting, vcov. = vcovHC(anxioustesting, type = "HC1"))

#revolted 
revoltedtesting <- lm(revolted ~ age + testingmode, data = df)
revoltedtestingEst <- coeftest(revoltedtesting, vcov. = vcovHC(revoltedtesting,  type = "HC1"))
revoltedtestingCI <- coefci(revoltedtesting, vcov. = vcovHC(revoltedtesting, type = "HC1"))

#emotion 
emotiontesting <- lm(emotion ~ age + testingmode, data = df)
emotiontestingEst <- coeftest(emotiontesting, vcov. = vcovHC(emotiontesting,  type = "HC1"))
emotiontestingCI <- coefci(emotiontesting, vcov. = vcovHC(emotiontesting, type = "HC1"))

#do 
dotesting <- lm(do ~ age + testingmode, data = df)
dotestingEst <- coeftest(dotesting, vcov. = vcovHC(dotesting,  type = "HC1"))
dotestingCI <- coefci(dotesting, vcov. = vcovHC(dotesting, type = "HC1"))

#think 
thinktesting <- lm(think ~ age + testingmode, data = df)
thinktestingEst <- coeftest(thinktesting, vcov. = vcovHC(thinktesting,  type = "HC1"))
thinktestingCI <- coefci(thinktesting, vcov. = vcovHC(thinktesting, type = "HC1"))

#feel 
feeltesting <- lm(feel ~ age + testingmode, data = df)
feeltestingEst <- coeftest(feeltesting, vcov. = vcovHC(feeltesting,  type = "HC1"))
feeltestingCI <- coefci(feeltesting, vcov. = vcovHC(feeltesting, type = "HC1"))

#RME 
RMEtesting <- lm(RME ~ age + testingmode, data = df)
RMEtestingEst <- coeftest(RMEtesting, vcov. = vcovHC(RMEtesting,  type = "HC1"))
RMEtestingCI <- coefci(RMEtesting, vcov. = vcovHC(RMEtesting, type = "HC1"))

#empathy 
empathytesting <- lm(empathy ~ age + testingmode, data = df)
empathytestingEst <- coeftest(empathytesting, vcov. = vcovHC(empathytesting,  type = "HC1"))
empathytestingCI <- coefci(empathytesting, vcov. = vcovHC(empathytesting, type = "HC1"))

#sarcasm 
sarcasmtesting <- lm(sarcasm ~ age + testingmode, data = df)
sarcasmtestingEst <- coeftest(sarcasmtesting, vcov. = vcovHC(sarcasmtesting,  type = "HC1"))
sarcasmtestingCI <- coefci(sarcasmtesting, vcov. = vcovHC(sarcasmtesting, type = "HC1"))

#sincere 
sinceretesting <- lm(sincere ~ age + testingmode, data = df)
sinceretestingEst <- coeftest(sinceretesting, vcov. = vcovHC(sinceretesting,  type = "HC1"))
sinceretestingCI <- coefci(sinceretesting, vcov. = vcovHC(sinceretesting, type = "HC1"))

#lies 
liestesting <- lm(lies ~ age + testingmode, data = df)
liestestingEst <- coeftest(liestesting, vcov. = vcovHC(liestesting,  type = "HC1"))
liestestingCI <- coefci(liestesting, vcov. = vcovHC(liestesting, type = "HC1"))

#Esarcasm 
Esarcasmtesting <- lm(Esarcasm ~ age + testingmode, data = df)
EsarcasmtestingEst <- coeftest(Esarcasmtesting, vcov. = vcovHC(Esarcasmtesting,  type = "HC1"))
EsarcasmtestingCI <- coefci(Esarcasmtesting, vcov. = vcovHC(Esarcasmtesting, type = "HC1"))



# standardised betas ------------------------------------------------------
#happy
testinghappyS <- lm(happy ~ age + testingmode, data = dfstandard1)
testinghappyBS <- coeftest(testinghappyS , vcov. = vcovHC(testinghappyS, type = "HC1"))

#neutral
testingneutralS <- lm(neutral ~ age + testingmode, data = dfstandard1)
testingneutralBS <- coeftest(testingneutralS , vcov. = vcovHC(testingneutralS, type = "HC1"))

#sad
testingsadS <- lm(sad ~ age + testingmode, data = dfstandard1)
testingsadBS <- coeftest(testingsadS , vcov. = vcovHC(testingsadS, type = "HC1"))

#angry
testingangryS <- lm(angry ~ age + testingmode, data = dfstandard1)
testingangryBS <- coeftest(testingangryS , vcov. = vcovHC(testingangryS, type = "HC1"))

#anxious
testinganxiousS <- lm(anxious ~ age + testingmode, data = dfstandard1)
testinganxiousBS <- coeftest(testinganxiousS , vcov. = vcovHC(testinganxiousS, type = "HC1"))

#revolted
testingrevoltedS <- lm(revolted ~ age + testingmode, data = dfstandard1)
testingrevoltedBS <- coeftest(testingrevoltedS , vcov. = vcovHC(testingrevoltedS, type = "HC1"))

#emotion
testingemotionS <- lm(emotion ~ age + testingmode, data = dfstandard1)
testingemotionBS <- coeftest(testingemotionS , vcov. = vcovHC(testingemotionS, type = "HC1"))

#think
testingthinkS <- lm(think ~ age + testingmode, data = dfstandard1)
testingthinkBS <- coeftest(testingthinkS , vcov. = vcovHC(testingthinkS, type = "HC1"))

#do
testingdoS <- lm(do ~ age + testingmode, data = dfstandard1)
testingdoBS <- coeftest(testingdoS , vcov. = vcovHC(testingdoS, type = "HC1"))

#feel
testingfeelS <- lm(feel ~ age + testingmode, data = dfstandard1)
testingfeelBS <- coeftest(testingfeelS , vcov. = vcovHC(testingfeelS, type = "HC1"))

#RME
testingRMES <- lm(RME ~ age + testingmode, data = dfstandard1)
testingRMEBS <- coeftest(testingRMES , vcov. = vcovHC(testingRMES, type = "HC1"))

#sarcasm
testingsarcasmS <- lm(sarcasm ~ age + testingmode, data = dfstandard1)
testingsarcasmBS <- coeftest(testingsarcasmS , vcov. = vcovHC(testingsarcasmS, type = "HC1"))

#sincere
testingsincereS <- lm(sincere ~ age + testingmode, data = dfstandard1)
testingsincereBS <- coeftest(testingsincereS , vcov. = vcovHC(testingsincereS, type = "HC1"))

#lies
testingliesS <- lm(lies ~ age + testingmode, data = dfstandard1)
testingliesBS <- coeftest(testingliesS , vcov. = vcovHC(testingliesS, type = "HC1"))

#Esarcasm
testingEsarcasmS <- lm(Esarcasm ~ age + testingmode, data = dfstandard1)
testingEsarcasmBS <- coeftest(testingEsarcasmS , vcov. = vcovHC(testingEsarcasmS, type = "HC1"))

#empathy
testingempathyS <- lm(empathy ~ age + testingmode, data = dfstandard1)
testingempathyBS <- coeftest(testingempathyS , vcov. = vcovHC(testingempathyS, type = "HC1"))

# Testing mode table ------------------------------------------------------

#data frames for each regression model 
#happy
happytestregression <- data.frame(Outcome = c("Happy"),
                                  Variable = c("Age", "Mode"), 
                                  R2 = c(summary(happytesting)$r.squared),
                                  B = c(happytestingEst[2:3,1]),
                                  BS = c(testinghappyBS[2:3,1]), 
                                  CILower = c(happytestingCI[2:3,1]),
                                  CIUpper = c(happytestingCI[2:3,2]),
                                  p = c(happytestingEst[2:3,4]))
#neutral
neutraltestregression <- data.frame(Outcome = c("neutral"),
                                    Variable = c("Age", "Mode"), 
                                    R2 = c(summary(neutraltesting)$r.squared),
                                    B = c(neutraltestingEst[2:3,1]),
                                    BS = c(testingneutralBS[2:3,1]), 
                                    CILower = c(neutraltestingCI[2:3,1]),
                                    CIUpper = c(neutraltestingCI[2:3,2]),
                                    p = c(neutraltestingEst[2:3,4]))

#sad
sadtestregression <- data.frame(Outcome = c("sad"),
                                Variable = c("Age", "Mode"), 
                                R2 = c(summary(sadtesting)$r.squared),
                                B = c(sadtestingEst[2:3,1]),
                                BS = c(testingsadBS[2:3,1]), 
                                CILower = c(sadtestingCI[2:3,1]),
                                CIUpper = c(sadtestingCI[2:3,2]),
                                p = c(sadtestingEst[2:3,4]))

#angry
angrytestregression <- data.frame(Outcome = c("angry"),
                                  Variable = c("Age", "Mode"), 
                                  R2 = c(summary(angrytesting)$r.squared),
                                  B = c(angrytestingEst[2:3,1]),
                                  BS = c(testingangryBS[2:3,1]), 
                                  CILower = c(angrytestingCI[2:3,1]),
                                  CIUpper = c(angrytestingCI[2:3,2]),
                                  p = c(angrytestingEst[2:3,4]))

#anxious
anxioustestregression <- data.frame(Outcome = c("anxious"),
                                    Variable = c("Age", "Mode"), 
                                    R2 = c(summary(anxioustesting)$r.squared),
                                    B = c(anxioustestingEst[2:3,1]),
                                    BS = c(testinganxiousBS[2:3,1]), 
                                    CILower = c(anxioustestingCI[2:3,1]),
                                    CIUpper = c(anxioustestingCI[2:3,2]),
                                    p = c(anxioustestingEst[2:3,4]))

#revolted
revoltedtestregression <- data.frame(Outcome = c("revolted"),
                                     Variable = c("Age", "Mode"), 
                                     R2 = c(summary(revoltedtesting)$r.squared),
                                     B = c(revoltedtestingEst[2:3,1]),
                                     BS = c(testingrevoltedBS[2:3,1]), 
                                     CILower = c(revoltedtestingCI[2:3,1]),
                                     CIUpper = c(revoltedtestingCI[2:3,2]),
                                     p = c(revoltedtestingEst[2:3,4]))

#emotion
emotiontestregression <- data.frame(Outcome = c("emotion"),
                                    Variable = c("Age", "Mode"), 
                                    R2 = c(summary(emotiontesting)$r.squared),
                                    B = c(emotiontestingEst[2:3,1]),
                                    BS = c(testingemotionBS[2:3,1]), 
                                    CILower = c(emotiontestingCI[2:3,1]),
                                    CIUpper = c(emotiontestingCI[2:3,2]),
                                    p = c(emotiontestingEst[2:3,4]))

#think
thinktestregression <- data.frame(Outcome = c("think"),
                                  Variable = c("Age", "Mode"), 
                                  R2 = c(summary(thinktesting)$r.squared),
                                  B = c(thinktestingEst[2:3,1]),
                                  BS = c(testingthinkBS[2:3,1]), 
                                  CILower = c(thinktestingCI[2:3,1]),
                                  CIUpper = c(thinktestingCI[2:3,2]),
                                  p = c(thinktestingEst[2:3,4]))

#do
dotestregression <- data.frame(Outcome = c("do"),
                               Variable = c("Age", "Mode"), 
                               R2 = c(summary(dotesting)$r.squared),
                               B = c(dotestingEst[2:3,1]),
                               BS = c(testingdoBS[2:3,1]), 
                               CILower = c(dotestingCI[2:3,1]),
                               CIUpper = c(dotestingCI[2:3,2]),
                               p = c(dotestingEst[2:3,4]))

#feel
feeltestregression <- data.frame(Outcome = c("feel"),
                                 Variable = c("Age", "Mode"), 
                                 R2 = c(summary(feeltesting)$r.squared),
                                 B = c(feeltestingEst[2:3,1]),
                                 BS = c(testingfeelBS[2:3,1]), 
                                 CILower = c(feeltestingCI[2:3,1]),
                                 CIUpper = c(feeltestingCI[2:3,2]),
                                 p = c(feeltestingEst[2:3,4]))

#RME
RMEtestregression <- data.frame(Outcome = c("RME"),
                                Variable = c("Age", "Mode"), 
                                R2 = c(summary(RMEtesting)$r.squared),
                                B = c(RMEtestingEst[2:3,1]),
                                BS = c(testingRMEBS[2:3,1]), 
                                CILower = c(RMEtestingCI[2:3,1]),
                                CIUpper = c(RMEtestingCI[2:3,2]),
                                p = c(RMEtestingEst[2:3,4]))

#sarcasm
sarcasmtestregression <- data.frame(Outcome = c("sarcasm"),
                                    Variable = c("Age", "Mode"), 
                                    R2 = c(summary(sarcasmtesting)$r.squared),
                                    B = c(sarcasmtestingEst[2:3,1]),
                                    BS = c(testingsarcasmBS[2:3,1]), 
                                    CILower = c(sarcasmtestingCI[2:3,1]),
                                    CIUpper = c(sarcasmtestingCI[2:3,2]),
                                    p = c(sarcasmtestingEst[2:3,4]))

#sincere
sinceretestregression <- data.frame(Outcome = c("sincere"),
                                    Variable = c("Age", "Mode"), 
                                    R2 = c(summary(sinceretesting)$r.squared),
                                    B = c(sinceretestingEst[2:3,1]),
                                    BS = c(testingsincereBS[2:3,1]), 
                                    CILower = c(sinceretestingCI[2:3,1]),
                                    CIUpper = c(sinceretestingCI[2:3,2]),
                                    p = c(sinceretestingEst[2:3,4]))

#lies
liestestregression <- data.frame(Outcome = c("lies"),
                                 Variable = c("Age", "Mode"), 
                                 R2 = c(summary(liestesting)$r.squared),
                                 B = c(liestestingEst[2:3,1]),
                                 BS = c(testingliesBS[2:3,1]), 
                                 CILower = c(liestestingCI[2:3,1]),
                                 CIUpper = c(liestestingCI[2:3,2]),
                                 p = c(liestestingEst[2:3,4]))

#Esarcasm
Esarcasmtestregression <- data.frame(Outcome = c("Esarcasm"),
                                     Variable = c("Age", "Mode"), 
                                     R2 = c(summary(Esarcasmtesting)$r.squared),
                                     B = c(EsarcasmtestingEst[2:3,1]),
                                     BS = c(testingEsarcasmBS[2:3,1]), 
                                     CILower = c(EsarcasmtestingCI[2:3,1]),
                                     CIUpper = c(EsarcasmtestingCI[2:3,2]),
                                     p = c(EsarcasmtestingEst[2:3,4]))

#empathy
empathytestregression <- data.frame(Outcome = c("empathy"),
                                    Variable = c("Age", "Mode"), 
                                    R2 = c(summary(empathytesting)$r.squared),
                                    B = c(empathytestingEst[2:3,1]),
                                    BS = c(testingempathyBS[2:3,1]), 
                                    CILower = c(empathytestingCI[2:3,1]),
                                    CIUpper = c(empathytestingCI[2:3,2]),
                                    p = c(empathytestingEst[2:3,4]))


#Combine all regression data frames together
combinedtestregressions <- rbind(happytestregression, neutraltestregression, sadtestregression, angrytestregression, 
                                 anxioustestregression, revoltedtestregression, emotiontestregression, thinktestregression, 
                                 dotestregression, feeltestregression, RMEtestregression, sarcasmtestregression, sinceretestregression,
                                 liestestregression, Esarcasmtestregression, empathytestregression)

#Create flexable object
fttest <- flextable(data = combinedtestregressions %>%
                      mutate(p = sprintf("%.3g", p))) %>%
  theme_apa() %>%
  autofit()

#Create a temp file
tmp <- tempfile(fileext = ".docx")

#Create docx file
read_docx() %>%
  body_add_flextable(fttest) %>%
  print(target = "testtable.docx")

#Create flexable object
fttest <- flextable(data = combinedtestregressions %>%
                      mutate(p = formatC(p, digits = 3, format = "fg"))) %>%
  theme_apa() %>%
  autofit()

