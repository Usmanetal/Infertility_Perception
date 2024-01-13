---
title: "Infertility Perception"
author: "Usman"
date: "2024-01-07"
output:
  pdf_document:
    toc: yes
  html_document:
    toc: yes
    number_sections: yes
---



## R Markdown

### **Initiation possible Statistical Analysis for Infertility Perception**


## **Including Plots and Tables of Interest**



```r
Perception_propt%>%count(SECTION.A..SOCIO.DEMOGRAPHIC)%>%
  mutate(P_Value=
              recode(SECTION.A..SOCIO.DEMOGRAPHIC,"26-35"="<0.001",
                     "36-45"="<0.001","<25 years"="<0.001",">45"="<0.001"))
```

```
##   SECTION.A..SOCIO.DEMOGRAPHIC  n P_Value
## 1                        26-35 88  <0.001
## 2                        36-45 93  <0.001
## 3                    <25 years 36  <0.001
## 4                          >45 29  <0.001
```

### **Multiple Correspondence Analysis (MCA) of respondent to identify similarities or differences


```r
# Multiple Correspondence Analysis (MCA)

Perception_propt[,c(2:26)]%>%MCA(ncp=2,graph=FALSE)%>%
  fviz_mca_biplot(geom="point",repel=TRUE,ggtheme=theme_minimal())
```

![](Infertility-Perception-Analysis_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

## **Again: preliminary test analysis


```r
# Preliminary socio-demographics 

newdat<-Perception_propt%>%mutate(Duration_infertility=
    X6..Duration.of.infertility,yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))

Perception_propt%>%mutate(Duration_infertility=
  X6..Duration.of.infertility,yes_no=ifelse(
  Duration_infertility=="Nil","Fertile","Infertile"))%>%group_by(yes_no)%>%
  count(age=SECTION.A..SOCIO.DEMOGRAPHIC)%>%
  pivot_wider(names_from = age,values_from = n)%>%
  column_to_rownames(var="yes_no")%>%mutate(p_value="=0.04")
```

```
##           26-35 36-45 <25 years >45 p_value
## Fertile      30    22        13   3   =0.04
## Infertile    58    71        23  26   =0.04
```

```r
# Logistic Regression Analysis
glm(factor(yes_no)~SECTION.A..SOCIO.DEMOGRAPHIC+X2..Gender+X5..Level.of.education,data = newdat,family = "binomial")%>%summary()
```

```
## 
## Call:
## glm(formula = factor(yes_no) ~ SECTION.A..SOCIO.DEMOGRAPHIC + 
##     X2..Gender + X5..Level.of.education, family = "binomial", 
##     data = newdat)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1688  -1.2295   0.6655   0.8758   1.1262  
## 
## Coefficients:
##                                   Estimate Std. Error z value Pr(>|z|)   
## (Intercept)                         0.7686     1.1864   0.648  0.51709   
## SECTION.A..SOCIO.DEMOGRAPHIC>45     2.2133     0.7639   2.897  0.00377 **
## SECTION.A..SOCIO.DEMOGRAPHIC26-35   0.4432     0.4574   0.969  0.33257   
## SECTION.A..SOCIO.DEMOGRAPHIC36-45   1.0777     0.4821   2.236  0.02538 * 
## X2..GenderMale                     -0.6386     0.3428  -1.863  0.06246 . 
## X5..Level.of.educationPrimary      -0.6675     1.5263  -0.437  0.66185   
## X5..Level.of.educationSecondary     0.4055     1.2627   0.321  0.74808   
## X5..Level.of.educationTertiary     -0.4514     1.2017  -0.376  0.70721   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 290.06  on 245  degrees of freedom
## Residual deviance: 274.05  on 238  degrees of freedom
## AIC: 290.05
## 
## Number of Fisher Scoring iterations: 4
```

### Final


```r
Do_Know_Infertility_Start_1Year<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
  yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%
  count(age=X7..Do.you.know.that.infertility.starts.to.count.after.1.year.of.unprotected.sexual.intercourse.with.an.opposite.sex.partner.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.32",""))

Who_Can_Infertile<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
  yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X8..Who.do.you.think.can.be.infertile)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.74","",""))

Who_is_To_Blamed<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X9..Who.is.being.blamed.for.infertility)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.03","","",""))

Primary_Infertility_Can_Affect_Who<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
  yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X10..Primary.infertility.can.affect.who)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(Fertile=str_replace_na(Fertile,"0"))%>%
  mutate(p_value=c("0.55","",""))
Primary_Infertility_Can_Affect_Who$Fertile<-as.integer(Primary_Infertility_Can_Affect_Who$Fertile)

Secondary_Infertility_can_Affect_Who<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
 yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X11..Secondary.infertility.can.affect.who)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.50","",""))


Can_Infertility_Treated<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X14..Do.you.think.infertility.can.and.should.be.treated.medically.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.60","",""))

Causes_of_Infertility<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X15..Who.do.you.think.should.go.for.laboratory.investigation.before.treatment.can.start.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(Fertile=str_replace_na(Fertile,"0"),p_value=c("0.48","",""))
Causes_of_Infertility$Fertile<-as.integer(Causes_of_Infertility$Fertile)

Whom_Would_You_Goto<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X16..Whom.would.you.go.to.for.your.treatment.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(Fertile=str_replace_na(Fertile,"0"),
                                         p_value=c("0.27","","","",""))
Whom_Would_You_Goto$Fertile<-as.integer(Whom_Would_You_Goto$Fertile)

Social_Acceptability_to_Abortion<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X19..Do.you.think.it.is.socially.acceptable.to.have.a.baby.through.surrogacy.in.Nigeria.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.015","",""))

Social_Acceptability_to_IVF<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X20..Do.you.think.it.is.socially.acceptable.to.have.a.baby.through.In.vitro.fertilization.in.Nigeria.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.90","",""))

Negativity_Infertility_on_Gender<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X21..Infertility.has.more.negative.effect.on.who.more.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.02","",""))


Social_Effect_of_Infertility_On_Gathering<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
         yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=X23..Do.staying.in.a.gathering.with.people.who.have.a.child.or.children.affect.one.s.social.health.)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.43","",""))

bind_rows(Do_Know_Infertility_Start_1Year=Do_Know_Infertility_Start_1Year,
          Who_Can_Infertile=Who_Can_Infertile,
          Who_is_To_Blamed=Who_is_To_Blamed,
          Primary_Infertility_Can_Affect_Who=Primary_Infertility_Can_Affect_Who,
          Secondary_Infertility_can_Affect_Who=Secondary_Infertility_can_Affect_Who,
          Can_Infertility_Treated=Can_Infertility_Treated,
          Causes_of_Infertility=Causes_of_Infertility,
          Whom_Would_You_Goto=Whom_Would_You_Goto,
          Social_Acceptability_to_Abortion=Social_Acceptability_to_Abortion,
          Social_Acceptability_to_IVF=Social_Acceptability_to_IVF,
          Negativity_Infertility_on_Gender=Negativity_Infertility_on_Gender,
          Social_Effect_of_Infertility_On_Gathering=Social_Effect_of_Infertility_On_Gathering,
          .id = "Variable")
```

```
##                                                          Variable Fertile
## No...1                            Do_Know_Infertility_Start_1Year      26
## Yes...2                           Do_Know_Infertility_Start_1Year      45
## Both men and women...3                          Who_Can_Infertile      62
## Men...4                                         Who_Can_Infertile       2
## Women...5                                       Who_Can_Infertile       7
## Both Husband and wife                            Who_is_To_Blamed      22
## Husband                                          Who_is_To_Blamed       1
## Neither Husband or wife                          Who_is_To_Blamed       9
## Wife                                             Who_is_To_Blamed      39
## Both men and wome              Primary_Infertility_Can_Affect_Who      58
## Women...11                     Primary_Infertility_Can_Affect_Who      13
## Men...12                       Primary_Infertility_Can_Affect_Who       0
## Both men and women...13      Secondary_Infertility_can_Affect_Who      55
## Men...14                     Secondary_Infertility_can_Affect_Who       3
## Women...15                   Secondary_Infertility_can_Affect_Who      13
## No...16                                   Can_Infertility_Treated       3
## Not sure...17                             Can_Infertility_Treated       9
## Yes...18                                  Can_Infertility_Treated      59
## Both men and women...19                     Causes_of_Infertility      67
## Women...20                                  Causes_of_Infertility       4
## Men...21                                    Causes_of_Infertility       0
## Faith healers                                 Whom_Would_You_Goto       1
## Gynaecologist                                 Whom_Would_You_Goto      63
## Herbalist                                     Whom_Would_You_Goto       1
## Others:                                       Whom_Would_You_Goto       6
## Self treatment                                Whom_Would_You_Goto       0
## No...27                          Social_Acceptability_to_Abortion      34
## Not sure...28                    Social_Acceptability_to_Abortion      14
## Yes...29                         Social_Acceptability_to_Abortion      23
## No...30                               Social_Acceptability_to_IVF      10
## Not sure...31                         Social_Acceptability_to_IVF      16
## Yes...32                              Social_Acceptability_to_IVF      45
## Both men and women...33          Negativity_Infertility_on_Gender      22
## Men...34                         Negativity_Infertility_on_Gender       4
## Women...35                       Negativity_Infertility_on_Gender      45
## No...36                 Social_Effect_of_Infertility_On_Gathering      14
## Not sure...37           Social_Effect_of_Infertility_On_Gathering      14
## Yes...38                Social_Effect_of_Infertility_On_Gathering      43
##                         Infertile p_value
## No...1                         78    0.32
## Yes...2                        97        
## Both men and women...3        157    0.74
## Men...4                         3        
## Women...5                      15        
## Both Husband and wife          63    0.03
## Husband                         2        
## Neither Husband or wife         5        
## Wife                          105        
## Both men and wome             146    0.55
## Women...11                     26        
## Men...12                        3        
## Both men and women...13       136    0.50
## Men...14                        3        
## Women...15                     36        
## No...16                         5    0.60
## Not sure...17                  17        
## Yes...18                      153        
## Both men and women...19       160    0.48
## Women...20                     10        
## Men...21                        5        
## Faith healers                   5    0.27
## Gynaecologist                 162        
## Herbalist                       1        
## Others:                         5        
## Self treatment                  2        
## No...27                        99   0.015
## Not sure...28                  48        
## Yes...29                       28        
## No...30                        21    0.90
## Not sure...31                  41        
## Yes...32                      113        
## Both men and women...33        80    0.02
## Men...34                        2        
## Women...35                     93        
## No...36                        23    0.43
## Not sure...37                  37        
## Yes...38                      115
```

### **Table 2, 3, and 4**


```r
age<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),yes_no=ifelse(
   Duration_infertility=="Nil","Fertile","Infertile"))%>%group_by(yes_no)%>%
  count(age=SECTION.A..SOCIO.DEMOGRAPHIC)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("=0.04","","",""))

Gender<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),yes_no=ifelse(
 Duration_infertility=="Nil","Fertile","Infertile"))%>%group_by(yes_no)%>%
  count(age=X2..Gender)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.58",""))

Religion<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),yes_no=ifelse(
   Duration_infertility=="Nil","Fertile","Infertile"))%>%group_by(yes_no)%>%
  count(age=X3..Religion)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.51",""))

Occupation<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),yes_no=ifelse(
  Duration_infertility=="Nil","Fertile","Infertile"))%>%group_by(yes_no)%>%
  count(age=X4..Occupation)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(Fertile=str_replace_na(Fertile,"0"))%>%
  mutate(p_value=c("=0.03","","","","",""))
Occupation$Fertile<-as.integer(Occupation$Fertile)
Occupation$Infertile<-as.integer(Occupation$Infertile)

Level_Education<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),yes_no=ifelse(
  Duration_infertility=="Nil","Fertile","Infertile"))%>%group_by(yes_no)%>%
  count(age=X5..Level.of.education)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%mutate(p_value=c("0.48","","",""))

Duration_of_Infertility<-Perception_propt%>%
  mutate(Duration_infertility=recode(X6..Duration.of.infertility,"Nil:"="Nil"),
  yes_no=ifelse(Duration_infertility=="Nil","Fertile","Infertile"))%>%
  group_by(yes_no)%>%count(age=Duration_infertility)%>%
  pivot_wider(names_from = yes_no,values_from = n)%>%
  column_to_rownames(var="age")%>%
  mutate(Fertile=str_replace_na(Fertile,"0"),
  Infertile=str_replace_na(Infertile,"0"))%>%
  mutate(p_value=c("<0.001","","","",""))
Duration_of_Infertility$Fertile<-as.integer(Duration_of_Infertility$Fertile)
Duration_of_Infertility$Infertile<-as.integer(Duration_of_Infertility$Infertile)
  Fertile<-as.integer(Duration_of_Infertility$Fertile)
bind_rows(Age=age,
      Gender=Gender,
      Religion=Religion,Occupation=Occupation,Level_Education=Level_Education,
      Duration_of_Infertility=Duration_of_Infertility,.id = "Variable")
```

```
##                                             Variable Fertile Infertile p_value
## 26-35                                            Age      30        58   =0.04
## 36-45                                            Age      24        69        
## <25 years                                        Age      13        23        
## >45                                              Age       4        25        
## Female                                        Gender      45       119    0.58
## Male                                          Gender      26        56        
## Christian                                   Religion      41        90    0.51
## Muslim                                      Religion      30        85        
## Civil servant                             Occupation       2         4   =0.03
## Civil servant: Public sector              Occupation      18        60        
## Private sector                            Occupation      31        47        
## Self employed                             Occupation      11        40        
## Student                                   Occupation       6         9        
## Unemployed                                Occupation       3        15        
## Informal                             Level_Education       1         3    0.48
## Primary                              Level_Education       2         3        
## Secondary                            Level_Education       6        28        
## Tertiary                             Level_Education      62       141        
## Nil                          Duration_of_Infertility      71         0  <0.001
## 1-5 years                    Duration_of_Infertility       0        98        
## 11-15 years                  Duration_of_Infertility       0        17        
## 16-20 years                  Duration_of_Infertility       0        10        
## 6-10 years                   Duration_of_Infertility       0        50
```

```r
# Table 2 Knowledge and common misconceptions about factors that 
# may affect sterility

# Common missconception about infertility
Common_MisConcept_About_Infertility<-Perception_propt%>%
  separate(X13..Common.misconception.about.the.causes.of.infertility...Tick.as.many.as.apply.,c("an1","an2","an3","an4"),sep = ";")%>%
  select(an1,an2,an3,an4)%>%head(10)
```

```
## Warning: Expected 4 pieces. Additional pieces discarded in 10 rows [19, 26, 36, 51, 58,
## 59, 124, 173, 237, 238].
```

```
## Warning: Expected 4 pieces. Missing pieces filled with `NA` in 191 rows [3, 4, 5, 6, 7,
## 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 23, 25, 27, ...].
```

```r
Common_MisConcept_About_Infertility
```

```
##             an1          an2                            an3
## 1       Natural    Spiritual                    Black magic
## 2  Supernatural    Spiritual                    Black magic
## 3     Spiritual         <NA>                           <NA>
## 4     Spiritual  Black magic Curses by ancestors or deities
## 5       Natural Supernatural                      Spiritual
## 6       Natural    Spiritual                    Black magic
## 7     Spiritual  Black magic Curses by ancestors or deities
## 8       Natural Supernatural                    Black magic
## 9     Spiritual  Black magic        Curses from individuals
## 10    Spiritual  Black magic                           <NA>
##                               an4
## 1  Curses by ancestors or deities
## 2  Curses by ancestors or deities
## 3                            <NA>
## 4                            <NA>
## 5                            <NA>
## 6                            <NA>
## 7                            <NA>
## 8  Curses by ancestors or deities
## 9                            <NA>
## 10                           <NA>
```

```r
# Causes of Infertility Known by Respondent

Causes_Infertility_Known<-Perception_propt%>%
  separate(X12..What.are.the.causes.of.infertility.that.you.know..Tick.as.many.as.apply.,c("an1","an2","an3","an4","an5","an6","an7","an8","an9","an10","an11"),sep = ";")%>%
  select(an1,an2,an3,an4,an5,an6,an7,an8,an9,an10,an11)%>%head(10)
```

```
## Warning: Expected 11 pieces. Missing pieces filled with `NA` in 233 rows [3, 5, 6, 7, 8,
## 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, ...].
```

```r
Causes_Infertility_Known
```

```
##                            an1                                            an2
## 1    Hormonal imbalance in Men                    Hormonal imbalance in women
## 2  Hormonal imbalance in women History of infection of genital tract in women
## 3    Hormonal imbalance in Men                    Hormonal imbalance in women
## 4    Hormonal imbalance in Men                    Hormonal imbalance in women
## 5    Hormonal imbalance in Men                    Hormonal imbalance in women
## 6  Hormonal imbalance in women  History of infection of genital tract  in men
## 7    Hormonal imbalance in Men                    Hormonal imbalance in women
## 8    Hormonal imbalance in Men                    Hormonal imbalance in women
## 9    Hormonal imbalance in men                    Hormonal imbalance in women
## 10   Hormonal imbalance in men                    Hormonal imbalance in women
##                                               an3
## 1   History of infection of genital tract  in men
## 2                                         Smoking
## 3   History of infection of genital tract  in men
## 4   History of infection of genital tract  in men
## 5   History of infection of genital tract  in men
## 6  History of infection of genital tract in women
## 7   History of infection of genital tract  in men
## 8   History of infection of genital tract  in men
## 9   History of infection of genital tract  in men
## 10  History of infection of genital tract  in men
##                                               an4
## 1  History of infection of genital tract in women
## 2                            Environmental factor
## 3  History of infection of genital tract in women
## 4  History of infection of genital tract in women
## 5  History of infection of genital tract in women
## 6                                         Smoking
## 7  History of infection of genital tract in women
## 8  History of infection of genital tract in women
## 9  History of infection of genital tract in women
## 10 History of infection of genital tract in women
##                                       an5                          an6
## 1                                 Smoking         Environmental factor
## 2  Use of family planning device by women         Psychological stress
## 3                                 Smoking         Environmental factor
## 4  Use of family planning device by women         Psychological stress
## 5                    Environmental factor         Psychological stress
## 6  Use of family planning device by women         Psychological stress
## 7  Use of family planning device by women         Psychological stress
## 8  Use of family planning device by women Obesity in both men and wome
## 9                            Blocked tube                        Drugs
## 10 Use of family planning device by women        Natural (will of God)
##                             an7                          an8
## 1          Psychological stress Obesity in both men and wome
## 2  Obesity in both men and wome        Natural (will of God)
## 3  Obesity in both men and wome                 Blocked tube
## 4  Obesity in both men and wome        Natural (will of God)
## 5  Obesity in both men and wome        Natural (will of God)
## 6  Obesity in both men and wome        Natural (will of God)
## 7  Obesity in both men and wome        Natural (will of God)
## 8         Natural (will of God)       Rhesus incompatibility
## 9                          <NA>                         <NA>
## 10                 Blocked tube                        Drugs
##                       an9         an10  an11
## 1   Natural (will of God) Blocked tube Drugs
## 2  Rhesus incompatibility Blocked tube Drugs
## 3                   Drugs         <NA>  <NA>
## 4  Rhesus incompatibility Blocked tube Drugs
## 5            Blocked tube         <NA>  <NA>
## 6  Rhesus incompatibility Blocked tube  <NA>
## 7            Blocked tube        Drugs  <NA>
## 8            Blocked tube        Drugs  <NA>
## 9                    <NA>         <NA>  <NA>
## 10                   <NA>         <NA>  <NA>
```

```r
# Awareness of Hormonal Laboratory Investigation in Treatment of Infertility

Awareness_of_Hormonal_Laboratory_Investigation<-Perception_propt%>%
  separate(X17..Are.you.aware.of.these.hormonal.laboratory.investigations.that.can.be.conducted.for.infertility.which.aids.in.the.treatment.in.both.men.and.women...Tick.as.many.as.apply.,c("an1","an2","an3","an4","an5","an6","an7"),sep = ";")%>%
  select(an1,an2,an3,an4,an5,an6,an7)%>%head(10)
```

```
## Warning: Expected 7 pieces. Missing pieces filled with `NA` in 216 rows [1, 2, 3, 4, 6,
## 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 20, 21, 22, 23, 24, ...].
```

```r
Awareness_of_Hormonal_Laboratory_Investigation
```

```
##                                   an1                                an2
## 1            Leutinizing Hormone (LH)                          Prolactin
## 2  Follicle Stimulating Hormone (FSH)                          Prolactin
## 3            Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
## 4  Follicle Stimulating Hormone (FSH)                           Estrogen
## 5            Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
## 6            Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
## 7            Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
## 8            Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
## 9            Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
## 10           Leutinizing Hormone (LH) Follicle Stimulating Hormone (FSH)
##             an3          an4          an5                          an6
## 1      Estrogen Progesterone Testosterone Anti-Mullerian hormone (AMH)
## 2      Estrogen Progesterone Testosterone                         <NA>
## 3     Prolactin     Estrogen Progesterone                 Testosterone
## 4  Progesterone         <NA>         <NA>                         <NA>
## 5     Prolactin     Estrogen Progesterone                 Testosterone
## 6     Prolactin Testosterone         <NA>                         <NA>
## 7     Prolactin     Estrogen Progesterone                 Testosterone
## 8      Estrogen Progesterone Testosterone                         <NA>
## 9          <NA>         <NA>         <NA>                         <NA>
## 10    Prolactin     Estrogen Progesterone                 Testosterone
##                             an7
## 1                          <NA>
## 2                          <NA>
## 3                          <NA>
## 4                          <NA>
## 5  Anti-Mullerian hormone (AMH)
## 6                          <NA>
## 7  Anti-Mullerian hormone (AMH)
## 8                          <NA>
## 9                          <NA>
## 10                         <NA>
```

```r
Feeling_After_Failing_Conception<-Perception_propt%>%
  separate(X22..How.do.you.feel.when.you.are.not.able.to.conceive.after.1.year.of.unprotected.sexual.intercourse.with.your.partner..Tick.as.many.as.apply.,c("an1","an2","an3","an4","an5"),sep = ";")%>%
  select(an1,an2,an3,an4,an5)%>%head(10)
```

```
## Warning: Expected 5 pieces. Missing pieces filled with `NA` in 235 rows [1, 2, 3, 4, 6,
## 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 20, 21, 22, 23, 24, ...].
```

```r
Feeling_After_Failing_Conception
```

```
##          an1       an2      an3      an4              an5
## 1        Sad Depressed  Anxious Distress             <NA>
## 2        Sad Depressed  Anxious Distress             <NA>
## 3    Anxious      <NA>     <NA>     <NA>             <NA>
## 4        Sad Depressed  Anxious Distress             <NA>
## 5        Sad Depressed  Anxious Distress Suicidal thought
## 6        Sad   Anxious     <NA>     <NA>             <NA>
## 7        Sad Depressed  Anxious Distress             <NA>
## 8  Depressed      <NA>     <NA>     <NA>             <NA>
## 9        Sad Depressed  Anxious Distress             <NA>
## 10       Sad   Anxious Distress     <NA>             <NA>
```

