library(tidyverse)
scott <- read.csv("/Users/gregorymatthews/Dropbox/baboon_git/scottGreg.csv")
names(scott) <-
  c(
    "id",
    "sex",
    "age_cast",
    "age_death",
    "sfbrid",
    "leftright",
    "BM_cast",
    "BD_cast",
    "LM_cast",
    "LD_cast",
    "BM_ost",
    "BD_ost",
    "LM_ost",
    "LD_ost"
  )

cor(scott[,c("BM_cast","BD_cast","LM_cast","LD_cast")])
cor(scott[,c("BM_ost","BD_ost","LM_ost","LD_ost")])

scott <- scott %>% pivot_longer(cols = BM_cast:LD_ost) %>% mutate(
  BL = substring(name, 1, 1),
  MD = substring(name, 2, 2),
  BLMD = substring(name, 1, 2),
  castost = substring(name, 4, nchar(name)),
  age = ifelse(castost == "cast",age_cast,age_death)
)

start <- scott %>% filter(castost == "ost") %>% mutate(value = 1, age = 1)

scott <- rbind(start,scott)

summary(scott)
ggplot() + geom_path(alpha = 0.5,aes(
  x = age,
  y = value,
  group = id,
  color = sex
), data = scott) + geom_point(alpha = 0.5,aes(
  x = age,
  y = value,
  group = id,
  color = sex
), data = scott) + geom_smooth(aes(
  x = age,
  y = value,
  color = sex
), data = scott, color = "black") + facet_grid(BL~MD + sex)

ggplot(aes(
  x = age,
  y = value,
  color = sex
), data = scott) + geom_point() +geom_smooth() + facet_grid(BL~MD + sex)

ggplot(aes(
  x = age,
  y = value,
  color = sex
), data = scott) + geom_point() +geom_smooth(formula = y ~ log(x),method = "lm") + facet_grid(BL~MD + sex)

ggplot(aes(
  x = (age),
  y = (value),
  color = sex
), data = scott) + geom_point() +geom_smooth() + facet_grid(BL~MD + sex)



scott <- scott %>% mutate(ageyear = age/12)
#Modeling
library(lme4)
a <- lm(value ~ age + sex, data = scott)
a <- lm(log(value) ~ log(age)*sex + BLMD, data = scott)
plot(scott$age,exp(fitted(a)))
plot(a)
z <- lmer((value) ~ (age)+sex+BLMD + (1|id), data= scott)
a <- lmer(log(value) ~ log(age)*sex*BLMD  + (1|id), data= scott)
a2 <- lmer(log(value) ~ (log(age)+sex+BLMD)^2  + (1|id), data= scott)
alm <- lm(log(value) ~ log(age)*sex*BLMD , data= scott)
b <- lmer(log(value) ~ log(age)+sex+BLMD + (1|id), data= scott)
AIC(a,a2,alm,b,z)
plot(scott$age,exp(predict(a, re.form = NA)))
points(scott$age, scott$value, col = "red")

d <-lmer(log(value) ~ (log(ageyear)*sex) * BLMD + 0 + (1 |id), data = scott)
AIC(a)
AIC(a,alm,b,z,d)

c(10,100,0,1,0,0,0,0,0,0,0,0,0,0,0,0) %*% vcov(a) %*% c(10,100,0,1,0,1,0,0,10,0,0,100,0,0,1,0)

rep(10,16) %*% vcov(a) %*% rep(10,16)




