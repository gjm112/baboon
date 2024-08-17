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

library(corrplot)
m <- cor(scott[,c("BM_cast","BD_cast","LM_cast","LD_cast")])
corrplot(m, method = 'square', order = 'AOE', addCoef.col = 'white', tl.pos = 'd')

m <- cor(scott[,c("BM_ost","BD_ost","LM_ost","LD_ost")])
corrplot(m, method = 'square', order = 'AOE', addCoef.col = 'white', tl.pos = 'd')

m <- cor(scott[,c("BM_cast","BD_cast","LM_cast","LD_cast","BM_ost","BD_ost","LM_ost","LD_ost")])
corrplot(m, method = 'square', order = 'AOE', addCoef.col = 'white')


scott_long <- scott %>% pivot_longer(cols = BM_cast:LD_ost) %>% mutate(
  BL = substring(name, 1, 1),
  MD = substring(name, 2, 2),
  BLMD = substring(name, 1, 2),
  castost = substring(name, 4, nchar(name)),
  age = ifelse(castost == "cast",age_cast,age_death)
)

start <- scott_long %>% filter(castost == "ost") %>% mutate(value = 1, age = 1)

scott_long <- rbind(start,scott_long)

#summary(scott)
ggplot() + geom_path(alpha = 0.5,aes(
  x = age,
  y = value,
  group = id,
  color = sex
), data = scott_long) + geom_point(alpha = 0.5,aes(
  x = age,
  y = value,
  group = id,
  color = sex
), data = scott_long) + geom_smooth(aes(
  x = age,
  y = value,
  color = sex
), data = scott_long, color = "black") + facet_grid(BL~MD + sex)

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


#t-tests 
t.test(scott$BM_cast, scott$BM_ost, paired = TRUE)
t.test(scott$BD_cast, scott$BD_ost, paired = TRUE)
t.test(scott$LM_cast, scott$LM_ost, paired = TRUE)
t.test(scott$LD_cast, scott$LD_ost, paired = TRUE)



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

scott <- scott %>% mutate(ageyear = age/12)

#Modeling from aging curves
a <- lmer(value ~ 0 + BLMD + BLMD:(0+age +I(age^2)) + (0+BLMD|id),
                                    data = scott_long,
                                    control=lmerControl(
                                      optimizer="bobyqa",
                                      check.nobs.vs.nlev="ignore",
                                      check.nobs.vs.nRE="ignore"))
# modlist_quadratic[[m]] <- lmer(value ~ 0 + stat + stat:(0+AGE_30 + AGE2_30) + (0+stat|BATTER_KEY),
#                                data = dat %>% filter(.imp == m) , 
#                                weights = PA, 
#                                control=lmerControl(
#                                  optimizer="bobyqa",
#                                  check.nobs.vs.nlev="ignore",
#                                  check.nobs.vs.nRE="ignore"))


#Format the data for STAN
cast <- scott %>% select(
  id,
  sex,
  age = age_cast,
  BM = BM_cast,
  BD = BD_cast,
  LM = LM_cast,
  LD = LD_cast
) %>% mutate(ostcast = "cast", idrank = rank(id))

ost <- scott %>% select(
  id,
  sex,
  age = age_death,
  BM = BM_ost,
  BD = BD_ost,
  LM = LM_ost,
  LD = LD_ost
) %>% mutate(ostcast = "ost", idrank = rank(id))

dat <- rbind(cast,ost)
dat %>% arrange(id)


dat <- dat %>% mutate(I_female = ifelse(sex == "female",1,0))

Y <- dat %>% select(BM, BD, LM, LD) %>% as.matrix()
D <- ncol(y)
N <- nrow(y)
X <- dat %>% select(I_female,age) %>% as.matrix()
X <- cbind(1,X)
K <- ncol(X)
z <- dat$idrank
G <- max(z)

library(rstan)


model = stan_model("baboon2.stan")

fit = sampling(model,list(N=N,K=K, G=G, D=D,Y=Y,X=X, z = z),iter=200,chains=4)

print(fit)

params = extract(fit)
hist(params$beta[,1,1])

cov2cor(params$Sigma[1,,])
