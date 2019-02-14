#
# Code from "broom: An R Package for Converting Statistical Analysis
#                   Objects Into Tidy Data Frames"
#            by David Robinson
#
#
###
#     History: 10 Jan 2018 (wj) initial code copy (w/ simple adaptations)
#
#
###
#
#     Be sure to look at the table on page 4
#
###
# conditional install
if(!require(plyr)      )  { install.packages('plyr');      library(plyr)      }
if(!require(tidyverse) )  { install.packages('tidyverse'); library(tidyverse) }
if(!require(broom)     )  { install.packages('broom');     library(broom)     }
if(!require(Lahman)    )  { install.packages('Lahman');    library(Lahman)    }
if(!require(splines)   )  { install.packages('splines');   library(splines)   }
if(!require(survival)  )  { install.packages('survival');  library(survival)  }

rm(list=ls(all=TRUE))


# Section 2 code

# page 2

fit          <-  lm(mpg~wt+qsec,data=mtcars)
summary(fit)

# page 3
df1 <-tidy(fit)
df2 <- augment(fit)

head(augment(fit))

glance(fit)

# Section 3.1 code: Split-apply-combine example

#page 6

rm(list=ls(all=TRUE))
regression   <-  lm(mpg ~ wt+qsec,data=mtcars)
df1 <- tidy(regression, conf.int=T)

# with dplyr functionality
regressions  <-  mtcars  %>%
  group_by(am)  %>%      # group by automated & manual 
  do(tidy(lm(mpg ~ wt + qsec, data=.), conf.int=T))

regressions

# fixed (24 jan 2018)
p6 <-  regressions %>% 
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=estimate, y=term, color=factor(am), group=am) ) +
  geom_point() + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept=0)

p6

# page 7

# retain the individual lm() results (note: "data=" left out of second param to lm() )
rm(list=ls(all=TRUE))
regressions  <-  mtcars %>%
  group_by(am)%>%
  do(mod = lm(mpg~wt+qsec, .))
regressions

# now apply tidy() to each model
t            <-  regressions          %>%
  tidy(mod, conf.int=T)

t
class(t)

regressions  %>%  glance(mod)

# page 8

# baseball data from Lahman package (Batting)

rm(list=ls(all=TRUE))
merged  <-  Batting                     %>%
  tbl_df()                    %>%
  inner_join(Salaries)        %>%
  mutate(average = H/AB)      %>%
  filter(salary > 0, AB >= 50, !(playerID %in% Pitching$playerID))

merged

ggplot(merged, aes(average, salary, color=yearID)) +
  geom_point() +
  scale_y_log10()

# page 9

salary_fit  <-  lm(log10(salary)~average + yearID, merged)
summary(salary_fit)

team_regressions  <-  merged            %>%
  group_by(teamID)  %>%
  do(tidy(lm(log10(salary) ~ average + yearID, . ), conf.int=T))

team_regressions

# page 10

coefs  <-  team_regressions           %>%
  ungroup()                  %>%
  filter(term == "average")  %>%
  mutate(teamID = reorder(teamID, estimate))

ggplot(coefs, aes(x = estimate, y = teamID )) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high )) +
  geom_vline(color = "red", xintercept = 0.0 )

cors  <-  merged    %>%
  group_by(teamID)   %>%
  do(tidy(cor.test(.$salary, .$average, method = "spearman" )))  %>%
  ungroup                                                        %>%
  arrange(estimate)

tail(cors, 3)

# section 3.2: bootstrapping

# page 11

# baseline model

rm(list=ls(all=TRUE))
ggplot(mtcars, aes(mpg, wt)) + geom_point()

nlsfit         <-  nls(mpg ~ k/wt + b, mtcars, start=list(k=1, b=0) )
summary(nlsfit)

# page 12

confint(nlsfit)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_line(aes(y=predict(nlsfit)))

# observation level bootstrap
#
# set RNG seed for reproducibility
#
set.seed(2014)

bootnls  <-  mtcars          %>%
  bootstrap(500)  %>%
  do(tidy(nls(mpg ~ k/wt + b, ., start=list(k=1, b=0))))

# page 13

head(bootnls)

alpha  <-  0.05
t      <-  bootnls         %>%
  group_by(term)  %>%
  summarize(  conf.low  =  quantile(estimate, alpha/2),
              conf.high =  quantile(estimate, 1-alpha/2))
t

# reset seed to same value
set.seed(2014)
bootnls         <-  mtcars          %>%
  bootstrap(500)  %>%
  do(augment(nls(mpg ~ k/wt + b, ., start=list(k=1, b=0)), .))

alpha		<-  0.05
bootnls_bytime  <-  bootnls       %>%
  group_by(wt)  %>%
  summarize( conf.low	    = quantile(.fitted, alpha/2),
             conf.high    = quantile(.fitted, 1-alpha/2),
             median_value = median(.fitted))
head(bootnls_bytime)

ggplot(mtcars, aes(wt)) +
  geom_point(aes(y=mpg)) +
  geom_line(aes(y=.fitted), data=augment(nlsfit)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), data = bootnls_bytime, alpha = 0.1 )


# page 14

# not in article, but reseed anyhow
set.seed(2014)
alpha  <-  0.05

# splines example

bootsplines  <-  mtcars         %>%
  bootstrap(500) %>%
  do(augment(lm(mpg ~ ns(wt, 4), .), .))

bootnls$method      <-  "nls"
bootsplines$method  <-  "spline"

allboot       <-  rbind_list(bootnls, bootsplines)
allboot_bywt  <-  allboot			%>%
  group_by(wt, method)	%>%
  summarize( conf.low  = quantile(.fitted, alpha/2),
             conf.high = quantile(.fitted, 1-alpha/2),
             med       = median(.fitted))

ggplot(allboot_bywt, aes(wt, med, color = method)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), lty = 2, alpha = 0.1)

# page 15

# again, reseed
set.seed(2014)
alpha  <-  0.05

# survival example

bootstraps_survival  <-  lung           %>%
  bootstrap(500) %>%
  do(tidy(survfit(coxph(Surv(time, status) ~ age + sex, .))))
head(bootstraps_survival)

# included in article for clarity but not needed if you are going to run the entire
# script

alpha              <-  0.05
bootstraps_bytime  <-  bootstraps_survival	%>%
  group_by(time)		%>%
  summarize( conf.low  = quantile(estimate, alpha/2),
             conf.high = quantile(estimate, 1-alpha/2),
             med_est   = median(estimate))

# does not work (estimate not in DF) ============= Review article
ggplot(bootstraps_bytime, aes(time, estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), lty = 2, alpha = 0.2)

# example 3.3: k-means

# page 16

set.seed(2014)

centers  <-  data.frame(  oracle = factor(1:3),
                          size = c(100, 150, 50),
                          x1 = c(5,0,-3),
                          x2 = c(-1, 1, -2))

kdat  <-  centers           %>%
  group_by(oracle)  %>%
  do(data.frame( x1 = rnorm(.$size[1], .$x1[1]),
                 x2 = rnorm(.$size[1], .$x2[1])))

# page 17
# note: 'inflate' is depricated, use 'tidyr::crossing' instead
d        <-  data.frame(a = 1:3, b = 8:10)
#t        <-  d  %>%
#             inflate(x = c("apple", "orange"), y = c("car", "boat"))
t        <- d %>%
  crossing(x= c("apple", "orange"), y= c("car", "boat") )
t

kclusts  <-     kdat               %>%
  crossing(k = 1:9)  %>%
  group_by(k)        %>%
  do(clust = kmeans(select(., x1, x2), .$k[1], nstart = 5))
kclusts

# page 18

# 'clust' is component of 'kclusts' this code generates warnings but works
clusters     <-  kclusts  %>%  tidy(clust)
assignments  <-  kclusts  %>%  augment(clust, kdat)
clusterings  <-  kclusts  %>%  glance(clust)

head(assignments)

p1  <-  ggplot(assignments, aes(x1, x2)) +
  geom_point(aes(color=.cluster, shape = oracle)) +
  facet_wrap(~k)
p1

#page 19

p2  <-  p1 + geom_point(data=clusters, size=10, shape="x")
p2

# from article: plots a line but k (number of clusters) is continuous
ggplot(clusterings, aes(k, tot.withinss)) + geom_line()

# not from article: shows steps for k (as a factor, i.e. discrete)
ggplot(clusterings, aes(factor(k), tot.withinss)) + geom_boxplot()

# page 20

set.seed(2014)
kdat_sd     <-  centers                                                  %>%
  crossing(sd = c(0.5, 1, 2, 4), replication = 1:50)       %>%
  group_by(oracle, sd, replication)                        %>%
  do( data.frame( x1 = rnorm(.$size[1], .$x1[1], .$sd[1]),
                  x2 = rnorm(.$size[1], .$x2[1], .$sd[1])))

kclusts_sd  <-  kdat_sd                       %>%
  crossing(k = 1:9)             %>%
  group_by(k, sd, replication)  %>%
  do(dat = (.), clust = kmeans(select(., x1, x2), .$k[1], nstart = 5))

# many warnings
clusters_sd     <-  kclusts_sd  %>%
  tidy(clust)

glances_sd      <-  kclusts_sd  %>%
  glance(clust)

assignments_sd  <-  kclusts_sd                    %>%
  group_by(k, sd, replication)  %>%
  do(augment(.$clust[[1]], .$dat[[1]]))

# change from article: save result in "t"
t               <-  clusters_sd     %>%
  filter(k == 3)  %>%
  ggplot(aes(x1, x2)) +
  geom_point() +
  geom_point(data = centers, size = 7, color = "red", shape = "x") +
  facet_wrap(~sd)

t

# page 21 (again, slight modifications)

p21  <-  ggplot(glances_sd, aes(k, tot.withinss, group = replication)) +
  geom_line() +
  facet_wrap(~sd, scales = "free_y")
p21


# page 22

accuracies  <-  assignments_sd                               %>%
  filter(k==3)                                 %>%
  count(replication, sd, oracle, .cluster)     %>%
  group_by(replication, sd, .cluster)          %>%
  summarize(correct = max(n), total = sum(n))  %>%
  group_by(replication, sd)                    %>%
  summarize(purity = sum(correct) / sum(total))

ggplot(accuracies, aes(factor(sd), purity)) + geom_boxplot()
