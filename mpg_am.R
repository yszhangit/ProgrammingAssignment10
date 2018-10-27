# factor wont work with lot of functions
# use factor funciton when plotting
#MTcars <- mtcars
#MTcars$am <- factor(mtcars$am, levels = c(0,1), labels = c('auto','manual'))
#MTcars$cyl <- factor(mtcars$cyl)
#MTcars$gear <- factor(mtcars$gear)
#MTcars$vs <- factor(mtcars$vs)
#MTcars$carb <- factor(mtcars$carb)
#MTcars$vs <- factor(mtcars$vs, levels = c(0,1), labels = c("v-shaped","straight"))

mod1 <- lm(mpg ~ am, data = mtcars)
mod2 <- lm(mpg ~ wt + qsec + am, data = mtcars)
summary(mod1)
#resid(mod1)
plot(density(resid(mod1))) 

# A quantile normal plot - good for checking normality
qqnorm(resid(mod1)) 
qqline(resid(mod1))


plot(mtcars$am, mtcars$mpg)
abline(mod1)

# ANOVA
anova1 <- aov(mpg~am, data = mtcars)
summary(anova1)
f_crit <- round(qf(.95, 2, 30),2)

## qqplot
library(ggplot2)
ggplot(data = mtcars, aes(x=am, y=mpg)) + 
  geom_point(aes(size=3, alpha=.4)) +
  stat_smooth(method = "lm") +
  ggtitle("simple linear regression between MPG and AM")

ggplot(data = mtcars, aes(mod1$residuals)) + 
  geom_histogram(color='black', fill='white', aes(y=..density..) ) +
  geom_density(alpha=0.4, fill="grey") +
  ggtitle("fit model residual density and histogram")

# multi variables
library(leaps)
# nbest: number of best subset in order
regsubsets.out <- regsubsets(mpg ~ ., data = mtcars, nbest = 2)
as.data.frame(summary(regsubsets.out)$outmat)
# from output below
"
if you want to chose 1 variable, best candidate is wt, second best is cyl,
if you want to chose 2 variables, best with cyl+wt, second best hp+wt
and so on.
NOTE, this output is nbest=2

          cyl disp hp drat wt qsec vs am gear carb
1  ( 1 )                   *                     
1  ( 2 )   *                                     
2  ( 1 )   *               *                     
2  ( 2 )           *       *                     
3  ( 1 )                   *    *     *          
3  ( 2 )   *       *       *                     
4  ( 1 )           *       *    *     *          
4  ( 2 )                   *    *     *         *
5  ( 1 )        *  *       *    *     *          
5  ( 2 )                *  *    *     *         *
6  ( 1 )        *  *    *  *    *     *          
6  ( 2 )        *  *       *    *     *    *     
7  ( 1 )        *  *    *  *    *     *    *     
7  ( 2 )   *    *  *    *  *    *     *          
8  ( 1 )        *  *    *  *    *     *    *    *
8  ( 2 )        *  *    *  *    *  *  *    *     
"
# plot display good selection in dark color 
"the higher adjusted R2 , the better candidate"
plot(regsubsets.out, scale="adjr2")

# lm sepreatly by AM, 2 regression lines
ggplot(data = mtcars, aes(x=wt, y=mpg, color=factor(am))) + geom_point(size =3, alpha =0.5) +
  stat_smooth(method="lm") +
  ggtitle("linear regression between MPG and WT factored by value of AM")

# model with mpg ~ hp, color with am
ggplot(data = mtcars, aes(x=wt, y=mpg)) + geom_point(size =3, alpha =0.5, aes(color=factor(am))) +
  stat_smooth(method="lm") +
  ggtitle("linear regression between MPG and WT+AM")

