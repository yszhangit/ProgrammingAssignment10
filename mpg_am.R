# factor wont work with lot of functions
# use factor funciton when plotting
#MTcars <- mtcars
#MTcars$am <- factor(mtcars$am, levels = c(0,1), labels = c('auto','manual'))
#MTcars$cyl <- factor(mtcars$cyl)
#MTcars$gear <- factor(mtcars$gear)
#MTcars$vs <- factor(mtcars$vs)
#MTcars$carb <- factor(mtcars$carb)
#MTcars$vs <- factor(mtcars$vs, levels = c(0,1), labels = c("v-shaped","straight"))

library(ggplot2)
# ggplot2 has mpg 
attach(mtcars)




mod.sim <- lm(mpg ~ am, data = mtcars)
mod.mul <- lm(mpg ~ wt + qsec + am, data = mtcars)
summary(mod.sim)
#resid(mod1)
plot(density(resid(mod.sim))) 

# A quantile normal plot - good for checking normality
qqnorm(resid(mod.sim)) 
qqline(resid(mod.sim))

plot(am, mpg)
abline(mod1)

# ANOVA
anova1 <- aov(mpg~am, data = mtcars)
summary(anova1)
f_crit <- round(qf(.95, 2, 30),2)

## qqplot
ggplot(data = mtcars, aes(x=am, y=mpg)) + 
  geom_point(aes(size=3, alpha=.4)) +
  stat_smooth(method = "lm") +
  ggtitle("simple linear regression between MPG and AM")

ggplot(data = mtcars, aes(mod.sim$residuals)) + 
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

# comprae residual
se <- function(x) sqrt(var(x)/length(x))
par(mfrow=c(2,2))
res.range <- ceiling(max(sapply(list(mod.sim$residuals, mod.mul$residuals), range)))
plot(mod.sim$residuals, ylim = res.range * c(1,-1), main="simple", ylab = "variation")
abline(h=0)
plot(mod.mul$residuals, ylim = res.range * c(1,-1), main="multi", ylab = "variation")
abline(h=0)
hist(mod.sim$residuals, breaks = 30, main="simple", xlab = "variation")
hist(mod.mul$residuals, breaks = 30, main="multi", xlab = "variation")
mtext("figure 4, residuals", side = 1, outer = T, line = -3)
par(mfrow=c(1,1))

# 3D plot
library(plot3D)
#http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

# init grid size
grid.lines = 20
#  fill points in between min and max value of indenpent variables
x.pred <- seq(min(wt),max(wt), length.out = grid.lines)
y.pred <- seq(min(qsec),max(qsec), length.out = grid.lines)
# resize grid
xy <- expand.grid( x = x.pred, y = y.pred)
# z value by prediction
#z.pred <- matrix(predict(mod2, newdata = xy), nrow =grid.lines, ncol = grid.lines)
z.auto.pred <- matrix(
  predict(mod.mul, newdata = data.frame(
    wt = xy$x,
    qsec = xy$y,
    am=0)), 
  nrow =grid.lines, ncol = grid.lines
  )
z.manual.pred <- matrix(
  predict(mod.mul, newdata = data.frame(
    wt = xy$x,
    qsec = xy$y,
    am=1)), 
  nrow =grid.lines, ncol = grid.lines
  )
# droplines to surface
#fitpoints <- predict(mod2)
#plot

points.auto <- mtcars[mtcars$am == 0,]
points.manual <- mtcars[mtcars$am == 1,]

scatter3D(points.auto$wt, points.auto$qsec, points.auto$mpg, 
          ylim = c(10,30), zlim = c(0,45),
          col = "blue",
          colkey=F, pch=20, cex=2, alpha = 0.5,
          phi = -5, theta = 40, 
          bty = "g", ticktype = "detailed",
          xlab ="wt", ylab="qsec",zlab="mpg",
          surf = list(x = x.pred, y = y.pred, z=z.auto.pred, facets = NA , col = "blue"),
          main = "MPG with auto(blue) and manual(red)"
)
scatter3D(points.manual$wt, points.manual$qsec, points.manual$mpg, 
          col = "red", alpha = 0.5,
          pch=20, ticktpe = "detailed", cex=2 , 
          surf = list(x = x.pred, y = y.pred, z=z.manual.pred, facets = NA , col = "red"),
          add = T
)
