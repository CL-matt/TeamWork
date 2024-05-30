modelo1 <- lm(mpg ~ hp, data=mtcars)
summary(modelo1)

install.packages("remotes")
remotes::install_github("David-hervas/repmod")
library(repmod)
report(modelo1)

mod2 <- lm(mpg ~ hp + wt, data=mtcars)
report(mod2)

mod3 <- lm(mpg ~ hp * wt, data=mtcars)
report(mod3)

#Comprobar que si hay interación entre ello
if(!require(visreg)){
  install.packages("visreg")
  library(visreg)
}
visreg(mod3, "hp", by="wt", overlay=TRUE)

