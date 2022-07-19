library(dismo) #instead of gbm
data(Anguilla_train)
head(Anguilla_train)

#gbm.step is an alternative to the cross-validation provided in the gbm package
angaus.tc5.lr01 <- gbm.step(data = Anguilla_train, gbm.x = 3:13, gbm.y = 2, family = "bernoulli",
                            tree.complexity = 5, learning.rate = 0.01, bag.fraction = 0.5)
angaus.tc5.lr01$fitted # fitted values from the final tree on the response scale
angaus.tc5.lr01$fitted.vars # variance of fitted valu on the response scale
angaus.tc5.lr01$residuals # residuals for the fitted values
angaus.tc5.lr01$contributions # relative importance of the variables
angaus.tc5.lr01$cv.statistics
