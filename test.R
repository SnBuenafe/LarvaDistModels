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

#### Grid search ####

library(tidyverse)

d <- mtcars %>% 
  # Convert `am` to factor and select relevant variables
  mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>% 
  select(am, mpg, hp)

ggplot(d, aes(mpg, hp, color = am)) +
  geom_point()

library(rpart)
library(rpart.plot)

# Set minsplit = 2 to fit every data point
full_fit <- rpart(am ~ mpg + hp, data = d, minsplit = 2)
prp(full_fit)

# Training-test split
set.seed(245)
n <- nrow(d)
train_rows <- sample(seq(n), size = .8 * n)
train <- d[ train_rows, ]
test  <- d[-train_rows, ]

# Create the grid
# Define a named list of parameter values
gs <- list(minsplit = c(2, 5, 10),
           maxdepth = c(1, 3, 8)) %>% 
  cross_df() # Convert to data frame grid
gs