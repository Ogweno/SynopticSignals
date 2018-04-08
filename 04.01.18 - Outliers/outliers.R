# load packages


# load dataset
data(iris)
head(iris)




# Univariate approach to outliers, uses IQR. 
outlier_values_SL <- boxplot.stats(iris$Sepal.Length)$out  # outlier values.
boxplot(iris$Sepal.Length, main="Sepal Length", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

# only variable with outliers is sepal width with 4, (4.4, 4.1, 4.2, 2)
outlier_values_SW <- boxplot.stats(iris$Sepal.Width)$out  # outlier values.
boxplot(iris$Sepal.Width, main="Sepal Width", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values_PL <- boxplot.stats(iris$Petal.Length)$out  # outlier values.
boxplot(iris$Petal.Length, main="Petal Length", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values_PW <- boxplot.stats(iris$Petal.Width)$out  # outlier values.
boxplot(iris$Petal.Width, main="Petal Width", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

# Bivariate approach with Species, outliers appear in multiple plots
boxplot(Sepal.Length ~ Species, data=iris,
     main="Box Plot - SL",
     xlab="Species",
     ylab="Sepal Length")

boxplot(Sepal.Width ~ Species, data=iris,
     main="Box Plot - SW",
     xlab="Species",
     ylab="Sepal Width")

boxplot(Petal.Length ~ Species, data=iris,
     main="Box Plot - PL",
     xlab="Species",
     ylab="Petal Length")

boxplot(Petal.Width ~ Species, data=iris,
     main="Box Plot - PW",
     xlab="Species",
     ylab="Petal Width")

# Cooks distance
mod <- lm(Sepal.Width ~ Species, data=iris)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(iris[influential, ])  # influential observations.

summary(mod)

# https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset  
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# new variable for sepal.width without outliers. Slight change to model.
iris$Sepal.Width_noout <- remove_outliers(iris$Sepal.Width)

mod_noout <- lm(Sepal.Width_noout ~ Species, data=iris)

summary(mod_noout)