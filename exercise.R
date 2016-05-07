# Return truncated Celcius values given Fahrenheit inputs 
# with some artificial error introduced
c.from.f <- function(xs) {
    err <- rnorm(length(xs))
    ((xs + err) - 32) * (5/9)
}

# Generate Celcius from a model
c.from.model <- function(xs, slope, intercept) {
    slope * xs + intercept
}

# Select 5000 'random' termperature values with some error
# Generate the Celcius values also with some error
# In a true experiment, these would be observed and not generated
# and the error likely part of the observation
observations <- sample(seq(-32, 451, by=0.1), 5000, replace=TRUE)
temps.f <- observations + rnorm(length(observations))
temps.c <- c.from.f(observations)

# Perform the linear regression
lr <- lm(temps.c ~ temps.f)

slope <- lr$coefficients[[2]]
intercept <- lr$coefficients[[1]]

# Calculate the values from our model
calc.c <- c.from.model(temps.f, slope, intercept)

print(sprintf("Correlation (orig): %0.9f", cor(temps.f, temps.c)))
print(sprintf("Equation: C = %0.6f * F %s %0.6f", 
              slope, ifelse(intercept < 0, '-', '+'), abs(intercept)))

