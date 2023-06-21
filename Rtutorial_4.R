# R tutorial: Hypothesis Testing

################
#### Part 1 ####
#### Objective: Perform hypothesis tests using earnings data

setwd("~/Desktop/ECO220_Quin/tutorial")

library(ggplot2)

earn <- read.csv("earnings_data.csv")
str(earn)
summary(earn) # no NA in earnings

## 1. Is the sample on earnings approximately normally distributed? How about log earnings?
##    can be checked visually using histogram or a normal probability plot (or a Q-Q plot)

ggplot(earn, aes(x=earnings)) + geom_histogram()
ggplot(earn, aes(sample=earnings)) + stat_qq()

earn$logearnings <- log(earn$earnings)
ggplot(earn, aes(x=logearnings)) + geom_histogram()
ggplot(earn, aes(sample=logearnings)) + stat_qq()

## 2. At the 5% level of significance, test the null hypothesis that the mean of log earnings
##    is 2.75. (3 approaches: by hand, implement the same procedure in R, use R function)

# H0 : mu = 2.75, H1 : mu != 2.75
# t = (xbar - 2.75) / (sdx/sqrt(n))
# pvalue = 2*Pr(T > |t|) where T ~ t(n-1)

mu0 <- 2.75
sample_size <- nrow(earn)
(mean_logearnings <- mean(earn$logearnings))
(sd_logearnings <- sd(earn$logearnings))
(tvalue <- (mean_logearnings - mu0) / (sd_logearnings/sqrt(sample_size)))
(pvalue <- 2*pt(abs(tvalue), df=sample_size-1, lower.tail=FALSE))

# alternatively
t.test(earn$logearnings, mu=2.75, alternative="two.sided", conf.level=0.95)

## 3. At the 5% significance level, test the null hypothesis that at least 68% of the
##    population have earnings below average level.

# H0 : p >= 0.68, H1 : p < 0.68
# z = (phat - 0.68) / sqrt(0.68*(1-0.68)/n)
# pvalue = Pr(Z < z) where Z ~ N(0,1)

p0 <- 0.68
earn$below_average <- with(earn, ifelse(earnings < mean(earnings), 1, 0))
(phat <- mean(earn$below_average))
(zvalue <- (phat - p0) / sqrt(p0*(1-p0)/sample_size))
(pvalue <- pnorm(zvalue))

# alternatively
prop.test(x=sum(earn$below_average), n=sample_size, p=p0,
	        alternative="less", conf.level=0.95, correct=FALSE)

## 4. Suppose the true proportion of individuals earning below average is 0.66.
##    Calculate the power of the test in 3.

# power = Pr(reject H0 ; p = 0.66)
#       = Pr(zstat < cv ; p = 0.66)

cv <- qnorm(0.05)

p1 <- 0.66
se0 <- sqrt(p0*(1-p0)/sample_size)
se1 <- sqrt(p1*(1-p1)/sample_size)
(rr <- (se0*cv + p0 - p1) / se1)
(power1 <- pnorm(rr))

################
#### Part 2 ####
#### In a representative season of soccer the home team won a match 47% of the time
#### out of a sample of 380 matches.

## 1. At the 5% significance level, test the null hypothesis that the home team wins
##    at least 50% of all games.

# H0 : p >= 0.5, H1 : p < 0.5
# z = (phat - 0.5) / sqrt(0.5*(1-0.5)/n)
# pvalue = Pr(Z < z) where Z ~ N(0,1)

n <- 380
phat <- 0.47
p0 <- 0.5
(zvalue <- (phat - p0) / sqrt(p0*(1-p0)/n))
(pvalue <- pnorm(zvalue))

prop.test(n*phat, n, p=p0, alternative="less", correct=FALSE)

## 2. What is the p-value for the test in 1? At what level of significance would we
##    reject the null?

pvalue # reject whenever the prespecified significance level exceeds the pvalue

## 3. Suppose the true winning rate for the home team is 55%. What was the probability of
##    getting a sample of 380 where the home team won 47% or fewer? Would we have made
##    the right decision if we repeated the test in 1?

p1 <- 0.55
pbinom(n*phat, n, p1)

# We weren't able to reject the null that p is at least 50%, and 55% is within the null.
# So we'd make the right decision not to reject. Indeed, the power of the test is low:
cv <- qnorm(0.05)
se0 <- sqrt(p0*(1-p0)/n)
se1 <- sqrt(p1*(1-p1)/n)
(rr <- (se0*cv + p0 - p1) / se1)
(power1 <- pnorm(rr))

################
#### Part 3 ####
#### The instructor in a particular statistics class felt his students were stronger this year
#### than in the previous year. In the previous year, the average grade for test 1 was 65% and
#### the standard deviation was 10% across all students. The instructor decided to put together
#### a random sample of 25 students from test 1 this year and found the average sample test
#### score was 70% with a sample standard deviation of 12%. The instructor plans to use the
#### test 1 sample to determine if the overall average grade this year will be higher than last year.

## 1. What assumptions must you make to undertake this test?

# The grade distribution for test 1 this year is normally distributed.

## 2. If your assumptions set out above are true, at the 0.5% significance level, test whether there
##    is any substance in the instructor's belief.

# H0 : mu <= 65 , H1 : mu > 65
# t = (xbar - 65) / (sdx/sqrt(n))
# pvalue = Pr(T > t) where T ~ t(n-1)

n <- 25
xbar <- 70
sdx <- 12
mu0 <- 65

(tvalue <- (xbar - mu0) / (sdx/sqrt(n)))
(pvalue <- pt(tvalue, df=n-1, lower.tail=FALSE))

## 3. Assume the instructor was right to believe the students improved and he gathered a bigger
##    sample but the sample statistics remained unchanged as the sample size continued to increase.
##    What size of sample would he need, given the sample statistics remain unchanged, in order to
##    reject the idea that the students this year had not improved at the 0.5% level of significance?

# reject when t > cv

(cv <- qt(0.995, df=n-1)) # note: this is not the correct critical value for the new sample size,
                          #       but the correct cv will be smaller because n is large,
                          #       so the lower bound obtained is still valid.
(lower_bound <- (sdx/(xbar-mu0) * cv)^2)
(new_n <- ceiling(lower_bound))

# more precisely, one can devise a test function to check whether the sample size is large enough
# for the critical value given the sample size:
test_fun <- function(n) {
	lower_bound <- (sdx/(xbar-mu0) * qt(0.995, df=n-1))^2
	ifelse(n > lower_bound, 1, 0)
}

# one way to proceed is to guess a region where the new_n may fall in:
candidate_n <- 25:100
plot(candidate_n, test_fun(candidate_n), cex=0.5)
(new_n <- candidate_n[sum(test_fun(candidate_n) <= 0) + 1])

# another way to proceed would be to construct a while loop:
new_n <- 25
while(test_fun(new_n) == 0 & new_n < 10000) {
	new_n <- new_n + 1
}
new_n