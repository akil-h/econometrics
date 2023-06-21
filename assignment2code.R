#Initialize packages

install.packages("wooldridge")
library('wooldridge')

# Open up 'recid'

data('recid')

# Create a linear model

recid_model <- lm(durat ~ person+property+felon+black+educ+age+black+
                    drugs+cens+follow, data=recid)
summary(recid_model)

# Create unrestricted linear model

ur_model <- lm(durat ~ black+alcohol+drugs+super+married+felon+workprg+property+person
           +priors+educ+rules+age+tserved+follow+cens, data=recid)
summary(ur_model) # The R^2 adjusted value for the unrestricted linear model is 0.8324

# Restricted linear model

r_model<- lm(durat ~ black+alcohol+drugs+married+felon+property+person
             +priors+educ+rules+age+tserved+follow, data=recid) 
                 # We restrict the variable "black"
summary(r_model) # The R^2 adjusted value for the restricted linear model is 0.115

# Dummy Variable and interaction term model

model_2 <- lm(durat ~ educ + educ*black, data=recid)
summary(model_2)
