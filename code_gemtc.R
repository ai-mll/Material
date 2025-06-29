library(rjags)
library(gemtc)

data5 = read.csv("C:/Users/PC/Desktop/Material/Bayesian Network Meta-Analysis/data_gemtc.csv",header=T) 
data5= data5[,1:4]
network1 <- mtc.network(data.re  = data5)
summary(network1)

#Network Plot
plot(network1, 
     use.description = TRUE) # Use full treatment names

# We give our compiled model the name 'model1'.
model1 <- mtc.model(network1,
                    type = 'consistency',
                    likelihood = "normal",
                    link = "identity",
                    linearModel = "random",
                    n.chain = 4)

#Markov Chain & Monte Carlo（MCMC）simulation
mcmc1 <- mtc.run(model1, n.adapt = 10000, n.iter = 1e5, thin = 10)

cat(model$code)  #Look at the code for jags
summary(mcmc1)  #View the results

###Evaluate the convergence of the model
#trace plot
plot(mcmc1)

#Gelman-Rubin plot
gelman.plot(mcmc1)

#Access the overall (Potential Scale Reduction Factor)PSRF of model
gelman.diag(mcmc1)$mpsrf

#Assessing Inconsistency: The Nodesplit Method
nodesplit1 <- mtc.nodesplit(network1, 
                            linearModel = "random", 
                            likelihood = "normal",
                            link = "identity",
                            n.adapt = 10000, 
                            n.iter = 1e5, 
                            thin = 10)

#View the results of inconsistency
b1=summary(nodesplit1)
print(b1)

#Forest plot for the nodesplit model
plot(summary(nodesplit1))

#Generating the Network Meta-Analysis Results
rank1 <- rank.probability(mcmc1) 
plot(rank1, beside=TRUE)


#Forest plot of Network Meta-Analysis Results
forest(relative.effect(mcmc1, t1 = "crops"), 
       use.description = TRUE, # Use long treatment names
       xlim = c(-1, 2.5))

#View the Surface Under the Cumulative Ranking (SUCRA) score
library(dmetar)
sucra1 <- dmetar::sucra(rank1, lower.is.better = FALSE)
sucra1
plot(sucra1)
#relative.effect.table
results <- relative.effect.table(mcmc1)
save(results, file = "results.csv")

