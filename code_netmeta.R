library(netmeta)

data1 <- read.csv("C:/Users/PC/Desktop/Material/data_netmeta.csv",header=T)

#Transforming data from arm-based to contrast-based format.
p2 <- pairwise(treat=list(LandType.t, LandType.c,LandType3),
               mean = list(mean.t, mean.c,mean3), sd = list(sd.t, sd.c,sd3),
               n = list(n.t, n.c,n3),
               data = data1, studlab = article,
               sm = "SMD")

#Model Fitting
m.netmeta1 <- netmeta(TE = TE,
                      seTE = seTE,
                      treat1 = treat1,
                      treat2 = treat2,
                      studlab = studlab,
                      data = p2,
                      sm = "SMD",
                      common = FALSE,
                      random = TRUE,
                      method.tau = "REML",
                      reference.group = "crops",
                      details.chkmultiarm = TRUE,
                      sep.trts = " vs ")
summary(m.netmeta1)  #I^2>50%,P<0.05, random-effects model is employed


###Network Plot
#Show treatment order (shortened labels)
netgraph(m.netmeta1,
         cex.points = 4, cex = 1.5,col.points = 'black',
         number.of.studies = TRUE, cex.number.of.studies = 1.5,
         labels = m.netmeta1$trts)

##Assessing Inconsistency: The Nodesplit Method
b1=netsplit(m.netmeta1)
forest(netsplit(m.netmeta1))



#Treatment Ranking
netrank(m.netmeta1, small.values = "bad", method = "SUCRA",random = TRUE)
#Forest plot of Network Meta-Analysis Results
forest(m.netmeta1, 
       pooled = 'random',
       reference.group = "crops",
       leftcols = c('studlab','SUCRA'),
       xlab = 'Forest plot of Network Meta-Analysis Results',
       baseline.reference = TRUE,
       small.values = 'bad',
       sortvar = -TE,
       xlim = c(-0.5, 2),
       drop.reference.group = TRUE,
       labels = labels)



##relative.effect.table
netleague <- netleague(m.netmeta1, 
                       bracket = "(", # use round brackets
                       digits=2)      # round to two digits
netleague$random
write.csv(netleague$random, "C:/Users/PC/Desktop/Material/netleague.csv")

#Comparison-Adjusted Funnel Plots
funnel(m.netmeta1, 
       order = c("bare_fields", "crops", "flower_strips", "grass_strips", "spontaneous_plants"), 
       pch = c(1:4, 5,6,7,8), 
       col = c("blue", "red", "purple", "black", "grey", "green",'pink','darkblue'), 
       method.bias = 'Egger')


