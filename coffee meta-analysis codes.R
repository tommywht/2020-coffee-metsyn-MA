##Overall:
library(metafor)
library(tidyverse)

##turn off graphics
graphics.off()
## first import the dataset name "coffee_metadata_overall"
## calculate the log odds ratio 
coffee_metadata_overall$logmidor <- log(coffee_metadata_overall$midor)
coffee_metadata_overall$loghighor <- log(coffee_metadata_overall$highor)

## calculate the standard error from the 95% confidence intervals extracted
coffee_metadata_overall$midsei <- with(coffee_metadata_overall, ((log(midul)-log(midll))/(2*1.96)))
coffee_metadata_overall$highsei <- with(coffee_metadata_overall, ((log(highul)-log(highll))/(2*1.96)))

## run the meta-analysis using the REML estimator - incl all studies
meta_high_REML <- rma(yi=loghighor, sei=highsei, data=coffee_metadata_overall, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml <- forest(meta_high_REML, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Highest vs. lowest",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.45,4.4),10.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
# export - save as image, metafile - 700 x 550

# pasting only the word "overall"
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall ")))
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML$I2,digits=2,format="f")),"%)")))
# obsolete
text(-4,10.5,pos=4,cex=0.8,"Sex-adjusted, highest vs lowest")

# excl those without mid level
coffee_metadata_overall2 <- coffee_metadata_overall %>% filter(midyn == 1)

## run the meta-analysis (high) using the REML estimator - only those with more than 2 responses
meta_high_REML2 <- rma(yi=loghighor, sei=highsei, data=coffee_metadata_overall2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml2 <- forest(meta_high_REML2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median: 3.75 cups/day)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.45,4.4),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)

## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML2$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML2$I2,digits=2,format="f")),"%)")))
# export - save as image, metafile - 700 x 550

## run the meta-analysis (medium) using the REML estimator - only those with more than 2 responses
meta_med_REML2 <- rma(yi=logmidor, sei=midsei, data=coffee_metadata_overall2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metamedreml2 <- forest(meta_med_REML2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median: 2 cups/day)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.45,4.4),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for med level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall")))
# export - save as image, metafile - 700 x 550

## for removing one study at a time, use 
coffee_metadata_overall2 <- coffee_metadata_overall[-9, ]
## then run only high level syntax below. Change the number to remove the next study

##Men:
  ## first import csv "coffee_metadata_men"
  ## calculate the log odds ratio 
  coffee_metadata_men$logmidor <- log(coffee_metadata_men$midor)
coffee_metadata_men$loghighor <- log(coffee_metadata_men$highor)

## calculate the standard error from the 95% confidence intervals extracted
coffee_metadata_men$midsei <- with(coffee_metadata_men, ((log(midul)-log(midll))/(2*1.96)))
coffee_metadata_men$highsei <- with(coffee_metadata_men, ((log(highul)-log(highll))/(2*1.96)))

## run the meta-analysis using the REML estimator - high level - incl all studies
meta_high_REML_men <- rma(yi=coffee_metadata_men$loghighor, sei=coffee_metadata_men$highsei, data=coffee_metadata_men, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_men <- forest(meta_high_REML_men, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Highest vs. lowest",xlim=c(-4,6),cex=0.8,mlab="",alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.2,4.2),7.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
# paste only the word "overall"
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_men$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_men$I2,digits=2,format="f")),"%)")))


## create dataset exclude studies without medium
coffee_metadata_men2 <- filter(coffee_metadata_men, midyn == 1)
## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_men2 <- rma(yi=coffee_metadata_men2$logmidor, sei=coffee_metadata_men2$midsei, data=coffee_metadata_men2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_men2 <- forest(meta_mid_REML_men2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median: 2 cups/day)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.2,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall")))

## run the meta-analysis using the REML estimator - high level
meta_high_REML_men2 <- rma(yi=coffee_metadata_men2$loghighor, sei=coffee_metadata_men2$highsei, data=coffee_metadata_men2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_men2 <- forest(meta_high_REML_men2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median: 4.27 cups/day)",xlim=c(-4,6),cex=0.8,mlab="",alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.2,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)


##women:
## first import the dataset "coffee_metadata_women"
## calculate the log odds ratio 
coffee_metadata_women$logmidor <- log(coffee_metadata_women$midor)
coffee_metadata_women$loghighor <- log(coffee_metadata_women$highor)

## calculate the standard error from the 95% confidence intervals extracted
coffee_metadata_women$midsei <- with(coffee_metadata_women, ((log(midul)-log(midll))/(2*1.96)))
coffee_metadata_women$highsei <- with(coffee_metadata_women, ((log(highul)-log(highll))/(2*1.96)))

## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_women <- rma(yi=coffee_metadata_women$logmidor, sei=coffee_metadata_women$midsei, data=coffee_metadata_women, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_women <- forest(meta_mid_REML_women, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median: 2 cups/day)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.2,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall")))

## run the meta-analysis using the REML estimator - high level
meta_high_REML_women <- rma(yi=coffee_metadata_women$loghighor, sei=coffee_metadata_women$highsei, data=coffee_metadata_women, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_women <- forest(meta_high_REML_women, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median: 4.27 cups/day)",xlim=c(-4,6),cex=0.8,mlab="",alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_women$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_women$I2,digits=2,format="f")),"%)")))
text(-4,5.2,cex=0.8,pos=4,"High consumption (median = 3.33 cups/day)")


##women crude:
## first import the csv file name "coffee_metadata_women_crude"
## calculate the log odds ratio 
coffee_metadata_women_crude$logmidor <- log(coffee_metadata_women_crude$midor)
coffee_metadata_women_crude$loghighor <- log(coffee_metadata_women_crude$highor)

## calculate the standard error from the 95% confidence intervals extracted
coffee_metadata_women_crude$midsei <- with(coffee_metadata_women_crude, ((log(midul)-log(midll))/(2*1.96)))
coffee_metadata_women_crude$highsei <- with(coffee_metadata_women_crude, ((log(highul)-log(highll))/(2*1.96)))

## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_women_crude <- rma(yi=logmidor, sei=midsei, data=coffee_metadata_women_crude, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_women_crude <- forest(meta_mid_REML_women_crude, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="odds ratio (crude results, women only)",xlim=c(-4,6),mlab="",alim=c(0,2),cex=0.8)
## create headers for mid level graphs
text(c(-4,3.1,4.2),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_women_crude$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_women_crude$I2,digits=2,format="f")),"%)")))
text(-4,5.2,cex=0.8,pos=4,"Medium consumption (median = 2 cups/day)")

## run the meta-analysis using the REML estimator - high level
meta_high_REML_women_crude <- rma(yi=loghighor, sei=highsei, data=coffee_metadata_women_crude, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_women_crude <- forest(meta_high_REML_women_crude, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="odds ratio (crude results, women only)",cex=0.8,xlim=c(-4,6),alim=c(0,2),mlab="")
## create headers for high level graphs
text(c(-4,2.25,3.7),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_women_crude$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_women_crude$I2,digits=2,format="f")),"%)")))
text(-4,5.25,cex=0.8,pos=4,"High consumption (median = 3.33 cups/day)")

## sensitivity analysis for overall data only
## first import the dataset name "coffee_metadata_overall"
## calculate the log odds ratio 
coffee_metadata_overall$logmidor <- log(coffee_metadata_overall$midor)
coffee_metadata_overall$loghighor <- log(coffee_metadata_overall$highor)

## calculate the standard error from the 95% confidence intervals extracted
coffee_metadata_overall$midsei <- with(coffee_metadata_overall, ((log(midul)-log(midll))/(2*1.96)))
coffee_metadata_overall$highsei <- with(coffee_metadata_overall, ((log(highul)-log(highll))/(2*1.96)))

## analyse only the Caucasian studies
coffee_overall_caucas <- filter(coffee_metadata_overall,ethnicity=="caucas")
##then run the whole analysis again
## run the meta-analysis using the REML estimator including all studies
meta_high_REML_caucas <- rma(yi=loghighor, sei=highsei, data=coffee_overall_caucas, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_caucas <- forest(meta_high_REML_caucas, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Highest vs. lowest (Caucasians only)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.4,4.4),7.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
# paste only the word "overall"
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall(",paste(italic("p"))," = ",.(formatC(meta_high_REML_caucas$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_caucas$I2,digits=2,format="f")),"%)")))


## exclude study without medium consumption
coffee_overall_caucas2 <- coffee_overall_caucas %>% filter(midyn == 1)
## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_caucas2 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_caucas2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_caucas2 <- forest(meta_mid_REML_caucas2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_caucas2$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_caucas2$I2,digits=2,format="f")),"%)")))

## run the meta-analysis using the REML estimator - high level
meta_high_REML_caucas2 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_caucas2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_caucas2 <- forest(meta_high_REML_caucas2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.33 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_caucas2$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_caucas2$I2,digits=2,format="f")),"%)")))

## exclude study with T1DM
coffee_overall_caucas3 <- filter(coffee_overall_caucas,author != "Stutz et al.")
## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_caucas3 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_caucas3, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_caucas3 <- forest(meta_mid_REML_caucas3, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_caucas3$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_caucas3$I2,digits=2,format="f")),"%)")))
text(-4,6.5,pos=4,cex=0.8,"Caucasians only")

## run the meta-analysis using the REML estimator - high level
meta_high_REML_caucas3 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_caucas3, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_caucas3 <- forest(meta_high_REML_caucas3, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.33 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_caucas3$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_caucas3$I2,digits=2,format="f")),"%)")))

# high level without stutz and without studies without medium
coffee_overall_caucas4 <- coffee_overall_caucas3 %>% filter(midyn == 1)
meta_high_REML_caucas4 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_caucas4, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_caucas4 <- forest(meta_high_REML_caucas4, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.33 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_caucas4$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_caucas4$I2,digits=2,format="f")),"%)")))

## now only analyse the Asian studies
coffee_overall_asian <- filter(coffee_metadata_overall,ethnicity=="asian")
##then run the whole analysis again
## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_asian <- rma(yi=logmidor, sei=midsei, data=coffee_overall_asian, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_asian <- forest(meta_mid_REML_asian, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_asian$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_asian$I2,digits=2,format="f")),"%)")))
text(-4,5.5,pos=4,cex=0.8,"Asians only")

## run the meta-analysis using the REML estimator - high level
meta_high_REML_asian <- rma(yi=loghighor, sei=highsei, data=coffee_overall_asian, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_asian <- forest(meta_high_REML_asian, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.75 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_asian$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_asian$I2,digits=2,format="f")),"%)")))
text(-4,5.5,pos=4,cex=0.8,"Asians only")

## run the meta-analysis using the REML estimator - high level - asian males only
coffee_overall_asian2 <- coffee_metadata_men[c(3, 4, 5),]
meta_high_REML_asian2 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_asian2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_asian2 <- forest(meta_high_REML_asian2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.75 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_asian2$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_asian2$I2,digits=2,format="f")),"%)")))


## run the meta-analysis using the REML estimator - high level
meta_high_REML_sens <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_sens <- forest(meta_high_REML_sens, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="odds ratio (multivariate-adjusted, all participants)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens$I2,digits=2,format="f")),"%)")))
text(-4,9.5,pos=4,cex=0.8,"High consumption (median = 3.75 cups/d)")

## analyse by definition of metabolic syndrome
coffee_overall_idf06 <- filter(coffee_metadata_overall,metsyndef=="idf06")
## mid level analysis not possible as Grosso 2014 dont have mid level
## run the meta-analysis using the REML estimator - high level
meta_high_REML_idf06 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_idf06, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_idf06 <- forest(meta_high_REML_idf06, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (mean = 1.25 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),3.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_idf06$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_idf06$I2,digits=2,format="f")),"%)")))
text(-4,4.5,pos=4,cex=0.8,"High consumption (median = 3.33 cups/d)")

## analyse by definition of metabolic syndrome
coffee_overall_idf09 <- filter(coffee_metadata_overall,metsyndef=="idf09")
## run the meta-analysis using the REML estimator - mid level
meta_mid_REML_idf09 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_idf09, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_idf09 <- forest(meta_mid_REML_idf09, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_idf09$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_idf09$I2,digits=2,format="f")),"%)")))
text(-4,6.5,pos=4,cex=0.8,"Medium consumption (median = 2 cups/d)")

## run the meta-analysis using the REML estimator - high level
meta_high_REML_idf09 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_idf09, method="REML", slab=paste(author,year,sep=", "), digits = 2, control = list(stepadj = 0.5))
## create the forest plots - high level
for_metahighreml_idf09 <- forest(meta_high_REML_idf09, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.33 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.2,4.1),5.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_idf09$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_idf09$I2,digits=2,format="f")),"%)")))
text(-4,6.5,pos=4,cex=0.8,"High consumption (median = 3.33 cups/d)")

## analyse by definition of metabolic syndrome
coffee_overall_ncep <- filter(coffee_metadata_overall,metsyndef=="ncep")
## run the meta-analysis using the REML estimator - high level - all studies
meta_high_REML_ncep <- rma(yi=loghighor, sei=highsei, data=coffee_overall_ncep, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_ncep <- forest(meta_high_REML_ncep, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.75 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),4.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_ncep$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_ncep$I2,digits=2,format="f")),"%)")))
text(-4,4.5,pos=4,cex=0.8,"High consumption (median = 3.75 cups/d)")

## run the meta-analysis using the REML estimator - mid level
coffee_overall_ncep2 <- filter(coffee_overall_ncep, midyn == 1)
meta_mid_REML_ncep2 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_ncep2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_ncep2 <- forest(meta_mid_REML_ncep2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="Medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),3.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_ncep2$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_ncep2$I2,digits=2,format="f")),"%)")))
text(-4,4.5,pos=4,cex=0.8,"Medium consumption (median = 2 cups/d)")

# ncep - high level excl studies without med
meta_high_REML_ncep2 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_ncep2, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metahighreml_ncep2 <- forest(meta_high_REML_ncep2, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),3.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_ncep2$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_ncep2$I2,digits=2,format="f")),"%)")))

## remove kim 2014 
coffee_overall_sens3 <- coffee_metadata_overall[-5, ]
## run the meta-analysis using the REML estimator - high level
meta_high_REML_sens3 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens3, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_sens3 <- forest(meta_high_REML_sens3, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="odds ratio (multivariate-adjusted, all participants)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens3$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens3$I2,digits=2,format="f")),"%)")))
text(-4,9.5,pos=4,cex=0.8,"High consumption")

## run the meta-analysis using the REML estimator - mid level
coffee_overall_sens4 <- coffee_overall_sens3 %>% filter(midyn == 1)
meta_mid_REML_sens4 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_sens4, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_sens4 <- forest(meta_mid_REML_sens4, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="odds ratio (multivariate-adjusted, all participants)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),7.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_sens4$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_sens4$I2,digits=2,format="f")),"%)")))
text(-4,8.5,pos=4,cex=0.8,"Medium consumption")

# high
meta_high_REML_sens4 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens4, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metahighreml_sens4 <- forest(meta_high_REML_sens4, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="odds ratio (multivariate-adjusted, all participants)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),7.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens4$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens4$I2,digits=2,format="f")),"%)")))

## remove grosso 2014 
coffee_overall_sens5 <- coffee_metadata_overall[-3, ]
## run the meta-analysis using the REML estimator - high level
meta_high_REML_sens5 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens5, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_sens5 <- forest(meta_high_REML_sens5, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.75 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens5$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens5$I2,digits=2,format="f")),"%)")))

## run the meta-analysis using the REML estimator - mid level excl studies without medium
coffee_overall_sens6 <- coffee_overall_sens5 %>% filter(midyn == 1)
meta_mid_REML_sens6 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_sens6, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_sens6 <- forest(meta_mid_REML_sens6, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_sens6$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_sens6$I2,digits=2,format="f")),"%)")))

# high without studies without medium
meta_high_REML_sens6 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens6, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_sens6 <- forest(meta_high_REML_sens6, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens6$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens6$I2,digits=2,format="f")),"%)")))

## remove stutz 2018 
coffee_overall_sens7 <- coffee_metadata_overall[-8, ]
## run the meta-analysis using the REML estimator - high level
meta_high_REML_sens7 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens7, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - high level
for_metahighreml_sens7 <- forest(meta_high_REML_sens7, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="High consumption (median = 3.75 cups/d)",xlim=c(-4,6),cex=0.8,mlab="", alim=c(0,2))
## create headers for high level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for high level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens7$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens7$I2,digits=2,format="f")),"%)")))

## run the meta-analysis using the REML estimator - mid level excl studies without medium
coffee_overall_sens8 <- coffee_overall_sens7 %>% filter(midyn == 1)
meta_mid_REML_sens8 <- rma(yi=logmidor, sei=midsei, data=coffee_overall_sens8, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_sens8 <- forest(meta_mid_REML_sens6, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_mid_REML_sens8$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_mid_REML_sens8$I2,digits=2,format="f")),"%)")))

# high without studies without medium
meta_high_REML_sens8 <- rma(yi=loghighor, sei=highsei, data=coffee_overall_sens8, method="REML", slab=paste(author,year,sep=", "))
## create the forest plots - mid level
for_metamidreml_sens8 <- forest(meta_high_REML_sens8, transf=exp, digits=2, refline=1, showweight=TRUE, xlab="medium consumption (median = 2 cups/d)",xlim=c(-4,6),cex=0.8, mlab="", alim=c(0,2))
## create headers for mid level graphs
text(c(-4,3.1,4.2),8.5,c("Author & year","Weight","OR (95% CI)"),cex=0.8,pos=4)
## create the summary statement for mid level graphs
text(-4, -1,pos=4,cex=0.8,bquote(paste("Overall (",paste(italic("p"))," = ",.(formatC(meta_high_REML_sens8$pval,digits=2,format="f"))," ; ",I^2," = ",.(formatC(meta_high_REML_sens8$I2,digits=2,format="f")),"%)")))
