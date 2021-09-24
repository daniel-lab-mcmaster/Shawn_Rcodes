#installing and running necessary packages
library(metafor)
library(meta)
library(grid)
library(ggplot2)
library(gridExtra)
library(metaviz)

#set working directory
setwd("/Volumes/GoogleDrive/My Drive/African Sys Review/Current Documents/Meta-analysis R")

##importing data table from excel
full<-read.csv("meta_full.csv", header=T)
full_co <- read.csv("meta_full_co.csv", header=T)
full_reg <- read.csv("meta_full_reg.csv", header=T)
full_ER_PR <- read.csv("meta_full_ER_PR.csv", header=T)
full_HER2 <- read.csv("meta_full_HER2.csv", header=T)
full_combo <- read.csv("meta_full_tool_combo.csv", header = T)


##meta-analysis data frame

m1<-metaprop(full$case, full$pop, studlab=paste(author,year,sep=", "), 
             data = full, subset = NULL,sm="PFT",level = 0.95, level.comb = 0.95,comb.fixed=FALSE, 
             comb.random=TRUE,hakn=FALSE,prediction=FALSE,pscale=100, outclab = "", print.byvar = F)

m_co<-metaprop(full_co$case, full_co$pop, studlab=paste(author,year,sep=", "), 
               data = full_co, subset = NULL,sm="PFT",level = 0.95, level.comb = 0.95,comb.fixed=FALSE, 
               comb.random=TRUE,hakn=FALSE,prediction=FALSE,pscale=100, outclab = "", print.byvar = F)

m_reg<-metaprop(full_reg$case, full_reg$pop, studlab=paste(author,year,sep=", "), 
               data = full_reg, subset = NULL,sm="PFT",level = 0.95, level.comb = 0.95,comb.fixed=FALSE, 
               comb.random=TRUE,hakn=FALSE,prediction=FALSE,pscale=100, outclab = "", print.byvar = F)

m_ER_PR<-metaprop(full_ER_PR$case, full_ER_PR$pop, studlab=paste(author,year,sep=", "), 
               data = full_ER_PR, subset = NULL,sm="PFT",level = 0.95, level.comb = 0.95,comb.fixed=FALSE, 
               comb.random=TRUE,hakn=FALSE,prediction=FALSE,pscale=100, outclab = "", print.byvar = F)

m_HER2<-metaprop(full_HER2$case, full_HER2$pop, studlab=paste(author,year,sep=", "), 
               data = full_HER2, subset = NULL,sm="PFT",level = 0.95, level.comb = 0.95,comb.fixed=FALSE, 
               comb.random=TRUE,hakn=FALSE,prediction=FALSE,pscale=100, outclab = "", print.byvar = F)

m_val<-metaprop(full_combo$case, full_combo$pop, studlab=paste(author,year,sep=", "), 
             data = full_combo, subset = NULL,sm="PFT",level = 0.95, level.comb = 0.95,comb.fixed=FALSE, 
             comb.random=TRUE,hakn=FALSE,prediction=FALSE,pscale=100, outclab = "", print.byvar = F)


##sort meta-analysis by country
mu_co <- update(m_co, byvar = country)
mu_co
##print PDF of results
pdf(file = 'forest_country_penul.pdf',width=10,height=31) 
forest(mu_co, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-analysis by region
mu_reg <- update(m_reg, byvar= region)
mu_reg
##print PDF of results
pdf(file = 'forest_region_penul.pdf',width=10,height=19) 
forest(mu_reg, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-analysis by ROB
mu3 <- update(m1, byvar=ROB)
mu3
##print PDF of results
pdf(file = 'forest_ROB.pdf',width=10,height=17) 
forest(mu3, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-analysis by validated tool
mu4 <- update(m1, byvar=tool)
mu4
##print PDF of results
pdf(file = 'forest_tool.pdf',width=10,height=17) 
forest(mu4, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-analysis by ER/PR tool
mu_ER_PR <- update(m_ER_PR, byvar=tool_info_ER_PR)
mu_ER_PR
##print PDF of results
pdf(file = 'forest_ER_PR_penul.pdf',width=10,height=9) 
forest(mu_ER_PR, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-anlysis by HER2 tool
mu_HER2 <- update(m_HER2, byvar=tool_info_HER2)
mu_HER2
##print PDF of results
pdf(file = 'forest_HER2_penul.pdf',width=10,height=8) 
forest(mu_HER2, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-analysis by ASCO 
mu_val <- update(m_val, byvar = ASCO)
mu_val
##print PDF of results
pdf(file = 'forest_ASCO.pdf',width=10,height=13) 
forest(mu_val, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()

##sort meta-analysis by year
mu5 <- update(m1, byvar=year)
mu5
##print PDF of results
pdf(file = 'forest_year.pdf',width=10,height=25) 
forest(mu5, weight = "random", leftcols = c("studlab", "event", "n"), rightcols = c("effect", "ci", "w.random"), 
       leftlabs = c("Study", "Cases", "Total"), rightlabs = c("TNBC Frequency (%)", "95% CI", "Weight"), 
       bysort = T, xlab = "TNBC Frequency (%)", fs.study = 10, fs.study.label = 11, ff.study = "plain", ff.heading = "bold", 
       ff.fixed = "plain", col.square = "blue", colgap.forest.left = "1.5cm",  col.by= "black", fs.heading= 13, 
       addspace = T, digits = 1, squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2", print.byvar = F, xlim= c(0,100))
dev.off()


#metaregression
mr1 <- metareg(m_co, country)
mr1
mr2 <- metareg(m_reg, region)
mr2
mr3 <- metareg(m_HER2, tool_info_HER2)
mr3
mr4 <- metareg(m_ER_PR, tool_info_ER_PR)
mr4
bubble(mr4)
mr5 <- metareg(m1, tool_info_ER_PR)
mr5
mr6 <- metareg(m1, tool_info_HER2)
mr6
mr7 <- metareg(m1, mean_age)
mr7
bubble(mr7, lwd = 2, col.line = "blue", cex = "fixed", 
       xlab = "Mean age at diagnosis", ylab = "Treatment effect")
mr8 <- metareg(m1, median_age)
mr8
bubble(mr8, lwd = 2, col.line = "blue", cex = "fixed", 
       xlab = "Median age at diagnosis", ylab = "Treatment effect")
bubble(mr8)
mr9 <- metareg(m1, tool)
bubble(mr9, lwd = 2, col.line = "blue", cex = "fixed", 
       xlab = "Tool for Receptor Status", ylab = "Treatment effect")
mr9
bubble(mr9)
mr10 <-metareg(m_val, ASCO)
mr10
bubble(mr10)
mr11 <- metareg(m1, year)
mr11
bubble(mr11, lwd = 2, col.line = "blue", cex = "fixed", 
       xlab = "Year of Publication", ylab = "Treatment effect", xlim = c(2009,2021))

##Influence analysis
mi<-metainf(m1, pooled = "random")
forest(mi, rightcols = c("tau2", "I2"), rightlabs = c("tau2", "I2"), col.square = "blue")
pdf(file = 'meta_inf.pdf',width=8,height=15) 
forest(mi, layout = "JAMA")
dev.off()

##Baujat analysis
mb<-baujat(m1)
baujat(m1, yscale = 10, xmin = 75, ymin = 10, pos= 4, ylim = c(0,100), xlim = c(0,120))
baujat(m1, yscale = 10, xmin = 4, ymin = 1,
       pos = 1, xlim = c(0, 6.5))
##Print Baujat analysis
pdf(file = 'forest_b.pdf',width=11,height=12) 
baujat(m1, yscale = 10, xmin = 75, ymin = 10, pos = 4, ylim = c(0,100), xlim = c(0,120))
dev.off()
## Egger's Test
metabias(m1, method.bias = "Egger", plotit = T)$pval
metabias(m1, method.bias = "linreg")$p.value
metabias(m1, method.bias = "Egger", plotit = T)
