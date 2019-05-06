# code for 2019_BIBM with Jing and Xinyuan
library("plyr")

# read data
new_only.tmp <- read.csv("clean_data_newmed_only_0225_edit.csv",header = T)[,-1]
old_only.tmp <- read.csv("clean_data_oldmed_only_edit.csv",header = T)[,-1]

#### clean the data
# dimension
new_only.tmp[0,] 
old_only.tmp[0,] 

length(unique(new_only.tmp$sk)) 
length(unique(old_only.tmp$sk)) 

# old only
dup.id = unique(old_only.tmp[duplicated(old_only.tmp$sk),]$sk) # id that duplicated
dup.old_only = old_only.tmp[old_only.tmp$sk %in% dup.id,] # data frame with all duplicated rows
update.dup.old_only.tmp = data.frame()
for (i in dup.id){
  tmp.data.0 = dup.old_only[dup.old_only$sk == i,]
  tmp.data.1 = tmp.data.0[which(tmp.data.0$age_y == max(tmp.data.0$age_y)),]
  update.dup.old_only.tmp = rbind(update.dup.old_only.tmp, tmp.data.1)
}
update.dup.old_only = update.dup.old_only.tmp[!duplicated(update.dup.old_only.tmp[,c(1:23)]),]
not.dup.old_only = old_only.tmp[!(old_only.tmp$sk %in% dup.id),]
old_only = rbind(not.dup.old_only, update.dup.old_only)

# new only
dup.id = unique(new_only.tmp[duplicated(new_only.tmp$sk),]$sk) # id that duplicated
dup.new_only = new_only.tmp[new_only.tmp$sk %in% dup.id,] # data frame with all duplicated rows
update.dup.new_only.tmp = data.frame()
for (i in dup.id){
  tmp.data.0 = dup.new_only[dup.new_only$sk == i,]
  tmp.data.1 = tmp.data.0[which(tmp.data.0$age_y == max(tmp.data.0$age_y)),]
  update.dup.new_only.tmp = rbind(update.dup.new_only.tmp, tmp.data.1)
}
update.dup.new_only = update.dup.new_only.tmp[!duplicated(update.dup.new_only.tmp[,c(1:26)]),]
not.dup.new_only = new_only.tmp[!(new_only.tmp$sk %in% dup.id),]
new_only = rbind(not.dup.new_only, update.dup.new_only)

# add indicator for group
# therapy A = old drugs only
# therapy B = new drugs only
old_only$therapy = rep("A",dim(old_only)[1])
new_only$therapy = rep("B",dim(new_only)[1])

# combine the data
full <- rbind.fill(new_only, old_only)

# change the columns' name

list_name_old_only <- c( "sk", "age_x","Abdominal pain", 
                "Anaemia","Arthralgia","Ascites", 
                "Asthenia", "Diarrhoea", "Dizziness",
                "Dyspnoea",  "Malaise",
                "Nausea","Neutropenia","Pain", 
                "Pancytopenia", "Pneumonia","Pruritus", 
                "Pyrexia","Thrombocytopenia", "Vomiting",
                "Weight decreased",
                  "Haemoglobin decreased", "Platelet count decreased", "race",
                "age_y","gender" ,"marital_status","therapy" )
list_name_long <- c( "sk", "age_x","Abdominal pain", 
                     "Anaemia","Arthralgia","Ascites", 
                     "Asthenia", "Diarrhoea", "Dizziness",
                     "Dyspnoea", "Headache",  "Malaise",
                     "Nausea","Neutropenia","Pain", 
                     "Pancytopenia", "Pneumonia","Pruritus", 
                     "Pyrexia","Thrombocytopenia", "Vomiting",
                     "Weight decreased","White blood cell count decreased", "Decreased appetite",
                     "Haemoglobin decreased", "Platelet count decreased", "race",
                     "age_y","gender","marital_status","therapy" )


colnames(old_only) <- list_name_old_only
colnames(new_only) <- list_name_long
colnames(full) <- list_name_long


# gender
A_male <- sum(old_only$gender == "Male") / dim(old_only)[1]
A_male 

B_male <- sum(new_only$gender == "Male") / dim(new_only)[1]
B_male 


# age
c(mean(old_only$age_y), sd(old_only$age_y))
c(mean(new_only$age_y), sd(new_only$age_y))

# ============================================================================
# histogram of total number of AEs in each report (24 adverse events in total)
full[is.na(full) =="TRUE"] = 0

# change the number of AE = 2 to 1
old_only[,-c(1,2,24,25,26,27,28)][old_only[,-c(1,2,24,25,26,27,28)] > 1]=1
new_only[,-c(1,2,27,28,29,30,31)][new_only[,-c(1,2,27,28,29,30,31)] > 1]=1

# full$total <- rowSums(full[,2:25])
num.old  = table(rowSums(old_only[,-c(1,2,24,25,26,27,28)]))/dim(old_only)[1]
num.old = c(num.old[1:9],sum(num.old[-c(1:9)]))
num.new_only = table(rowSums(new_only[,-c(1,2,27,28,29,30,31)]))/dim(new_only)[1]
num.new_only = c(num.new_only[1:9],sum(num.new_only[-c(1:9)]))


# num = rbind(num.old,num.new_only,num.new)
num = rbind(num.old,num.new_only)

colnames(num)[10]<- ">=10"


num.old*dim(old_only)[1];
num.new_only*dim(new_only)[1];

pdf("AE_EHR.pdf",width=12,height=8)
par(mar = c(5, 4, 5, 2) + 0.2)
barplot(num, xlab="", col=c("cornflowerblue","darkorange2"),
        ylab="", beside=TRUE,ylim=c(0,0.6),cex.axis =1.5,cex.names = 1.5)
legend(20,0.56, c("Old Drug","New Drug"), 
       bty="n",
       fill = c("cornflowerblue","darkorange2","grey"), cex=2)
mtext("Distribution of Total Number of Adverse Events Reported by Each Subject",side=3,at=15,line=2,cex=2)
mtext("Total Number of Adverse Events Reported by Each Subject",side=1,at=15,line=3,cex=2)
dev.off()


# ============================================================================
# Frequencies of 24 adverse events by drugs
# subtract the columns which are not adverse events

full.tmp = full[,-c(1,2,27,28,29,30,31)]
full.tmp[,c(25,26,27,28,29,30)] = 0

colnames(full.tmp)[colnames(full.tmp) == "V25"] <- "Fatigue"
colnames(full.tmp)[colnames(full.tmp) == "V26"] <- "Rash"
colnames(full.tmp)[colnames(full.tmp) == "V27"] <- "Drug ineffective"
colnames(full.tmp)[colnames(full.tmp) == "V28"] <- "Off label use"
colnames(full.tmp)[colnames(full.tmp) == "V29"] <- "Blood bilirubin increased"
colnames(full.tmp)[colnames(full.tmp) == "V30"] <- "Leukopenia"


f.t = full.tmp

tmp = f.t[,c("Fatigue", "Anaemia", "Nausea", "Rash", "Headache" , "Pruritus" ,"Vomiting",
                  "Diarrhoea", "Pyrexia" , "White blood cell count decreased", "Asthenia", 
                  "Decreased appetite" ,"Dyspnoea", "Dizziness", "Platelet count decreased", 
                  "Weight decreased", "Haemoglobin decreased","Pain","Malaise","Drug ineffective", 
                  "Arthralgia","Neutropenia","Thrombocytopenia","Pneumonia","Abdominal pain","Leukopenia",
                  "Pancytopenia","Off label use",
                  "Ascites","Blood bilirubin increased")]
full.tmp = tmp

# change the number of AE = 2 to 1
full.tmp[full.tmp > 1]=1
full.order = cbind(full[,31],full.tmp)
colnames(full.order)[1]="therapy"


pvalue=NULL
for(i in 1:30){
  tbl = table(full.order[,i+1], full.order[,1])
  pvalue[i]= format(round(chisq.test(tbl)$p.value,3),nsmall=3)
}

pvalue=paste("=",pvalue,sep="")
pvalue[c(1,4,20,26,28,30)] = " = NA"
pvalue[pvalue =="=0.000"]="<=0.001"


pie.old = colMeans(full.order[full.order[,1]=="A",][,-1])
pie.new = colMeans(full.order[full.order[,1]=="B",][,-1])

bar.dat = rbind(pie.old,pie.new)*100
allitem = colnames(bar.dat)


pdf("freq_barplot_EHR.pdf",width=16,height=20)
par(mar = c(18, 5, 8, 2) + 0.2)
par(mfrow=c(2,1))
barplot(bar.dat[,1:15],col=c("cornflowerblue","darkorange2"),ylab="",
        beside=TRUE,ylim=c(0,40),xaxt='n',cex.axis = 1.5)
legend(35,35, c("Old Drug","New Drug"), 
       bty="n",
       fill = c("cornflowerblue","darkorange2"), cex=2)
text(seq(0, 14, by=1)*3+1.7, par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = allitem[1:15], cex=1.5)
text(seq(0, 14, by=1)*3+2.6, par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels =paste("p-value", pvalue[1:15]), cex=1.5)
mtext("Top 1-15 Adverse Events by Drugs",side=3,at=23,line=3,cex=2.5)
mtext("Frequencies %",side=2,at=20,line=2.5,cex=2)

barplot(bar.dat[,16:30],col=c("cornflowerblue","darkorange2"),ylab="",
        beside=TRUE,ylim=c(0,40),xaxt='n',cex.axis = 1.5)
text(seq(0, 14, by=1)*3+1.7, par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = allitem[16:30], cex=1.5)
text(seq(0, 14, by=1)*3+2.6, par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels =paste("p-value", pvalue[16:30]), cex=1.5)
mtext("Top 16-30 Adverse Events by Drugs",side=3,at=23,line=3,cex=2.5)
mtext("Frequencies %",side=2,at=20,line=2.5,cex=2)
dev.off()


 # ============================================================================
# Fig. 4.	Comparison of estimated odds ratio
#get the index therapy of new_only and new
full <- rbind.fill(new_only, old_only)
full.tmp.tmp = full[,list_name_old_only]
full.tmp <- full.tmp.tmp[,-c(1,2,24,25,26,27,28)]

order(colMeans(full.tmp))
allitem = (colnames(full.tmp))[order(colMeans(full.tmp),decreasing=T)]

full.order = cbind(full[,31],full.tmp[,order(colMeans(full.tmp),decreasing=T)])
colnames(full.order)[1]="therapy"

pie.old = colMeans(full.order[full.order[,1]=="A",][,-1])
pie.new = colMeans(full.order[full.order[,1]=="B",][,-1])

bar.dat = rbind(pie.old,pie.new)*100
allitem = colnames(bar.dat)


indi.new_only = as.numeric((full.order$therapy=="B")) 

mean(old_only$age_y)

# define function expit
expit = function(x){exp(x)/(1+exp(x))}

# define the variables (gender & race)
x.age = full$age_y - 51.75
x.sex = as.numeric(full$gender) - 2

# run logistic regression without race and gender
new.est.0 = both.est.0 = array(dim=c(3,21))
for (i in 1:21){
  y = full.order[,colnames(full.order)==colnames(bar.dat)[i]]
  m = glm(y ~ indi.new_only, family=binomial)
  
  new.est.0[1,i] = summary(m)$coefficient[2,1]
  new.est.0[2,i] = summary(m)$coefficient[2,1]- 1.96*summary(m)$coefficient[2,2]
  new.est.0[3,i] = summary(m)$coefficient[2,1]+ 1.96*summary(m)$coefficient[2,2]
}


# run logistic regression with race and gender
new.est = both.est = array(dim=c(3,21))
for (i in 1:21){
  y = full.order[,colnames(full.order)==colnames(bar.dat)[i]]
  m = glm(y ~ indi.new_only+x.age+x.sex, family=binomial)
  
  new.est[1,i] = summary(m)$coefficient[2,1]
  new.est[2,i] = summary(m)$coefficient[2,1]- 1.96*summary(m)$coefficient[2,2]
  new.est[3,i] = summary(m)$coefficient[2,1]+ 1.96*summary(m)$coefficient[2,2]
}

new.est = exp(new.est)
new.est.0 = exp(new.est.0)
colnames(new.est) = colnames(bar.dat)
colnames(new.est.0) = colnames(bar.dat)

df = array(dim = c(3,9), 0)
colnames(df) = c("Fatigue","Headache" ,"Rash","White blood cell count decreased",
              "Decreased appetite","Drug ineffective",
              "Leukopenia","Off label use","Blood bilirubin increased")

new.est.full = cbind(new.est, df)
new.est.0.full = cbind(new.est.0, df)

final_name = c("Fatigue", "Anaemia", "Nausea", "Rash", "Headache" , "Pruritus" ,"Vomiting",
  "Diarrhoea", "Pyrexia" , "White blood cell count decreased", "Asthenia", 
  "Decreased appetite" ,"Dyspnoea", "Dizziness", "Platelet count decreased", 
  "Weight decreased", "Haemoglobin decreased","Pain","Malaise","Drug ineffective", 
  "Arthralgia","Neutropenia","Thrombocytopenia","Pneumonia","Abdominal pain","Leukopenia",
  "Pancytopenia","Off label use",
  "Ascites","Blood bilirubin increased")

new.est.full = new.est.full[,final_name]
new.est.0.full = new.est.0.full[,final_name]

# ============================================================================
# ============================================================================
# ============================================================================
# ============================================================================
pdf("oddsratio_EHR_BA.pdf",width=6,height=8)
par(mar = c(4, 0, 1.5, 1) + 1.5);par(mfrow=c(1,1))
plot(c(new.est.0.full[2,1],new.est.0.full[3,1]),c(-1,-1),type="l",ylim=c(-30,-1),xlim=c(0,6),
     xlab = "Estimated odds ratio and 95% confidence interval", ylab ="",yaxt="n")
points(new.est.0.full[1,1],-1,type="p",pch=18,col="black",cex=0.5)
lines(c(new.est.full[2,1],new.est.full[3,1]),c(-1.25,-1.25),col="blue")
points(new.est.full[1,1],-1.25,type="p",pch=18,col="blue",cex=0.5)


cord.y = seq(-1,-30)
for(i in 2:30){
  lines(c(new.est.0.full[2,i],new.est.0.full[3,i]),c(cord.y[i],cord.y[i]),col="black")
  points(new.est.0.full[1,i],cord.y[i],type="p",pch=18,col="black",cex=0.5)
  lines(c(new.est.full[2,i],new.est.full[3,i]),c(cord.y[i]-0.25,cord.y[i]-0.25),col="blue")
  points(new.est.full[1,i],cord.y[i]-0.25,type="p",pch=18,col="blue",cex=0.5)
}


mtext("New Drug vs. Old Drug",side=3,line=0)
abline(v=0,col="grey",lty=2,lwd=0.8)
abline(v=1,col="grey",lty=2,lwd=0.8)
abline(v=2,col="grey",lty=2,lwd=0.8)
abline(v=3,col="grey",lty=2,lwd=0.8)
abline(v=4,col="grey",lty=2,lwd=0.8)
abline(v=5,col="grey",lty=2,lwd=0.8)
abline(v=6,col="grey",lty=2,lwd=0.8)
dev.off()

