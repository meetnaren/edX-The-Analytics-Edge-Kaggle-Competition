poll=read.csv("train2016.csv",na.strings=c("","NA"))
poll$EducationLevel=factor(poll$EducationLevel,levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree"),ordered=TRUE)
poll$Income=factor(poll$Income,levels=c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000"),ordered=TRUE)
poll=poll[-c(which((poll$YOB>2005 & !is.na(poll$YOB)) | (poll$YOB<1920 & !is.na(poll$YOB)))),]
poll$Age=2016-poll$YOB
#poll$NACounts=rowSums(is.na(poll[,8:107]))
poll$YOB=NULL

polltest=read.csv("test2016.csv",na.strings=c("","NA"))
polltest$EducationLevel=factor(polltest$EducationLevel,levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree"),ordered=TRUE)
polltest$Income=factor(polltest$Income,levels=c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000"),ordered=TRUE)
polltest$Age=2016-polltest$YOB
polltest$YOB=NULL
polltest$Party=NA

