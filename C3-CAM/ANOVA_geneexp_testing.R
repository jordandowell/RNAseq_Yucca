##required input: tpm data matrix with columns specifying libraries, rows transcripts. One column must be the orthogroup ID of the transcript. Additionally, species-specific design files must be generated that have library id, treatment, timepoint. 

##NOTE - these commands can only assess expression fit of one transcript per species at a time. 

YF2727<-as.data.frame(subset(YFtpm, orthogroup=="2727"))
YF2727$orthogroup<-NULL
YF2727<-as.data.frame(t(YF2727))
YF2727$time<-YFdesign$time
YF2727$treat<-YFdesign$drought

YA2727<-as.data.frame(subset(YAtpm, orthogroup=="2727"))
YA2727$orthogroup<-NULL
YA2727<-as.data.frame(t(YA2727))
YA2727$time<-YAdesign$time
YA2727$treat<-YAdesign$drought

YG2727<-as.data.frame(subset(YGtpm, orthogroup=="2727"))
YG2727$orthogroup<-NULL
YG2727<-as.data.frame(t(YG2727))
YG2727$time<-YGdesign$time
YG2727$treat<-YGdesign$drought


YF2727sub<-YF2727
colnames(YF2727sub)<-c("exp", "time", "treat")
YF2727sub$genus<-c("YF")
YF2727subW<-subset(YF2727sub, treat=="1")
YF2727subD<-subset(YF2727sub, treat=="0")
YF2727subW$scaled<-YF2727subW$exp/max(YF2727subW$exp)
head(YF2727subW)
YF2727subD$scaled<-YF2727subD$exp/max(YF2727subD$exp)

YA2727sub<-YA2727
colnames(YA2727sub)<-c("exp", "time", "treat")
YA2727sub$genus<-c("YA")
YA2727subW<-subset(YA2727sub, treat=="1")
YA2727subD<-subset(YA2727sub, treat=="0")
YA2727subW$scaled<-YA2727subW$exp/max(YA2727subW$exp)
head(YA2727subW)
YA2727subD$scaled<-YA2727subD$exp/max(YA2727subD$exp)


YG2727sub<-YG2727
colnames(YG2727sub)<-c("exp", "time", "treat")
YG2727sub$genus<-c("YG")
YG2727subW<-subset(YG2727sub, treat=="1")
YG2727subD<-subset(YG2727sub, treat=="0")
YG2727subW$scaled<-YG2727subW$exp/max(YG2727subW$exp)
head(YG2727subW)
YG2727suYAbD$scaled<-YG2727subD$exp/max(YG2727subD$exp)

merged2727subW<-rbind(YA2727subW, YF2727subW, YG2727subW)
merged2727subD<-rbind(YA2727subD, YF2727subD, YG2727subD)

fit0<-lm(scaled ~ poly(time, 5), data=merged2727subW)
fit1<-lm(scaled ~ poly(time, 5)*genus, data=merged2727subW)
anova(fit0, fit1)

fit0<-lm(scaled ~ poly(time, 5), data=merged2727subD)
fit1<-lm(scaled ~ poly(time, 5)*genus, data=merged2727subD)
anova(fit0, fit1)
