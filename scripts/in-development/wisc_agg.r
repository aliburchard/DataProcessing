#####Script to aggregate parsed CSV File
###load file
Rawdata<-read.csv(file.choose(), stringsAsFactors=F)###choose parsed file

####First two rows of raw data are useless, remove them
Rawdata<-Rawdata[c(-1, -2), -1]

####Renaming species in English format....  
Rawdata$choice[Rawdata$choice=='BBCT']<-'BOBCAT'
Rawdata$choice[Rawdata$choice=='BDGR']<-'BADGER'
Rawdata$choice[Rawdata$choice=='BR']<-'BEAR'
Rawdata$choice[Rawdata$choice=='BVR']<-'BEAVER'
Rawdata$choice[Rawdata$choice=='CGR']<-'COUGAR'
Rawdata$choice[Rawdata$choice=='CRNSNDHLL']<-'CRANESANDHILL'
Rawdata$choice[Rawdata$choice=='CRNSNDHLL']<-'CRANEWHOOPING'
Rawdata$choice[Rawdata$choice=='CT']<-'COYOTE'
Rawdata$choice[Rawdata$choice=='CTDMSTC']<-'CATDOMESTIC'
Rawdata$choice[Rawdata$choice=='CTTNTL']<-'COTTONTAIL'
Rawdata$choice[Rawdata$choice=='DGDMSTC']<-'DOGDOMESTIC'
Rawdata$choice[Rawdata$choice=='DR']<-'DEER'
Rawdata$choice[Rawdata$choice=='FSHR']<-'FISHER'
Rawdata$choice[Rawdata$choice=='FXGR']<-'FOXGRAY'
Rawdata$choice[Rawdata$choice=='FXRD']<-'FOXRED'
Rawdata$choice[Rawdata$choice=='GRS']<-'GROUSE'
Rawdata$choice[Rawdata$choice=='HMN']<-'HUMAN'
Rawdata$choice[Rawdata$choice=='JCKRBBT']<-'JACKRABBIT'
Rawdata$choice[Rawdata$choice=='LK']<-'ELK'
Rawdata$choice[Rawdata$choice=='LNX']<-'LYNX'
Rawdata$choice[Rawdata$choice=='MNK']<-'MINK'
Rawdata$choice[Rawdata$choice=='MPHBNSNDRPTLS']<-'REPTILESANDAMPHIBIANS'
Rawdata$choice[Rawdata$choice=='RPTLSNDMPHBNS']<-'REPTILESANDAMPHIBIANS'
Rawdata$choice[Rawdata$choice=='MS']<-'MOOSE'
Rawdata$choice[Rawdata$choice=='MRTN']<-'MARTEN'
Rawdata$choice[Rawdata$choice=='MSKRT']<-'MUSKRAT'
Rawdata$choice[Rawdata$choice=='NTHNGHR']<-'NOTHINGHERE'
Rawdata$choice[Rawdata$choice=='PGFRL']<-'PIGFERAL'
Rawdata$choice[Rawdata$choice=='PHSNT']<-'PHEASANT'
Rawdata$choice[Rawdata$choice=='PRCPN']<-'PORCUPINE'
Rawdata$choice[Rawdata$choice=='PSSM']<-'OPOSSUM'
Rawdata$choice[Rawdata$choice=='RCCN']<-'RACCOON'
Rawdata$choice[Rawdata$choice=='SKNKSTRPD']<-'SKUNKSTRIPED'
Rawdata$choice[Rawdata$choice=='SKNKSPTTD']<-'SKUNKSPOTTED'
Rawdata$choice[Rawdata$choice=='SNWSHHR']<-'SNOWSHOEHARE'
Rawdata$choice[Rawdata$choice=='SQRRLSNDCHPMNKS']<-'SQUIRRELSANDCHIPMUNKS'
Rawdata$choice[Rawdata$choice=='CHPMNKSNDSQRRLS']<-'SQUIRRELSANDCHIPMUNKS'
Rawdata$choice[Rawdata$choice=='THRBRD']<-'OTHERBIRD'
Rawdata$choice[Rawdata$choice=='BRD']<-'OTHERBIRD'
Rawdata$choice[Rawdata$choice=='THRRDNT']<-'OTHERSMALLMAMMAL'
Rawdata$choice[Rawdata$choice=='THRSMLLMMML']<-'OTHERSMALLMAMMAL'
Rawdata$choice[Rawdata$choice=='THRDMSTC']<-'OTHERDOMESTIC'
Rawdata$choice[Rawdata$choice=='TRK']<-'TURKEY'
Rawdata$choice[Rawdata$choice=='TTR']<-'OTTER'
Rawdata$choice[Rawdata$choice=='WLF']<-'WOLF'
Rawdata$choice[Rawdata$choice=='WDCHCK']<-'WOODCHUCK'
Rawdata$choice[Rawdata$choice=='WLVRN']<-'WOLVERINE'
Rawdata$choice[Rawdata$choice=='WSL']<-'WEASEL'

Rawdata$ANS_HWMN<-apply(Rawdata[,c("ANS_HOWMANY", "ANS_HWMN")], 1, max)
Rawdata$ANS_NG<-apply(Rawdata[,c("ANS_NG", "ANS_YOUNG")], 1, max)
Rawdata$ANS_DLTS<-apply(Rawdata[,c("ANS_DLTS", "ANS_ADULTS")], 1, max)
Rawdata$ANS_DLTNTLRD<-apply(Rawdata[,c("ANS_DLTNTLRD", "ANS_ADULTANTLERED")], 1, max)
Rawdata$ANS_DLTNTLRLSS<-apply(Rawdata[,c("ANS_DLTNTLRLSS", "ANS_ADULTANTLERLESS")], 1, max)
Rawdata$ANS_DLTHDNTVSBL<-apply(Rawdata[,c("ANS_DLTHDNTVSBL", "ANS_ADULTHEADNOTVISIBLE")], 1, max)



####Manipulate the varied categorical questions so that they are similar
Rawdata$ANS_NGPRSNT[Rawdata$ANS_NGPRSNT=="0"]<-NA
Rawdata$ANS_NGPRSNT[Rawdata$ANS_NGPRSNT=="N"]<-"0"
Rawdata$ANS_NGPRSNT[Rawdata$ANS_NGPRSNT=="S"]<-"1"

Rawdata$ANS_YOUNGPRESENT[Rawdata$ANS_NGPRSNT=="0"]<-NA
Rawdata$ANS_YOUNGPRESENT[Rawdata$ANS_YOUNGPRESENT=="NO"]<-"0"
Rawdata$ANS_YOUNGPRESENT[Rawdata$ANS_YOUNGPRESENT=="YES"]<-"1"
Rawdata$ANS_NGPRSNT<-apply(Rawdata[,c("ANS_NGPRSNT", "ANS_YOUNGPRESENT")], 1, max, na.rm=T)


Rawdata$ANS_CLLRVSBL[Rawdata$ANS_CLLRVSBL=="0"]<-NA
Rawdata$ANS_CLLRVSBL[Rawdata$ANS_CLLRVSBL=="N"]<-"0"
Rawdata$ANS_CLLRVSBL[Rawdata$ANS_CLLRVSBL=="S"]<-"1"

Rawdata$ANS_CLLRPRSNT[Rawdata$ANS_CLLRPRSNT=="0"]<-NA
Rawdata$ANS_CLLRPRSNT[Rawdata$ANS_CLLRPRSNT=="N"]<-"0"
Rawdata$ANS_CLLRPRSNT[Rawdata$ANS_CLLRPRSNT=="S"]<-"1"

Rawdata$ANS_COLLARVISIBLE[Rawdata$ANS_COLLARVISIBLE=="0"]<-NA
Rawdata$ANS_COLLARVISIBLE[Rawdata$ANS_COLLARVISIBLE=="NO"]<-"0"
Rawdata$ANS_COLLARVISIBLE[Rawdata$ANS_COLLARVISIBLE=="YES"]<-"1"


Rawdata$ANS_CLLRVSBL<-apply(Rawdata[,c("ANS_CLLRVSBL", "ANS_COLLARVISIBLE", "ANS_CLLRPRSNT")],1, max, na.rm=T)


###Load neccessary packages
require(dplyr)
require(tidyr)

###Needed function
###Okay, don't do this
#Mode <- function(x) {
#  ux <- unique(x)
# if(!anyDuplicated(x)){
#    NA_character_ } else { 
#     tbl <-   tabulate(match(x, ux))
#     paste(ux[tbl==max(tbl)], collapse = " and/or ")
#    }
#}

###Alternative function, which basically does the same thing better
Mode2 <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
  paste(ux[tab==max(tab)],collapse = " and/or ")
}

###Manipulate raw data
#remove characters from timestamps
Rawdata$retired_at<-gsub('T', " ", Rawdata$retired_at)
Rawdata$retired_at<-gsub('Z', "", Rawdata$retired_at)

###note, parsing script could really use actual time photo was taken as well...
Rawdata$finished_at<-gsub('T', " ", Rawdata$finished_at)
Rawdata$finished_at<-gsub('Z', "", Rawdata$finished_at)

Rawdata$retired_at<-as.POSIXlt(Rawdata$retired_at, format="%Y-%m-%d %H:%M:%S")
Rawdata$finished_at<-as.POSIXlt(Rawdata$finished_at, format="%Y-%m-%d %H:%M:%S")

#Remove classifications happening after image is retired.
#Should these be kept? Unclear.

Rawdata<-Rawdata[Rawdata$finished_at < Rawdata$retired_at,]


###Change all Posix back to a format usable by Plyr
Rawdata$finished_at<-as.character(Rawdata$finished_at)
Rawdata$retired_at<-as.character(Rawdata$retired_at)

####Remove duplicated Rawdata based upon species/count
Rawdata<-distinct(Rawdata, ANS_DLTHDNTVSBL, ANS_DLTNTLRD, ANS_DLTNTLRLSS, ANS_DLTS,
                  ANS_HWMN, ANS_NG, ANS_NGPRSNT, choice, user_ip, subject_id)
###Could be more strict, and remove everything with the same s_id, user, species

###Create dataframe to hold aggregated results
RawAgg<-Rawdata[!duplicated(Rawdata$subject_id),]###1 row, 1 sequence
#RawAgg<-RawAgg[,c(1,2,25,26,27,28,33,34,35)]###Just adding a bunch of requisite date/time/camera things
###Note, remove specific workflows?
###The raw data includes beta-testing...dificult to evaluate
###Note, 1-5-2017, parsed file needs grid_id, and actual date/time of photograph
RawAgg<-RawAgg[,c(19,18,14)]

##Let's pull aside gold standard and other expert classifications
RawData_Experts<-Rawdata[Rawdata$expert == "expert",]
RawData_GS<-Rawdata[Rawdata$gold_standard!="",]

#RawData_GS<-Rawdata[!is.na(Rawdata$gold_standard), ]
#RawData_Experts<-RawData_Experts[,c(19, 23)]
#RawData_GS<-RawData_GS[,c(19, 23)]

###Here, we arrange expert/GS votes for subjects in descending order and take the top 1;
###for whatever reason, Mode function does not seem to work here
###Update! Mode2 function works
ExpertChoice<-RawData_Experts %>% group_by(subject_id) %>% summarise(Expert_Species=Mode2(choice))
GS_Choice<-RawData_GS %>% group_by(subject_id) %>% summarise(GS_Species =Mode2(choice))
#RawAgg<-left_join(RawAgg, ExpertChoice, by='subject_id')
RawAgg<-left_join(RawAgg, GS_Choice, by='subject_id')
###Aggregate now contains columns for any expert votes, as well as GS votes.
###May not want to keep expert votes that are not gold standard.

rm(ExpertChoice, GS_Choice)



###Now need to aggregate initial choicees
###Mode (multiple options permitted) of raw votes
#x<-Rawdata %>%group_by(subject_id) %>%summarise(VoteConsensus=Mode2(choice))
###Number classifications per subject
y<-Rawdata %>% group_by(subject_id) %>%summarise(numclass=n())
###Number users per subject
z<-Rawdata %>% group_by(subject_id, user_ip)  %>%summarise(numclass=n()) %>% summarise(numusers=n_distinct(user_ip))
#x2<-Rawdata %>%group_by(subject_id, choice) %>% tally() ###number raw votes for species by sequence
#x3<-left_join(x2, y)
#colnames(x3)[3]<-'nvotesforspecies'
####Proportion of **votes** for a species
#x3$propvotes<-x3$nvotesforspecies/x3$numclass

###Now a bunch of rearrangement
#x4<-left_join(x, x3, by='subject_id')
#x5<-left_join(x4, z, by='subject_id')
#x5.2<- x5 %>% arrange(subject_id, desc(propvotes))
#x5.3<- x5.2 %>% group_by(subject_id)  %>% summarise(n_species_voted_for=n_distinct(choice))
#x5.4<-left_join(x5.2, x5.3)
###first part of evenness calculation
#x5.4$pielou_vote_spec<--1*(x5.4$propvotes*log(x5.4$propvotes))
###Second
#x5.5<-x5.4 %>% group_by(subject_id) %>%summarize(pielouvote=sum(pielou_vote_spec))
###Third
#X5.5<-left_join(x5.4, x5.5)
#X5.5$Pielou_votes<-X5.5$pielouvote/log(X5.5$n_species_voted_for)
#X5.5$Pielou_votes[is.nan(X5.5$Pielou_votes)]<-0

###Remove duplicate rows, and extraneous columns
#x6<-X5.5[!duplicated(X5.5$subject_id),]
#x6$choices<-NULL
#x6$pielou_vote_spec<-NULL
#x6$pielouvote<-NULL
#x6$choice<-NULL

###Join to aggregate file
y<-left_join(y, z)
RawAgg<-left_join(RawAgg, y, by='subject_id')



###One way to derive a sense of how many species...how many classifications/user?
RawAgg$MeanNspecies<-RawAgg$numclass/RawAgg$numusers

###The above is entirely driven by number of votes, thus vulnerable to users voting twice.
###I think the stuff below may be more reliable (but we'll check).

y2<-Rawdata %>%group_by(subject_id, user_ip, choice) %>%summarise(numuservotesspec=n())
y2.2<- y2 %>% group_by(subject_id)%>% summarise(ConsensusSpecies=Mode2(choice), ConsensusNspec=Mode2(numuservotesspec))
RawAgg<-left_join(RawAgg, y2.2, by='subject_id')
###Note, NspecMode reflects the most common number of species seen in a picture, but the mode function preserves 'ties'...note, that
###all photos in Seasons 1-3 seem to have one species, although this is objectively incorrect.
###Through seasons 1/2, 3 species seems to be the maximum number ever suggested (a total of one time), and perhaps the easiest
###thing to do is simply take the three most popular species...[done below]


###Calculations for metrics by unique users rather than votes
y3<-y2 %>%group_by(subject_id, choice) %>% summarise(numuservotes=n())
y3.1<-left_join(y3, z)
y3.1$propusers<-y3.1$numuservotes/y3.1$numusers
y3.1<-y3.1%>%arrange(subject_id, desc(propusers))
y3.2<-y3.1[!duplicated(y3.1$subject_id),]
y3.2$numusers<-NULL

#y3.1$pielou_user_spec<--1*(y3.1$propusers*log(y3.1$propusers))
#y3.2<-y3.1%>% group_by(subject_id) %>%summarize(pielouuser=sum(pielou_user_spec))
#y3.3<-left_join(y3.1, y3.2)
#y3.3<-left_join(y3.3, x5.3, by='subject_id')
#y3.3$Pielou_users<-y3.3$pielouuser/log(y3.3$n_species_voted_for)
###Note, Pielou is not bound at one when considering users because users can vote twice...

#y3.3$Pielou_users[is.nan(y3.3$Pielou_users)]<-0
#y3.3<-y3.3[!duplicated(y3.3$subject_id),]
#y3.3<-y3.3[,c(1, 5,9)]
RawAgg<-left_join(RawAgg, y3.2, by='subject_id')


###If there are multiple species within an image, we may want to know what these animals are:
###I've capped at three because I can't think of any picture I've seen that would have any more than that.
multimodes<-y3.1[,c(1:3, 5)]
multimodes2<- multimodes %>%group_by(subject_id)%>%mutate(rank=row_number(-propusers))
multimodes3<-multimodes2[,c(1, 2, 5)]
multimodes4<-multimodes2[,c(1, 3, 5)]
multimodes5<-multimodes2[,c(1,4,5)]

multimodes3<-multimodes3 %>% spread(rank, choice)
multimodes4<-multimodes4 %>% spread(rank, numuservotes)
multimodes5<-multimodes5 %>% spread(rank, propusers)

multimodes3<-multimodes3[,1:4]
multimodes4<-multimodes4[,1:4]
multimodes5<-multimodes5[,1:4]

colnames(multimodes3)<-c('subject_id', 'species1', 'species2', 'species3')
colnames(multimodes4)<-c('subject_id', 'usersspecies1', 'usersspecies2', 'usersspecies3')
colnames(multimodes5)<-c('subject_id', 'propusersspecies1', 'propusersspecies2', 'propusersspecies3')

multimodes<-left_join(multimodes3, multimodes5)
#multimodes<-left_join(multimodes, multimodes5)
RawAgg<-left_join(RawAgg, multimodes)

###Write it out
##write.csv(RawAgg, file=paste("Species_Aggregate",Sys.Date(),".csv", sep=""))

rm(list=setdiff(ls(), c("RawAgg", 'Rawdata', 'Mode', 'Mode2')))



###Add column to represent N_animals!

Rawdata<-Rawdata %>% rowwise() %>% mutate(NAnimals=sum(ANS_HWMN, ANS_DLTS, ANS_NG, ANS_DLTNTLRD,
                                                       ANS_DLTNTLRLSS, ANS_DLTHDNTVSBL, na.rm=T))
RawData_Experts<-Rawdata[Rawdata$expert == "expert",]
RawData_GS<-Rawdata[Rawdata$gold_standard!="",]
#RawData_GS<-Rawdata[!is.na(Rawdata$gold_standard), ]
###Remove outlying counts of animals
#Rawdata = Rawdata %>% group_by(subject_id) %>%
#filter(!(abs(NAnimals - mean(NAnimals)) > 2*sd(NAnimals)))
#We seem to lose about 90 sequences by doing this....                                                                                                                
  

####First Gold_standard or Expert Counts)
ExpertNum<-RawData_Experts %>% group_by(subject_id) %>% summarise(ExpertHowMany=Mode2(ANS_HWMN), expertnAdults=
                                                                    Mode2(ANS_DLTS), expertnYoung=Mode2(ANS_NG), expertYpresent=Mode2(ANS_NGPRSNT),
                                                                  expertNAntlered=Mode2(ANS_DLTNTLRD), expertNAntlerless=Mode2(ANS_DLTNTLRLSS),
                                                                  expertNnohead=Mode2(ANS_DLTHDNTVSBL), ExpertNAnimals=Mode2(NAnimals))

GS_NUM<-RawData_GS%>% group_by(subject_id) %>% summarise(GSHowMany=Mode2(ANS_HWMN), GSnAdults=
                                                           Mode2(ANS_DLTS), GSnYoung=Mode2(ANS_NG), GSYpresent=Mode2(ANS_NGPRSNT),
                                                         GSNAntlered=Mode2(ANS_DLTNTLRD), GSNAntlerless=Mode2(ANS_DLTNTLRLSS),
                                                         GSnohead=Mode2(ANS_DLTHDNTVSBL), GSNAnimals=Mode2(NAnimals))


###Crowd counts
###This incoporates uncertainty in species, which maybe we don't want because it shrinks everything but NAnimals towards 0

###RawAgg_count<-Rawdata %>% group_by(subject_id) %>% summarise(ModeCrowdnHowMany=Mode2(ANS_HWMN), ModeCrowdnAdults=
#                               Mode2(ANS_DLTS), ModeCrowdnYoung=Mode2(ANS_NG), ModeCrowdYpresent=Mode2(ANS_NGPRSNT),
#                              ModeCrowdNAntlered=Mode2(ANS_DLTNTLRD), ModeCrowdNAntlerless=Mode2(ANS_DLTNTLRLSS),
#                             ModeCrowdNnohead=Mode2(ANS_DLTHDNTVSBL), ModeCrowdNAnimals=Mode2(NAnimals))

RawAgg_count<-Rawdata %>% group_by(subject_id) %>% summarise(ModeCrowdNAnimals=Mode2(NAnimals))



###This all takes forever



DeerConsensus<-data.frame(RawAgg$subject_id[RawAgg$ConsensusSpecies=="DEER"], stringsAsFactors=F)
TBEConsensus<-data.frame(RawAgg$subject_id[RawAgg$ConsensusSpecies=="TURKEY"|RawAgg$ConsensusSpecies=="ELK"|RawAgg$ConsensusSpecies=="BEAR"],stringsAsFactors=F)
OtherConsensus<-data.frame(RawAgg$subject_id[RawAgg$ConsensusSpecies!='DEER'&RawAgg$ConsensusSpecies!='ELK'&RawAgg$ConsensusSpecies!="BEAR"&RawAgg$ConsensusSpecies!="TURKEY"],stringsAsFactors=F)

RawDataDeer<-Rawdata[Rawdata$choice=="DEER",]
RawDataTBE<-Rawdata[Rawdata$choice=="ELK"|Rawdata$choice=="TURKEY"|Rawdata$choice=="BEAR",]
RawDataOther<-Rawdata[Rawdata$choice!="ELK"& Rawdata$choice!="TURKEY"& Rawdata$choice!="BEAR"& Rawdata$choice!='DEER',]


RawAgg_Deer_Count<-RawDataDeer %>% group_by(subject_id) %>% summarise(ModeCrowdnYoung=Mode2(ANS_NG),
                      ModeCrowdNAntlered=Mode2(ANS_DLTNTLRD), ModeCrowdNAntlerless=Mode2(ANS_DLTNTLRLSS),
                          ModeCrowdNnohead=Mode2(ANS_DLTHDNTVSBL))

RawAgg_TBE_Count<-RawDataTBE %>% group_by(subject_id) %>% summarise(ModeCrowdnAdults=
                                Mode2(ANS_DLTS), ModeCrowdnYoung=Mode2(ANS_NG))
                                                                     
RawAgg_Other_Count<-RawDataOther %>%group_by(subject_id)%>% summarise(ModeCrowdnHowMany=Mode2(ANS_HWMN),ModeCrowdYpresent=Mode2(ANS_NGPRSNT))

colnames(DeerConsensus)[1]<-"subject_id"
colnames(TBEConsensus)[1]<-"subject_id"
colnames(OtherConsensus)[1]<-"subject_id"


DeerConsensus<-left_join(DeerConsensus, RawAgg_Deer_Count)
TBEConsensus<-left_join(TBEConsensus, RawAgg_TBE_Count)
OtherConsensus<-left_join(OtherConsensus, RawAgg_Other_Coun)

RawAgg<-left_join(RawAgg, DeerConsensus)
RawAgg<-left_join(RawAgg, TBEConsensus)
RawAgg<-left_join(RawAgg, OtherConsensus)


write.csv(RawAgg, file='<whatever>.csv')
###Probably want an Evenness measure for the count of total animals

###Note, the mode is not reliable when a specific state is only observed once (i.e., one person says 6 young, nobody else votes for young at all)
###Thus, the total number of animals may be useful for screening outlying values. An alternative is to consider the mean.

#function needed to get a reasonable mean
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}



###Sumarising individual columns/attributes, i.e., average value for HWMN, DLTS, etc.
RawAgg_count2<-na.zero(Rawdata) %>% group_by(subject_id) %>% summarise(MeanCrowdnHowMany=mean(ANS_HWMN), MeanCrowdnAdults=
                                                               mean(ANS_DLTS), MeanCrowdnYoung=mean(ANS_NG), 
                                                             MeanCrowdNAntlered=mean(ANS_DLTNTLRD), MeanCrowdNAntlerless=mean(ANS_DLTNTLRLSS),
                                                             MeanCrowdNnohead=mean(ANS_DLTHDNTVSBL),
                                                             VarCrowdnHowMany=var(ANS_HWMN), VarCrowdnAdults=
                                                            var(ANS_DLTS), VarCrowdnYoung=var(ANS_NG), VarCrowdYpresent=var(ANS_NGPRSNT),
                                                             VarCrowdNAntlered=var(ANS_DLTNTLRD), VarCrowdNAntlerless=var(ANS_DLTNTLRLSS),
                                                             VarCrowdNnohead=var(ANS_DLTHDNTVSBL
                                                             ), MeanCrowdNAnimals=mean(NAnimals), VarCrowdNAnimals=var(NAnimals))


RawAgg_count<-left_join(RawAgg_count, RawAgg_count2, by='subject_id')
RawAgg_count<-left_join(RawAgg_count, GS_NUM, by='subject_id')
rm(RawAgg_count2, GS_NUM, ExpertNum)



###Get votes for each category/count
#RawAgg_count6<-na.zero(Rawdata[,c(12:15, 20:21, 35:36)])

#DLTS
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, ANS_DLTS) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(ANS_DLTS, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'Dlts0', 'Dlts1', 'Dlts2', 'Dlts3', 'Dlts4', 'Dlts5', 'Dlts6')
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.Dlts0=sum(Dlts0, na.rm=T),
                                                                    Prop.Dlts1=sum(Dlts1, na.rm=T),
                                                                    Prop.Dlts2=sum(Dlts2, na.rm=T),
                                                                    Prop.Dlts3=sum(Dlts3, na.rm=T),
                                                                    Prop.Dlts4=sum(Dlts4, na.rm=T),
                                                                    Prop.Dlts5=sum(Dlts5, na.rm=T),
                                                                    Prop.Dlts6=sum(Dlts6, na.rm=T))

RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)



##NG
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, ANS_NG) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(ANS_NG, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'NG0', 'NG1', 'NG2', 'NG3', 'NG4', 'NG5', 'NG6')
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.NG0=sum(NG0, na.rm=T),
                                                                    Prop.NG1=sum(NG1, na.rm=T),
                                                                    Prop.NG2=sum(NG2, na.rm=T),
                                                                    Prop.NG3=sum(NG3, na.rm=T),
                                                                    Prop.NG4=sum(NG4, na.rm=T),
                                                                    Prop.NG5=sum(NG5, na.rm=T),
                                                                    Prop.NG6=sum(NG6, na.rm=T))
RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)

##HWMN
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, ANS_HWMN) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(ANS_HWMN, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'HWMN0', 'HWMN1', 'HWMN2', 'HWMN3', 'HWMN4', 'HWMN5', 'HWMN6')
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.HWMN0=sum(HWMN0, na.rm=T),
                                                                    Prop.HWMN1=sum(HWMN1, na.rm=T),
                                                                    Prop.HWMN2=sum(HWMN2, na.rm=T),
                                                                    Prop.HWMN3=sum(HWMN3, na.rm=T),
                                                                    Prop.HWMN4=sum(HWMN4, na.rm=T),
                                                                    Prop.HWMN5=sum(HWMN5, na.rm=T),
                                                                    Prop.HWMN6=sum(HWMN6, na.rm=T))
RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)

##DLTNTLRD
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, ANS_DLTNTLRD) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(ANS_DLTNTLRD, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'DLTNTLRD0', 'DLTNTLRD1', 'DLTNTLRD2', 'DLTNTLRD3', 'DLTNTLRD4', 'DLTNTLRD5', 'DLTNTLRD6')
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.DLTNTLRD0=sum(DLTNTLRD0, na.rm=T),
                                                                    Prop.DLTNTLRD1=sum(DLTNTLRD1, na.rm=T),
                                                                    Prop.DLTNTLRD2=sum(DLTNTLRD2, na.rm=T),
                                                                    Prop.DLTNTLRD3=sum(DLTNTLRD3, na.rm=T),
                                                                    Prop.DLTNTLRD4=sum(DLTNTLRD4, na.rm=T),
                                                                    Prop.DLTNTLRD5=sum(DLTNTLRD5, na.rm=T),
                                                                    Prop.DLTNTLRD6=sum(DLTNTLRD6, na.rm=T))
RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)


##DLTNTLRLSS
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, ANS_DLTNTLRLSS) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(ANS_DLTNTLRLSS, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'DLTNTLRLSS0', 'DLTNTLRLSS1', 'DLTNTLRLSS2', 'DLTNTLRLSS3', 'DLTNTLRLSS4', 'DLTNTLRLSS5', 'DLTNTLRLSS6')
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.DLTNTLRLSS0=sum(DLTNTLRLSS0, na.rm=T),
                                                                    Prop.DLTNTLRLSS1=sum(DLTNTLRLSS1, na.rm=T),
                                                                    Prop.DLTNTLRLSS2=sum(DLTNTLRLSS2, na.rm=T),
                                                                    Prop.DLTNTLRLSS3=sum(DLTNTLRLSS3, na.rm=T),
                                                                    Prop.DLTNTLRLSS4=sum(DLTNTLRLSS4, na.rm=T),
                                                                    Prop.DLTNTLRLSS5=sum(DLTNTLRLSS5, na.rm=T),
                                                                    Prop.DLTNTLRLSS6=sum(DLTNTLRLSS6, na.rm=T))
RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)

##DLTHDNTVSBL
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, ANS_DLTHDNTVSBL) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(ANS_DLTHDNTVSBL, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'DLTHDNTVSBL0', 'DLTHDNTVSBL1', 'DLTHDNTVSBL2', 'DLTHDNTVSBL3', 'DLTHDNTVSBL4', 'DLTHDNTVSBL5', 'DLTHDNTVSBL6')
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.DLTHDNTVSBL0=sum(DLTHDNTVSBL0, na.rm=T),
                                                                    Prop.DLTHDNTVSBL1=sum(DLTHDNTVSBL1, na.rm=T),
                                                                    Prop.DLTHDNTVSBL2=sum(DLTHDNTVSBL2, na.rm=T),
                                                                    Prop.DLTHDNTVSBL3=sum(DLTHDNTVSBL3, na.rm=T),
                                                                    Prop.DLTHDNTVSBL4=sum(DLTHDNTVSBL4, na.rm=T),
                                                                    Prop.DLTHDNTVSBL5=sum(DLTHDNTVSBL5, na.rm=T),
                                                                    Prop.DLTHDNTVSBL6=sum(DLTHDNTVSBL6, na.rm=T))
RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)

##Nanimals
RawAgg_count7<-Rawdata %>% group_by(subject_id, user_ip, NAnimals) %>% tally()
RawAgg_count7$n[RawAgg_count7$n > 1 ]<-1
RawAgg_count8 <-RawAgg_count7 %>% spread(NAnimals, n)
colnames(RawAgg_count8)<-c('subject_id', 'user_ip', 'NAnimals0', 'NAnimals1', 'NAnimals2', 'NAnimals3', 'NAnimals4', 'NAnimals5', 'NAnimals6',
                           'NAnimals7', 'NAnimals8', 'NAnimals9', 'NAnimals10', 'NAnimals11', 'NAnimals12',
                           'NAnimals13', 'NAnimals14', 'NAnimals15', 'NAnimals16', 'NAnimals17', 'NAnimals18',
                           'NAnimals19',  'NAnimals20','NAnimals21', 'NAnimals24')###May have to update as needed
RawAgg_count9<-RawAgg_count8 %>% group_by(subject_id) %>% summarise(Prop.NAnimals0=sum(NAnimals0, na.rm=T),
                                                                    Prop.NAnimals1=sum(NAnimals1, na.rm=T),
                                                                    Prop.NAnimals2=sum(NAnimals2, na.rm=T),
                                                                    Prop.NAnimals3=sum(NAnimals3, na.rm=T),
                                                                    Prop.NAnimals4=sum(NAnimals4, na.rm=T),
                                                                    Prop.NAnimals5=sum(NAnimals5, na.rm=T),
                                                                    Prop.NAnimals6=sum(NAnimals6, na.rm=T),
                                                                    Prop.NAnimals7=sum(NAnimals7, na.rm=T),
                                                                    Prop.NAnimals8=sum(NAnimals8, na.rm=T),
                                                                    Prop.NAnimals9=sum(NAnimals9, na.rm=T),
                                                                    Prop.NAnimals10=sum(NAnimals10, na.rm=T),
                                                                    Prop.NAnimals11=sum(NAnimals11, na.rm=T),
                                                                    Prop.NAnimals12=sum(NAnimals12, na.rm=T),
                                                                    Prop.NAnimals13=sum(NAnimals13, na.rm=T),
                                                                    Prop.NAnimals14=sum(NAnimals14, na.rm=T),
                                                                    Prop.NAnimals15=sum(NAnimals15,na.rm=T),
                                                                    Prop.NAnimals16=sum(NAnimals16, na.rm=T),
                                                                    Prop.NAnimals17=sum(NAnimals17, na.rm=T),
                                                                    Prop.NAnimals18=sum(NAnimals18, na.rm=T),
                                                                    Prop.NAnimals19=sum(NAnimals19, na.rm=T),
                                                                    Prop.Nanimals20=sum(NAnimals20, na.rm=T),
                                                                    Prop.NAnimals21=sum(NAnimals21, na.rm=T),
                                                                    Prop.NAnimals24=sum(NAnimals24, na.rm=T))
RawAgg_count<-left_join(RawAgg_count, RawAgg_count9, by='subject_id')
rm(RawAgg_count7, RawAgg_count8, RawAgg_count9)

###Join this back to the species-level aggregate
RawAgg_2<-left_join(RawAgg, RawAgg_count, by='subject_id')


Pielou<-function (c) {
  require(vegan)
  S<-sum(c!=0)
  H<-diversity(c)
  paste(H/log(S))
}
####Note, this is just calculating evenness based upon the # votes for 1 adult, or young, or head not visible
RawAgg_2$Pielou_DLTS<-apply(RawAgg_2[,61:67], 1, Pielou)
RawAgg_2$Pielou_NG<-apply(RawAgg_2[,68:74], 1, Pielou)
RawAgg_2$Pielou_HWMN<-apply(RawAgg_2[,75:81], 1, Pielou)
RawAgg_2$Pielou_DLTNTLRD<-apply(RawAgg_2[,82:88], 1, Pielou)
RawAgg_2$Pielou_DLTNTLRLSS<-apply(RawAgg_2[,89:95], 1, Pielou)
RawAgg_2$Pielou_DLTHDNTVSBL<-apply(RawAgg_2[,96:102], 1, Pielou)
RawAgg_2$Pielou_NAnimals<-apply(RawAgg_2[,103:124], 1, Pielou)



#RawAgg_2$SNG<-apply(RawAgg_2[,68:74], 1, function(c) sum(c!=0))
#RawAgg_2$SHWMN<-apply(RawAgg_2[,75:81], 1, function(c) sum(c!=0))
#RawAgg_2$SDLTNTLRD<-apply(RawAgg_2[,82:88], 1, function(c) sum(c!=0))
#RawAgg_2$SDLTNTLRLSS<-apply(RawAgg_2[,89:95], 1, function(c) sum(c!=0))
#RawAgg_2$SDLTHDNTVSBL<-apply(RawAgg_2[,96:102], 1, function(c) sum(c!=0))
#RawAgg_2$SNAnimals<-apply(RawAgg_2[,103:124], 1, function(c) sum(c!=0))
#RawAgg_2$Simpson_DLTS<-apply(RawAgg_2[,61:67], 1, diversity)

RawAgg_2[,61:124]<-RawAgg_2[,61:124]/RawAgg_2$numusers
RawAgg_2$maxprop_DLTS<-apply(RawAgg_2[,61:67], 1, max)
RawAgg_2$maxprop_NG<-apply(RawAgg_2[,68:74], 1, max)
RawAgg_2$maxprop_HWMN<-apply(RawAgg_2[,75:81], 1, max)
RawAgg_2$maxprop_DLTNTLRD<-apply(RawAgg_2[,82:88], 1, max)
RawAgg_2$maxprop_DLTNTLRLSS<-apply(RawAgg_2[,89:95], 1, max)
RawAgg_2$maxprop_DLTHDNTVSBL<-apply(RawAgg_2[,96:102], 1, max)
RawAgg_2$maxprop_NAnimals<-apply(RawAgg_2[,103:124], 1, max)

write.csv(RawAgg_2, file=paste("Species_Aggregate_Count",Sys.Date(),".csv", sep=""))

####This thing is quasi-functional (it works, but...). Things to do:
####1) Write-up behavior chunk, and write up chunk that provides information regarding
#### the proportion of USERS that vote for a specific behavior or a specific count (1:6) for all
#### available states (HWMN, DLTS, etc.). If we needed to consider raw rather than summarized counts as 
#### a random variable/input, multinomial/categorical makes most sense given classification options. part done 12_30
####2) Remove completely duplicated user votes (same photo, species, count, behavior, etc.) from data for count aggregation
####...doing so may make it easier to aggregate based upon votes alone rather than grouping votes by user, etc. Done 12_30
####3) Annotation....
####4) Chunk to remove outlying values for counts: which seem uncommon, but I see no reason to 
#### even consider a vote that implies 15 + animals when all other votes suggest 1-2 within the aggregation.  Done12_30
###Edit,1_5: removing outliers as attempted seems to  remove ~100 entire triggers! now commented out
####5) Longer-term: remove metrics, and making piping more efficient. A lot of redundant metrics, which hopefully 
#### can be thinned after an expert-based comparison. As size of parsed file increases, will be preferable to 
#### avoid storing multiple objects in memory. Could also think about aggregating things in chunks.

###Updated 1/26 to create estimates of evenness for counts of certain classes, proportion of most voted count



###Clear workspace
rm(list = ls())

