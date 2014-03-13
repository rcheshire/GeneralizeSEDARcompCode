###########################################################################
# Generalized code to generate length and age composition from raw data.
#   compositions can be weighted by landings at different levels
#   3 data elements: length data, landings data, trip data (number of trips), and age data
#     expected data structure:
#       1. Length data: gear, year, time, region, lentype, len
#       2. Landings data: gear, year, time, region, landnum, landwt
#       3. Age data: gear, year, time, region, age, len
# gear, year, time, and region should be number sequence starting with one
# lentype should by defined as sl, tl, or fl
# units are defined in header
# Created by R. Cheshire, August 2013
###############################################################################
# To Do
# add toggle to turn off regional or temporal weighting
# add option to exclude compositions if sample size is below a cutoff (default=30)
#     ....evaluated by year (possibly feed or generate matrix of 1s and 0s based on sample size)
# method to combine objects when years do not match  (by rowname?)
# 
###############################################################################

windows(record=TRUE)
library(doBy)
library(statmod)
library(sfsmisc)

rnd = function(x) trunc(x+0.5) #define round function (r's version of round is not typical US version)

genComp=function(comp.type=1, inpath=getwd(), outpath=getwd(),sl=function(x){x=1.0 +1.0*x},
tl=function(x){x=2.0+1.0*x},fl=function(x){x=0.0+1.0*x},len.unit='FL',in.unit='mm', in.a.unit='mm', first.l=min(len$cm),last.l=max(len$cm),
pool.first.l=TRUE,pool.last.l=TRUE,first.a=min(age$age),last.a=max(age$age),pool.first.a=TRUE,pool.last.a=TRUE,
bin.size.l=1,bin.size.a=1, a.cut.type='fish',l.cut.type='fish',a.cutoff=30,l.cutoff=30){
setwd(inpath) #set working directory for input files
if(comp.type==1){                                     #weighted length and age comps
#read in data files
len=read.csv('in.len.csv', header=TRUE)
land=read.csv('in.land.csv',header=TRUE)
l.trips=read.csv('in.l.trips.csv',header=TRUE)
age=read.csv('in.age.csv',header=TRUE)
a.trips=read.csv('in.a.trips.csv',header=TRUE)
#make data objects available in working environment
assign("land", land, envir=globalenv()) 
assign("l.trips", l.trips, envir=globalenv()) 
assign("a.trips", a.trips, envir=globalenv()) 
setwd=outpath
#convert lengths to preferred unit
if(in.unit=='mm'){
len$cm[len$lentype=="FL"]=fl(len$len[len$lentype=="FL"])
len$cm[len$lentype=="TL"]=tl(len$len[len$lentype=="TL"])
len$cm[len$lentype=="SL"]=sl(len$len[len$lentype=="SL"])
  } else {
len$len=len$len*10
len$cm[len$lentype=="FL"]=fl(len$len[len$lentype=="FL"])
len$cm[len$lentype=="TL"]=tl(len$len[len$lentype=="TL"])
len$cm[len$lentype=="SL"]=sl(len$len[len$lentype=="SL"])
} 
#round lengths in mm to 1 cm bins
if(in.unit=='mm'){
  len$cm=rnd(len$cm/10)
  } else if(in.unit=='cm'){len$cm=rnd(len$cm)}
  assign("len", len, envir=globalenv())
#pool or truncate smallest lengths
if(pool.first.l==TRUE){
  len$cm[len$cm<first.l]=first.l
  }else{len=len[len$cm>=first.l,]}
#pool or truncate largest lengths  
if(pool.last.l==TRUE){
  len$cm[len$cm>last.l]=last.l
  } else {len=len[len$cm<=last.l,]}
  assign("len", len, envir=globalenv())
###AGE  
#convert lengths to preferred unit
if(in.unit=='mm'){
age$cm[age$lentype=="FL"]=fl(age$len[age$lentype=="FL"])
age$cm[age$lentype=="TL"]=tl(age$len[age$lentype=="TL"])
age$cm[age$lentype=="SL"]=sl(age$len[age$lentype=="SL"])
  } else {
age$len=age$len*10
age$cm[age$lentype=="FL"]=fl(age$len[age$lentype=="FL"])
age$cm[age$lentype=="TL"]=tl(age$len[age$lentype=="TL"])
age$cm[age$lentype=="SL"]=sl(age$len[age$lentype=="SL"])
} 
#round lengths of aged fish in mm to 1 cm bins
if(in.a.unit=='mm'){
  age$cm=rnd(age$cm/10)
  } else if(in.unit=='cm'){age$cm=rnd(len$cm)}  
  #pool or truncate smallest ages
if(pool.first.a==TRUE){
  age$age[age$age<first.a]=first.a
  } else{age=age[age$age>=first.a,]}
#pool or truncate largest ages  
if(pool.last.a==TRUE){
  age$age[age$age>last.a]=last.a
  } else {age=age[age$age<=last.a,]}
assign("age", age, envir=globalenv())
#begin unweighted length comp
gear=levels(as.factor(len$gear))
for(i in 1:length(gear)){
if('num' %in% colnames(len))
  {unw.l=summaryBy(num~year+cm,data=len[len$gear==gear[i],],FUN=sum)
  tab.len=xtabs(num.sum~year+cm,data=unw.l)
  nfish=rowSums(tab.len)
  norm.unw.len=tab.len/nfish
  norm.unw.len=cbind(rownames(norm.unw.len),norm.unw.len)
  colnames(norm.unw.len)[1]='year'
#fill any missing columns with 0s
unw.len.all=matrix(data=0,nrow=nrow(norm.unw.len),ncol=(length(seq(first.l,last.l))+1))
colnames(unw.len.all)=c('year',seq(first.l,last.l))
unw.len.all[,1]=norm.unw.len[,1]
x=as.numeric(colnames(norm.unw.len)[-1])-(min(len$cm)-2) #create index number 
for (j in 1:dim(norm.unw.len)[2]-1){
    unw.len.all[,x[j]]=norm.unw.len[,j+1]
    }
  } else {unw.l=summaryBy(cm~year+cm,data=len[len$gear==gear[i],],FUN=length)
  tab.len=xtabs(cm.length~year+cm,data=unw.l)
  nfish=rowSums(tab.len)
  norm.unw.len=tab.len/nfish
    norm.unw.len=cbind(rownames(norm.unw.len),norm.unw.len)
  colnames(norm.unw.len)[1]='year'
#fill any missing columns with 0s
unw.len.all=matrix(data=0,nrow=nrow(norm.unw.len),ncol=(length(seq(first.l,last.l))+1))
colnames(unw.len.all)=c('year',seq(first.l,last.l))
unw.len.all[,1]=norm.unw.len[,1]
x=as.numeric(colnames(norm.unw.len)[-1])-(min(len$cm)-2) #create index number 
for (j in 1:dim(norm.unw.len)[2]-1){
    unw.len.all[,x[j]]=norm.unw.len[,j+1]
    }
  }
write.csv(unw.len.all,file=paste(outpath,paste('unw.lcomp',gear[i],'csv',sep='.'),sep="\\"),row.names=FALSE)
}
##############  WEIGHTED LENGTH COMPOSITION
#remvoe#add landings vector to lengths
#remove#wgt.len.tmp=merge(len,land,by=c('year','gear','tmp','region'), all.x=TRUE)
#remove#lenNoLand=wgt.len.tmp[wgt.len.tmp$land=='NA',]  #removes length records with no associated landings
#remove#write.csv(lenNoLand,file='lenNoLand.csv',row.names=FALSE)
#remove########### ADD OPTION LATER TO FILL NAs WITH AVERAGES OR ADJACENT VALUES
#remove#wgt.len.tmp=wgt.len.tmp[wgt.len.tmp$land!='NA',]
#remove##report number of lentgth records missing associated landings
#remove#numDiff=dim(lenNoLand)[1]
#remove#print(paste(numDiff,"records out of", dim(wgt.len.tmp)[1],"with no associated landings (lenNoLand.csv)",sep=" "))
wgt.len.tmp=len  #temporary to keep from changing object names below
r=1
y=1
z=1

gear=levels(as.factor(wgt.len.tmp$gear))
region=levels(as.factor(wgt.len.tmp$region))
tmp=levels(as.factor(wgt.len.tmp$tmp))
den.landa=summaryBy(land~year+gear,data=land,FUN=sum)
den.land=xtabs(land.sum~year+gear,data=den.landa)
######################### WHAT TO DO IF YEAR WITHOUT COMP BY GEAR,REGION,TMP?????
for(r in 1:length(gear)){
  for(y in 1:length(region)){
    for(z in 1:length(tmp)){
  
      wgt.len=wgt.len.tmp[wgt.len.tmp$gear==gear[r]&wgt.len.tmp$region==region[y]&wgt.len.tmp$tmp==tmp[z],]
      #create factor for weighting (vector of weight by year)
      num.wt=land$land[land$gear==gear[r]&land$region==region[y]&land$tmp==tmp[z]]  #numerator for weighting
      den.wt=den.land[,gear[r]]
      wt=num.wt/den.wt
      write.csv(den.wt,file=paste('den.wt',r,y,z,'.csv',sep=''))
      write.csv(num.wt,file=paste('num.wt',r,y,z,'.csv',sep=''))
      write.csv(wt,file=paste('lcomp.wt',r,y,z,'.csv',sep=''))

if('num' %in% colnames(len))
  {w.l=summaryBy(num~year+cm,data=wgt.len,FUN=sum)
  tab.len=xtabs(num.sum~year+cm,data=w.l)
  nfish=rowSums(tab.len)  #nfish actually rowsums of number
  norm.w.len=tab.len/nfish
  norm.w.len=cbind(rownames(norm.w.len),norm.w.len)                                                     
  colnames(norm.w.len)[1]='year'
#fill any missing columns with 0s
w.len.all=matrix(data=0,nrow=nrow(norm.w.len),ncol=(length(seq(first.l,last.l))+1))
colnames(w.len.all)=c('year',seq(first.l,last.l))
w.len.all[,1]=norm.w.len[,1]
x=as.numeric(colnames(norm.w.len)[-1])-(min(len$cm)-2) #create index number 
for (k in 1:dim(norm.w.len)[2]-1){
    w.len.all[,x[k]]=norm.w.len[,k+1]
    }
  } else {w.l=summaryBy(cm~year+cm,data=wgt.len,FUN=length)
  tab.len=xtabs(cm.length~year+cm,data=w.l)
  nfish=rowSums(tab.len)    #nfish=sum of landings across by year
  norm.w.len=tab.len/nfish 
  norm.w.len=cbind(rownames(norm.w.len),norm.w.len)
  colnames(norm.w.len)[1]='year'
#fill any missing columns with 0s
w.len.all=matrix(data=0,nrow=nrow(norm.w.len),ncol=(length(seq(first.l,last.l))+1))
colnames(w.len.all)=c('year',seq(first.l,last.l))
w.len.all[,1]=norm.w.len[,1]
x=as.numeric(colnames(norm.w.len)[-1])-(min(len$cm)-2) #create index number 
for (j in 1:dim(norm.w.len)[2]-1){
    w.len.all[,x[j]]=norm.w.len[,j+1]
    }
  }
 w.len.all=apply(w.len.all,1,as.character)
 w.len.all=apply(w.len.all,1,as.numeric) 
 w.len.all=w.len.all[,-1]
 w.len.all=w.len.all*wt
 rownames(w.len.all)=norm.w.len[,1] 
 colnames(w.len.all)=seq(first.l,last.l)
  
write.csv(w.len.all,file=paste(outpath,paste('w.lcompG',r,'R',y,'T',z,'csv',sep='.'),sep="\\"))
assign(paste('w.lcomp',r,y,z,sep=''),w.len.all)
}
}}
# Combine into one comp for each gear     ############## needs work
r=1
for(r in 1:length(gear)){
  x=ls()
  y=grep(paste("w.lcomp",r,sep=""),x)      
  l=x[y]
  x1=as.data.frame(get(l[1]))  
  #savex1=x1                              
  #x1=x1[,-1]        #remove row names  
  #x1=apply(x1,1,as.character)
  #x1=apply(x1,1,as.numeric)  #change factors to numeric (must go to character first)                      
  #colnames(x1)=as.character(1:dim(x1)[2])
  for(g in 2:length(l)){
   xnext=as.data.frame(get(l[g]))
   #xnext=xnext[,-1]        #remove row names  
  #xnext=apply(xnext,1,as.character)
  #xnext=apply(xnext,1,as.numeric) 
  #colnames(xnext)=as.character(1:dim(xnext)[2])
   temp=cbind(x1,xnext)
   x1=temp
   }         #loop creates object 'temp' with length bins repeated for each gear...then combine
 xall=sapply(unique(colnames(x1)),     #combine bins with the same labels
       function(x) rowSums(x1[, colnames(x1) == x, drop = FALSE])) 
 #normalize to sum to 1 and add column and row names
 rsum=rowSums(xall)
 w.lcomp=xall/rsum
 #colnames(w.lcomp)=colnames(savex1)[-1]
 #rownames(w.lcomp)=savex1[,1]
 write.csv(w.lcomp,file=paste('w.lcomp',r,'csv',sep='.'))
       
  
###############################up to here

}
}  ##############STOP POINT FOR TESTING OF CODE WHEN COMP.TYPE=1

#}
#else if(comp.type==2){                                      #weighted length comp only
##import data
#len=read.csv('in.len.csv', header=TRUE)
#land=read.csv('in.land.csv',header=TRUE)
#l.trips=read.csv('in.l.trips.csv',header=TRUE)
##make data objects available in working environment
#assign("land", land, envir=globalenv()) 
#assign("l.trips", l.trips, envir=globalenv()) 
#setwd=outpath
##convert to lenghts
#len$cm[len$lentype=="FL"]=fl(len$len[len$lentype=="FL"])
#len$cm[len$lentype=="TL"]=tl(len$len[len$lentype=="TL"])
#len$cm[len$lentype=="SL"]=sl(len$len[len$lentype=="SL"])
##round lengths in mm to 1 cm bins
#len$cm=rnd(len$cm/10)
##pool or truncate smallest lengths
#if(pool.first.l==TRUE){
#  len$cm[len$cm<first.l]=first.l}
#else{len=len[len$cm>=first.l,]}
##pool or truncate largest lengths  
#if(pool.last.l==TRUE){
#  len$cm[len$cm>last.l]=last.l}
#else {len=len[len$cm<=last.l,]}
#  assign("len", len, envir=globalenv()) 
#}
#else if(comp.type==3){                                      #unweighted lenth composition only
##import data
#len=read.csv('in.len.csv', header=TRUE)
#l.trips=read.csv('in.l.trips.csv',header=TRUE)
##make data objects available in working environment
#assign("l.trips", l.trips, envir=globalenv()) 
#setwd=outpath
##convert to lenghts
#len$cm[len$lentype=="FL"]=fl(len$len[len$lentype=="FL"])
#len$cm[len$lentype=="TL"]=tl(len$len[len$lentype=="TL"])
#len$cm[len$lentype=="SL"]=sl(len$len[len$lentype=="SL"])
##round lengths in mm to 1 cm bins
#len$cm=rnd(len$cm/10)
##pool or truncate smallest lengths
#if(pool.first.l==TRUE){
#  len$cm[len$cm<first.l]=first.l}
#else{len=len[len$cm>=first.l,]}
##pool or truncate largest lengths  
#if(pool.last.l==TRUE){
#  len$cm[len$cm>last.l]=last.l}
#else {len=len[len$cm<=last.l,]}
#  assign("len", len, envir=globalenv())  
#}
#else if(comp.type==4){                                       #unweighted length and age composition
##import data
#len=read.csv('in.len.csv', header=TRUE)
#l.trips=read.csv('in.l.trips.csv',header=TRUE)
#age=read.csv('in.age.csv',header=TRUE)
#a.trips=read.csv('in.a.trips.csv',header=TRUE)
##make data objects available in working environment
#assign("l.trips", l.trips, envir=globalenv()) 
#assign("age", age, envir=globalenv())
#assign("a.trips", a.trips, envir=globalenv()) 
#setwd=outpath
##convert to lenghts
#len$cm[len$lentype=="FL"]=fl(len$len[len$lentype=="FL"])
#len$cm[len$lentype=="TL"]=tl(len$len[len$lentype=="TL"])
#len$cm[len$lentype=="SL"]=sl(len$len[len$lentype=="SL"])
##round lengths in mm to 1 cm bins
#len$cm=rnd(len$cm/10)
##pool or truncate smallest lengths
#if(pool.first.l==TRUE){
#  len$cm[len$cm<first.l]=first.l}
#else{len=len[len$cm>=first.l,]}
##pool or truncate largest lengths  
#if(pool.last.l==TRUE){
#  len$cm[len$cm>last.l]=last.l}
#else {len=len[len$cm<=last.l,]}
#  assign("len", len, envir=globalenv()) 
# #pool or truncate smallest ages
#if(pool.first.a==TRUE){
#  age$age[age$age<first.a]=first.a}
#else{age=age[age$age>=first.a,]}
##pool or truncate largest ages  
#if(pool.last.a==TRUE){
#  age$age[age$age>last.a]=last.a}
#else {age=age[age$age<=last.a,]}
#assign("age", age, envir=globalenv())  
#}
#
#else if(comp.type==5){                                       #unweighted age composition
##import data
#age=read.csv('in.age.csv',header=TRUE)
#a.trips=read.csv('in.a.trips.csv',header=TRUE) 
##make data objects available in working environment
#assign("age", age, envir=globalenv())
#assign("a.trips", a.trips, envir=globalenv()) 
#setwd=outpath
##convert to lenghts
#len$cm[len$lentype=="FL"]=fl(len$len[len$lentype=="FL"])
#len$cm[len$lentype=="TL"]=tl(len$len[len$lentype=="TL"])
#len$cm[len$lentype=="SL"]=sl(len$len[len$lentype=="SL"])
##round lengths in mm to 1 cm bins
#len$cm=rnd(len$cm/10)
##pool or truncate smallest lengths
#if(pool.first.l==TRUE){
#  len$cm[len$cm<first.l]=first.l}
#else{len=len[len$cm>=first.l,]}
##pool or truncate largest lengths  
#if(pool.last.l==TRUE){
#  len$cm[len$cm>last.l]=last.l}
#else {len=len[len$cm<=last.l,]}
#  assign("len", len, envir=globalenv())
# #pool or truncate smallest ages
#if(pool.first.a==TRUE){
#  age$age[age$age<first.a]=first.a}
#else{age=age[age$age>=first.a,]}
##pool or truncate largest ages  
#if(pool.last.a==TRUE){
#  age$age[age$age>last.a]=last.a}
#else {age=age[age$age<=last.a,]}
#assign("age", age, envir=globalenv())  
#}
#else{warning("compsition type (comp.type) not found", call. = TRUE, immediate. = FALSE, domain = NULL)}
#}
###lcomp
##if (letter='TRUE'){windows(width=7,height=10)
##par(mfcol=c(5,3),mar=c(1.5, 2, 1, 1) + 0.1,oma=c(4,4,0,0))}
###acomp
##
##
###Usage
###genComp(in.len,in.land,in.trips,in.age,inpath=getwd(),outpath=getwd(),acomp=TRUE, u.pool=1,u.trunc=1,l.pool=1,l.trunc=1,bin.size=1,weight.l=TRUE,
###  weight.a=TRUE, letter=TRUE, lplotdimx=3, lplotdimy=5, aplotdimx=3, aplotdimy=5,sizelim1=1,sizelimyr=1, 
##
##
##
##
##
##
##
##
##
##
##
##
############convert lengths to preferred unit
##com.len$FL[com.len$lentype=="FL"]=com.len$len[com.len$lentype=="FL"]
##com.len$FL[com.len$lentype=="TL"]=tltofl(com.len$len[com.len$lentype=="TL"])
##com.len$FL[com.len$lentype=="SL"]=sltofl(com.len$len[com.len$lentype=="SL"])
############# pooling/truncating data
##u.pool=
##u.trunc=
##l.pool=
##l.trunc=
##
#default values to test outside function
comp.type=1
inpath=getwd()
outpath=getwd()
sl=function(x){x=1.0 +1.0*x}
tl=function(x){x=2.0+1.0*x}
fl=function(x){x=0.0+1.0*x}
len.unit='FL'
in.unit='mm'
in.a.unit='mm'
pool.first.l=TRUE
pool.last.l=TRUE
pool.first.a=TRUE
pool.last.a=TRUE
bin.size.l=1
bin.size.a=1
##################    run setup to here then bring in data and run from here
first.l=min(len$cm)
last.l=max(len$cm)
first.a=min(age$age)
last.a=max(age$age)
