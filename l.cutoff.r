################cutoff.r###########################
# function that subsets length data based on sample size cutoff
#
#
# written by Rob Cheshire 3/25/2014
# last edited by Rob Cheshire 4/17/2014
#
############################ Options ###############
# cutoff=30       value - acceptable sample size
# cut.type=rep('s',length(x$gear))
#                 's'-applies cutoff for each strata, vector with length equal to gear
#                 'a'-applies cutoff annually, vector with length equal to gear
# ss='trip'       'trip' cutoff is applied to the number of trips
#                  'fish' cutoff is applied to the number of fish
##########################
require(doBy)
l.cutoff=function(x,cutoff=30,ss='fish',cut.type='s'){
if(ss=='fish'){
  if (cut.type=='a'){
        cut=summaryBy(len~gear+year, data=x,FUN=length)
        cut=cut[cut$len.length>=cutoff,]
        y=x[x$gear%in%cut$gear&x$year%in%cut$year,]
        y}
    else if(cut.type=='s'){
        cut=summaryBy(len~gear+year+region+tmp, data=x,FUN=length)
        cut=cut[cut$len.length>=cutoff,]
        y=x[x$gear%in%cut$gear&x$year%in%cut$year&x$region%in%cut$region&x$tmp%in%cut$tmp,]
      y} else {
      message('cut.type error:  cutoff must be applied by strata (s) or annually (a)')  
      }
}

if(ss=='trip'){
  x.trip=x[duplicated(x$tripid)==FALSE,]
  if (cut.type=='a'){
    cut=summaryBy(len~gear+year, data=x.trip,FUN=length)
    cut=cut[cut$len.length>=cutoff,]
    y=x[x$gear%in%cut$gear&x$year%in%cut$year,]
    y}
  else if(cut.type=='s'){
    cut=summaryBy(len~gear+year+region+tmp, data=x.trip,FUN=length)
    cut=cut[cut$len.length>=cutoff,]
    y=x[x$gear%in%cut$gear&x$year%in%cut$year&x$region%in%cut$region&x$tmp%in%cut$tmp,]
    y} else {
      message('cut.type error:  cutoff must be applied by strata (s) or annually (a)')  
    }
}    
}


#cut.type=rep('s',length(levels(as.factor(x$gear)))),  ##add later to allow different cutoffs for each gear type