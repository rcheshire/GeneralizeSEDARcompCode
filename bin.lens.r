#FUNCTION TO POOL OR TRUNCATE BINS AND/OR CONVERT 1 CM BINS TO 3 AND 5 CM BINS
#Created by Rob Cheshire 3/14/2014
#Last edited 3/18/2014
#  DATA - x is an object with fish length in 1-cm bins with no decimal 

#################### options ############################
# from.bin=min(x)  can set to any value for the minimum size in the comp
# to.bin=max(x)    can set to any value for the maximum size in the comp
# by.bin=3,        can set to 3 or 5 cm bins (others can be added)
# pool.min=TRUE    True converts all values equal or smaller than from.bin to from.bin
#                  FALSE truncates all values smaller than from.bin 
# pool.max=FALSE   True converts all values equal or greater than to.bin to to.bin
#                  FALSE truncates all values larger than from.bin 

bin.lens=function(x,from.bin=min(x),to.bin=max(x),by.bin=3,pool.min=TRUE,pool.max=TRUE){
                  bin.seq=seq(from.bin, to.bin, by=by.bin)
                  pool.cm=rep(0,length(x))
                  #pool or truncate tails
                  if(pool.min==TRUE){x[x<from.bin]=from.bin}
                  if(pool.min==FALSE){x=x[x>=from.bin]}
                  if(pool.max==TRUE){x[x>max(bin.seq)]=max(bin.seq)}
                  if(pool.max==FALSE){x=x[x<=max(bin.seq)]}
                  #convert to 3cm bins
                  if(by.bin==3){
                    tmp.x=x-1
                    x[tmp.x%in%bin.seq]=x[tmp.x%in%bin.seq]-1 
                    tmp.x=x+1
                    x[tmp.x%in%bin.seq]=x[tmp.x%in%bin.seq]+1 
                  }
                  if(by.bin==5){
                    tmp.x=x-1
                    x[tmp.x%in%bin.seq]=x[tmp.x%in%bin.seq]-1 
                    tmp.x=x-2
                    x[tmp.x%in%bin.seq]=x[tmp.x%in%bin.seq]-2 
                    tmp.x=x+1
                    x[tmp.x%in%bin.seq]=x[tmp.x%in%bin.seq]+1 
                    tmp.x=x+2
                    x[tmp.x%in%bin.seq]=x[tmp.x%in%bin.seq]+2 
                  }
                  #print(x)
                  x
                  }

#############  notes #####################################################
# for 1 cm bins function just pools or truncates tails
# for 3 cm bins, +1,-1 in bin then +1,-1
# for 5 cm bins, +1 or +2 or -1 or -2 in bin.seq then add or subtract 1 or 2 
