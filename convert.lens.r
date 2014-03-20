#####################################################
# Function to convert length units to a common standard
# Fish lengths are typically taken in 1 of 3 units; 
# standard length (sl). fork length (fl), or total length (tl).

# This program requires user specified conversions among length
#     measurement types.  It also requires a field with the length
#     unit for each  length observation (The naming convention can be 
#     changed in the options).
#
# Created by Rob Cheshire 3/18/2014
# last edited by Rob Cheshire 3/18/2014
######################################################

######################################################
# Data is assumed to be in mm.

convert.lens=function(x,unit.field='LENGTH_TYPE1',
                      unit.names=c('STANDARD LENGTH','FORK LENGTH','TOTAL LENGTH'),
                      out.unit='fl',
                      sl=function(x){x=0.0 +1.0*x},
                      tl=function(x){x=0.0+1.0*x},
                      fl=function(x){x=0.0+1.0*x})
{if(to.lower(substr(out.unit)=='s'){
  x$out.unit[tolower(substr(x$unit.field]))=='s',]=x
  x$out.unit[tolower(substr(x$unit.field]))=='f',]=fl(x)
  x$out.unit[tolower(substr(x$unit.field]))=='t',]=tl(x)}
if(to.lower(substr(out.unit)=='f'){
  x$out.unit[tolower(substr(x$unit.field]))=='f',]=x  
  x$out.unit[tolower(substr(x$unit.field]))=='s',]=sl(x)
  x$out.unit[tolower(substr(x$unit.field]))=='t',]=tl(x)}
if(to.lower(substr(out.unit)=='t'){
  x$out.unit[tolower(substr(x$unit.field]))=='t',]=x    
  x$out.unit[tolower(substr(x$unit.field]))=='f',]=fl(x)
  x$out.unit[tolower(substr(x$unit.field]))=='s',]=sl(x)}
}