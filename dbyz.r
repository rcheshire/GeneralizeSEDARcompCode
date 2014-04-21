########  Function that allows division by 0
#  only created so that missing years can be included in composition matrix
#  in order to maintain size of matrix so for subsequent actions
dbyz=function(x,y) ifelse(y==0,0,base:::"/"(x,y))