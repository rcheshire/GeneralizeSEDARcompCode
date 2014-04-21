Project to generalize code to create length and age comps
Created by Rob Cheshire 4/8/2014
Last edited by Rob Cheshire 3/13/2014

Input files  (created for test purposes, rtc Nov 2013, e:\genCompCode\makedata)
input files are in the 'input data' folder and consist of length data (in.len.csv), landings data (in.land.csv), age data (in.age.csv) 
   and two files with a summary of the number of trips for length (in.l.trips.csv) and age (in.a.trips.csv).  Ideally we will get a trip identification added 
   to the length and age files from the providers in the future and these separate files for trips will be unnecessary.

headers for input data
in.len.csv
year	gear	tmp	region	len	lentype tripid
1995	1	1	1	273	SL	12
1995	1	1	1	233	FL	12

in.land.csv
land		year	gear	tmp	region
32675.06576	1995	1	1	1
122596.5604	1995	1	1	2
82283.74346	1995	2	1	1

in.age.csv
year	gear	tmp	region	len	frac.age	age	lentype tripid
1995	1	1	1	214	3.889611957	4	FL
1995	1	1	1	240	4.383012705	4	SL
1995	1	1	1	271	5.031008234	5	SL


analysis files
genComp.r
this is the initial attempt to generalize the method, some components of this function will be combined with the efforts to use the markdown language .rmd for the 
  commercial 2014 gag update length and age comps.

gag_comp.rmd
most recent use of code to create length and age comps, improved method of consistent size of composition objects (year by bin for all strata-specific comps)

genComp.rmd
most recent generalized code

bin.lens.r
function that bins, truncates/pools bins at tails

convert.lens.r
function that standardizes multiple measurement types 

dbyz.r
function that allows division by 0 (only used where all bins for a year and therefore sum are equal to 0 and still included so that the dimension of the individual matrices by strata are identical)


###################################
data processing steps 
Commercial:
commercial lengths include a quantity field.  To execute the genComp code each record should represent one fish.  To expand 
each record by the quantity use the code below assuming the num=quantity:
temp2=temp[rep(seq(dim(temp)[1]),temp$num),]

   