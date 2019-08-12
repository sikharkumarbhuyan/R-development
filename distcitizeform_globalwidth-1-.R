library("infotheo", lib.loc="~/R/win-library/3.2")
library("discretization", lib.loc="~/R/win-library/3.2")
mydata_temp=discretize(abcd, disc="globalequalwidth", nbins=2)
mydata=mydata_temp-1                                                                            
write.table(mydata, "d:/abcd_Discrete.tsv", sep="\t")
