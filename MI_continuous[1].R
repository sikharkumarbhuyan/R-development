p=1
tp=0
sigmax=0;
sigmay=0;
sigmaxy=1;
row=length(abcd[,2])
col=length(abcd[2,])
f=1
c=col
for(c in c:1)
{
  f=c*f
  c=c-1
}
f1=1
c=col-2
for(c in c:1)
{
  f1=c*f1
  c=c-1
}
f=f/f1
f=f/2



mi=1:(f)
pxy=array(0,dim=c(col,col))


for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    s=0;
    sigma=0;
    m1=mean(abcd[,i]);
    m2=mean(abcd[,k]);
    j=1;
    for(j in j:row)
    {
      s=s+(abcd[j,i]-(1-abcd[j,i]))*(abcd[j,k]-(1-abcd[j,k]));
      sigmax=sigmax+(abcd[j,i])*(abcd[j,i]);
      sigmay=sigmay+(abcd[j,k])*(abcd[j,k]);
    }
    sigmax=sigmax-(m1*m1);
    sigmay=sigmay-(m2*m2);
    sigmax=sqrt(sigmax);
    sigmay=sqrt(sigmay);
    sigmaxy=sigmax*sigmay;
    s=s/20;
    mi[p]=(s/sigmaxy);
    
    pxy[i,k]=mi[p];
    pxy[k,i]=mi[p];
    
    print (i)
    print (k)
    print(mi[p]);
    p=p+1;
    
    
  }
}

write.table(pxy, "d:/mi_continuous.tsv", sep="\t")



i=1;
thr=mean(mi);
for(i in i:f)
{
  if(mi[i]>=thr)
    print(mi[i]);
}


for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    if(pxy[i,k]>=thr)
    {
      pxy[i,k]=1;
      pxy[k,i]=1;
      
    }
    else
    {
      pxy[i,k]=0;
      pxy[k,i]=0;
    }
  }
}

write.table(pxy, "d:/mi_continuousNodes.tsv", sep="\t")

row1=length(gold[,2])
for(i in 1:(col-1))
{
  j=i+1;
  for(j in j:col)
  {
    if(pxy[i,j]==1)
      for(k in 1:row1)
      {
        if((gold[k,1]==i)||(gold[k,1]==j))
          if((gold[k,2]==j)||(gold[k,2]==i))
            tp=tp+1
      }
  }
}
print(tp)
