p=1
tp=0
row=length(abcd_Discrete[,2])
col=length(abcd_Discrete[2,])
print(row)
print(col)
c=col

f=1
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

if(col==2)
  f=1

mi=1:(f)
pxy=array(0,dim=c(col,col))
for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    c00=0
    c01=0
    c10=0
    c11=0
    c0=0
    c1=0
    c2=0
    c3=0
    for(r in 1:row)
    {
      if(abcd_Discrete[r,i]==0)
        c0=c0+1;
      if(abcd_Discrete[r,i]==1)
        c1=c1+1;
      if(abcd_Discrete[r,k]==0)
        c2=c2+1;
      if(abcd_Discrete[r,k]==1)
        c3=c3+1;
      if(abcd_Discrete[r,i]==0)
      {
        if(abcd_Discrete[r,k]==0)
        {c00=c00+1;}
      }
      if(abcd_Discrete[r,i]==0)
      {
        if(abcd_Discrete[r,k]==1)
        {c01=c01+1;}
      }
      if(abcd_Discrete[r,i]==1)
      {
        if(abcd_Discrete[r,k]==0)
        {c10=c10+1;}
      }
      if(abcd_Discrete[r,i]==1)
      {
        if(abcd_Discrete[r,k]==1)
        {c11=c11+1;}
      }
    }
    
    c0=c0/row;
    c1=c1/row;
    c2=c2/row;
    c3=c3/row;
    c00=c00/row;
    c01=c01/row;
    c10=c10/row;
    c11=c11/row;
    print (i)
    print (k)
    
    mi[p]=c00*log2(c00/(c0*c2))+c01*log2(c01/(c0*c3))+c10*log2(c10/(c1*c2))+c11*log2(c11/(c1*c3)); 
    if(c00==0)
      mi[p]=c01*log2(c01/(c0*c3))+c10*log2(c10/(c1*c2))+c11*log2(c11/(c1*c3));
    if(c01==0)
      mi[p]=c00*log2(c00/(c0*c2))+c10*log2(c10/(c1*c2))+c11*log2(c11/(c1*c3));
    if(c10==0)
      mi[p]=c00*log2(c00/(c0*c2))+c01*log2(c01/(c0*c3))+c11*log2(c11/(c1*c3));
    if(c11==0)
      mi[p]=c00*log2(c00/(c0*c2))+c01*log2(c01/(c0*c3))+c10*log2(c10/(c1*c2));
    
    if(c00==0)
      if(c01==0)
        mi[p]=c10*log2(c10/(c1*c2))+c11*log2(c11/(c1*c3));
    if(c00==0)
      if(c10==0)
        mi[p]=c01*log2(c01/(c0*c3))+c11*log2(c11/(c1*c3));
    if(c00==0)
      if(c11==0)
        mi[p]=c01*log2(c01/(c0*c3))+c10*log2(c10/(c1*c2));
    if(c01==0)
      if(c11==0)
        mi[p]=c00*log2(c00/(c0*c2))+c10*log2(c10/(c1*c2));
    if(c01==0)
      if(c10==0)
        mi[p]=c00*log2(c00/(c0*c2))+c11*log2(c11/(c1*c3));
    if(c01==0)
      if(c11==0)
        mi[p]=c00*log2(c00/(c0*c2))+c10*log2(c10/(c1*c2));
    if(c10==0)
      if(c11==0)
        mi[p]=c00*log2(c00/(c0*c2))+c01*log2(c01/(c0*c3));
    
    
    
    if(c00==0)
      if(c01==0)
        if(c10==0)
          mi[p]=c11*log2(c11/(c1*c3))
    if(c00==0)
      if(c01==0)
        if(c11==0)
          mi[p]=c10*log2(c10/(c1*c2))
    
    if(c00==0)
      if(c10==0)
        if(c11==0)
          mi[p]=c01*log2(c01/(c0*c3))
    if(c01==0)
      if(c10==0)
        if(c11==0)
          mi[p]=c00*log2(c00/(c0*c2))
    
    
    pxy[i,k]=mi[p];
    pxy[k,i]=mi[p];
    
    
    
    
    print(mi[p]);
    p=p+1
  }
}


i=1;
thr=mean(mi);
for(i in i:f)
{
  if(mi[i]>=thr)
    print(mi[i]);
}
write.table(pxy, "d:/mi_globaleqlfreq.tsv", sep="\t")

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

write.table(pxy, "d:/mi_golbaleqlfreqNodes.tsv", sep="\t")

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
