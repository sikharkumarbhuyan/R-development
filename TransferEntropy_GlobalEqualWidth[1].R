row=length(abcd_Discrete[,2])
col=length(abcd_Discrete[2,])
p=1
tp=0
pxy=array(0,dim=c(col,col))
pxy1=array(0,dim=c(col,col))
pxy2=array(0,dim=c(col,col))

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


tp1=1:(f)
tp2=1:(f)


for(i in 1:(col-1))
{
  k=i+1
  for(k in k:col)
  {
    
    p000=0
    p001=0
    p010=0
    p011=0
    p100=0
    p101=0
    p110=0
    p111=0
    
    p00x=0
    p01x=0
    p10x=0
    p11x=0
    
    p00y=0
    p01y=0
    p10y=0
    p11y=0
    
    x0=0
    x1=0
    
    z1=0
    z2=0
    z3=0
    z4=0
    z5=0
    z6=0
    z7=0
    z8=0
    
    
    
    for(j in 1:(row-1))
    {
      
      if((abcd_Discrete[j+1,i])==0)
        if((abcd_Discrete[j,i])==0)
          if((abcd_Discrete[j,k])==0)
            p000=p000+1
          
          
          if((abcd_Discrete[j+1,i])==0)
            if((abcd_Discrete[j,i])==0)
              if((abcd_Discrete[j,k])==1)
                p001=p001+1
              
              
              if((abcd_Discrete[j+1,i])==0)
                if((abcd_Discrete[j,i])==1)
                  if((abcd_Discrete[j,k])==0)
                    p010=p010+1
                  
                  
                  if((abcd_Discrete[j+1,i])==0)
                    if((abcd_Discrete[j,i])==1)
                      if((abcd_Discrete[j,k])==1)
                        p011=p011+1
                      
                      
                      if((abcd_Discrete[j+1,i])==1)
                        if((abcd_Discrete[j,i])==0)
                          if((abcd_Discrete[j,k])==0)
                            p100=p100+1
                          
                          
                          if((abcd_Discrete[j+1,i])==1)
                            if((abcd_Discrete[j,i])==0)
                              if((abcd_Discrete[j,k])==1)
                                p101=p101+1
                              
                              
                              if((abcd_Discrete[j+1,i])==1)
                                if((abcd_Discrete[j,i])==1)
                                  if((abcd_Discrete[j,k])==0)
                                    p110=p110+1
                                  
                                  
                                  if((abcd_Discrete[j+1,i])==1)
                                    if((abcd_Discrete[j,i])==1)
                                      if((abcd_Discrete[j,k])==1)
                                        p111=p111+1
                                      
                                      
                                      if((abcd_Discrete[j+1,i]==0))
                                        if((abcd_Discrete[j,i])==0)
                                          p00x=p00x+1
                                        
                                        
                                        if((abcd_Discrete[j+1,i]==0))
                                          if((abcd_Discrete[j,i])==1)
                                            p01x=p01x+1
                                          
                                          
                                          if((abcd_Discrete[j+1,i]==1))
                                            if((abcd_Discrete[j,i])==0)
                                              p10x=p10x+1
                                            
                                            
                                            if((abcd_Discrete[j+1,i]==1))
                                              if((abcd_Discrete[j,i])==1)
                                                p11x=p11x+1
                                              
                                              if((abcd_Discrete[j,i])==0)
                                                if((abcd_Discrete[j,k]==0))
                                                  p00y=p00y+1
                                                
                                                if((abcd_Discrete[j,i])==0)
                                                  if((abcd_Discrete[j,k]==1))
                                                    p01y=p01y+1
                                                  
                                                  if((abcd_Discrete[j,i])==1)
                                                    if((abcd_Discrete[j,k]==0))
                                                      p10y=p10y+1
                                                    
                                                    if((abcd_Discrete[j,i])==1)
                                                      if((abcd_Discrete[j,k]==1))
                                                        p11y=p11y+1    
                                                      
                                                      if((abcd_Discrete[j,i])==0)
                                                        x0=x0+1
                                                      
                                                      if((abcd_Discrete[j,i])==1)
                                                        x1=x1+1
                                                      
    }
    
    print(i)
    print(k)
    
    if((abcd_Discrete[j+1,i]==0))
      if((abcd_Discrete[j+1,k])==0)
        p00y=p00y+1
    
    
    if((abcd_Discrete[j+1,i]==0))
      if((abcd_Discrete[j+1,k])==1)
        p01y=p01y+1
    
    
    if((abcd_Discrete[j+1,i]==1))
      if((abcd_Discrete[j+1,k])==0)
        p10y=p10y+1
    
    
    if((abcd_Discrete[j+1,i]==1))
      if((abcd_Discrete[j+1,k])==1)
        p11y=p11y+1    
    
    if((abcd_Discrete[j+1,i])==0)
      x0=x0+1
    
    if((abcd_Discrete[j+1,i])==1)
      x1=x1+1
    
    
    p000=p000/row
    p001=p001/row
    p010=p010/row
    p011=p011/row
    p100=p100/row
    p101=p101/row
    p110=p110/row
    p111=p111/row
    
    p00x=p00x/row
    p01x=p01x/row
    p10x=p10x/row
    p11x=p11x/row
    
    p00y=p00y/row
    p01y=p01y/row
    p10y=p10y/row
    p11y=p11y/row
    
    x0=x0/row
    x1=x1/row
    
    
    
    if(p000==0 || ((p000*x0)==0) || ((p00y*p00x)==0))
      z1=0
    else
      z1=(p000*log2((p000*x0)/(p00y*p00x)))
    
    if(p001==0 || ((p001*x0)==0) || ((p01y*p00x)==0))
      z2=0
    else
      z2=(p001*log2((p001*x0)/(p01y*p00x)))
    
    if(p010==0 || ((p010*x1)==0) || ((p10y*p01x)==0))
      z3=0
    else
      z3=(p010*log2((p010*x1)/(p10y*p01x)))
    
    if(p011==0 || ((p011*x1)==0) || ((p11y*p01x)==0))
      z4=0
    else
      z4=(p011*log2((p011*x1)/(p11y*p01x)))
    
    if(p100==0 || ((p100*x0)==0) || ((p00y*p10x)==0))
      z5=0
    else
      z5=(p100*log2((p100*x0)/(p00y*p10x)))
    
    if(p101==0 || ((p101*x0)==0) || ((p01y*p10x)==0))
      z6=0
    else
      z6=(p101*log2((p101*x0)/(p01y*p10x)))
    
    if(p110==0 || ((p110*x1)==0) || ((p10y*p11x)==0))
      z7=0
    else
      z7=(p110*log2((p110*x1)/(p10y*p11x)))
    
    if(p111==0 || ((p111*x1)==0) || ((p11y*p11x)==0))
      z8=0
    else
      z8=(p111*log2((p111*x1)/(p11y*p11x)))
    
    
    
    
    z=z1+z2+z3+z4+z5+z6+z7+z8
    tp1[p]=z
    pxy1[k,i]=tp1[p];
    print(tp1[p])
    p=p+1
    
  }
}





p=1

for(i in col:2)
{
  k=i-1
  for(k in k:1)
  {
    p000=0
    p001=0
    p010=0
    p011=0
    p100=0
    p101=0
    p110=0
    p111=0
    
    p00x=0
    p01x=0
    p10x=0
    p11x=0
    
    p00y=0
    p01y=0
    p10y=0
    p11y=0
    
    y0=0
    y1=0
    
    z1=0
    z2=0
    z3=0
    z4=0
    z5=0
    z6=0
    z7=0
    z8=0
    
    
    
    for(j in 1:(row-1))
    {
      if((abcd_Discrete[j+1,i])==0)
        if((abcd_Discrete[j,k])==0)
          if((abcd_Discrete[j,i])==0)
            p000=p000+1
          
          if((abcd_Discrete[j+1,i])==0)
            if((abcd_Discrete[j,k])==0)
              if((abcd_Discrete[j,i])==1)
                p001=p001+1
              
              
              if((abcd_Discrete[j+1,i])==0)
                if((abcd_Discrete[j,k])==1)
                  if((abcd_Discrete[j,i])==0)
                    p010=p010+1
                  
                  
                  if((abcd_Discrete[j+1,i])==0)
                    if((abcd_Discrete[j,k])==1)
                      if((abcd_Discrete[j,i])==1)
                        p011=p011+1
                      
                      
                      if((abcd_Discrete[j+1,i])==1)
                        if((abcd_Discrete[j,k])==0)
                          if((abcd_Discrete[j,i])==0)
                            p100=p100+1
                          
                          if((abcd_Discrete[j+1,i])==1)
                            if((abcd_Discrete[j,k])==0)
                              if((abcd_Discrete[j,i])==1)
                                p101=p101+1
                              
                              
                              if((abcd_Discrete[j+1,i])==1)
                                if((abcd_Discrete[j,k])==1)
                                  if((abcd_Discrete[j,i])==0)
                                    p110=p110+1
                                  
                                  
                                  if((abcd_Discrete[j+1,i])==1)
                                    if((abcd_Discrete[j,k])==1)
                                      if((abcd_Discrete[j,i])==1)
                                        p111=p111+1
                                      
                                      
                                      if((abcd_Discrete[j+1,i]==0))
                                        if((abcd_Discrete[j,i])==0)
                                          p00x=p00x+1
                                        
                                        
                                        if((abcd_Discrete[j+1,i]==0))
                                          if((abcd_Discrete[j,i])==1)
                                            p01x=p01x+1
                                          
                                          
                                          if((abcd_Discrete[j+1,i]==1))
                                            if((abcd_Discrete[j,i])==0)
                                              p10x=p10x+1
                                            
                                            
                                            if((abcd_Discrete[j,i])==1)
                                              if((abcd_Discrete[j+1,i]==1))
                                                p11x=p11x+1
                                              
                                              
                                              if((abcd_Discrete[j,k]==0))
                                                if((abcd_Discrete[j,i])==0)
                                                  p00y=p00y+1
                                                
                                                
                                                if((abcd_Discrete[j,k]==0))
                                                  if((abcd_Discrete[j,i])==1)
                                                    p01y=p01y+1
                                                  
                                                  
                                                  if((abcd_Discrete[j,k]==1))
                                                    if((abcd_Discrete[j,i])==0)
                                                      p10y=p10y+1
                                                    
                                                    
                                                    if((abcd_Discrete[j,k]==1))
                                                      if((abcd_Discrete[j,i])==1)
                                                        p11y=p11y+1    
                                                      
                                                      if((abcd_Discrete[j,i])==0)
                                                        y0=y0+1
                                                      
                                                      if((abcd_Discrete[j,i])==1)
                                                        y1=y1+1
                                                      
    }
    
    print(i)
    print(k)
    
    if((abcd_Discrete[j+1,k]==0))
      if((abcd_Discrete[j+1,i])==0)
        p00y=p00y+1
    
    
    if((abcd_Discrete[j+1,k]==0))
      if((abcd_Discrete[j+1,i])==1)
        p01y=p01y+1
    
    
    if((abcd_Discrete[j+1,k]==1))
      if((abcd_Discrete[j+1,i])==0)
        p10y=p10y+11
    
    
    if((abcd_Discrete[j+1,k]==1))
      if((abcd_Discrete[j+1,i])==1)
        p11y=p11y+1    
    
    if((abcd_Discrete[j+1,i])==0)
      y0=y0+1
    
    if((abcd_Discrete[j+1,i])==1)
      y1=y1+1
    
    p000=p000/row
    p001=p001/row
    p010=p010/row
    p011=p011/row
    p100=p100/row
    p101=p101/row
    p110=p110/row
    p111=p111/row
    
    p00x=p00x/row
    p01x=p01x/row
    p10x=p10x/row
    p11x=p11x/row
    
    p00y=p00y/row
    p01y=p01y/row
    p10y=p10y/row
    p11y=p11y/row
    
    y0=y0/row
    y1=y1/row
    
    
    if(p000==0 || ((p000*y0)==0) || ((p00y*p00x)==0))
      z1=0
    else
      z1=(p000*log2((p000*y0)/(p00y*p00x)))
    
    if(p001==0 || ((p001*y0)==0) || ((p01y*p01x)==0))
      z2=0
    else
      z2=(p001*log2((p001*y0)/(p01y*p01x)))
    
    if(p010==0 || ((p010*y1)==0) || ((p10y*p00x)==0))
      z3=0
    else
      z3=(p010*log2((p010*y1)/(p10y*p00x)))
    
    if(p011==0 || ((p011*y1)==0) || ((p11y*p01x)==0))
      z4=0
    else
      z4=(p011*log2((p011*y1)/(p11y*p01x)))
    
    if(p100==0 || ((p100*y0)==0) || ((p00y*p10x)==0))
      z5=0
    else
      z5=(p100*log2((p100*y0)/(p00y*p10x)))
    
    if(p101==0 || ((p101*y0)==0) || ((p01y*p11x)==0))
      z6=0
    else
      z6=(p101*log2((p101*y0)/(p01y*p11x)))
    
    if(p110==0 || ((p110*y1)==0) || ((p10y*p10x)==0))
      z7=0
    else
      z7=(p110*log2((p110*y1)/(p10y*p10x)))
    
    if(p111==0 || ((p111*y1)==0) || ((p11y*p11x)==0))
      z8=0
    else
      z8=(p111*log2((p111*y1)/(p11y*p11x)))
    
    
    
    
    z=z1+z2+z3+z4+z5+z6+z7+z8
    tp2[p]=z
    pxy2[k,i]=tp2[p];
    print(tp2[p])
    p=p+1
    
  }
}



thr1=mean(tp1);
for(i in 1:f)
{
  if(tp1[i]>=thr1)
    print(tp1[i]);
}


thr2=mean(tp2);
for(i in 1:f)
{
  if(tp2[i]>=thr2)
    print(tp2[i]);
}



for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    pxy[k,i]=pxy1[k,i]
    pxy[i,k]=pxy2[i,k]
  }
}

write.table(pxy, "d:/GlobalEqlWidth_TE.tsv", sep="\t")



for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    if(pxy1[k,i]<thr1)
    {
      pxy1[k,i]=0;
    }
  }
}


for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    if(pxy2[i,k]<thr2)
    {
      pxy2[i,k]=0;
    }
  }
}


pxy=array(0,dim=c(col,col))

for(i in 1:(col-1))
{
  k=i+1;
  for(k in k:col)
  {
    if(pxy1[k,i]>pxy2[i,k])
    {
      pxy[k,i]=1;
    }
    else
      pxy[i,k]=1;
  }
}

write.table(pxy, "d:/GlobalEqlWidth_TE_Nodes.tsv", sep="\t")



row1=length(gold[,2])
for(i in 1:(col-1))
{
  j=i+1;
  for(j in j:col)
  {
    if(pxy[i,j]==1)
      for(k in 1:row1)
      {
        if(gold[k,1]==i)
          if(gold[k,2]==j)
            tp=tp+1
      }
  }
}



print(tp)



