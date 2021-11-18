import pandas as pd
import numpy as np
import scipy.stats as stats


def tchisq(obs):
  obs=obs+0.0001 
  esp=np.zeros((2,obs.shape[1]))
  for i in range(2):
    for j in range(obs.shape[1]):
      esp[i,j]=(obs[i,:].sum()*obs[:,j].sum())/np.sum(obs)

    chi=np.nan_to_num(((obs-esp)**2 /esp),posinf=0.0)
    test=np.sum(chi)
    return test

def utils(data,ind,alpha):
  n=data.shape[0]
  p=data.shape[1]
  classe=len(np.unique(data.iloc[:,p-1]))
  discredata=data
  threshold=stats.chi2.ppf(1-alpha, df=classe-1)
  z=np.sort(np.unique(data.iloc[:,ind]))
  midpoint=np.repeat(0.0,len(z)+1)
  midpoint[0]=np.min(z)
  midpoint[len(z)]=np.max(z)
  for k in range(1,len(z)):
    midpoint[k]=(z[k-1]+z[k])/2
  cutpoint=np.delete(midpoint,[0,len(z)])
  while True:
    a=pd.cut(data.iloc[:,ind],bins=midpoint,include_lowest=True)
    b=pd.crosstab(a,data.iloc[:,p-1])
    b=b.to_numpy()
    m=b.shape[0]
    if m==1:
      break
    test=np.repeat(0.0,(m-1))
    d=np.zeros((2,classe))
    for k in range(m-1):
      d[0,:]=b[k,:]
      d[1,:]=b[k+1,:]
      test[k]=tchisq(d)
    if np.min(test)<threshold:
      index=np.repeat(-1,m)
      serie=np.arange(0,m-1,1)
      for k in range(m-1):
        if test[k]==np.min(test):
          index[k]=serie[k]
      index=np.sort(np.unique(index))
      index=np.delete(index,0)
      outpoint=cutpoint[min(index)]
      for k in range(len(midpoint)):
        if midpoint[k]==outpoint:
          midpoint[k]=0

      midpoint=np.sort(midpoint)
      midpoint=np.delete(midpoint,0)
      cutpoint=np.delete(cutpoint,min(index))
    else :
      break
  return pd.cut(data.iloc[:,ind],bins=midpoint,include_lowest=True)

def chiMerge(data,varcon,alpha=0.5):
  discredata=data
  p=data.shape[1]
  colnames=data.columns
  res={}
  for j in varcon :
    discredata.iloc[:,j]=utils(data,j,alpha)
    res[colnames[j]]=pd.crosstab(discredata.iloc[:,j],data.iloc[:,p-1])
  return discredata,res

