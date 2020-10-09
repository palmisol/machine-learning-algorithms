WK_R <- function(cluster1, cluster2)
{
  
  n = length(cluster1)
  a1 = 0
  a2 = 0
  a3 = 0
  a4 = 0
  
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (cluster1[i] == cluster1[j])
      {   match1=1  }
      else
      {   match1=0 }
      if (cluster2[i] == cluster2[j])
      {   match2=1  }
      else
      {   match2=0 }
      
      if (match1==1 & match2==1) a1=a1+1
      if (match1==1 & match2==0) a2=a2+1
      if (match1==0 & match2==1) a3=a3+1
      if (match1==0 & match2==0) a4=a4+1
    }
  }
  
  n1 = a1 + a3
  n2 = a2 + a4
  f1 = a1 + a2
  f2 = a3 + a4
  
  wkappa = (2 * (a1 * a4 - a2 * a3) ) / (n1 * f2 + n2 * f1)
  
}