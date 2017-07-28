
impute<-function(data)
{
  print(summarizecol(data))
  uniquevalue(data)
  proportion<-FindNum(data)
  print(proportion)
  data1 <- missing(data)
  summary(data1)
}

summarizecol <- function(x){
  cols <- names(x)
  data <- as.data.table(x)
  
  
  Summary <- data[,.(Col_names=cols,Col_type=lapply(.SD,class),Mean=lapply(.SD,mean,na.rm=T),
                     Median=lapply(.SD,median,na.rm=T),SD=lapply(.SD,sd,na.rm=T),Min=lapply(.SD,min,na.rm=T),Max=lapply(.SD,max,na.rm=T),
                     N_levels=lapply(data,uniq)),.SDcols=cols]
  return(Summary)
}

uniq <- function(x){
  a=length(unique(x))
  return(a)
}

uniquevalue<-function(data)
{
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
       {
         uniq<-length(unique(data[,i]))
         if(uniq<4)
         {
           print(sprintf("Looking at the variabilty,we think %s might be a categorical varaiable",names(data)[i]))
           print('Do you want to convert this to a categorical variable ?')
           choice<-readline(prompt = "Press 1 to convert the variable into categorical,Press 0 to continue anyway")
           if(choice==1)
           {
             data[,i]<-as.factor(data[,i])
           }
         }
    }
  }
  return()
}




#missing values,proportion of missing values and number of outliers on upper and lower side
FindNum <- function(data1){
  data1 <- as.data.table(data1)
  Missing <- data1[,.(VariableName=names(data1),NumberofNA=lapply(data1, function(x) sum(is.na(x))),
                      PercentofNA=lapply(data1, function(x) (round(sum(is.na(x)*100/nrow(data1)),2))),
                      No_Of_Outliers_UB= lapply(data1,function(x) sum(x>Upper_Bound(x),na.rm = TRUE)),
                      No_Of_Outliers_LB= lapply(data1,function(x) sum(x<Lower_Bound(x),na.rm = TRUE))
  )]
  suppressWarnings(mice_plot <- aggr(data1, col = c('blue', 'green'),
                    numbers = T, sortVars = T,
                    labels = names(data1), cex.axis = 0.7,
                    gap = 4, ylab = c("Missing Values", "Pattern")))
  
  return(Missing)
  
}


Upper_Bound <- function(u){
  ub = min(max(u,na.rm = TRUE),quantile(u,0.75,na.rm=TRUE)+ (1.5 *IQR(u,na.rm=TRUE)))
  return(ub)
}

Lower_Bound <- function(l){
  lb = max(min(l,na.rm = TRUE),quantile(l,0.25,na.rm=TRUE)- (1.5 *IQR(l,na.rm=TRUE)))
  return(lb)
}



missing<-function(data)
{
  for(i in 1:ncol(data))
  {
    miss<-length(data[,i][is.na(data[,i])])
    miss_percent<-miss/nrow(data)*100
    print(sprintf("The variable %s has %f missing values",names(data)[i],miss_percent))
    if(miss_percent!=0)
    {
      if(miss_percent>50)
      {
        print("We suggest you go ahead with modelling imputation")
        print("Here we have to call the model imputation function")
      }
      else
      {
        if(is.numeric(data[,i]))
        {
          suggestions(data[,i])
          print('Enter 1 for Mean Imputation')
          print('Enter 2 for Median Imputation')
          choice<-readline(prompt = "Enter Your Choice: ")
          if(choice==1){
            print('Call the mean imputation function')
            data[is.na(data[,i]),i] <- mean(data[,i], na.rm = TRUE)
          }else
            {print('Call the mode imputation function')
            data[is.na(data[,i]),i] <- median(data[,i],na.rm = T)
        }}
        else
          {print('We have to call the mode imputation function')
          data[is.na(data[,i]),i] <- Mode(data,i)
      }}
    }
    else
      print("Kudos!There are no missing values")
    print("---------------------------------")
  }
return(data)}

#Mode Function
Mode <- function(data1,i)
{
  tab <- table(data1[,i])
  Mode <- names(tab)[tab==max(tab)]
  return(Mode)
}
suggestions<-function(variable)
{
  skew<-skewness(variable,na.rm = T)
  kurt<-kurtosis(variable,na.rm=T)
  if((skew<=0.8 && skew>=-0.8)&&(kurt>=-3 && kurt<=3))
    print('We suggest a mean imputation')
  else
    print('We suggest a median imputation')  
  return()
}


