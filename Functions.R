
options(warn=-1)
impute<-function(data)
{
  library(data.table)
  library(moments)
  library(VIM)
  library(missForest)
  options(warn=-1)
  print(summarizecol(data))
  cat('\n\n')
  data1 <- uniquevalue(data)
  cat('\n\n')
  proportion<-FindNum(data)
  print(proportion)
  cat('\n\n')
  data1 <- missing(data1)
  cat('\n\n')
  summary(data1)
}

summarizecol <- function(x1){
  if (!"install.load" %in% rownames(installed.packages()))
    install.packages("install.load")
  library(install.load)
  #install the required packages
  pkgs_to_install_load <- c("data.table")
  sapply(pkgs_to_install_load,install_load)
  a1 <- split(names(x1),sapply(x1, function(x) paste(class(x))))
  cols <- c(a1$integer,a1$numeric)
  data <- as.data.table(x1)


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
           Sys.sleep(1)
           cat('\n')
           print(sprintf("Looking at the variabilty,we think %s might be a categorical varaiable",names(data)[i]))
           print('Do you want to convert this to a categorical variable ?')
           choice<-readline(prompt = "Press 1 to convert the variable into categorical,Press 0 to continue anyway:  ")
           if(choice==1)
           {
             data[,i]<-as.factor(data[,i])
           }
         }
    }
  }
  return(data)
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
  if (!"install.load" %in% rownames(installed.packages()))
    install.packages("install.load")
  library(install.load)
  #install the required packages
  pkgs_to_install_load <- c("missForest")
  sapply(pkgs_to_install_load,install_load)
  for(i in 1:ncol(data))
  {
    cat('\n')
    miss<-length(data[,i][is.na(data[,i])])
    miss_percent<-miss/nrow(data)*100
    Sys.sleep(time = 3)
    print(sprintf("The variable %s has %.2f percent missing values",names(data)[i],miss_percent))
    if(miss_percent!=0)
    {
      if(miss_percent>=10 & miss_percent<=50)
        print("These type of variables will be imputed automatically towards the end")
      else if(miss_percent>50)
      {
        data[,i]<-NULL
      }
      else if(miss_percent<10)
      {
        if(is.numeric(data[,i]))
        {
          suggestions(data[,i])
          print('Enter 1 for Mean Imputation')
          print('Enter 2 for Median Imputation')
          choice<-readline(prompt = "Enter Your Choice: ")
          if(choice==1){
            data[is.na(data[,i]),i] <- mean(data[,i], na.rm = TRUE)
          }else
            {
            data[is.na(data[,i]),i] <- median(data[,i],na.rm = T)
        }}
        else
          {
          data[is.na(data[,i]),i] <- Mode(data,i)
          }}
    }
    else
      print("Kudos!There are no missing values")
    print("---------------------------------")
  }
  Sys.sleep(2)
  print("Imputing columns with more than 10% missing data")
  options(warn=-1)
  data.imp<-missForest(data)
  data<-data.imp$ximp

  print("Congratulations! Your dataset has been successfully imputed and is ready for some SMART Analysis")
  Sys.sleep(5)
  cat('\n\n')
  print('Here is a summary of the data set')
  summary(data)
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
  if (!"install.load" %in% rownames(installed.packages()))
    install.packages("install.load")
  library(install.load)
  #install the required packages
  pkgs_to_install_load <- c("moments")
  sapply(pkgs_to_install_load,install_load)
  skew<-skewness(variable,na.rm = T)
  kurt<-kurtosis(variable,na.rm=T)
  if((skew<=0.8 && skew>=-0.8)&&(kurt>=-3 && kurt<=3))
    print('We suggest a mean imputation')
  else
    print('We suggest a median imputation')
  return()
}


