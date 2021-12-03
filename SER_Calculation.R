library(RadioGx)

#CI<-function(a,A,b,B)  {a/A + b/B} #Calculating the combination index
#linear.quadratic<-function(D,a,b){exp(-a*D-b*D^2)} #Linear quadratic function

ComputeDX<-function(a,b,S) { 
  if (a & b > 0){ (-a + sqrt(a^2-(4*b*log(S))))/(b*2)
  } else if (a ==0 & b> 0){(sqrt(log(S)/-b)) 
  } else if (a>0 & b == 0) { (log(S)/-a)
  } else {NA}}
##Equivalent to RadioGX computeD10
##Use for other effect levels 

##Where are your data files located?
##Need them in a very simple data format with surviving fraction, drug doses as columns, radiation doses as rows
##setwd("/Users/meghanlambie/Documents/R Work For Project/Paper2/abema")

##Write out all your cell lines here 
cell_lines<-c("Cal33", "KYSE410", "KYSE510", "UT-SCC-60A", "UT-SCC-74B", "UT-SCC-79B")
#Add your doses here
doses<- c("0", "0.05", "0.1","0.25","0.5", "1")

##Make the empty data frames for the mid-results, and the final SER results 
##Have set up here for many different levels of SER, can only do SER37 if you want 

# DOSE10<-matrix(nrow = length(cell_lines), ncol = length(doses))
# DOSE30<-matrix(nrow = length(cell_lines), ncol = length(doses))
# DOSE50<-matrix(nrow = length(cell_lines), ncol = length(doses))
# DOSE70<-matrix(nrow = length(cell_lines), ncol = length(doses))
# 
# FINAL10<-matrix(nrow = length(cell_lines), ncol = length(doses))
# FINAL30<-matrix(nrow = length(cell_lines), ncol = length(doses))
# FINAL50<-matrix(nrow = length(cell_lines), ncol = length(doses))
# FINAL70<-matrix(nrow = length(cell_lines), ncol = length(doses))

DOSE37<-matrix(nrow = length(cell_lines), ncol = length(doses))
rownames(DOSE37)<-cell_lines
colnames(DOSE37)<-doses
FINAL37<-matrix(nrow = length(cell_lines), ncol = length(doses))

docs<-list.files()
i<-1
for (i in 1:length(docs)) {
  Cell_Line<-read.csv(docs[i], header = F)
  
  Cell_Line_Result<-matrix(data = NA, nrow = 2, ncol = ncol(Cell_Line))
  rownames(Cell_Line_Result)<-c("alpha", "beta")
  j<-2
  for (j in 2:7){
    result <- linearQuadraticModel(Cell_Line[,1], Cell_Line[,j]) ## Cell_Line[,1] should be your radiation doses
    Cell_Line_Result[1,j]<-result[1]
    Cell_Line_Result[2,j]<-result[2]
  }
  
  Cell_Line_Result<-Cell_Line_Result[,-1]
  colnames(Cell_Line_Result)<-doses
  
  
  # S<-0.7
  # k<-1
  # for (k in 1:ncol(Cell_Line_Result)){
  #   a<-Cell_Line_Result[1,k]
  #   b<-Cell_Line_Result[2,k]
  #   
  #   Dx<- ComputeDX(a, b, S)
  #   DOSE70[i,k]<-Dx
  # }
  # 
  # SF<-0.5
  # k<-1
  # for (k in 1:ncol(Cell_Line_Result)){
  #   a<-Cell_Line_Result[1,k]
  #   b<-Cell_Line_Result[2,k]
  #   
  #   Dx<- ComputeDX(a, b, SF)
  #   DOSE50[i,k]<-Dx
  # }
  
  # SF<-0.3
  # k<-1
  # for (k in 1:ncol(Cell_Line_Result)){
  #   a<-Cell_Line_Result[1,k]
  #   b<-Cell_Line_Result[2,k]
  #   
  #   Dx<- ComputeDX(a, b, SF)
  #   DOSE30[i,k]<-Dx
  # }
  
  # SF<-0.1
  # k<-1
  # for (k in 1:ncol(Cell_Line_Result)){
  #   a<-Cell_Line_Result[1,k]
  #   b<-Cell_Line_Result[2,k]
  # 
  #   Dx<- ComputeDX(a, b, SF)
  #   DOSE10[i,k]<-Dx
  # }
  
  
  
  SF<-0.37
  k<-1
  for (k in 1:ncol(Cell_Line_Result)){
    a<-Cell_Line_Result[1,k]
    b<-Cell_Line_Result[2,k]
    
    Dx<- ComputeDX(a, b, SF)
    DOSE37[i,k]<-Dx
  }
}

#Calculate SER

##replace with whatever dose level you want 

i<-1
j<-1
for (j in 1:nrow(DOSE37)) {
  
  for (i in 1:ncol(DOSE37)){
    
    FINAL37[j,i]<-DOSE37[j,1]/DOSE37[j,i]
    
  }}




##If you want all the different SER levels:

effect_levels<-c(DOSE70, DOSE50, DOSE30, DOSE10)
result_levels<-c(FINAL70, FINAL50, FINAL30, FINAL10)

i<-1
j<-1
k<-1
for (k in 1:length(k)){
  
  for (j in 1:nrow(effect_levels[k])) {
    
    for (i in 1:ncol(effect_levels[k])){
      
      result_leve[j,i]<-DOSE37[j,1]/DOSE37[j,i]
}
}}

