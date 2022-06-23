# Convert Prem 5-year Contact matrices to User-defined *smaller* matrices
# Not tested for expanding Contact matrices beyond 5 year bands



br <-c(1,4,8,13,14,15,16, 17) # Define breaks in 5-age-band categories to form new categories; HACK:last element = last category+1 length(br)==G+1

#conversion function
def_contact <- function(pc, br){
  pc_sum<-array(0, c(G,G)) #define model contact matrix template
  for (i in 1:G){
    for  (j in 1:G) {
      pc_sum[i,j] <- (rowSums(as.matrix(pc[br[i]:(br[i+1]-1),br[j]:(br[j+1]-1) ]))%*%natpop16[br[i]:(br[i+1]-1)])
    }
  }
  pc_g <- pc_sum*(1/natpopG)
  return(pc_g)
}


# pc <-prem$contact_all_5[,-1]
# try<-def_contact(pc, br)
