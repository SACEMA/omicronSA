# Process contact matrices

# NOTE: Contact matrices in this code should follow the convention that
# The row gives the individual and the column gives the contact
# This follows the convention used in the supplementary files in Prem et al.
# Contact matrices need to be processed into Age categories for modelling (of length G)

#### Source contact matrix processing scripts ####

br <-c(1,4,8,13,14,15,16, 17) # Define breaks in 5-age-band categories to form new categories; HACK:last element = last category+1 length(br)==G+1

#' shrink a contact matrix to combined age groups
#'
#' @param contacts numeric, dim NxN, rows = to individuals, cols = from individuals
#' @param newupperlimits integer, dim M < N,
#'   the indices of the old population breaks corresponding to the new population
#'   upperbounds. assert: sorted, no duplicates, last element == N
#' @param population numeric, dim N, the population associated with original breaks
#' 
#' @return numeric matrix, dim MxM, the new contact matrix
#' 
#' @details
#' Takes the total contact inputs by from individuals (contact columns) toward
#' to individuals (contact rows), then determines the weighted average of those
#' according to the relative contribution of the old from columns within the new
#' consolidated column
#' 
shrink_contacts <- function(
  contacts, newupperlimits, population
) {
  pc_sum <- array(dim = rep(length(newupperlimits), 2))
  uls <- newupperlimits
  lls <- c(0, head(uls, -1)) + 1
  for (to in 1:dim(pc_sum)[1]) for (from in 1:dim(pc_sum)[2]) {
    toslc <- lls[to]:uls[to]
    fromslc <- lls[from]:uls[from]
    pc_sum[to, from] <- sum(colSums(contacts[toslc, fromslc]) * population[fromslc]) / sum(population[fromslc])
  }
  return(pc_sum)
}


source('Data/contact/convert_contacts.R') # Read in function to converting to user-define age bands
source('Data/contact/mixpref.R') # Read in functions for manipulating matrices
source('Data/contact/unPop.R') # Read in UN estimates for 2017 for initial scaling





natpop16 <- rowSums(SApop17[,2:10]); natpop16[16]<-sum(natpop16[16:17]); natpop16<-natpop16[1:16] # National pop Prem 5 yr cat
natpopG <- rowSums(SApop[,2:10]) # Nat pop Model defined cat
provpop16 <- as.matrix(SApop17[,-1]); provpop16[16,]<-colSums(provpop16[16:17,]);  provpop16<-as.data.frame(provpop16[1:16,] )
all.equal(sum(provpop16),sum(natpop16),sum(natpopG)); all.equal(rowSums(provpop16),natpop16 ) #Check equivalence

prem_home <- as.matrix(prem$contact_home_5[,-1])
prem_work <- as.matrix(prem$contact_work_5[,-1])
prem_school <- as.matrix(prem$contact_school_5[,-1])
prem_other <- as.matrix(prem$contact_other_5[,-1])
prem_all <- as.matrix(prem$contact_all_5[,-1])

### Daily contact matrices (National)
nat_con_home <- convertPrem(prem_home, sa, natpop16)
nat_con_work <- convertPrem(prem_work, sa, natpop16)
nat_con_school <- convertPrem(prem_school, sa, natpop16)
nat_con_other <- convertPrem(prem_other, sa, natpop16)
nat_con_all <- convertPrem(prem_all, sa, natpop16)


### Annual contact matrices (Provincial - Eastern Cape as example)
#Accounting for diff pop structure and model age bands
ec_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$EC)), br)
ec_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$EC)), br)
ec_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$EC)), br)
ec_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$EC)), br)
ec_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$EC)), br)

fs_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$FS)), br)
fs_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$FS)), br)
fs_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$FS)), br)
fs_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$FS)), br)
fs_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$FS)), br)

gp_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$GP)), br)
gp_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$GP)), br)
gp_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$GP)), br)
gp_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$GP)), br)
gp_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$GP)), br)

kzn_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$KZN)), br)
kzn_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$KZN)), br)
kzn_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$KZN)), br)
kzn_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$KZN)), br)
kzn_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$KZN)), br)

lp_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$LP)), br)
lp_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$LP)), br)
lp_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$LP)), br)
lp_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$LP)), br)
lp_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$LP)), br)

mp_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$MP)), br)
mp_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$MP)), br)
mp_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$MP)), br)
mp_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$MP)), br)
mp_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$MP)), br)

nc_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$NC)), br)
nc_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$NC)), br)
nc_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$NC)), br)
nc_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$NC)), br)
nc_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$NC)), br)

nw_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$NW)), br)
nw_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$NW)), br)
nw_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$NW)), br)
nw_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$NW)), br)
nw_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$NW)), br)

wc_con_home <- def_contact(as.data.frame(365*convertPrem(prem_home, sa,provpop16$WC)), br)
wc_con_work <- def_contact(as.data.frame(365*convertPrem(prem_work, sa,provpop16$WC)), br)
wc_con_school <- def_contact(as.data.frame(365*convertPrem(prem_school, sa,provpop16$WC)), br)
wc_con_other <- def_contact(as.data.frame(365*convertPrem(prem_other, sa,provpop16$WC)), br)
wc_con_all <- def_contact(as.data.frame(365*convertPrem(prem_all, sa,provpop16$WC)), br)

#Compile Provincial contact matrices into a list
con_home_prov<-list(ec_con_home,fs_con_home,gp_con_home, kzn_con_home, lp_con_home, mp_con_home, nc_con_home, nw_con_home, wc_con_home)
con_work_prov<-list(ec_con_work,fs_con_work,gp_con_work, kzn_con_work, lp_con_work, mp_con_work, nc_con_work, nw_con_work, wc_con_work)
con_school_prov<-list(ec_con_school,fs_con_school,gp_con_school, kzn_con_school, lp_con_school, mp_con_school, nc_con_school, nw_con_school, wc_con_school)
con_other_prov<-list(ec_con_other,fs_con_other,gp_con_other, kzn_con_other, lp_con_other, mp_con_other, nc_con_other, nw_con_other, wc_con_other)
con_all_prov<-list(ec_con_all,fs_con_all,gp_con_all, kzn_con_all, lp_con_all, mp_con_all, nc_con_all, nw_con_all, wc_con_all)
lapply(con_all_prov,checkContactMatrixOrdering)

### National Annual contact matrices
con_home_nat<-365*nat_con_home
con_work_nat<-365*nat_con_work
con_school_nat<-365*nat_con_school
con_other_nat<-365*nat_con_other
con_all_nat<-365*nat_con_all

#### Convert Prem matrices from 5-year to model-agebands
con_home_nat<-def_contact(as.data.frame(con_home_nat), br)
con_work_nat<-def_contact(as.data.frame(con_work_nat), br)
con_school_nat<-def_contact(as.data.frame(con_school_nat), br)
con_other_nat<-def_contact(as.data.frame(con_other_nat), br)
con_all_nat<-def_contact(as.data.frame(con_all_nat), br)

checkContactMatrixOrdering(con_all_nat)

# # Converted age matrix output ####
# EC<-convertPrem(prem_all, sa,provpop16$EC)
# FS<-convertPrem(prem_all, sa,provpop16$FS)
# GP<-convertPrem(prem_all, sa,provpop16$GP)
# KZN<-convertPrem(prem_all, sa,provpop16$KZN)
# LP<-convertPrem(prem_all, sa,provpop16$LP)
# MP<-convertPrem(prem_all, sa,provpop16$MP)
# NC<-convertPrem(prem_all, sa,provpop16$NC)
# NW<-convertPrem(prem_all, sa,provpop16$NW)
# WC<-convertPrem(prem_all, sa,provpop16$WC)
# contactProvAge_all<-list(EC=EC, FS=FS, GP=GP, KZN=KZN, LP=LP, MP=MP, NC=NC, NW=NW, WC=WC)
# 
# EC<-convertPrem(prem_home, sa,provpop16$EC)
# FS<-convertPrem(prem_home, sa,provpop16$FS)
# GP<-convertPrem(prem_home, sa,provpop16$GP)
# KZN<-convertPrem(prem_home, sa,provpop16$KZN)
# LP<-convertPrem(prem_home, sa,provpop16$LP)
# MP<-convertPrem(prem_home, sa,provpop16$MP)
# NC<-convertPrem(prem_home, sa,provpop16$NC)
# NW<-convertPrem(prem_home, sa,provpop16$NW)
# WC<-convertPrem(prem_home, sa,provpop16$WC)
# contactProvAge_home<-list(EC=EC, FS=FS, GP=GP, KZN=KZN, LP=LP, MP=MP, NC=NC, NW=NW, WC=WC)
# 
# EC<-convertPrem(prem_work, sa,provpop16$EC)
# FS<-convertPrem(prem_work, sa,provpop16$FS)
# GP<-convertPrem(prem_work, sa,provpop16$GP)
# KZN<-convertPrem(prem_work, sa,provpop16$KZN)
# LP<-convertPrem(prem_work, sa,provpop16$LP)
# MP<-convertPrem(prem_work, sa,provpop16$MP)
# NC<-convertPrem(prem_work, sa,provpop16$NC)
# NW<-convertPrem(prem_work, sa,provpop16$NW)
# WC<-convertPrem(prem_work, sa,provpop16$WC)
# contactProvAge_work<-list(EC=EC, FS=FS, GP=GP, KZN=KZN, LP=LP, MP=MP, NC=NC, NW=NW, WC=WC)
# 
# EC<-convertPrem(prem_school, sa,provpop16$EC)
# FS<-convertPrem(prem_school, sa,provpop16$FS)
# GP<-convertPrem(prem_school, sa,provpop16$GP)
# KZN<-convertPrem(prem_school, sa,provpop16$KZN)
# LP<-convertPrem(prem_school, sa,provpop16$LP)
# MP<-convertPrem(prem_school, sa,provpop16$MP)
# NC<-convertPrem(prem_school, sa,provpop16$NC)
# NW<-convertPrem(prem_school, sa,provpop16$NW)
# WC<-convertPrem(prem_school, sa,provpop16$WC)
# contactProvAge_school<-list(EC=EC, FS=FS, GP=GP, KZN=KZN, LP=LP, MP=MP, NC=NC, NW=NW, WC=WC)
# 
# EC<-convertPrem(prem_other, sa,provpop16$EC)
# FS<-convertPrem(prem_other, sa,provpop16$FS)
# GP<-convertPrem(prem_other, sa,provpop16$GP)
# KZN<-convertPrem(prem_other, sa,provpop16$KZN)
# LP<-convertPrem(prem_other, sa,provpop16$LP)
# MP<-convertPrem(prem_other, sa,provpop16$MP)
# NC<-convertPrem(prem_other, sa,provpop16$NC)
# NW<-convertPrem(prem_other, sa,provpop16$NW)
# WC<-convertPrem(prem_other, sa,provpop16$WC)
# contactProvAge_other<-list(EC=EC, FS=FS, GP=GP, KZN=KZN, LP=LP, MP=MP, NC=NC, NW=NW, WC=WC)
# #check
# all.equal(365*contactProvAge_all$GP,365*(contactProvAge_home$GP+contactProvAge_work$GP+contactProvAge_school$GP+contactProvAge_other$GP ))
# 
# saveRDS(contactProvAge_home, file = "Output/Specific object requests/contactProvAge_home.RDS" )
# saveRDS(contactProvAge_work, file = "Output/Specific object requests/contactProvAge_work.RDS" )
# saveRDS(contactProvAge_school, file = "Output/Specific object requests/contactProvAge_school.RDS" )
# saveRDS(contactProvAge_other, file = "Output/Specific object requests/contactProvAge_other.RDS" )
# saveRDS(contactProvAge_all, file = "Output/Specific object requests/contactProvAge_all.RDS" )
# 
# 
