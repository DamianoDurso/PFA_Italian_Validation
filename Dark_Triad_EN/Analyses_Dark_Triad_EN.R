### Data Analysis Rotated Results EFA
# 1 - Extract the empirical loading matrix
# 2 - Compare each factor, if recovered, with the corresponding empirical factor
# 3 - If full structure is recovered, compare the full structure? just a test
# 4 - Compare correlation matrices

#### IMPORTANT!!! ALL ANALYSES BELOW HEAVILY DEPENDS ON THE RIGHT ORDER OF THE 'Results loadings overview!.xlsx' file.
#### IF THE ORDER OF THE FILE (E.G., THE FACTORS ORDER) HAS BEEN CHANGED FROM THE SOURCE FILE, YOU NEED TO DOUBLE CHECK 
#### ALL THE ANALYSES ABOVE HAVE TO BE RECHECKED!

#install.packages('readxl')
library(readxl)
library(psych) #for congruence
library(FactoMineR) #for corr matrix similarity

#Function to load and rename results
load_and_prep = function(file){
    res = as.matrix(read_csv(file)) 
    #change rownames
    names = rownames(res) <- res[,1]
    #remove colname unncessary
    res = res[,-c(1)]
    #transform to numeric
    res = apply(res,2, as.numeric)
    #rename rows correctly
    rownames(res) = names
    return(res)
}

#---------------------------------------- Target Rotation ---------------------------------------#
#---------------------------------------- Target Rotation ---------------------------------------#

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#

# Create the data frame with the factor analysis data from the validation study (Lee, 2004)

#first we re-order according to the model
# Specify the desired variable order
var_order = c("I.tend.to.manipulate.others.to.get.my.way","I.have.used.deceit.or.lied.to.get.my.way","I.have.use.flattery.to.get.my.way","I.tend.to.exploit.others.towards.my.own.end"               , #Machiavennalism               
"I.tend.to.lack.remorse","I.tend.to.be.unconcerned.with.the.morality.of.my.actions.","I.tend.to.be.callous.or.insensitive","I.tend.to.be.cynical",#Psychopathy                                   
"I.tend.to.want.others.to.admire.me","I.tend.to.want.others.to.pay.attention.to.me","I.tend.to.seek.prestige.or.status","I.tend.to.expect.special.favors.from.others") #Narcissism


#loadings matrix based on cite Jonason, Peter K.; Webster, Gregory D. . (2010). The dirty dozen: A concise measure of the dark triad.. Psychological Assessment, 22(2), 420–432. doi:10.1037/a0019265
# Table 8
                       #M     #P     #N    
valid_load = matrix(c(0.81,  0.04,   0.02, 
                      0.84,  0.04,  -0.08, 
                      0.74, -0.16,   0.12, 
                      0.64,  0.28,  -0.02, 
                     -0.04,  0.83,  -0.02, 
                      0.04,  0.77,   0.00, 
                      0.02,  0.87,  -0.03, 
                     -0.07,  0.57,   0.13, 
                      0.02, -0.01,   0.89, 
                      0.01, -0.08,   0.90, 
                      0.03,  0.08,   0.83, 
                     -0.22,  0.14,   0.55), byrow = TRUE, 12, 3)

rownames(valid_load) = var_order 

colnames(valid_load) = c("Mach", "Psyc", "Narc")
reordered_matrix = valid_load[var_order, ]

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = as.matrix(read.csv(file ='./Dark_Triad_EN/Results_target_item_labeled_reordered_EN.csv'))
#change rownames
names = rownames(res) <- res[,1]
#remove colname unncessary
res = res[,-c(1)]
#transform to numeric
res = apply(res,2, as.numeric)
#rename rows correctly
rownames(res) = names

#Check final matrix
res
colnames(res)
#create empty matrix
val_atomic_congruence = matrix(NA, 1, 3)

distilRoberta = psych::factor.congruence(x = reordered_matrix[,c(1:3)], y = res[,c(1:3)])
val_atomic_congruence[1, 1:3] = rbind(diag(distilRoberta)) #distilroberta

miniLM = psych::factor.congruence(x = reordered_matrix, y = res[,c(4:6)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(miniLM)) #minilm

mpnet = psych::factor.congruence(x = reordered_matrix, y = res[,c(7:9)])
val_atomic_congruence = rbind(val_atomic_congruence, c(NA, diag(mpnet)[2], NA)) 

wulff = psych::factor.congruence(x = reordered_matrix, y = res[,c(10:12)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(wulff)) 

t5 = psych::factor.congruence(x = reordered_matrix, y = res[,c(13:15)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(t5)) 

mpnet_en = psych::factor.congruence(x = reordered_matrix, y = res[,c(16:18)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(mpnet_en)) 

t5 = psych::factor.congruence(x = reordered_matrix, y = res[,c(19:21)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(t5)) 

avg_trans = psych::factor.congruence(x = reordered_matrix, y = res[,c(22:24)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(avg_trans)) 

#rename rows and cols
rownames(val_atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 'Wulff','e5', 'mpnet_en', 't5','avg_transf')
colnames(val_atomic_congruence) = c("Machiavellanism", "Psychopathy", "Narcissism")
val_atomic_congruence

results_val = val_atomic_congruence
results_val = rbind(results_val, colMeans(results_val, na.rm = TRUE))
results_val = cbind(results_val, rowMeans(results_val, na.rm = TRUE))
results_val

## store congruence results in the appropriate folder. 
#write.csv(results_val,
#          './01 Johnson (2014) facet level 300 items/Results_after_ordering/Congruence_target_val.csv')

