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
var_order = c("Tendo.a.manipolare.gli.altri.per.ottenere.ciò.che.voglio","Ho.usato..l.inganno.o.la.menzogna.per.ottenere.ciò.che.volevo", "Ho.usato.l.adulazione.per.ottenere.ciò.che.volevo","Tendo.a.sfruttare.gli.altri.per.i.miei.scopi" , #Machiavennalism               
"Tendo.a.non.provare.rimorso", "Tendo.a.non.interessarmi.della.moralità.delle.mie.azioni","Tendo.ad.essere.freddo.o.insensibile.","Tendo.ad.essere.cinico." ,#Psychopathy                                   
"Tendo.a.volere.che.gli.altri.mi.ammirino.","Tendo.a.volere.che.gli.altri.mi.diano.attenzione.","Tendo.a.cercare.prestigio.o.potere.","Tendo.ad.aspettarmi.favori.speciali.dagli.altri." ) #Narcissism


#loadings matrix based on cite Dark Triad paper Schimmenti et al. 2019 doi: 10.1007/s12144-017-9588-6
                       #M     #P     #N    
valid_load = matrix(c(0.71,  0.03,   0.11, 
                      0.70,  0.06,   0.06, 
                      0.83, -0.05,  -0.07, 
                      0.66,  0.04,   0.00, 
                     -0.20,  0.75,   0.06, 
                      0.11,  0.72,  -0.16, 
                      0.09,  0.66,   0.01, 
                      0.12,  0.54,   0.14, 
                     -0.09,  0.01,   0.90, 
                     -0.01, -0.06,   0.84, 
                      0.24,  0.13,   0.56, 
                      0.28,  0.05,   0.42), byrow = TRUE, 12, 3)

rownames(valid_load) = var_order 

colnames(valid_load) = c("Mach", "Psyc", "Narc")
reordered_matrix = valid_load[var_order, ]

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = as.matrix(read.csv(file ='./Dark_Triad/Results_target_item_labeled_reordered.csv'))
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
val_atomic_congruence[1, 1:3] = rbind(NA, diag(distilRoberta)[2], NA) #distilroberta

miniLM = psych::factor.congruence(x = reordered_matrix, y = res[,c(4:6)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(miniLM)) #minilm

mpnet = psych::factor.congruence(x = reordered_matrix, y = res[,c(7:9)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(mpnet)) 

wulff = psych::factor.congruence(x = reordered_matrix, y = res[,c(10:12)])
val_atomic_congruence = rbind(val_atomic_congruence, c(NA, diag(wulff)[2], NA)) 

t5 = psych::factor.congruence(x = reordered_matrix, y = res[,c(13:15)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(t5)) 

avg_trans = psych::factor.congruence(x = reordered_matrix, y = res[,c(16:18)])
val_atomic_congruence = rbind(val_atomic_congruence, c(NA, diag(avg_trans)[2], NA)) 

#rename rows and cols
rownames(val_atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 'wulff', 'e5','avg_trans')
colnames(val_atomic_congruence) = c("Machiavellanism", "Psychopathy", "Narcissism")
val_atomic_congruence

results_val = val_atomic_congruence
results_val = rbind(results_val, colMeans(results_val, na.rm = TRUE))
results_val = cbind(results_val, rowMeans(results_val, na.rm = TRUE))
results_val

## store congruence results in the appropriate folder. 
#write.csv(results_val,
#          './01 Johnson (2014) facet level 300 items/Results_after_ordering/Congruence_target_val.csv')

