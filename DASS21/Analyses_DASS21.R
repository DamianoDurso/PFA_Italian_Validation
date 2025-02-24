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
var_order = c("Non.riuscivo.proprio.a.provare.delle.emozioni.positive", "Ho.avuto.un.estrema.difficoltà.nel.cominciare.quello.che.dovevo.fare","Non.vedevo.nulla.di.buono.nel.mio.futuro","Mi.sono.sentito.scoraggiato.e.depresso","Non.c.era.nulla.che.mi.dava.entusiasmo","Sentivo.di.valere.poco.come.persona","Sentivo.la.vita.priva.di.significato", #Depression                                                                                                                   
  "Mi.sono.accorto.di.avere.la.bocca.secca","Mi.sono.sentito.molto.in.affanno.e.con.difficoltà.a.respirare..per.es..respiro.molto.accelerato..sensazione.di.forte.affanno.in.assenza.di.sforzo.fisico","Ho.avuto.tremori..per.es..alle.mani.","Ho.temuto.di.trovarmi.in.situazioni.in.cui.sarei.potuto.andare.nel.panico.e.rendermi.ridicolo","Ho.sentito.di.essere.vicino.ad.avere.un.attacco.di.panico","Ho.percepito.distintamente.il.battito.del.mio.cuore.senza.aver.fatto.uno.sforzo.fisico..per.es..battito.cardiaco.accelarato.o.perdita.di.un.battito.","Mi.sono.sentito.spaventato.senza.ragione", #Anxiety                                                                                                                
  "Ho.provato.molta.tensione.e.ho.avuto.difficoltà","Ho.avuto.la.tendenza.a.reagire.in.maniera.eccessiva.alle.situazioni","Ho.sentito.che.stavo.impiegando.molta.energia.nervosa","Mi.sono.sentito.stressato","Ho.avuo.difficoltà.a.rilassarmi","Non.riuscivo.a.tollerare.per.nulla.eventi.o.situazioni.che.mi.impedivano.di.portare.avanti.ciò.che.stavo.facendo","Mi.sono.sentito.piuttosto.irritabile") #stress




#Data for DASS21 is taken from paper by Iannattone et al. 2024, table2, doi: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0299229
                       #M     #P     #N    
valid_load = matrix(c(0.50, -0.07,   0.28, 
                      0.37, -0.03,   0.12,  
                      0.76,  0.02,   0.03, 
                      0.59,  0.02,   0.27, 
                      0.70, -0.17,   0.20, 
                      0.87,  0.01,   0.01, 
                     -0.11,  0.54,   0.09, 
                     -0.16,  0.86,  -0.28, 
                     -0.21,  0.83,  -0.02, 
                      0.11,  0.68,  -0.06, 
                     -0.03,  0.81,  -0.24, 
                     -0.18,  0.86,   0.02,
                      0.01,  0.68,   0.01,
                     -0.10,  0.36,   0.34,
                     -0.01,  0.34,   0.67,
                     -0.04,  0.19,   0.49,
                     -0.01, -0.28,   0.50,
                      0.22, -0.31,   0.46, 
                      0.17,  0.22,   0.48, 
                      0.01,  0.01,   0.80), byrow = TRUE, 21, 3)

# We now use the paper data and run an EFA since the above matrix is based on
# ESEM. 

library(readxl)
df <- read_excel('./DASS21/DB_validation DASS21_Italian adolescents.xlsx')

head(df)
describe(df)

df_dass = df[,4:24]
librar(psych)

colnames(df)
var_order = c('DASS-21_3', 'DASS-21_5', 'DASS-21_10', 'DASS-21_13', 'DASS-21_16', 'DASS-21_17', 'DASS-21_21', #DEP
              'DASS-21_2', 'DASS-21_4', 'DASS-21_7', 'DASS-21_9', 'DASS-21_15', 'DASS-21_19', 'DASS-21_20', #ANX
              'DASS-21_1', 'DASS-21_6', 'DASS-21_8', 'DASS-21_11', 'DASS-21_12', 'DASS-21_14', 'DASS-21_18') #STR

df_dass = df_dass[, var_order]

fa_dass21 = psych::fa(df_dass,nfactors = 3, rotate = 'oblimin')
round(fa_dass21$loadings[,],2)

#oblimin
valid_load = matrix(c(0.51,  0.05,  0.29,
               0.37, -0.02,  0.32,
               0.75,  0.02, -0.03,
               0.61,  0.11,  0.16,
               0.69, -0.05,  0.14,
               0.72,  0.16, -0.07,
               0.87, -0.03, -0.06,
               0.00,  0.36,  0.19,
              -0.01,  0.81, -0.04,
              -0.06,  0.76,  0.03,
               0.19,  0.48,  0.13,
               0.12,  0.75, -0.03,
              -0.03,  0.74,  0.04,
               0.10,  0.53,  0.16,
               0.01,  0.34,  0.47,
              -0.04,  0.06,  0.66,
               0.03,  0.19,  0.59,
               0.17,  0.11,  0.52,
               0.25,  0.12,  0.45,
               0.18,  0.03,  0.47,
               0.00, -0.06,  0.79), byrow =TRUE,21,3)
rownames(valid_load) = var_order 
                      
colnames(valid_load) = c("Dep", "Anx", "Str")
rownames(valid_load) = var_order
reordered_matrix = valid_load#[var_order, ]

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = as.matrix(read.csv(file ='./DASS21/Results_target_item_labeled_reordered.csv'))
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

#create empty matrix
val_atomic_congruence = matrix(NA, 1, 3)

distilRoberta = psych::factor.congruence(x = reordered_matrix[,c(1:3)], y = res[,c(1:3)])
val_atomic_congruence[1, 1:3] = rbind(diag(distilRoberta)[1], NA, NA) #distilroberta

miniLM = psych::factor.congruence(x = reordered_matrix, y = res[,c(4:6)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(miniLM)) #minilm

mpnet = psych::factor.congruence(x = reordered_matrix, y = res[,c(7:9)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(mpnet)) 

wulff = psych::factor.congruence(x = reordered_matrix, y = res[,c(10:12)])
val_atomic_congruence = rbind(val_atomic_congruence, c(NA, wulff[3,2], NA)) 

t5 = psych::factor.congruence(x = reordered_matrix, y = res[,c(13:15)])
val_atomic_congruence = rbind(val_atomic_congruence, c(diag(t5)[1], NA, NA)) 

avg_trans = psych::factor.congruence(x = reordered_matrix, y = res[,c(16:18)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(avg_trans)) 

#rename rows and cols
rownames(val_atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 'wulff' ,'e5', 'avg_trans')
colnames(val_atomic_congruence) = c("Dep", "Anx", "Str")
val_atomic_congruence

results_val = cbind(val_atomic_congruence)
results_val = rbind(results_val, colMeans(results_val, na.rm = TRUE))
results_val = cbind(results_val, rowMeans(results_val, na.rm = TRUE))
results_val
## store congruence results in the appropriate folder. 
write.csv(results_val,
          './01 Johnson (2014) facet level 300 items/Results_after_ordering/Congruence_target_val.csv')
