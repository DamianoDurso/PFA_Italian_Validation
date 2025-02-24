library(psych)

#please check location of this R file before running code to make sure results are stored in the right folder

#### Functions######
# apparently something goes wrong in the saving of the csv files (double check in python)
# this makes the matrices non-symmetric (which shouldn't be possible). 
# I am going to force it for now but will double-check in python

#In the function below we conduct the following operations
# 1- copy upper triangle of the matrix to lower triangle
# 2- align rownames and colnames
# 3- reorder matrix by the order specified in var 
symm <- function(m, var) {
    m[upper.tri(m)] <- t(m)[upper.tri(m)] 
    rownames(m) <- colnames(m)
    orig_order <- colnames(m)
    ord_ind <- match(var, orig_order)
    reordered_matrix <- m[ord_ind, ord_ind]
    return(reordered_matrix)
}

# Updated function to label factors, return enhanced information, and modify factor names
label_factors_enhanced <- function(loading_matrix, groups, method = "mean", mod = NA, cor_matrix = NA) {
  # Validate method input
  if (!method %in% c("mean", "median")) {
    stop("Invalid method. Use 'mean' or 'median'.")
  }

  if (is.na(mod)){
    stop("Provide a model name")
  }
  
  if (all(is.na(cor_matrix))){
    stop('Provide a Factor correlation matrix')
  }
  
  # Initialize list to store results with labels
  labeled_factors <- list()
  # Copy of the original loading_matrix to modify factor names
  labeled_loading_matrix <- loading_matrix
  labeled_cor_matrix <- cor_matrix

  for(i in seq_along(groups)) {
    group <- groups[[i]]
    group_loadings <- abs(loading_matrix[group, ]) # Subset and take absolute values
    
    # Calculate average or median loading for each factor
    if (method == "mean") {
      dominant_values <- colMeans(group_loadings) # Mean
    } else {
      dominant_values <- apply(group_loadings, 2, median) # Median
    }
    
    # Determine which factor has the highest average/median loading
    dominant_factor <- which.max(dominant_values)
    
    # Store result
    labeled_factors[[length(labeled_factors) + 1]] <- list(
      group_index = i,
      group_items = group,
      dominant_factor = dominant_factor,
      dominant_value = dominant_values[dominant_factor]
    )
    
    # Assign new label to the factor column in the modified loading matrix accounting for collapsing
    if (colnames(labeled_loading_matrix)[dominant_factor] == colnames(loading_matrix)[dominant_factor]){
        colnames(labeled_loading_matrix)[dominant_factor] <- paste0(names(groups)[i],mod)
        colnames(labeled_cor_matrix)[dominant_factor] <- rownames(labeled_cor_matrix)[dominant_factor] <- paste0(names(groups)[i],mod)
    } else {
        colnames(labeled_loading_matrix)[dominant_factor] <- paste0(colnames(labeled_loading_matrix)[dominant_factor], paste0(names(groups)[i],mod))
        colnames(labeled_cor_matrix)[dominant_factor] <- rownames(labeled_cor_matrix)[dominant_factor] <- paste0(colnames(labeled_cor_matrix)[dominant_factor], paste0(names(groups)[i],mod))
    }


  }
  
  # Return both the labeled factors and the modified loading matrix with updated factor names
  return(list(
    labeled_factors = labeled_factors,
    labeled_loading_matrix = labeled_loading_matrix,
    labeled_cor_matrix = labeled_cor_matrix
  ))
}


# Target matrix
# Function to create a matrix where NA values are replaced with 1 and rest with 0
replace_na_with_1 <- function(mat) {
  result <- ifelse(is.na(mat), 1, 0)
  return(result)
}

#Weight matrix
# Function to create a matrix where NA values are replaced with 0 and rest with 1
replace_0_with_1 <- function(mat) {
  result <- ifelse(is.na(mat), 0, 1)
  return(result)
}
#####################
#first we re-order according to the model
# Specify the desired variable order
var_order = c("Non.riuscivo.proprio.a.provare.delle.emozioni.positive", "Ho.avuto.un.estrema.difficoltà.nel.cominciare.quello.che.dovevo.fare","Non.vedevo.nulla.di.buono.nel.mio.futuro","Mi.sono.sentito.scoraggiato.e.depresso","Non.c.era.nulla.che.mi.dava.entusiasmo","Sentivo.di.valere.poco.come.persona","Sentivo.la.vita.priva.di.significato", #Depression                                                                                                                   
  "Mi.sono.accorto.di.avere.la.bocca.secca","Mi.sono.sentito.molto.in.affanno.e.con.difficoltà.a.respirare..per.es..respiro.molto.accelerato..sensazione.di.forte.affanno.in.assenza.di.sforzo.fisico","Ho.avuto.tremori..per.es..alle.mani.","Ho.temuto.di.trovarmi.in.situazioni.in.cui.sarei.potuto.andare.nel.panico.e.rendermi.ridicolo","Ho.sentito.di.essere.vicino.ad.avere.un.attacco.di.panico","Ho.percepito.distintamente.il.battito.del.mio.cuore.senza.aver.fatto.uno.sforzo.fisico..per.es..battito.cardiaco.accelarato.o.perdita.di.un.battito.","Mi.sono.sentito.spaventato.senza.ragione", #Anxiety                                                                                                                
  "Ho.provato.molta.tensione.e.ho.avuto.difficoltà","Ho.avuto.la.tendenza.a.reagire.in.maniera.eccessiva.alle.situazioni","Ho.sentito.che.stavo.impiegando.molta.energia.nervosa","Mi.sono.sentito.stressato","Ho.avuo.difficoltà.a.rilassarmi","Non.riuscivo.a.tollerare.per.nulla.eventi.o.situazioni.che.mi.impedivano.di.portare.avanti.ciò.che.stavo.facendo","Mi.sono.sentito.piuttosto.irritabile") #stress

#create a list which will be necessary for labeling factors later on
label = list(c(var_order[1:7]), c(var_order[8:14]), c(var_order[15:21]))
names(label) <- c("Dep", "Anx", "Str")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Below we do the following, for both the empirical data
# and the embeddings data:
# - Import data
# - Run EFA (no need for rotation because construct is unidimensional) 
# - Store the loadings results


# Step 1: Import data
# read embeddings data
models = c('distilroberta', 'miniLM', 'mpnet', 'Wulff',
           'e5')

item_embed = list()
item_rev_embed = list()

#load the matries and use colnames as variabl enames
for (i in 1:length(models)){
    item_embed[[i]] = symm(as.matrix(read.csv(paste0('./DASS21/cos_matrices/matrix_concatenated_item_', models[i], '.csv'))),var_order)
}

#add average across transformers
item_embed[[i+1]] = apply(simplify2array(item_embed)[,,c(1:length(models))], 1:2, mean)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
library(GPArotation)
#specify initial target matrix according to empirical validation study results
                      #Dark Triad
target.mat = as.matrix(cbind(c(rep(1,7),rep(0,14)),
                             c(rep(0,7),rep(1,7), rep(0,7)),
                             c(rep(0,14),rep(1,7))))

rownames(target.mat) = var_order
target.mat

### EFA w/ target rotation for empirical data
library(esemComp)

emp_esem_target_efa = matrix(NA, nrow = 21, ncol =3)
colnames(emp_esem_target_efa) = names(label)
rownames(emp_esem_target_efa) = var_order
emp_esem_target_Phi = matrix(rnorm(9), ncol = 3, nrow = 3)
colnames(emp_esem_target_Phi) = rownames(emp_esem_target_Phi) = names(label)

# create empty loading and cor matrix for each type of embedding, and store the empirical target-rotated 
# loadings in that matrix. Below we will populate this matrices with the pfa-based target-rotated ones
targ_load_item = emp_esem_target_efa
targ_cor_item =  emp_esem_target_Phi

colnames(targ_load_item) = paste0(colnames(targ_load_item), '_emp')
colnames(targ_cor_item) = paste0(colnames(targ_cor_item), '_emp')

#create list for item embed target rotated loadings
models = c('distilroberta', 'miniLM', 'mpnet','Wulff',
           'e5', 'avg_transf')

#loop over item embeddings and calculate efa
for (i in 1:length(item_embed)){
target_efa_item <- esem_efa(item_embed[[i]], 
                            nfactors = 3,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')
emb_esem_target_efa_item = target_efa_item$loadings #loadings matrix
emb_cor_item = target_efa_item$Phi #factor cor matrix
colnames(emb_esem_target_efa_item) = colnames(emb_cor_item) = paste0(colnames(emb_esem_target_efa_item), '_item_', models[i]) #rename
targ_load_item = cbind(targ_load_item,emb_esem_target_efa_item) #all loadings in one place
targ_cor_item = cbind(targ_cor_item,emb_cor_item) #all cor in one place
}

dim(targ_load_item)
targ_load_item
#store files that are not ordered
write.csv(round(targ_load_item,3), file = 'Results_target_item.csv')

#Now label factor using the absolute maximum average loading approach
targ_load_item_labeled = NA
targ_cor_item_labeled = NA

for (i in 1:(length(models)+1)){
   x = i*3 #multiple i*6 to select 6 columns at a time
   if (i == 1){
  #loadings
  print(NA)
  #cor
  print(NA)
   } else {
  #loadings
   targ_load_item_labeled <- cbind(targ_load_item_labeled, round(label_factors_enhanced(targ_load_item[,(x-2):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_item[,(x-2):x])$labeled_loading_matrix,3))
  #cor
    targ_cor_item_labeled <- cbind(targ_cor_item_labeled, round(label_factors_enhanced(targ_load_item[,(x-2):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_item[,(x-2):x])$labeled_cor_matrix,3))
   }
}

write.csv(round(targ_load_item_labeled,3), file = './DASS21/Results_target_item_labeled.csv')
write.csv(round(targ_cor_item_labeled,3), file = './DASS21/Results_target_item_labeled_cor.csv')

colnames(targ_cor_item_labeled)