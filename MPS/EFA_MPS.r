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


#####################
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

# create empty lists which we will populate with the
# different cosine matrices
item_embed = list()
item_rev_embed = list()

#load the matries and use colnames as variabl enames
for (i in 1:length(models)){
    item_embed[[i]] = symm(as.matrix(read.csv(paste0('./MPS/cos_matrices/matrix_concatenated_item_', models[i], '.csv'))),
                           colnames(as.matrix(read.csv(paste0('./MPS/cos_matrices/matrix_concatenated_item_', models[i], '.csv')))))
}

#add average across transformers
item_embed[[i+1]] = apply(simplify2array(item_embed)[,,c(1:4)], 1:2, mean)

#------------------------------------------------------------------------------#
# Step 2: Create matrix in line with MPS article (Di Nuovo, Rispoli, Genta, 2000)

mps_valid = matrix(c(0.77,
                     0.76,
                     0.75,
                     0.74,
                     0.72,
                     0.72,
                     0.70,
                     0.70,
                     0.69,
                     0.68,
                     0.67,
                     0.66,
                     0.66,
                     0.66,
                     0.66,
                     0.65,
                     0.65,
                     0.64,
                     0.63,
                     0.63,
                     0.62,
                     0.60,
                     0.60,
                     0.60,
                     0.60,
                     0.60,
                     0.59,
                     0.58,
                     0.57,
                     0.56,
                     0.56,
                     0.56,
                     0.55,
                     0.55,
                     0.54,
                     0.54,
                     0.53,
                     0.50,
                     0.43,
                     0.43,
                     0.42,
                     0.40,
                     0.34,
                     0.34,
                     0.32,
                     0.28,
                     0.27,
                     0.25,
                     0.24), 49,1)
rownames(mps_valid) <- rownames(item_embed[[1]])
colnames(mps_valid) = c('mps_empirical')

#------------------------------------------------------------------------------#
# Step 3: Estimate EFA 

# Create matrix to populate with LLM-specific loadings
efa_loads = matrix(NA, nrow(mps_valid), length(models)+1)

for (mod in 1:length(item_embed)){
  efa_loads[,mod] = psych::fa(item_embed[[mod]], nfactors = 1, fm = 'ml')$loadings
}
#colnames
colnames(efa_loads) = c(models, 'average')
rownames(efa_loads) = rownames(mps_valid)

#------------------------------------------------------------------------------#

# Step 4: Calculate results
combined_loads = cbind(mps_valid, efa_loads)
colnames(combined_loads)

round(cor(combined_loads)[1,-c(1)],3); mean(cor(combined_loads)[1,-c(1)])
#------------------------------------------------------------------------------#
psych::factor.congruence(mps_valid, efa_loads)

