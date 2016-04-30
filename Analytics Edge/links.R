#useful links

#http://mlwave.com/kaggle-ensembling-guide/

#' Francois-GuillaumeRideau wrote:
#'         @matt what does the syntax model (productline:storage:condition) mean ?
#' 
#' My model variable looks like this: "iPad.1:64:Used", "iPad.2:16:Seller.refurbished" so in that case it is just a separator in a string.
#' 
#' However, you can use this syntax when doing training to create a composite variable on the fly : glm(sold~productline:storage, ...)
#' 
#' # from the winner:
#' I was shocked I ended up in the first place in the private board (moving up 69 place). I submitted my best public score and an earlier entry. At the end, the earlier entry went from 0.852 to 0.883.

# I used an ensemble of avNNet, gbm, glmnet, random forest and svm with radial kernel (either blending ensemble or linear combination optimized for AUC using optim). This link explains how to implement the blending ensemble. avNNet has the biggest impact on the final model.
# 
# On the independent variables side, I added 2 additional variables for startprice (log and exp(-x)). I also used clustering (euclidean and Kmean) or PCA with bag of words matrix from the description field. 
# I added biddable, productionline, condition as additional words to the description field to work around the empty description field issue.



# The first feature reduction method -- removing near-zero variance columns -- did not seem to hurt, where removing low importance features did. But leaving in all 115 original columns was probably not good, so some did have to go.