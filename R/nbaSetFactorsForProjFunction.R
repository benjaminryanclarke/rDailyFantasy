#' nba Factors Setting Function for Projections
#'
#' @param train dataframe
#' @param target target variable for machine learning
#'
#' @return df w/ factors correctly set...
#' @export
#'
#' @examples nbaSetFactorsForProj(train=train,target="ActualPoints")
nbaSetFactorsForProj<-function(train=train,target="ActualPoints"){


nbafull2$p_own <- as.factor(nbafull2$p_own)
nbafull2$Days_Between_games <- as.factor(nbafull2$Days_Between_games)
nbafull2$Depth <- as.factor(nbafull2$Depth)
nbafull2$ProTrends_DK <- as.factor(nbafull2$ProTrends_DK)
nbafull2$ProTrends_FD <- as.factor(nbafull2$ProTrends_FD)
nbafull2$Salary <- as.factor(nbafull2$Salary)
nbafull2$Salary_DK <- as.factor(nbafull2$Salary_DK)
nbafull2$Salary_FD <- as.factor(nbafull2$Salary_FD)
nbafull2$Trend <- as.factor(nbafull2$Trend)
nbafull2$PlayerId <- as.factor(nbafull2$PlayerId)


# Get column classes
colClass <- sapply(nbafull2, class)

# Function for filling NA's in factors
fillNaFactor <- function(df=nbafull2,icol){
  # Number of each level in a given factor variable
  temp <- df[complete.cases(df[,icol]),] %>% group_by_(.dots=colnames(df)[icol]) %>%
    as.data.frame()
  ind.max <- which.max(temp[,1])
  temp[ind.max,1] # Return value
}

# Function for filling NA's in integers
fillNaInteger <- function(df=nbafull2,icol){
  median(df[complete.cases(df[,icol]),icol]) # Return the median
}

# Fill NA's using the functions above
for (icol in 1:ncol(nbafull2)){
  # Factor variables
  if (colClass[icol] == "factor"){
    l.NA <- is.na(nbafull2[,icol]) # NA's

    # Replace NA's with most common level
    if (sum(l.NA) < 100 && sum(l.NA) > 0){ # Less than 100 observations is NA
      nbafull2[l.NA,icol] <- fillNaFactor(df=nbafull2,icol)
    }
    # If there are too many NA's (i.e. 100 in this case) create a new level
    if (sum(l.NA) > 100){
      levels(nbafull2[,icol]) <- c(levels(nbafull2[,icol]), "None")
      nbafull2[l.NA,icol] <- "None"
    }
  }
  # Integers
  if (colClass[icol] == "integer"){
    l.NA <- is.na(nbafull2[,icol])
    #nbafull2[l.NA,icol] <- 0 # Replace NAs in integers with 0
    nbafull2[l.NA,icol] <- fillNaInteger(df=nbafull2,icol) # Replace with median
  }
  # Numerics
  if (colClass[icol] == "numeric"){
    l.NA <- is.na(nbafull2[,icol])
    #nbafull2[l.NA,icol] <- 0 # Replace NAs in integers with 0
    nbafull2[l.NA,icol] <- fillNaInteger(df=nbafull2,icol) # Replace with median
  }
}

# Dskew the columns
#deskew <- function(x){
# if (abs(skewness(x)) > 0.5){
#  x <- log(1+x)
#}
#x
#}

# Rescale columns
#rescale <- function(x) { (x-mean(x))/sd(x) }

# Save locations of numeric columns
numericCols <- setdiff(names(colClass[colClass == "integer" | colClass == "numeric"]),c(target))

# Deskew and standardize numeric columns
#data <- data %>% mutate_at(.vars=numericCols, funs(deskew)) %>% mutate_at(.vars=numericCols, funs(rescale))

# Split back to train and test
#train <- dplyr::filter(data, is.train == 1) %>% dplyr::select(-is.train)
#test <- dplyr::filter(data, is.train == 0) %>% dplyr::select(-is.train)
#Id.test <- test$Id # Id numbers for test set
#Id.train <- train$Id

# Predictor names (is.train  and Id is not a predictor)
predictorNames <- setdiff(names(train),c(target))

# Use Sparse Model Matrices
trainSparse <- sparse.model.matrix(~., data = train[,predictorNames])[,-1]
#testSparse <- sparse.model.matrix(~., data = test[,predictorNames])[,-1]

# Check unique elements in columns of training Matrix
#lenUniq <- function(x) {length(unique(x))}
#numUniq <- apply(trainSparse, 2, lenUniq)
#rmCol <- which(numUniq == 1) # Remove this column
#trainSparse <- trainSparse[,-c(rmCol)]#; testSparse <- testSparse[,-c(rmCol)]
train<-trainSparse
return(train)
}
