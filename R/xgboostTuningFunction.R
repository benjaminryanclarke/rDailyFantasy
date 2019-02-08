#' xgboost Tuning Script
#'
#' @param dataModel data
#' @param target regression Target Variable
#' @param saveAsName prefix for each saved object in NBA_Daily/machineLearning/...
#'
#' @return best tuned model
#' @export
#'
#' @examples agBoostTuning(dataModel=bbmDataLearning,target="ActualV",saveAsName="bbmDataLearning")
xgBoostTuning<-function(dataModel=bbmDataLearning,target="ActualV",saveAsName="bbmDataLearning"){

  require(vtreat)
  require(xgboost)


  dataM<-as.data.frame(dataModel)
  set.seed(48)
  inTrain<-caret::createDataPartition(dataM[,target],p=0.8,list=F)
  Train<-dataM[inTrain,]
  Test<-dataM[-inTrain,]

  ## Preparing matrix
  #library(vtreat)
  # variable names
  features <- setdiff(names(dataM), target)

  # Create the treatment plan from the training dataM
  treatplan <- vtreat::designTreatmentsZ(dataM, features, verbose = FALSE)
  save(treatplan,file=paste0("~/NBA_Daily/machineLearning/vTreat/",saveAsName,target,"treatplan.rda"))
  # Get the "clean" variable names from the scoreFrame
  new_vars <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%
    dplyr::filter(code %in% c("clean", "lev")) %>%
    magrittr::use_series(varName)
  save(new_vars,file=paste0("~/NBA_Daily/machineLearning/vTreat/",saveAsName,target,"new_vars.rda"))
  # Prepare the training dataM
  features_train <- vtreat::prepare(treatplan, Train, varRestriction = new_vars) %>% as.matrix()
  response_train <- Train[,target]

  # Prepare the test dataM
  features_test <- vtreat::prepare(treatplan, Test, varRestriction = new_vars) %>% as.matrix()
  response_test <- Test[,target]


  #default parameter List
  defaultParams <- list(
    eta = 0.1,
    max_depth = 5,
    min_child_weight = 1,
    gamma = 0,
    subsample = .8,
    colsample_bytree = .8,
    objective = "reg:linear"
    )


  xgbcvDefault <- xgb.cv(
    params = defaultParams,
    data = features_train,
    label = response_train,
    nrounds = 1000,
    verbose = 1,
    early_stopping_rounds = 20,
    eval_metric = "rmse",
    nfold = 5,
    seed = 48)

  xgbcvDefaultPlot<-ggplot(xgbcvDefault$evaluation_log) +
                    geom_line(aes(iter, train_rmse_mean), color = "red") +
                    geom_line(aes(iter, test_rmse_mean), color = "blue")
  # assess results
  xgbcvDefaultResults<-xgbcvDefault$evaluation_log %>%
      dplyr::summarise(
      ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
      error.train   = min(train_rmse_mean),
      ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
      error.test   = min(test_rmse_mean))

  defaultxgboost <-  xgboost(params = defaultParams,
                            nrounds = xgbcvDefaultResults$ntrees.test,
                            data = features_train,
                            label = response_train,
                            verbose = 1,
                            early_stopping_rounds = 20,
                            seed = 48)

  defaultxgboostResults <- defaultxgboost$evaluation_log %>% dplyr::summarise(bIter=which(train_rmse == min(train_rmse))[1], rmse = train_rmse[bIter])

  ##tune max_depth/min_child_weight
  # create hyperparameter grid
  hyper_grid1 <- expand.grid(
    eta = c(.1),
    max_depth = c(3,4,5,6,7,8,9,10),
    min_child_weight = c(1,2,3,4,5,6),
    gamma = c(0),
    subsample = c(.8),
    colsample_bytree = c(.8),
    alpha = c(0),
    optimal_trees = 0,               # a place to dump results
    min_RMSE = 0                     # a place to dump results
  )

  nrow(hyper_grid1)

  #grid search
  for(i in 1:nrow(hyper_grid1)) {
    # create parameter list
    params1 <- list(
      eta = hyper_grid1$eta[i],
      max_depth = hyper_grid1$max_depth[i],
      min_child_weight = hyper_grid1$min_child_weight[i],
      gamma = hyper_grid1$gamma[i],
      subsample = hyper_grid1$subsample[i],
      colsample_bytree = hyper_grid1$colsample_bytree[i],
      alpha = hyper_grid1$alpha[i]
    )
    # train model
    xgb.tune1 <- xgb.cv(
      params = params1,
      data = features_train,
      label = na.What(response_train),
      nrounds = xgbcvDefaultResults$ntrees.test,
      nfold = 5,
      objective = "reg:linear",  # for regression models
      verbose = 1,               # silent,
      early_stopping_rounds = 20, # stop if no improvement for 10 consecutive trees
      seed = 48
    )
    # add min training error and trees to grid
    hyper_grid1$optimal_trees[i] <- which.min(xgb.tune1$evaluation_log$test_rmse_mean)
    hyper_grid1$min_RMSE[i] <- min(xgb.tune1$evaluation_log$test_rmse_mean)
  }

#best tune hG1
hyper_grid1Optimal<-hyper_grid1[which(hyper_grid1$min_RMSE == min(hyper_grid1$min_RMSE)),]

###tune gamma hG2
hyper_grid2 <- expand.grid(
  eta = c(.1),
  max_depth = c(hyper_grid1Optimal$max_depth),
  min_child_weight = c(hyper_grid1Optimal$min_child_weight),
  gamma = c(0.001,0.01,0.1,0,1,5,10),
  subsample = c(.8),
  colsample_bytree = c(.8),
  alpha = c(0),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid2)

#grid search
for(i in 1:nrow(hyper_grid2)) {
  # create parameter list
  params2 <- list(
    eta = hyper_grid2$eta[i],
    max_depth = hyper_grid2$max_depth[i],
    min_child_weight = hyper_grid2$min_child_weight[i],
    gamma = hyper_grid2$gamma[i],
    subsample = hyper_grid2$subsample[i],
    colsample_bytree = hyper_grid2$colsample_bytree[i],
    alpha = hyper_grid2$alpha[i]
  )
  # train model
  xgb.tune2 <- xgb.cv(
    params = params2,
    data = features_train,
    label = na.What(response_train),
    nrounds = xgbcvDefaultResults$ntrees.test,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 1,               # silent,
    early_stopping_rounds = 20, # stop if no improvement for 10 consecutive trees
    seed = 48
  )
  # add min training error and trees to grid
  hyper_grid2$optimal_trees[i] <- which.min(xgb.tune2$evaluation_log$test_rmse_mean)
  hyper_grid2$min_RMSE[i] <- min(xgb.tune2$evaluation_log$test_rmse_mean)
}

#best tune hG1
hyper_grid2Optimal<-hyper_grid2[which(hyper_grid2$min_RMSE == min(hyper_grid2$min_RMSE)),]

#run CV for optimal rounds to update
paramsAfterGamma <- list(
  eta = 0.1,
  max_depth = c(hyper_grid2Optimal$max_depth),
  min_child_weight = c(hyper_grid2Optimal$min_child_weight),
  gamma = c(hyper_grid2Optimal$gamma),
  subsample = .8,
  colsample_bytree = .8,
  objective = "reg:linear",
  scale_pos_weight = 1)


xgbcvAfterGamma <- xgb.cv(
  params = paramsAfterGamma,
  data = features_train,
  label = response_train,
  nrounds = 1000,
  verbose = 1,
  early_stopping_rounds = 20,
  eval_metric = "rmse",
  nfold = 5,
  seed = 48)

xgbcvAfterGammaPlot<-ggplot(xgbcvAfterGamma$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")
# assess results
xgbcvAfterGammaResults<-xgbcvAfterGamma$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    error.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    error.test   = min(test_rmse_mean))

afterGammaxgboost <-  xgboost(params = paramsAfterGamma,
                           nrounds = xgbcvAfterGammaResults$ntrees.test,
                           data = features_train,
                           label = response_train,
                           verbose = 1,
                           early_stopping_rounds = 20,
                           seed = 48)

afterGammaxgboostResults <- afterGammaxgboost$evaluation_log %>% dplyr::summarise(bIter=which(train_rmse == min(train_rmse))[1], rmse = train_rmse[bIter])


#hg3 Tune
###tune gamma hG2
hyper_grid3 <- expand.grid(
  eta = c(.1),
  max_depth = c(hyper_grid1Optimal$max_depth),
  min_child_weight = c(hyper_grid1Optimal$min_child_weight),
  gamma = c(hyper_grid2Optimal$gamma),
  subsample = c(.6,.7,.8,.9,1),
  colsample_bytree = c(.6,.7,.8,.9,1),
  alpha = c(0),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid3)

#grid search
for(i in 1:nrow(hyper_grid3)) {
  # create parameter list
  params3 <- list(
    eta = hyper_grid3$eta[i],
    max_depth = hyper_grid3$max_depth[i],
    min_child_weight = hyper_grid3$min_child_weight[i],
    gamma = hyper_grid3$gamma[i],
    subsample = hyper_grid3$subsample[i],
    colsample_bytree = hyper_grid3$colsample_bytree[i],
    alpha = hyper_grid3$alpha[i]
  )
  # train model
  xgb.tune3 <- xgb.cv(
    params = params3,
    data = features_train,
    label = na.What(response_train),
    nrounds = xgbcvAfterGammaResults$ntrees.test,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 1,               # silent,
    early_stopping_rounds = 20, # stop if no improvement for 10 consecutive trees
    seed = 48
  )
  # add min training error and trees to grid
  hyper_grid3$optimal_trees[i] <- which.min(xgb.tune3$evaluation_log$test_rmse_mean)
  hyper_grid3$min_RMSE[i] <- min(xgb.tune3$evaluation_log$test_rmse_mean)
}

#best tune hG3
hyper_grid3Optimal<-hyper_grid3[which(hyper_grid3$min_RMSE == min(hyper_grid3$min_RMSE)),]



#hg4 Tune
###tune gamma hG4
hyper_grid4 <- expand.grid(
  eta = c(.1),
  max_depth = c(hyper_grid1Optimal$max_depth),
  min_child_weight = c(hyper_grid1Optimal$min_child_weight),
  gamma = c(hyper_grid2Optimal$gamma),
  subsample = c(hyper_grid3Optimal$subsample-0.05,hyper_grid3Optimal$subsample,hyper_grid3Optimal$subsample+0.05),
  colsample_bytree = c(hyper_grid3Optimal$colsample_bytree-0.05,hyper_grid3Optimal$colsample_bytree,hyper_grid3Optimal$colsample_bytree+0.05),
  alpha = c(0),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid4)
hyper_grid4<-hyper_grid4 %>% dplyr::filter(subsample<=1,colsample_bytree<=1)
nrow(hyper_grid4)
#grid search
for(i in 1:nrow(hyper_grid4)) {
  # create parameter list
  params4 <- list(
    eta = hyper_grid4$eta[i],
    max_depth = hyper_grid4$max_depth[i],
    min_child_weight = hyper_grid4$min_child_weight[i],
    gamma = hyper_grid4$gamma[i],
    #if(hyper_grid4$subsample[i]<=1){
    subsample = hyper_grid4$subsample[i],
    #}else{
    #  subsample = 1
    #},
    #if(hyper_grid4$colsample_bytree[i]<=1){
    colsample_bytree = hyper_grid4$colsample_bytree[i],
    #}else{
    #  colsample_bytree = 1
    #},
    alpha = hyper_grid4$alpha[i]
  )

  # train model
  xgb.tune4 <- xgb.cv(
    params = params4,
    data = features_train,
    label = na.What(response_train),
    nrounds = xgbcvAfterGammaResults$ntrees.test,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 1,               # silent,
    early_stopping_rounds = 20, # stop if no improvement for 10 consecutive trees
    seed = 48
  )
  # add min training error and trees to grid
  hyper_grid4$optimal_trees[i] <- which.min(xgb.tune4$evaluation_log$test_rmse_mean)
  hyper_grid4$min_RMSE[i] <- min(xgb.tune4$evaluation_log$test_rmse_mean)
}

#best tune hG4
hyper_grid4Optimal<-hyper_grid4[which(hyper_grid4$min_RMSE == min(hyper_grid4$min_RMSE)),]



#hg5 Tune
###tune gamma hG5
hyper_grid5 <- expand.grid(
  eta = c(.1),
  max_depth = c(hyper_grid1Optimal$max_depth),
  min_child_weight = c(hyper_grid1Optimal$min_child_weight),
  gamma = c(hyper_grid2Optimal$gamma),
  subsample = c(hyper_grid4Optimal$subsample),
  colsample_bytree = c(hyper_grid4Optimal$colsample_bytree),
  alpha = c(1e-5,0.01,0.1,1,100),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid5)

#grid search
for(i in 1:nrow(hyper_grid5)) {
  # create parameter list
  params5 <- list(
    eta = hyper_grid5$eta[i],
    max_depth = hyper_grid5$max_depth[i],
    min_child_weight = hyper_grid5$min_child_weight[i],
    gamma = hyper_grid5$gamma[i],
    subsample = hyper_grid5$subsample[i],
    colsample_bytree = hyper_grid5$colsample_bytree[i],
    alpha = hyper_grid5$alpha[i]
  )
  # train model
  xgb.tune5 <- xgb.cv(
    params = params5,
    data = features_train,
    label = na.What(response_train),
    nrounds = xgbcvAfterGammaResults$ntrees.test,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 1,               # silent,
    early_stopping_rounds = 20, # stop if no improvement for 10 consecutive trees
    seed = 48
  )
  # add min training error and trees to grid
  hyper_grid5$optimal_trees[i] <- which.min(xgb.tune5$evaluation_log$test_rmse_mean)
  hyper_grid5$min_RMSE[i] <- min(xgb.tune5$evaluation_log$test_rmse_mean)
}

#best tune hG4
hyper_grid5Optimal<-hyper_grid5[which(hyper_grid5$min_RMSE == min(hyper_grid5$min_RMSE)),]



#hg6 Tune
###tune gamma hG6
hyper_grid6 <- expand.grid(
  eta = c(.1),
  max_depth = c(hyper_grid1Optimal$max_depth),
  min_child_weight = c(hyper_grid1Optimal$min_child_weight),
  gamma = c(hyper_grid2Optimal$gamma),
  subsample = c(hyper_grid4Optimal$subsample),
  colsample_bytree = c(hyper_grid4Optimal$colsample_bytree),
  alpha = c(0/hyper_grid5Optimal$alpha,1/hyper_grid5Optimal$alpha,2/hyper_grid5Optimal$alpha,3/hyper_grid5Optimal$alpha,4/hyper_grid5Optimal$alpha,5/hyper_grid5Optimal$alpha),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid6)

#grid search
for(i in 1:nrow(hyper_grid6)) {
  # create parameter list
  params6 <- list(
    eta = hyper_grid6$eta[i],
    max_depth = hyper_grid6$max_depth[i],
    min_child_weight = hyper_grid6$min_child_weight[i],
    gamma = hyper_grid6$gamma[i],
    subsample = hyper_grid6$subsample[i],
    colsample_bytree = hyper_grid6$colsample_bytree[i],
    alpha = hyper_grid6$alpha[i]
  )
  # train model
  xgb.tune6 <- xgb.cv(
    params = params6,
    data = features_train,
    label = na.What(response_train),
    nrounds = xgbcvAfterGammaResults$ntrees.test,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 1,               # silent,
    early_stopping_rounds = 20, # stop if no improvement for 10 consecutive trees
    seed = 48
  )
  # add min training error and trees to grid
  hyper_grid6$optimal_trees[i] <- which.min(xgb.tune6$evaluation_log$test_rmse_mean)
  hyper_grid6$min_RMSE[i] <- min(xgb.tune6$evaluation_log$test_rmse_mean)
}

#best tune hG4
hyper_grid6Optimal<-hyper_grid6[which(hyper_grid6$min_RMSE == min(hyper_grid6$min_RMSE)),]



##train all Optimal models to find best
#combine all optimals
hyper_gridAllOptimal<-dplyr::bind_rows(hyper_grid1Optimal,hyper_grid2Optimal,hyper_grid3Optimal,hyper_grid4Optimal,hyper_grid5Optimal,hyper_grid6Optimal)


#train/test/evaluate on validation data all optimals

xgboostModelDF<-list(
  bIter = 0,
  trainRMSE = 0,
  trainCOR = 0,
  testRMSE = 0,
  testCOR = 0,
  eta = 0,
  max_depth = 0,
  min_child_weight = 0,
  gamma = 0,
  subsample = 0,
  colsample_bytree = 0,
  objective = 0
)
foreach(i=1:6) %do% {
  paramsOpt <- list(
    eta = 0.01,
    max_depth = hyper_gridAllOptimal$max_depth[i],
    min_child_weight = hyper_gridAllOptimal$min_child_weight[i],
    gamma = hyper_gridAllOptimal$gamma[i],
    subsample = hyper_gridAllOptimal$subsample[i],
    colsample_bytree = hyper_gridAllOptimal$colsample_bytree[i],
    objective = "reg:linear"
    )


  xgbcv <- xgb.cv(
    params = paramsOpt,
    data = features_train,
    label = response_train,
    nrounds = 2500,
    verbose = 1,
    early_stopping_rounds = 20,
    eval_metric = "rmse",
    nfold = 5,
    seed = 48)

  xgbcvResults<-xgbcv$evaluation_log %>%
    dplyr::summarise(
      ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
      error.train   = min(train_rmse_mean),
      ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
      error.test   = min(test_rmse_mean))

  xgboostModel <-  xgboost(params = paramsOpt,
                             nrounds = xgbcvResults$ntrees.test,
                             data = features_train,
                             label = response_train,
                             verbose = 1,
                             early_stopping_rounds = 20,
                            seed = 48)

  xgboostModelResults <- xgboostModel$evaluation_log %>% dplyr::summarise(bIter=which(train_rmse == min(train_rmse))[1], trainRMSE = train_rmse[bIter]) %>% data.frame()


  importance_matrix <- xgb.importance(model = xgboostModel)

  imp_plot <- xgb.plot.importance(importance_matrix, top_n = 20, measure = "Gain")

  # predict values for test data
  pred <- predict(xgboostModel, features_test)

  predTrain <- predict(xgboostModel, features_train)

  # results
  validRMSE <- caret::RMSE(pred, na.What(response_test))

  validCOR <- cor(pred,na.What(response_test))

  trainCOR <- cor(predTrain,na.What(response_train))

  xgboostModelDF$bIter[i] <- xgboostModelResults$bIter
  xgboostModelDF$trainRMSE[i] <- xgboostModelResults$trainRMSE
  xgboostModelDF$trainCOR[i] <- trainCOR
  xgboostModelDF$testRMSE[i] <- validRMSE
  xgboostModelDF$testCOR[i] <- validCOR
  xgboostModelDF$eta[i] <- paramsOpt$eta
  xgboostModelDF$max_depth[i] <- paramsOpt$max_depth
  xgboostModelDF$min_child_weight[i] <- paramsOpt$min_child_weight
  xgboostModelDF$gamma[i] <- paramsOpt$gamma
  xgboostModelDF$subsample[i] <- paramsOpt$subsample
  xgboostModelDF$colsample_bytree[i] <- paramsOpt$colsample_bytree
  xgboostModelDF$objective[i] <- paramsOpt$objective
}

xgboostModelDF <- xgboostModelDF %>% data.frame()

bbbbest <-  which.min(xgboostModelDF$testRMSE)

paramsBest <- list(
  eta = xgboostModelDF$eta[bbbbest],
  max_depth = xgboostModelDF$max_depth[bbbbest],
  min_child_weight = xgboostModelDF$min_child_weight[bbbbest],
  gamma = xgboostModelDF$gamma[bbbbest],
  subsample = xgboostModelDF$subsample[bbbbest],
  colsample_bytree = xgboostModelDF$colsample_bytree[bbbbest],
  objective = xgboostModelDF$objective[bbbbest]
)


xgboostFinal <-  xgboost(params = paramsBest,
                         nrounds = xgboostModelDF$bIter[bbbbest],
                         data = features_train,
                         label = response_train,
                         verbose = 1,
                         seed = 48
                         )

importance_matrix <- xgb.importance(model = xgboostFinal)

impPlot <- xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

# predict values for test data
predBest <- predict(xgboostFinal, features_test)

# results
validrmse<-caret::RMSE(predBest, na.What(response_test))

validcor<-cor(predBest,na.What(response_test))

predBesttrain<-predict(xgboostFinal, features_train)

traincor<-cor(predBesttrain,na.What(response_train))

save(xgboostFinal,file=paste0("~/NBA_Daily/machineLearning/algos/",saveAsName,target,"xgboostFinal.rda"))


modelReturnList<-list("finalModel"=xgboostFinal,"optResults"=xgboostModelDF,"RMSE"=xgboostModelDF$trainRMSE[bbbbest],"COR"=traincor,"validationRMSE"=validrmse,"validationCOR"=validcor,"importancePlot"=impPlot,"treatPlan"=paste0(saveAsName,target,"treatplan.rda"),"new_Vars"=paste0(saveAsName,target,"new_vars.rda"),"savedAs"=paste0(saveAsName,target,"xgboostFinal.rda"))

return(modelReturnList)
}







