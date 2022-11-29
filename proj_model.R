# required pacakage e1071, car, MASS, xgboost, randomForest, glmnet

# using skewness from e1071
library("e1071")

# using vif from car
library("car")

# using lm.ridge from MASS
library("MASS")

# using xgboost from xgboost
library("xgboost")

# using randomForest from randomForest
library("randomForest")

# using glmnet from glmnet
library("glmnet")

init<-function(){

  # initialize the data needed

  # read train data
	train_data<<-read.table("./train.csv", header=TRUE, sep=",", na.strings="NA")

  # read test data
  test_data<<-read.table("./test.csv", header=TRUE, sep=",", na.strings="NA")

  # append dummy column in order to binding the 2 dataframe
  dummy_col<-data.frame(c(1:nrow(test_data)))
	names(dummy_col)<-"SalePrice"
	test_data<<-cbind(test_data, dummy_col)

  # binding the 2 dataframe
	mod_data<<-rbind(train_data, test_data)

  # divide the data column into 3 type

  # numeric columns
	numeric_col<<-c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2",
									"BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF",	"LowQualFinSF",
									"GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath",
									"BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
									"GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch",
									"X3SsnPorch",	"ScreenPorch", "PoolArea", "MiscVal", "YearBuilt",
                								 "YearRemodAdd", "GarageYrBlt")

  # factor columns that need to label it manually, to keep data ordinal
	manual_label_col<<-c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "BsmtExposure",
										 "BsmtFinType1", "BsmtFinType2", "HeatingQC", "KitchenQual", "FireplaceQu",
										 "GarageFinish", "GarageCond", "GarageQual", "PavedDrive", "PoolQC",
										 "Fence")

  # label information
	ExterQual_label<<-c("Ex", "Gd", "TA", "Fa", "Po")
	ExterCond_label<<-c("Ex", "Gd", "TA", "Fa", "Po")
	BsmtQual_label<<-c("Ex", "Gd", "TA", "Fa", "Po", "NA")
	BsmtCond_label<<-c("Ex", "Gd", "TA", "Fa", "Po", "NA")
	BsmtExposure_label<<-c("Gd", "Av", "Mn", "No", "NA")
	BsmtFinType1_label<<-c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "NA")
	BsmtFinType2_label<<-c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "NA")
	HeatingQC_label<<-c("Ex", "Gd", "TA", "Fa", "Po")
	KitchenQual_label<<-c("Ex", "Gd", "TA", "Fa", "Po")
	FireplaceQu_label<<-c("Ex", "Gd", "TA", "Fa", "Po", "NA")
	GarageFinish_label<<-c("Fin", "RFn", "Unf", "NA")
	GarageCond_label<<-c("Ex", "Gd", "TA", "Fa", "Po", "NA")
	GarageQual_label<<-c("Ex", "Gd", "TA", "Fa", "Po", "NA")
	PavedDrive_label<<-c("Y", "P", "N")
	PoolQC_label<<-c("Ex", "Gd", "TA", "Fa", "NA")
	Fence_label<<-c("GdPrv", "MnPrv", "GdWo", "MnWw", "NA")

  # factor columns that should keep the same, no action should be taken
	factor_col<<-c("Id", "OverallCond", "MoSold", "YrSold", "OverallQual")

  # the predict target
	target_col<<-c("SalePrice")
}

lv_replace<-function(src_data, x){

  # replace the factor with numeric values (levels)

  # if it is a factor that has to label manually
	if (x %in% manual_label_col){
    # get the label data
		label_vector<-rev(get(paste(x, "label", sep="_")))
	}

  # if it is a factor that can be auto-labeled
	else if (!(x %in% c(numeric_col, factor_col, target_col))){

    # get the unique value and sort it in descending order
		label_vector<-sort(unique(src_data[x])[[1]], decreasing=TRUE)

    # if there is NA in the data, then append NA at the head of the label vector
    # since NA in factor columns have meaning but not stands for missing value
		if (sum(is.na(src_data[, x])) > 0){
			label_vector<-c("NA", label_vector)
		}
	}
  # if the data belong to numeric or SalePrice
	else{
    # no action taken
		label_vector<-NULL
	}

  # replace the data to levels
	for (i in label_vector){
		if (i == "NA"){
			src_data[which(is.na(src_data[, x])==TRUE), x]<-which(label_vector=="NA")
		}
		else{
			src_data[which(src_data[, x]==i), x]<-which(label_vector==i)
		}
	}

  # return the data
	src_data
}

num_present<-function(){

  # all strings are converted to numeric values

  # for each columns in mod_data
	for (i in colnames(mod_data)){
    # replace the factor strings to numeric levels
		mod_data<<-lv_replace(mod_data, i)
	}

  # write the data to mod_data.csv
	write.table(mod_data, "./mod_data.csv", sep=",", na="NA", row.names=FALSE, quote=FALSE)

  # reload the mod_data
  mod_data<<-read.table("./mod_data.csv", sep=",", header=TRUE, na.strings="NA")

}

cor_find<-function(src_data, vname, threshold){

  # calculate the correlation index and return the result

  # make a dataframe for storing the correlation index results
	cor_list<<-data.frame(colnames(src_data), NA)
	names(cor_list)<<-c("Column", "correlation")

  # calculate the correlation index
  for (i in colnames(src_data)){
		cor_list[which(cor_list[,1]==i), 2]<<-cor(src_data[, i], src_data[, vname])
	}

  # sort the dataframe according to the descending order of correlation index
	cor_list<<-cor_list[order(-cor_list$correlation), ]

  # return the data which have correlation index higher than the threshold and not equal to itself
	cor_list[which(abs(cor_list$correlation)>=threshold&cor_list$Column!=vname), ]
}

na_find<-function(){

  # finding the NA values and calculate the missing value percentage

  # make a dataframe for storing the na percentage results
	na_percent<-data.frame(colnames(mod_data), NA)
	names(na_percent)<-c("Column", "NA_percent")

  # calculate the NA%
	for (i in colnames(mod_data)){
		na_percent[which(na_percent$Column==i), 2]<-sum(is.na(mod_data[, i]))/length(mod_data[, i])
	}

  # return the result which has NA values in the data
	na_percent[which(na_percent$NA_percent>0), ]
}

# since R has no mode function provided, here is a self-defined mode function
smode<-function(x_vector){
	ux<-unique(x_vector)
	ux<-ux[which(is.na(ux)==FALSE)]
  ux[which.max(tabulate(match(x_vector, ux)))]
}

na_fill<-function(){

  # using na_find function to find the NA values
  # filling the NA values with some assumption
  # numeric type can be filled by 0 or means, according to their description
  # factor type can only filled by modes

	mod_data[which(is.na(mod_data[, "LotFrontage"])), "LotFrontage"]<<-mean(mod_data$LotFrontage, na.rm=TRUE)
	mod_data[which(is.na(mod_data[, "MasVnrArea"])), "MasVnrArea"]<<-0
	mod_data[which(is.na(mod_data[, "GarageYrBlt"])), "GarageYrBlt"]<<-mod_data[which(is.na(mod_data[, "GarageYrBlt"])), "YearBuilt"]
  mod_data[which(is.na(mod_data[, "BsmtFinSF1"])), "BsmtFinSF1"]<<-0
	mod_data[which(is.na(mod_data[, "BsmtFinSF2"])), "BsmtFinSF2"]<<-0
  mod_data[which(is.na(mod_data[, "BsmtUnfSF"])), "BsmtUnfSF"]<<-0
  mod_data[which(is.na(mod_data[, "TotalBsmtSF"])), "TotalBsmtSF"]<<-0
  mod_data[which(is.na(mod_data[, "BsmtFullBath"])), "BsmtFullBath"]<<-0
	mod_data[which(is.na(mod_data[, "BsmtHalfBath"])), "BsmtHalfBath"]<<-0
  mod_data[which(is.na(mod_data[, "KitchenQual"])), "KitchenQual"]<<-smode(mod_data$KitchenQual)
	mod_data[which(is.na(mod_data[, "GarageCars"])), "GarageCars"]<<-0
	mod_data[which(is.na(mod_data[, "GarageArea"])), "GarageArea"]<<-0
}

feature_append<-function(){

  # to find more linear relationship with SalePrice
  # several features has been combined and become new features

	mod_TotalSF<-data.frame(mod_data$X1stFlrSF+mod_data$X2ndFlrSF+mod_data$TotalBsmtSF)
	names(mod_TotalSF)<-"TotalSF"

	mod_TotalFinSF<-data.frame(mod_data$X1stFlrSF+mod_data$X2ndFlrSF+mod_data$BsmtFinSF1+mod_data$BsmtFinSF2)
	names(mod_TotalFinSF)<-"TotalFinSF"

	mod_TotalPorchSF<-data.frame(mod_data$OpenPorchSF+mod_data$EnclosedPorch+mod_data$X3SsnPorch+mod_data$ScreenPorch)
	names(mod_TotalPorchSF)<-"TotalPorchSF"

	mod_TotalFullBath<-data.frame(mod_data$BsmtFullBath+mod_data$FullBath)
	names(mod_TotalFullBath)<-"TotalFullBath"

	mod_TotalHalfBath<-data.frame(mod_data$BsmtHalfBath+mod_data$HalfBath)
	names(mod_TotalHalfBath)<-"TotalHalfBath"

  # bind the new features with current dataframe
	mod_data<<-cbind(mod_data, mod_TotalSF,
									 mod_TotalPorchSF, mod_TotalFinSF,
									 mod_TotalHalfBath, mod_TotalFullBath)

  # add them to the different type vector
	numeric_col[length(numeric_col)+1]<<-"TotalSF"
	numeric_col[length(numeric_col)+1]<<-"TotalPorchSF"
	numeric_col[length(numeric_col)+1]<<-"TotalFinSF"
	numeric_col[length(numeric_col)+1]<<-"TotalHalfBath"
	numeric_col[length(numeric_col)+1]<<-"TotalFullBath"
	#manual_label_col[length(manual_label_col)+1]<<-"BsmtFinType"
}

feature_remove<-function(){

  # since some features has been combined
  # the combined features has to be removed in order to avoid alias errors
  # some imbalanced and redundant features are also removed
  # they are decided manually with the help of vif function

  mod_data[, "GarageCond"]<<-NULL
	mod_data[, c("X1stFlrSF", "X2ndFlrSF")]<<-NULL
	mod_data[, c("OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch")]<<-NULL
	mod_data[, "Fireplaces"]<<-NULL
	mod_data[, c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF")]<<-NULL
	mod_data[, c("MiscFeature", "MiscVal")]<<-NULL
	mod_data[, c("Street", "Utilities")]<<-NULL
	mod_data[, c("PoolArea")]<<-NULL
  mod_data[, c("BsmtHalfBath", "BsmtFullBath", "FullBath", "HalfBath")]<<-NULL

  # remove their names in type vector
	numeric_col<<-numeric_col[which(!numeric_col %in% c("X1stFlrSF", "X2ndFlrSF", "GarageCond",
																											"OpenPorchSF", "EnclosedPorch", "X3SsnPorch",
																											"ScreenPorch", "Fireplaces", "BsmtFinSF1",
																											"BsmtFinSF2", "BsmtUnfSF", "MiscVal",
																											"PoolArea", "BsmtHalfBath", "BsmtFullBath",
																											"FullBath", "HalfBath"))]
}

data_split<-function(){

  # since we merge the training data and testing data at the beginning
  # now the data should be splited

  # for Id larger than 1460, they are testing data
  test_data<<-mod_data[which(mod_data$Id>1460), ]

  # the rest is training data
  train_data<<-mod_data[which(mod_data$Id<=1460), ]
}

feature_makenorm<-function(){

  # numeric data should be transformed to match the normal distribution
  # so that the linear model can better predict the results
  # skewness index tells the shape the density distribution
  # the skewness index more closer to zero, the distribution of data more tends to normal
  # log transformation is applied

	for(i in numeric_col){
		# if the skewness index is more closer to zero after log transformation
		if (abs(skewness(mod_data[, i])) > abs(skewness(log1p(mod_data[, i])))){
      # transform it
      mod_data[, i]<<-log1p(mod_data[, i])
		}
	}
}

xgb_tune<-function(csp, ssp, md, eta, nf, nr, sr, pn){

  # declare the parameters apply in the model
  xgb.params<<-list(colsample_bytree=csp,
                   subsample=ssp,
                   booster="gbtree",
                   max_depth=md,
                   eta=eta,
                   eval_metric="rmse",
                   objective="reg:squarederror",
                   gamma=0)

  # apply cross validation
  cv.model<<-xgb.cv(params=xgb.params,
                   data=xgb_train_data,
                   nfold=nf,
                   nrounds=nr,
                   early_stopping_rounds=sr,
                   print_every_n=pn)

  # find the most suitable iteration
  xgb_bestit<<-cv.model$best_iteration
  xgb_bestit
}

lm_construct<-function(){

  # construct the linear regression model, print the summary and save predicted results

  # Id is not useful to predict, so drop it
  train_data[, "Id"]<<-NULL

  # only features which has a correlation index more than 0.2 with SalePrice is used
  require_data_col<-c(cor_find(train_data, "SalePrice", 0.2)[, 1], "SalePrice")
  train_data<<-train_data[, require_data_col]

  # construct the model
  current_lm<<-lm(log1p(SalePrice)~. , data=train_data)

  # print the summary
	print(summary(current_lm))

  # get the predict results
	current_pred<<-predict(current_lm, test_data)

  # save predicted results to a csv file
	result<-data.frame(test_data$Id, round(expm1(current_pred)))
	names(result)<-c("Id", "SalePrice")
	write.table(result, file="./current_lm_predict.csv", sep=",", row.names=FALSE)
}

rm_construct<-function(){

  # construct the ridge regression model, print the summary and save predicted results

  # Id is not useful to predict, so drop it
  train_data[, "Id"]<<-NULL

  # select all features to apply in the model
  require_data_col<-cor_find(train_data, "SalePrice", 0)[, 1]
  train_data<<-train_data[, c(require_data_col, "SalePrice")]

  # construct the linear model
  current_rm<<-lm.ridge(log1p(SalePrice)~. , data=train_data, lambda=seq(0, 20, 0.05))

  # print the summary
  print(summary(current_rm))

  # get the predict results
  current_pred<-scale(test_data[, require_data_col], center=current_rm$xm, scale=current_rm$scales) %*% current_rm$coef[, which.min(current_rm$GCV)]+current_rm$ym

  # save predicted results to a csv file
  result<-data.frame(test_data$Id, round(expm1(current_pred)))
  names(result)<-c("Id", "SalePrice")
  write.table(result, file="./current_rm_predict.csv", sep=",", row.names=FALSE)
}

xgb_construct<-function(){

  # construct the xgboost model, print the summary and save predicted results

  test_data_id<-test_data$Id

  # Id is not useful to predict, so drop it
  train_data[, "Id"]<<-NULL
  test_data[, "Id"]<<-NULL

  # select all features to apply in the model
  require_data_col<<-cor_find(train_data, "SalePrice", 0)[, 1]
  train_data<<-train_data[, c(require_data_col, "SalePrice")]
  test_data<<-test_data[, c(require_data_col, "SalePrice")]

  # transform the data frame to dense matrix
  xgb_train_data<-xgb.DMatrix(data=as.matrix(train_data), label=train_data$SalePrice)
  xgb_test_data<-xgb.DMatrix(data=as.matrix(test_data), label=test_data$SalePrice)

  # train the xgboost model
  current_xgb<-xgb.train(params=xgb.params, data=xgb_train_data, nrounds=xgb_bestit)

  # get the predict results
  current_pred<<-predict(current_xgb, xgb_test_data)

  # save predicted results to a csv file
  result<-data.frame(test_data_id, round(current_pred))
  names(result)<-c("Id", "SalePrice")
  write.table(result, file="./current_xgb_predict.csv", sep=",", row.names=FALSE)
}

rf_construct<-function(){

  # construct the random forest model, print the summary and save predicted results

  # Id is not useful to predict, so drop it
  train_data[, "Id"]<<-NULL

  # select features which have correlation index > 0.2 and apply in the model
  require_data_col<-cor_find(train_data, "SalePrice", 0.2)[, 1]
  train_data<<-train_data[, c(require_data_col, "SalePrice")]

  # construct the model
  current_rf<<-randomForest(log1p(SalePrice)~. , data=train_data, ntree=150)

  # print the summary
  print(summary(current_rf))

  # get the predict results
  current_pred<-predict(current_rf, test_data)

  # save predicted results to a csv file
  result<-data.frame(test_data$Id, round(expm1(current_pred)))
  names(result)<-c("Id", "SalePrice")
  write.table(result, file="./current_rf_predict.csv", sep=",", row.names=FALSE)
}

ls_construct<-function(){

  # construct the LASSO regression model, print the summary and save predicted results

  # construct the model
  current_ls<<-cv.glmnet(x=as.matrix(train_data), y=log1p(train_data$SalePrice), alpha=1)

  # print the summary
  print(summary(current_ls))

  # get the predict results
  current_pred<-predict(current_ls, newx=as.matrix(test_data), s=current_ls$lambda.1se)

  # save predicted results to a csv file
  result<-data.frame(test_data$Id, round(expm1(current_pred)))
  names(result)<-c("Id", "SalePrice")
  write.table(result, file="./current_ls_predict.csv", sep=",", row.names=FALSE)
}

lm_demo<-function(){

  # simulate the progress

	init()
	num_present()
	na_fill()
	feature_append()
	feature_remove()
	feature_makenorm()
  data_split()
	lm_construct()
}

rm_demo<-function(){

  # simulate the progress

  init()
  num_present()
  na_fill()
  feature_append()
  feature_remove()
  feature_makenorm()
  data_split()
  rm_construct()
}

ls_demo<-function(){

  # simulate the progress

  init()
  num_present()
  na_fill()
  feature_append()
  feature_remove()
  feature_makenorm()
  data_split()
  ls_construct()
}

xgb_demo<-function(){

  # simulate the progress

  init()
  num_present()
  na_fill()
  feature_append()
  feature_remove()
  data_split()
  xgb_tune(0.05, 0.05, 2, 0.03, 5, 500, 200, 50)
  xgb_construct()
}

rf_demo<-function(){

  # simulate the progress

  init()
  num_present()
  na_fill()
  feature_append()
  feature_remove()
  data_split()
  rf_construct()
}
