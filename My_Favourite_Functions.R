library(pscl)



my_round=function(x, nocomma=F){ 
	if(abs(x)>100) y=prettyNum(round(x), big.mark=",")
	if(abs(x)<100&abs(x)>10) y=sprintf("%.1f", round(x,1))
	if(abs(x)<10&abs(x)>1) y=sprintf("%.2f", round(x,2))
	if(abs(x)<1) y=sprintf("%.3f", round(x,3))
	y
	}

my_round=Vectorize(my_round)

my_round_table=function(x){
	x=as.data.frame(x)
	for(i in 1:ncol(x)) x[,i]=my_round(x[,i])
	x
}

stars=function(x){
	stars=""
	if(x<.1) stars=paste(stars, "*", sep="")
	if(x<.05) stars=paste(stars, "*", sep="")
	if(x<.01) stars=paste(stars, "*", sep="")
	stars
}
stars=Vectorize(stars)


my_ols=function(x, keep=NULL){
	X=coef(summary(x))
	res=as.matrix(paste(my_round(X[,1]), " (", my_round(X[,2]), ")", stars(X[,4]), sep=""))
	row.names(res)=row.names(X)
	if(!is.null(keep)) res=as.matrix(res[keep,], ncol=1)
	model_info=rbind(my_round(summary(x)$r.squared), my_round(nobs(x)))
	row.names(model_info)=c("R-squared", "Number of Observations")
	rbind(res, model_info)
}

my_probit=function(x, null_model=NULL, keep=NULL){
	X=coef(summary(x))
	res=as.matrix(paste(my_round(X[,1]), " (", my_round(X[,2]), ")", stars(X[,4]), sep=""))
	row.names(res)=row.names(X)
	if(!is.null(keep)) res=as.matrix(res[keep,], ncol=1)
	pseudo=1-logLik(x)[1]/logLik(null_model)[1]
	model_info=rbind(my_round(pseudo), my_round(nobs(x)))
	row.names(model_info)=c("Pseudo R-squared", "Number of Observations")
	rbind(res, model_info)
}

my_speedglm=function(x,keep=NULL){
	X=coef(summary(x), stringsAsFactors=F)
	for(i in 1:4) X[,i]=as.numeric(as.character(X[,i]))
	res=as.matrix(paste(my_round(X[,1]), " (", my_round(X[,2]), ")", stars(X[,4]), sep=""))
	row.names(res)=row.names(X)
	if(!is.null(keep)) res=as.matrix(res[keep,], ncol=1)
	model_info=rbind(my_round(1-x$deviance/x$nulldev), my_round(nobs(x)))
	row.names(model_info)=c("Pseudo R-squared", "Number of Observations")
	rbind(res, model_info)
	}