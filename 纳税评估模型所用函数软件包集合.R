
#纳税评估模型所用函数软件包集合.R
#循环判断软件包是否已经安装，如果没有安装，则下载安装。
names_library<-c("msgps","mclust","kernlab","MASS")
for(i in 1:length(names_library))
{
    if(!(names_library[i]%in%library()$res[,1])) install.packages(names_library[i])
}
rm(i)
#载入模型所用到R包
library(msgps)
library(mclust)
library(kernlab)
library(MASS)
#载入常用变量名称
names_zcfzb<-c("HBZJ", "JYXJRZC", "YSPJ", "YSZK", "YUFZK", "YSLX", "YSGL", "QTYSK", "CH", "YNNDQFLDZC", "QTLDZC", "LDZCHJ", 
"KGCSJRZC", "CYZDQTZ", "CQYSK", "CQGQTZ", "TZXFDC", "GDZC", "ZJGC", "GCWZ", "GDZCQL", "ZCXSWZC", "YQZC", "WXZC", "KFZC", "SY", "CQDTFY", 
"DYSDSZC", "QTFLDZC", "FLDZCHJ", "ZCHJ", "DQJK", "JYXJRFZ", "YFPJ", "YFZK", "YUSZK", "YFZGXC", "YJSF", "YFLX", "YFGL", "QTYFK", "YNNDQFLDFZ", 
"QTLDFZ", "LDFZHJ", "CQJK", "YFZQ", "CQYFK", "ZXYFK", "YJFZ", "DYSY", "DYSDSFZ", "QTFLDFZ", "FLDFZHJ", "FZHJ", "SSZB", "ZBGJ", 
"KCG", "QTZHSY", "ZXCB", "YYGJ", "WFPLR", "SYZQYHJ", "FJJSYZQYHJ")
names_zzsb<-c("SYSL_XSE", "SYSL_YSHW_XSE", "SYSL_YSLW_XSE", "SYSL_NSJC_XSE", "JYZS_XSE", "JYZS_NSJC_XSE", "MDT_CKHW_XSE", 
"MS_XSE", "MS_HWXSE", "MS_LWXSE", "XXSE", "JXSE", "SQLDSE", "JXSEZC", "MDTHWYTSE", "SYSL_NSJC_YBJSE", "YDKSEHJ", "SJDKSE", "YNSE.ZZ", 
"QMLDSE", "JYZS_YNSE", "JYZS_NSJC_YBJSE", "YNSEJZE", "YNSEHJ", "QCWJSE", "CKZYJKS_TSE", "BQYJSE", "QZ_FCYJSE", "QZ_CKZYJKS_YJSE", 
"QZ_BQJNSQSE", "QZ_BQJNQJSE", "QMWJSE", "QZ_QJSE", "BQYBTSE", "QCWJCBSE", "BQRKCBSE", "QMWJCBSE")
names_sdsb<-c("YYSR", "YYCB", "YYSJFJ", "XSFY", "GLFY", "CWFY", "ZCJZSS", "GYJZBDSY", "TZSY", "YYLR", "YYWSR", "YYWZC", "LRZE", 
"NSTZZJE", "NSTZJSE", "BZSSR", "MSSR", "JJSR", "JMSXMSD", "JJKC", "DKYNSSDE", "JWYSSDMBKS", "NSTZHSD", "MBYQNDKS", "YNSSDE", "SL", 
"YNSDSE", "JMSDSE", "DMSDSE", "YNSE.SD", "JWYNSDSE", "JWDMSDSE", "SJYNSDSE", "BNLJSJYYJSDSE", "HZZJGFTYJSE", "HZZJGCZYJSE", "HZZJGSSYJSE", 
"HBJDYJBL", "HBJDYJSE", "BQYBSDSE", "YQNDDJDJSE", "SNYJWJBNRKSDSE")
names_lrb<-c("YYSR", "YYCB", "YYSJFJ", "XSFY", "GLFY", "CWFY", "ZCJZSS", "GYJZBDSY", "DLYQYHYQYTZSY", "YYLR", "TZSY", "YYWSR", 
"YYWZC", "FLDZCCZSS", "LRZE", "SDS", "JLR", "JBMGSY", "XSMGSY")
#names_alltables_include用于卡方法计算s值时选择原始的数值型变量。
names_alltables_include<-c(names_zzsb,paste(names_zcfzb,".x",sep=""),paste(names_zcfzb,".y",sep=""),names_lrb,names_sdsb,names_zzsb,paste(names_lrb,".y",sep=""))
#names(shdata[,which(names(shdata)%in%names_alltables_include)])



#载入常用函数
#行业数目统计函数，统计行业小类、行业中类、行业大类、行业门类包含的企业户数。结果保存到工作空间中，命名为行业数据统计1。
number_hy_calculate<-function(data_nsrxxb)
{
    shdata<-read.table("totaldata",sep="\t")
	nsrxxbtj<-data_nsrxxb[data_nsrxxb$NSRDM%in%shdata$NSRDM,]
	hydm<-sort(unique(nsrxxbtj$HY_DM))
	numhydm<-vector()
	namehydm<-vector()
	numhydm1<-vector()
	namehydm1<-vector()
	numhydm2<-vector()
	namehydm2<-vector()
	numhydm3<-vector()
	namehydm3<-vector()
	for(i in 1:length(hydm))
	{
		numhydm[i]<-length(which(nsrxxbtj$HY_DM==hydm[i]))
		namehydm[i]<-as.character(unique(nsrxxbtj[which(nsrxxbtj$HY_DM==hydm[i]),]$HY_MC3))
		namehydm1[i]<-as.character(unique(nsrxxbtj[which(nsrxxbtj$HY_DM==hydm[i]),]$SSDL))
		namehydm2[i]<-as.character(unique(nsrxxbtj[which(nsrxxbtj$HY_DM==hydm[i]),]$HY_MC1))
		namehydm3[i]<-as.character(unique(nsrxxbtj[which(nsrxxbtj$HY_DM==hydm[i]),]$HY_MC2))
	}
	numhy<-cbind(namehydm1,namehydm2,namehydm3,namehydm,hydm,numhydm)
	numhy<-as.data.frame(numhy,as.is=TRUE)
	numhy$numhydm<-as.numeric(as.character(numhy$numhydm))
	for(i in 1:length(hydm))
	{
		numhydm1[i]<-sum(as.numeric(numhy[which(numhy$namehydm1==as.character(unique(numhy[which(numhy$hydm==hydm[i]),]$namehydm1))),]$numhydm))
		numhydm2[i]<-sum(as.numeric(numhy[which(numhy$namehydm2==as.character(unique(numhy[which(numhy$hydm==hydm[i]),]$namehydm2))),]$numhydm))
		numhydm3[i]<-sum(as.numeric(numhy[which(numhy$namehydm3==as.character(unique(numhy[which(numhy$hydm==hydm[i]),]$namehydm3))),]$numhydm))
	}
	numhy$numhydm1<-numhydm1
	numhy$numhydm2<-numhydm2
	numhy$numhydm3<-numhydm3
	numhy1<-numhy[,c("namehydm1","numhydm1","namehydm2","numhydm2","namehydm3","numhydm3","namehydm","hydm","numhydm")]
	names(numhy1)<-c("行业门类名称","行业门类企业数目","行业大类名称","行业大类企业数目","行业中类名称","行业中类企业数目","行业小类名称","行业代码","行业小类企业数目")	
	write.table(numhy1,"行业数据统计1",sep="\t",row.names=F)
}




#循环次数函数，用于数理统计模型自选样算法循环次数的计算。
#■■需要根据需要设置样本比例■■
number<-function(data)
{
    n<-length(data[,1])
    if(n>500)
    {
        s<-floor(n/3)+floor(n/10)
    }
    else
    {
        s<-floor(n/3)
    }
    s
}
#自选样函数
#edit(names(hydata[[3]]))
model<-function(mxdata,number,formula)
{
    ma<-mxdata
    for(i in 1:number)
    {
        lm1<-lm(formula,data=ma)
		ma<-ma[row.names(ma)!=(as.numeric(names(sort(abs(lm1$res),decreasing=TRUE)[1]))),]
    }
    lm1<-lm(formula,data=ma)
    lm1
}
#特征选择函数，可以实现逐步回归和AdaptiveLASSO。适用于神经网络和数理统计两种方法，当神经网络算法调用时，生成向量形式的模型公式。
featherselection<-function(data_list,method,count_subset)
{
    formulastep<-list()
	for(i in 1:count_subset)
	{
	    formulastep[[i]]<-list()
	}
	if(method=="step"){
	    print("逐步回归选择变量")
		for(i in 1:count_subset)
		{
			for(j in 1:modelnum)
			{
				lm.reg<-lm(formula_formulaclass[[i]][[j]],data=data_list[[i]])
				formulastep[[i]][[j]]<-formula(step(lm.reg))
				if(count_subset==1) {formulastep[[i]][[j]]<-names(step(lm.reg)$model)}
			}
		}
	}else if(method=="alasso"){
	    jgalasso<-list()
		length(jgalasso)<-count_subset
		for(i in 1:count_subset)
		{
		    jgalasso[[i]]<-list()
		}
		print("Adaptive LASSO选择变量")
		for(i in 1:count_subset)
		{
		    for(j in 1:modelnum)
			{
			    jgalasso[[i]][[j]]<-msgps(X=as.matrix(data_list[[i]][,formula_vectorclass[[i]][[j]][-1]]),y=data_list[[i]][,formula_vectorclass[[i]][[j]][1]],penalty="alasso",gamma=1,lambda=0)
				#提取基于BIC的AdaptiveLASSO结果
				alassocoef<-coef(jgalasso[[i]][[j]])[,4]
				#提取非零自变量序号
				numcoef<-as.numeric(which(alassocoef!=0))
				#提取非零自变量名称
				namescoef<-names(alassocoef[numcoef])
				#加入因变量名称
				namescoef[1]<-formula_vectorclass[[i]][[j]][1]
				#保存因变量自变量名称
				formulastep[[i]][[j]]<-as.formula(paste(namescoef[1],"~", paste(namescoef[-1], collapse= "+")))	
				if(count_subset==1) formulastep[[i]][[j]]<-c(namescoef[1],namescoef)
			}
		}
	}else{
	    print("请正确输入变量选择方法，step或者alasso")
		break
	}
    formulastep	
}

#定义生成神经网络选样指标函数，生成神经网络选样指标A~K。
generate_samplevariable<-function(data_samplevariable)
{
    data<-data_samplevariable
	if("ZCHJ.y"%in%names(data)){#通过判断数据集中有无资产负债表字段，来选择神经网络选样指标。
	data$A<-abs(data$YYSR)
	data$B<-data$ZCHJ.y
	data$C<-data$YYLR
	data$D<-data$XSFY+data$GLFY+data$CWFY
	data$E<-data$XXSE
	data$F<-data$SJDKSE
	data$G<-data$YYCB
	data$H<-data$YYCB+data$XSFY+data$GLFY+data$CWFY+data$YYWZC+data$HBZJ.xy
	data$I<-data$SYSL_XSE+data$JYZS_XSE+data$MDT_CKHW_XSE+data$MS_XSE+data$YYWSR+data$CH.xy
	data$J<-data$SJYNSDSE+data$YNSEHJ
	data$K<-data$SYSL_XSE+data$JYZS_XSE+data$MDT_CKHW_XSE+data$MS_XSE+data$YYWSR
	}else{
	data$A<-abs(data$YYSR)
	data$B<-data$YYCB
	data$C<-data$YYLR
	data$D<-data$XSFY+data$GLFY+data$CWFY
	data$E<-data$XXSE
	data$F<-data$SJDKSE
	data$G<-data$YYCB
	data$H<-data$YYCB+data$XSFY+data$GLFY+data$CWFY+data$YYWZC
	data$I<-data$SYSL_XSE+data$JYZS_XSE+data$MDT_CKHW_XSE+data$MS_XSE+data$YYWSR
	data$J<-data$SJYNSDSE+data$YNSEHJ
	data$K<-data$SYSL_XSE+data$JYZS_XSE+data$MDT_CKHW_XSE+data$MS_XSE+data$YYWSR
	}	
	data
}
#test<-shdata[1:10,]
#edit(generate_samplevariable(test))
#定义卡方法计算s值的函数
svalue_calculate<-function(x)
{
    data<-x
    lengthdata<-length(data)
    m<-vector()
    for(i in 1:lengthdata)
    {
        y<-as.character(data[i])
		lengthy<-length(strsplit(y,"")[[1]])
		z<-substr(y,2,lengthy-1)
		m<-c(m,strsplit(z,"")[[1]])
    }
    xulie<-strsplit("0123456789","")[[1]]
    count<-vector()
    for(j in 1:10)
    {
        count[j]<-length(which(m==xulie[j]))
    } 
    p<-mean(count)
    s<-sum((count-p)^2)/p
    s
}
#定义处理单位为万元的函数，最终结果需要以万元为单位展示。
unit_10000<-function(data_modeljg)
{
   names_character<-c("NSRDM", "NSRMC", "HY_MC3", "HY_DM","s")
   data_modeljg[!(names(data_modeljg)%in%names_character)]<-data_modeljg[!(names(data_modeljg)%in%names_character)]/10000
   data_modeljg
}
#定义negative函数，模型第一次计算的结果需要负值取绝对值*0.3，正值*0.7，最后+1万元操作。
negative<-function(data)
{    
    for(i in 1:length(data))
    {
        if(data[i]<0){data[i]<-abs(data[i])*0.3+1} else{data[i]<-data[i]*0.7+1}
    }    
    data
}
#生成区间函数，用于综合评估中生成区间上下限。可以分为三种情况，6个结果生成
interval_generate<-function(data_vector)
{
    length_data<-length(data_vector)
	data_vector<-as.numeric(data_vector)
	if(length_data==6){lower<-mean(as.numeric(quantile(data_vector,probs=c(0,0.2,0.4))));upper<-mean(as.numeric(quantile(data_vector,probs=c(0,0.2,0.4,0.6))))}
	if(length_data==4){lower<-mean(as.numeric(quantile(data_vector,probs=c(0,1/3,2/3))));upper<-mean(as.numeric(quantile(data_vector,probs=c(1/3,2/3))))}
	if(length_data==3){lower<-mean(as.numeric(quantile(data_vector,probs=c(0))));upper<-mean(as.numeric(quantile(data_vector,probs=c(0,0.5))))}
	bilv<-upper/lower;lower_pgz<-lower;upper_pgz<-upper
	if(bilv<1.2) lower_pgz<-upper/(1.3+0.2*runif(1))
	if(bilv>3&bilv<=10) {lower_pgz<-lower*(1.3+0.2*runif(1));upper_pgz<-lower_pgz*(1.3+0.2*runif(1))}
	if(bilv>10&bilv<=20) {lower_pgz<-lower*(1.5+0.4*runif(1));upper_pgz<-lower_pgz*(1.5+0.4*runif(1))}
	if(bilv>20&bilv<=50) {lower_pgz<-lower*(2.1+0.4*runif(1));upper_pgz<-lower_pgz*(2.1+0.4*runif(1))}
	if(bilv>50) {lower_pgz<-lower*(2.5+0.4*runif(1));upper_pgz<-lower_pgz*(2.5+0.4*runif(1))}
	data_output<-c(lower,upper,lower_pgz,upper_pgz)
	data_output
}
#字符串处理函数。
#因为NSRDM过长，最长达到18位，R识别困难，因此将所有纳税人代码开头的3702用8来代替，这样所有的代码长度位数减3
stroperate<-function(x)
{
	for(i in 1:length(x))
	{
	    x[i]<-sub("3702","8",x[i])
	}
	x
}	



#生成一个列表，命名为hydmset其中第i个组件是第i个相对大类的行业代码集合，每个组件同样是一个列表，自列表中第一个组件是行业相对大类代码集合，第j个组件是该行业相对大类的第(j-1)个行业相对小类代码集合。
#例如，第8个相对大类为hydmset[[8]],hydmset包含行业分类信息，是重要的全局变量，行业的批量计算需要通过hydmset循环。

hydmset_generate<-function(fenlei_csv)
{
    fenlei<-fenlei_csv
	for(i in 1:length(fenlei[,1]))
	{if(fenlei[,3][i]!=""){fenlei$bianhao[i]<-fenlei[,3][i]}else {fenlei$bianhao[i]<-fenlei$bianhao[i-1]}}
	hydmset<-list()
	#得到大类数目
	daleinum<-0
	for(i in 1:length(unique(fenlei[,4])))
	{if(paste(i,1,sep=",")%in%unique(fenlei[,4])){daleinum<-daleinum+1}else{break}}
	for(i in 1:daleinum)
	{
		hydmset[[i]]<-list()
		xiaoleinum<-0
		for(j in 1:20)
		{if(paste(i,j,sep=",")%in%unique(fenlei[,4])){xiaoleinum<-xiaoleinum+1}else{break}}
		for(j in 1:xiaoleinum)
		{hydmset[[i]][[j+1]]<-fenlei[fenlei$bianhao==paste(i,j,sep=","),]$行业代码 }
		hydmset[[i]][[1]]<-vector()
		for(j in 1:xiaoleinum)
		{hydmset[[i]][[1]]<-c(hydmset[[i]][[1]],hydmset[[i]][[j+1]])}    
	}
	hydmset
}


