#建立R工作空间
Date<-"2016.04.14";city<-"qingdao";path="d:\\work\\model_nspg\\2016\\"
names_dir<-c("programmes\\","twotables_wd\\","threetables_wd\\","fourtables_wd\\","results\\")
setwd(paste(path,Date,city,"_model","\\",names_dir[3],sep=""))


#导入csv格式数据。导入后使用edit命令，查看数据是否正常导入。
threetables_datapath<-"D://工作//纳税评估模型//2015年//2015.09.16青岛纳税评估模型运算//2015.09.16青岛数据//2、纳税申报表、资产负债表齐全//"
zzsb<-read.csv(paste(threetables_datapath,"增值税申报表三表.csv",sep=""),colClasses=c(rep("character",2)))
sdsb<-read.csv(paste(threetables_datapath,"年度所得税申报表.csv",sep=""),colClasses=c(rep("character",2)))
nsrxxb<-read.csv(paste(threetables_datapath,"纳税人信息表.csv",sep=""),colClasses=c(rep("character",2)))
zcfzbnc<-read.csv(paste(threetables_datapath,"资产负债表期初数据.csv",sep=""),colClasses=c(rep("character",2)))
zcfzbnm<-read.csv(paste(threetables_datapath,"资产负债表年末数据.csv",sep=""),colClasses=c(rep("character",2)))
if(city=="qingdao"){
zzsb$NSRDM<-stroperate(zzsb$NSRDM);sdsb$NSRDM<-stroperate(sdsb$NSRDM);nsrxxb$NSRDM<-stroperate(nsrxxb$NSRDM);zcfzbnc$NSRDM<-stroperate(zcfzbnc$NSRDM);zcfzbnm$NSRDM<-stroperate(zcfzbnm$NSRDM)}
zzsb[,-(1:2)]<-apply(zzsb[,-(1:2)],2,as.numeric)
sdsb[,-(1:2)]<-apply(sdsb[,-(1:2)],2,as.numeric)
zcfzbnc[,-(1:2)]<-apply(zcfzbnc[,-(1:2)],2,as.numeric)
zcfzbnm[,-(1:2)]<-apply(zcfzbnm[,-(1:2)],2,as.numeric)
nsrxxb[,3]<-as.numeric(nsrxxb[,3])


#从各数据表提取所需字段,并按照后续建模需要改名称。
#提取资产负债表年末数表建模所需字段
zcfzbnm_model<-zcfzbnm[,c("NSRDM", "HBZJ", "JYXJRZC", "YSPJ", "YSZK", "YUFZK", "YSLX", "YSGL", "QTYSK", "CH", "YNNDQFLDZC", "QTLDZC", "LDZCHJ", 
"KGCSJRZC", "CYZDQTZ", "CQYSK", "CQGQTZ", "TZXFDC", "GDZC", "ZJGC", "GCWZ", "GDZCQL", "ZCXSWZC", "YQZC", "WXZC", "KFZC", "SY", "CQDTFY", 
"DYSDSZC", "QTFLDZC", "FLDZCHJ", "ZCHJ", "DQJK", "JYXJRFZ", "YFPJ", "YFZK", "YUSZK", "YFZGXC", "YJSF", "YFLX", "YFGL", "QTYFK", "YNNDQFLDFZ", 
"QTLDFZ", "LDFZHJ", "CQJK", "YFZQ", "CQYFK", "ZXYFK", "YJFZ", "DYSY", "DYSDSFZ", "QTFLDFZ", "FLDFZHJ", "FZHJ", "SSZB", "ZBGJ", 
"KCG", "QTZHSY", "ZXCB", "YYGJ", "WFPLR", "SYZQYHJ", "FJJSYZQYHJ")]

#提取资产负债表年初数表建模所需字段
zcfzbnc_model<-zcfzbnc[,c("NSRDM", "HBZJ", "JYXJRZC", "YSPJ", "YSZK", "YUFZK", "YSLX", "YSGL", "QTYSK", "CH", "YNNDQFLDZC", "QTLDZC", "LDZCHJ", 
"KGCSJRZC", "CYZDQTZ", "CQYSK", "CQGQTZ", "TZXFDC", "GDZC", "ZJGC", "GCWZ", "GDZCQL", "ZCXSWZC", "YQZC", "WXZC", "KFZC", "SY", "CQDTFY", 
"DYSDSZC", "QTFLDZC", "FLDZCHJ", "ZCHJ", "DQJK", "JYXJRFZ", "YFPJ", "YFZK", "YUSZK", "YFZGXC", "YJSF", "YFLX", "YFGL", "QTYFK", "YNNDQFLDFZ", 
"QTLDFZ", "LDFZHJ", "CQJK", "YFZQ", "CQYFK", "ZXYFK", "YJFZ", "DYSY", "DYSDSFZ", "QTFLDFZ", "FLDFZHJ", "FZHJ", "SSZB", "ZBGJ", 
"KCG", "QTZHSY", "ZXCB", "YYGJ", "WFPLR", "SYZQYHJ", "FJJSYZQYHJ")]

#提取增值税申报表
zzsb_model<-zzsb[,c("NSRDM", "YB_SYSL_XSE", "YB_YSHW_XSE", "YB_YSLW_XSE", "YB_NSTZ_XSE", "YB_JYZS_XSE", "YB_JYZS_NSTZ_XSE", "YB_MDT_XSE", 
"YB_MS_XSE", "YB_MSHW_XSE", "YB_MSLW_XSE", "YB_XXSE", "YB_JXSE", "YB_SQLDSE", "YB_JXSZCE", "YB_MDTHWYTSE", "YB_SYSL_NSJCBJSE", 
"YB_YDKSE", "YB_SJDKSE", "YB_YNSE", "YB_QMLDSE", "YB_JYJS_YNSE", "YB_JYJS_YBJSE", "YB_YNSEJZE", "YB_YNSEHJ", "YB_QCWJSE", "YB_SSJKSTSE", 
"YB_BQYJSE", "YB_FCYJSE", "YB_ZYJKSYJSE", "YB_BQJNSQSE", "YB_BQJNQSE", "YB_QMWJSE", "YB_QSE", "YB_BQYBTSE", "YB_QCWJCBSE", "YB_BQRKCBSE", "YB_QMWJCBSE")]
#通过names(zzsb)确认变量数目。并按照后续建模的需要改字段名称。
names(zzsb_model)[1:38]<-c("NSRDM",  "SYSL_XSE", "SYSL_YSHW_XSE", "SYSL_YSLW_XSE", "SYSL_NSJC_XSE", "JYZS_XSE", "JYZS_NSJC_XSE", "MDT_CKHW_XSE", 
"MS_XSE", "MS_HWXSE", "MS_LWXSE", "XXSE", "JXSE", "SQLDSE", "JXSEZC", "MDTHWYTSE", "SYSL_NSJC_YBJSE", "YDKSEHJ", "SJDKSE", "YNSE.ZZ", 
"QMLDSE", "JYZS_YNSE", "JYZS_NSJC_YBJSE", "YNSEJZE", "YNSEHJ", "QCWJSE", "CKZYJKS_TSE", "BQYJSE", "QZ_FCYJSE", "QZ_CKZYJKS_YJSE", 
"QZ_BQJNSQSE", "QZ_BQJNQJSE", "QMWJSE", "QZ_QJSE", "BQYBTSE", "QCWJCBSE", "BQRKCBSE", "QMWJCBSE")

#选出建模所需要的所得税表字段。
sdsb_model<-sdsb[,c("NSRDM", "YYSR", "YYCB", "YYSJFJ", "XSFY", "GLFY", "CWFY", "ZCJZSS", "GYJZBDSY", "TZSY", "YYLR", "YYWSR", "YYWZC", "LRZE", 
"NSTZZJE", "NSTZJSE", "BZSSR", "MSSR", "JJSR", "JMSXMSD", "JJKC", "DKYNSSDE", "JWYSSDMBKS", "NSTZHSD", "MBYQNDKS", "YNSSDE", "SL", 
"YNSDSE", "JMSDSE", "DMSDSE", "YNSE", "JWYNSDSE", "JWDMSDSE", "SJYNSDSE", "BNLJSJYYJSDSE", "HZZJGFTYJSE", "HZZJGCZYJSE", "HZZJGSSYJSE", 
"HBJDYJBL", "HBJDYJSE", "BQYBSDSE", "YQNDDJDJSE", "SNYJWJBNRKSDSE")]
names(sdsb_model)<-c("NSRDM",  "YYSR", "YYCB", "YYSJFJ", "XSFY", "GLFY", "CWFY", "ZCJZSS", "GYJZBDSY", "TZSY", "YYLR", "YYWSR", "YYWZC", "LRZE", 
"NSTZZJE", "NSTZJSE", "BZSSR", "MSSR", "JJSR", "JMSXMSD", "JJKC", "DKYNSSDE", "JWYSSDMBKS", "NSTZHSD", "MBYQNDKS", "YNSSDE", "SL", 
"YNSDSE", "JMSDSE", "DMSDSE", "YNSE.SD", "JWYNSDSE", "JWDMSDSE", "SJYNSDSE", "BNLJSJYYJSDSE", "HZZJGFTYJSE", "HZZJGCZYJSE", "HZZJGSSYJSE", 
"HBJDYJBL", "HBJDYJSE", "BQYBSDSE", "YQNDDJDJSE", "SNYJWJBNRKSDSE")
#提取纳税人信息表数据,并按照后续建模的需要提取所需字段
nsrxxb_model<-nsrxxb[,c("NSRDM", "NSRMC","HY_MC3", "HY_DM")]
#生成资产负债表年末减年初数表
zcfzb_model<-merge(zcfzbnc_model,zcfzbnm_model,by.x="NSRDM",by.y="NSRDM",all.x=TRUE)
#提取资产负债表的字段名称。
names_zcfzb<-names(zcfzbnc_model)[-1]
#abszz_onenum<-paste("abszz_one",1:13,sep="")
zcfzb_model[,paste(names_zcfzb,".xy",sep="")]<-zcfzb_model[,paste(names_zcfzb,".y",sep="")]-zcfzb_model[,paste(names_zcfzb,".x",sep="")]
#合成总表
zzsdsb<-merge(zzsb_model,sdsb_model,by.x="NSRDM",by.y="NSRDM",all.x=TRUE)
nsrzzsdb<-merge(nsrxxb_model,zzsdsb,by.x="NSRDM",by.y="NSRDM",all.x=TRUE)
totalhb<-merge(nsrzzsdb,zcfzb_model,by.x="NSRDM",by.y="NSRDM",all.x=TRUE)

totalhb$NSRDM<-as.character(totalhb$NSRDM)
totalhb$NSRMC<-as.character(totalhb$NSRMC)
totalhb$HY_MC3<-as.character(totalhb$HY_MC3)
totalhb$SXFY<-totalhb$XSFY+totalhb$GLFY+totalhb$CWFY
#永久剔除营业收入小于100的企业。
totalhb100<-totalhb[totalhb$YYSR>=100,]
write.table(totalhb100,"totaldata",sep="\t")

number_hy_calculate(nsrxxb)

hydmset<-hydmset_generate(read.csv("threetables_fenlei.csv",as.is=TRUE))
names(hydmset)<-c("制造业","批发业","纺织、服装及家庭用品批发","矿产品、建材及化工产品批发","机械设备、五金产品及电子产品批发","零售业","食品、饮料及烟草制品批发","橡胶和塑料制品业","金属制品业","通用设备制造业","专用设备制造业")



#导入经过处理后的总表
shdata<-read.table("totaldata",sep="\t")
lzdata<-shdata
#按照销售收入从小到大排序,下15%,上2%的企业不进入样本。
quantilelzdata<-quantile(lzdata$YYSR,probs=c(0.15,0.98))
lzdata<-lzdata[lzdata$YYSR>quantilelzdata[1]&lzdata$YYSR<quantilelzdata[2],]




#经过全变量筛选后剩余的变量如下：
lrbindex_del<-c("YYSR", "YYCB", "YYSJFJ", "XSFY", "GLFY", "CWFY",  "LRZE", "SJYNSDSE")
zcfzbindex_del<-c("HBZJ.y", "YSZK.y", "YUFZK.y", "QTYSK.y", "CH.y", "GDZC.y", "FLDZCHJ.y",  "YFZK.y", "YJSF.y", "QTYFK.y", "LDFZHJ.y", "ZBGJ.y", "WFPLR.y")
zzsbindex_del<-c("SYSL_YSHW_XSE", "XXSE", "JXSE", "SQLDSE", "YDKSEHJ", "SJDKSE",  "QCWJSE", "BQYJSE")

#后续模块生成formula_formulaclass和formula_vectorclass,formula_formulaclass为公式类型，formula_vectorclass为向量类型。
formula_vectorclass<-list()
for(i in 1:20)
{
    formula_vectorclass[[i]]<-list()
	formula_vectorclass[[i]][[1]]<-c("YYSR",lrbindex_del[-1])
    formula_vectorclass[[i]][[2]]<-c("YYCB",lrbindex_del[-2])
    formula_vectorclass[[i]][[3]]<-c("SXFY",lrbindex_del[-(4:6)])
    formula_vectorclass[[i]][[4]]<-c("YYSR",lrbindex_del[-1],zzsbindex_del)
    formula_vectorclass[[i]][[5]]<-c("YYCB",lrbindex_del[-2],zzsbindex_del)
    formula_vectorclass[[i]][[6]]<-c("SXFY",lrbindex_del[-(4:6)],zzsbindex_del)
    formula_vectorclass[[i]][[7]]<-c("YYSR",lrbindex_del[-1],zcfzbindex_del)
    formula_vectorclass[[i]][[8]]<-c("YYCB",lrbindex_del[-2],zcfzbindex_del)
    formula_vectorclass[[i]][[9]]<-c("SXFY",lrbindex_del[-(4:6)],zcfzbindex_del)
    formula_vectorclass[[i]][[10]]<-c("XXSE",lrbindex_del,zzsbindex_del[-2])
    formula_vectorclass[[i]][[11]]<-c("SJDKSE",lrbindex_del,zzsbindex_del[-6])
    formula_vectorclass[[i]][[12]]<-c("XXSE",zzsbindex_del[-2],zcfzbindex_del)
    formula_vectorclass[[i]][[13]]<-c("SJDKSE",zzsbindex_del[-6],zcfzbindex_del)
    formula_vectorclass[[i]][[14]]<-c("YNSE.ZZ",lrbindex_del,zzsbindex_del[-2],zcfzbindex_del)

}
#生成formula格式的公式。储存在列表formula_formulaclass中
formula_formulaclass<-formula_vectorclass
for(i in 1:20)
{
    for(j in 1:length(formula_vectorclass[[1]]))
    {
        formula_formulaclass[[i]][[j]]<-as.formula(paste(formula_vectorclass[[i]][[j]][1],"~", paste(formula_vectorclass[[i]][[j]][-1], collapse= "+")))
    }
}
#统计经济模型数目
modelnum<-length(formula_vectorclass[[1]])

#sltj_alltables(hydmset[[7]],"alasso",names(hydmset)[7])
for(i in 1:length(hydmset))
{
    sltj_alltables(hydmset[[i]],"step",names(hydmset)[i])
}
for(i in 1:length(hydmset))
{
    sltj_alltables(hydmset[[i]],"alasso",names(hydmset)[i])
}


#sjwl_alllabels(hydmset[[6]][[1]],"step",names(hydmset)[6])
for(i in 1:length(hydmset))
{
    sjwl_alllabels(hydmset[[i]][[1]],"step",names(hydmset)[i])
}
for(i in 1:length(hydmset))
{
    sjwl_alllabels(hydmset[[i]][[1]],"alasso",names(hydmset)[i])
}

data_ydjg<-data.frame()
for(i in 1:length(hydmset))
{
    for(j in 1:(length(hydmset[[i]])-1))
    {
        data_ydjg<-rbind(data_ydjg,ydkm_calculate(hydmset[[i]][[j+1]]))
    }
}
write.table(data_ydjg,"data_ydjg",sep="\t")


shdata$s<-0
col_calculate<-which(names(shdata)%in%names_alltables_include)
for(i in 1:length(shdata[,1]))
{
    shdata$s[i]<-svalue_calculate(shdata[i,col_calculate])
}
lzdata<-shdata[shdata$NSRDM%in%lzdata$NSRDM,]

kfjg_regression<-data.frame()
for(i in 1:length(hydmset))
{
    kfjg_regression<-rbind(kfjg_regression,regression_kf(hydmset[[i]][[1]]))
}
jgdata_kf<-unit_10000(kfjg_regression)
kfjg_chae_num<-paste("kfjg_chae",1:6,sep="")
jgdata_kf[,c("sd_kf_lower","sd_kf_upper","sd_kf_pgz_lower","sd_kf_pgz_upper","zz_kf_lower","zz_kf_upper","zz_kf_pgz_lower","zz_kf_pgz_upper")]<-0
for(i in 1:length(jgdata_kf[,1]))
{
    jgdata_kf[i,c("sd_kf_lower","sd_kf_upper","sd_kf_pgz_lower","sd_kf_pgz_upper")]<-interval_generate(negative(jgdata_kf[i,kfjg_chae_num[1:3]]))
	jgdata_kf[i,c("zz_kf_lower","zz_kf_upper","zz_kf_pgz_lower","zz_kf_pgz_upper")]<-interval_generate(negative(jgdata_kf[i,kfjg_chae_num[4:6]]))
}
write.table(jgdata_kf,"卡方法结果",sep="\t")

zhpg_alltables("step")
zhpg_alltables("alasso")


