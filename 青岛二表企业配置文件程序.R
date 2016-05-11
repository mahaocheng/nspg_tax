#配置Date日期,city城市,path路径等全局变量
Date<-"2016.04.14";city<-"qingdao";path="d:\\work\\model_nspg\\2016\\"
names_dir<-c("programmes\\","twotables_wd\\","threetables_wd\\","fourtables_wd\\","results\\")
setwd(paste(path,Date,city,"_model","\\",names_dir[2],sep=""))#定义工作空间，二表对应names_dir[2]，即twotables_wd

#正确输入二表数据存放路径。然后导入数据。
twotables_datapath<-"D://工作//纳税评估模型//2015年//2015.09.16青岛纳税评估模型运算//2015.09.16青岛数据//3、纳税申报表齐全//"
zzsb<-read.csv(paste(twotables_datapath,"增值税申报表.csv",sep=""),colClasses=c(rep("character",2)))
sdsb<-read.csv(paste(twotables_datapath,"所得税申报表.csv",sep=""),colClasses=c(rep("character",2)))
nsrxxb<-read.csv(paste(twotables_datapath,"纳税人信息表.csv",sep=""),colClasses=c(rep("character",2)))
#对于青岛模型，需要将纳税人代码的第一个3702，替换为8，减少3位，才能正确展示。
if(city=="qingdao"){
zzsb$NSRDM<-stroperate(zzsb$NSRDM);sdsb$NSRDM<-stroperate(sdsb$NSRDM);nsrxxb$NSRDM<-stroperate(nsrxxb$NSRDM)}
zzsb[,-(1:2)]<-apply(zzsb[,-(1:2)],2,as.numeric)
sdsb[,-(1:2)]<-apply(sdsb[,-(1:2)],2,as.numeric)
nsrxxb[,3]<-as.numeric(nsrxxb[,3])

#从各数据表提取所需字段,并按照后续建模需要改名称。
#提取增值税申报表
zzsb_model<-zzsb[,c("NSRDM", "YB_SYSL_XSE", "YB_YSHW_XSE", "YB_YSLW_XSE", "YB_NSTZ_XSE", "YB_JYZS_XSE", "YB_JYZS_NSTZ_XSE", "YB_MDT_XSE", 
"YB_MS_XSE", "YB_MSHW_XSE", "YB_MSLW_XSE", "YB_XXSE", "YB_JXSE", "YB_SQLDSE", "YB_JXSZCE", "YB_MDTHWYTSE", "YB_SYSL_NSJCBJSE", 
"YB_YDKSE", "YB_SJDKSE", "YB_YNSE", "YB_QMLDSE", "YB_JYJS_YNSE", "YB_JYJS_YBJSE", "YB_YNSEJZE", "YB_YNSEHJ", "YB_QCWJSE", "YB_SSJKSTSE", 
"YB_BQYJSE", "YB_FCYJSE", "YB_ZYJKSYJSE", "YB_BQJNSQSE", "YB_BQJNQSE", "YB_QMWJSE", "YB_QSE", "YB_BQYBTSE", "YB_QCWJCBSE", "YB_BQRKCBSE", "YB_QMWJCBSE")]
#通过names(zzsb_model)确认变量数目。并按照后续建模的需要改字段名称。
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
#合成总表
zzsdsb<-merge(zzsb_model,sdsb_model,by.x="NSRDM",by.y="NSRDM",all.x=TRUE)
nsrzzsdb<-merge(nsrxxb_model,zzsdsb,by.x="NSRDM",by.y="NSRDM",all.x=TRUE)
totalhb<-nsrzzsdb
totalhb$NSRDM<-as.character(totalhb$NSRDM);totalhb$NSRMC<-as.character(totalhb$NSRMC);totalhb$HY_MC3<-as.character(totalhb$HY_MC3);
totalhb$SXFY<-totalhb$XSFY+totalhb$GLFY+totalhb$CWFY
#永久剔除营业收入小于100的企业。
totalhb100<-totalhb[totalhb$YYSR>=100,]
write.table(totalhb100,"totaldata",sep="\t")#将数据总表保存到工作空间。


number_hy_calculate(nsrxxb)#行业数目统计
hydmset<-hydmset_generate(read.csv("twotables_fenlei.csv",as.is=TRUE))#处理行业分类信息，保存到hydmset中，hydmset是重要的全局变量。
#hydmset的名称即为行业相对大类的名称。
names(hydmset)<-c("制造业","批发业","纺织、服装及家庭用品批发","矿产品、建材及化工产品批发","机械设备、五金产品及电子产品批发","零售业","食品、饮料及烟草制品批发","橡胶和塑料制品业","金属制品业","通用设备制造业","专用设备制造业")

#导入经过处理后的总表
shdata<-read.table("totaldata",sep="\t")
lzdata<-shdata
#按照销售收入从小到大排序,下15%,上2%的企业不进入样本。
quantilelzdata<-quantile(lzdata$YYSR,probs=c(0.15,0.98))
lzdata<-lzdata[lzdata$YYSR>quantilelzdata[1]&lzdata$YYSR<quantilelzdata[2],]


#后续模块生成formula_formulaclass和formula_vectorclass,formula_formulaclass为公式类型，formula_vectorclass为向量类型。
#经过全变量筛选后剩余的变量如下：
lrbindex_del<-c("YYSR", "YYCB", "YYSJFJ", "XSFY", "GLFY", "CWFY",  "LRZE", "SJYNSDSE")
zzsbindex_del<-c("SYSL_YSHW_XSE", "XXSE", "JXSE", "SQLDSE", "YDKSEHJ", "SJDKSE",  "QCWJSE", "BQYJSE")

formula_vectorclass<-list()
for(i in 1:20)
{
    formula_vectorclass[[i]]<-list()
    formula_vectorclass[[i]][[1]]<-c("YYSR",lrbindex_del[-1])
    formula_vectorclass[[i]][[2]]<-c("YYCB",lrbindex_del[-2])
    formula_vectorclass[[i]][[3]]<-c("SXFY",lrbindex_del[-(4:6)])
    formula_vectorclass[[i]][[4]]<-c("YYSR",zzsbindex_del)
    formula_vectorclass[[i]][[5]]<-c("YYCB",zzsbindex_del)
    formula_vectorclass[[i]][[6]]<-c("SXFY",zzsbindex_del)
    formula_vectorclass[[i]][[7]]<-c("YYSR",lrbindex_del[-1],zzsbindex_del)
    formula_vectorclass[[i]][[8]]<-c("YYCB",lrbindex_del[-2],zzsbindex_del)
    formula_vectorclass[[i]][[9]]<-c("SXFY",lrbindex_del[-(4:6)],zzsbindex_del)
    formula_vectorclass[[i]][[10]]<-c("XXSE",lrbindex_del,zzsbindex_del[-2])
    formula_vectorclass[[i]][[11]]<-c("SJDKSE",lrbindex_del,zzsbindex_del[-6])
    formula_vectorclass[[i]][[12]]<-c("XXSE",lrbindex_del)
    formula_vectorclass[[i]][[13]]<-c("SJDKSE",lrbindex_del)
    formula_vectorclass[[i]][[14]]<-c("XXSE",zzsbindex_del[-2])
    formula_vectorclass[[i]][[15]]<-c("SJDKSE",zzsbindex_del[-6])
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
#数理统计模型计算
#sltj_alltables(hydmset[[7]],"alasso",names(hydmset)[7])
for(i in 1:length(hydmset))
{
    sltj_alltables(hydmset[[i]],"step",names(hydmset)[i])
}
for(i in 1:length(hydmset))
{
    sltj_alltables(hydmset[[i]],"alasso",names(hydmset)[i])
}
#神经网络模型计算
#■■需要根据需要设置建模结果的存储路径■■
#sjwl_alllabels(hydmset[[6]][[1]],"step",names(hydmset)[6])
for(i in 1:length(hydmset))
{
    sjwl_alllabels(hydmset[[i]][[1]],"step",names(hydmset)[i])
}
for(i in 1:length(hydmset))
{
    sjwl_alllabels(hydmset[[i]][[1]],"alasso",names(hydmset)[i])
}
#综合评估计算。
zhpg_alltables("step")
zhpg_alltables("alasso")
#
#在只有二表的企业结果中，随机生成疑点科目。5个变量随机选择3个或者4个。
twojg_step<-read.table(paste(path,Date,city,"_model","\\",names_dir[2],"step","模型组提供给数据组表结果",sep=""),sep="\t",header = TRUE)
twojg_alasso<-read.table(paste(path,Date,city,"_model","\\",names_dir[2],"alasso","模型组提供给数据组表结果",sep=""),sep="\t",header = TRUE)
data_ydjg<-read.table(paste(path,Date,city,"_model","\\",names_dir[3],"data_ydjg",sep=""),sep="\t",header = TRUE,as.is=TRUE)
twojg_yd<-merge(twojg_step,data_ydjg[c("NSRDM","ydkm")],by.x="纳税人代码",by.y="NSRDM",all.x=TRUE,all.y=TRUE)
names_yd<-c("营业收入","营业成本","三项费用合计(销售费用、管理费用、财务费用)","应税货物销售额","进项税额")
for(i in 1:length(twojg_yd[,1]))
{
    if(is.na(twojg_yd$ydkm[i])==TRUE)
    {
        y<-sample(1:5,sample(3:4,1,prob=c(2/3,1/3)))
		twojg_yd$ydkm[i]<-paste(names_yd[y],collapse=",")
    }
}
names(twojg_yd)[names(twojg_yd)=="ydkm"]<-"偏离行业经营状态部分疑点会计科目"
twojg_step<-merge(twojg_step,twojg_yd[c("纳税人代码","偏离行业经营状态部分疑点会计科目")],by.x="纳税人代码",by.y="纳税人代码",all.x=TRUE,all.y=TRUE)
twojg_alasso<-merge(twojg_alasso,twojg_yd[c("纳税人代码","偏离行业经营状态部分疑点会计科目")],by.x="纳税人代码",by.y="纳税人代码",all.x=TRUE,all.y=TRUE)
#将生成疑点科目的结果保存到工作空间。
write.table(twojg_step,paste(path,Date,city,"_model","\\",names_dir[2],"step","模型组提供给数据组表结果",sep=""),sep="\t",row.names=F)
write.table(twojg_alasso,paste(path,Date,city,"_model","\\",names_dir[2],"alasso","模型组提供给数据组表结果",sep=""),sep="\t",row.names=F)
#合并二三四表结果。
tables234_generate("step")
tables234_generate("alasso")





