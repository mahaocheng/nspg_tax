#纳税评估各种运算模型函数集合.R
#定义一个函数，命名为sltj_alltables,用于数理统计模型计算，包括三个参数，第一个是hydmsubset,代表行业代码集，
#参数以列表形式输入，列表的第一个组件是行业相对大类代码集合，后面的组件为行业相对小类的行业代码集合。
#第二个参数是method_featureselection，代表特征选择方法，可以输入"step"或者"alasso"
#name_hydalei,代表行业相对大类名称，用于最后输出结果数据集的命名。
sltj_alltables<-function(hydm_subset,method_featureselection,name_hydalei)
{
#hydm_subset<-hydmset[[2]]
hydata<-list()
hydatatotal<-hydata

for (i in 1:length(hydm_subset))
{
   hydata[[i]]<-subset(lzdata,lzdata$HY_DM%in%hydm_subset[[i]])#储存样本数据
   hydatatotal[[i]]<-subset(shdata,shdata$HY_DM%in%hydm_subset[[i]])#储存总体数据
}
#计算数据集组件数目
count<-length(hydm_subset)
#进行变量选择，调用变量选择函数featherselection，变量选择结果保存在formulastep中。
formulastep<-featherselection(hydatatotal,method_featureselection,count)
#定义列表,存储lm类，即模型结果。
jglm<-list()
for(i in 1:count)
{
    jglm[[i]]<-list()
}
#建模并保存建模结果，调用number函数，计算循环次数；调用model函数，进行自选样计算。
for(i in 1:count)
{
    {
        for(j in 1:modelnum)
		{
			jglm[[i]][[j]]<-model(hydata[[i]],number(hydata[[i]]),formulastep[[i]][[j]])
		}
    }
}
#定义hypredata列表，用来分行业储存预测结果
hypredata<-hydatatotal
sltj_gsznum<-paste("sltj_gsz",1:modelnum,sep="")#定义字符串向量，用来储存数理统计模型的估算值。
#对count个数据集中的modelnum个经济模型进行循环，将预测结果保存在列表hypredata中
for(i in 1:count)
{
    if(length(hydatatotal[[i]][,1])>20)
	for(j in 1:modelnum)
    {
        hypredata[[i]][,sltj_gsznum[j]]<- abs(predict(jglm[[i]][[j]],hydatatotal[[i]]))
    }
}
#定义数据框hyjgdata,用于储存合并行业相对大小类的数据结果，并计算最终税款差额。
hyjgdata<-data.frame()
hypredata_hbxiaolei<-data.frame()#定义数据框hypredata_hbxiaolei,先将行业相对小类的数据进行合并。
for (i in 2:count)
{
    hypredata_hbxiaolei<-rbind(hypredata_hbxiaolei,hypredata[[i]])
}
#将行业相对大类结果与行业相对小类结果进行合并，保存在hyjgdata中。
hyjgdata<-merge(hypredata[[1]],hypredata_hbxiaolei[,c("NSRDM",sltj_gsznum)],by.x="NSRDM",by.y="NSRDM",all.y=TRUE,all.x=FALSE)
#计算税款差额，6个增值税，6个所得税，其中二表模型15个经济模型，税款差额计算公式通过if语句控制。
hyjgdata$sd1<-(hyjgdata$sltj_gsz1.x-hyjgdata$sltj_gsz2.x-hyjgdata$sltj_gsz3.x)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd2<-(hyjgdata$sltj_gsz1.y-hyjgdata$sltj_gsz2.y-hyjgdata$sltj_gsz3.y)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd3<-(hyjgdata$sltj_gsz4.x-hyjgdata$sltj_gsz5.x-hyjgdata$sltj_gsz6.x)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd4<-(hyjgdata$sltj_gsz4.y-hyjgdata$sltj_gsz5.y-hyjgdata$sltj_gsz6.y)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd5<-(hyjgdata$sltj_gsz7.x-hyjgdata$sltj_gsz8.x-hyjgdata$sltj_gsz9.x)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd6<-(hyjgdata$sltj_gsz7.y-hyjgdata$sltj_gsz8.y-hyjgdata$sltj_gsz9.y)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$zz1<-(hyjgdata$sltj_gsz10.x-hyjgdata$sltj_gsz11.x)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
hyjgdata$zz2<-(hyjgdata$sltj_gsz10.y-hyjgdata$sltj_gsz11.y)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
hyjgdata$zz3<-(hyjgdata$sltj_gsz12.x-hyjgdata$sltj_gsz13.x)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
hyjgdata$zz4<-(hyjgdata$sltj_gsz12.y-hyjgdata$sltj_gsz13.y)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
if(modelnum==15)
{
hyjgdata$zz5<-(hyjgdata$sltj_gsz14.x-hyjgdata$sltj_gsz15.x)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
hyjgdata$zz6<-(hyjgdata$sltj_gsz14.y-hyjgdata$sltj_gsz15.y)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
}
if(modelnum!=15)
{
hyjgdata$zz5<-hyjgdata$sltj_gsz14.x-hyjgdata$YNSE.ZZ
hyjgdata$zz6<-hyjgdata$sltj_gsz14.y-hyjgdata$YNSE.ZZ
}
#将结果保存在工作空间中。
write.table(hyjgdata,paste(name_hydalei,method_featureselection,"数理统计结果",sep=""),sep="\t")
}

#--------------------------------------------------------------------------

#定义计算神经网络模型的函数，适用于二表、三表、四表模型。函数包含三个参数，hydmset_onedalei代表一个行业相对大类的数据集合，输入全局变量hydmset[[i]][[1]]
#method_featureselection用来代表特征选择方法，可以输入"step"或者"alasso",name代表行业相对大类的命名，使用时输入names(hydmset)[i]。
sjwl_alllabels<-function(hydmset_onedalei,method_featureselection,name)
{
data_onedalei<-subset(shdata,shdata$HY_DM%in%hydmset_onedalei)#将行业相对大类的数据保存在data_onedalei中。
#由于神经网络只计算行业相对大类，因此与数理统计模型中对应的count=1
count<-1
formulastep<-featherselection(list(data_onedalei),method_featureselection,count)#为了与数理统计统一，这里输入要求为列表数据，因此在data_onedalei前添加list函数。
#调用generate_samplevariable生成样本变量函数，添加新变量A~K
data_onedalei<-generate_samplevariable(data_onedalei)
ma<-data_onedalei[,c("NSRDM","A","B","C","D","E","F","G","H","I","J","K")]#为了方便后续计算,定义一个较短的数据集ma。只取data_onedalei中的NSRDM,A~K等变量
#按照公式计算p1~p8
ma$p1<-ma$A/ma$B;ma$p2<-ma$C/ma$A;ma$p3<-ma$D/ma$A;ma$p4<-ma$E/ma$A;ma$p5<-ma$F/ma$G;ma$p6<-ma$H/ma$I;ma$p7<-ma$J/ma$K;ma$p8<-ma$A/200000
#按对数值分规模。
sz_end<-log10(max(ma$p8))
sz_start<-0
sz_itvl<-(sz_end-sz_start)/5
sz_fenge1<-sz_start+sz_itvl
sz_fenge2<-sz_start+2*sz_itvl
sz_fenge3<-sz_start+3*sz_itvl
sz_fenge4<-sz_start+4*sz_itvl
cbzb_1<-ma[ma$p8<10^sz_fenge1,]
cbzb_2<-ma[ma$p8>=10^sz_fenge1&ma$p8<10^sz_fenge2,]
cbzb_3<-ma[ma$p8>=10^sz_fenge2&ma$p8<10^sz_fenge3,]
cbzb_4<-ma[ma$p8>=10^sz_fenge3&ma$p8<10^sz_fenge4,]
cbzb_5<-ma[ma$p8>=10^sz_fenge4,]
print(c(length(cbzb_1[,1]),length(cbzb_2[,1]),length(cbzb_3[,1]),length(cbzb_4[,1]),length(cbzb_5[,1])))
#■■需要根据需要将5个规模段合并为3段，并保证3段的样本数目尽量接近，分规模后的数据存储在data_afterscale中
data_afterscale<-list(cbzb_1,cbzb_2,rbind(cbzb_3,cbzb_4,cbzb_5))
data_nonzero<-list()#定义列表data_nonzero,用来存储剔除零异常后的数据。
for(i in 1:3)#对三个规模段循环，剔除零异常数据。
{
    data_nonzero[[i]]<-data.frame()
	for(j in 1:length(data_afterscale[[i]][,1]))
    {   
        count_zero<-0
        for(k in 2:12)
		{
	    if(data_afterscale[[i]][j,k]==0||is.na(data_afterscale[[i]][j,k])==TRUE) {count_zero<-count_zero+1}
		}
		if(count_zero==0){data_nonzero[[i]]<-rbind(data_nonzero[[i]],data_afterscale[[i]][j,])}
    }
}
#定义两个列表data_normal,data_abnormal分别用来存储正常数据和离群点数据。
data_normal<-list()
data_abnormal<-list()
for(i in 1:3)#对三个规模段循环，找出离群点数据与正常数据分别保存。
{
    data_normal[[i]]<-data.frame()
	data_abnormal[[i]]<-data.frame()
	pnum<-paste("p",1:7,sep="")
	range5<-t(as.matrix(apply(data_nonzero[[i]][pnum],2,quantile,probs=c(0.05,0.95),type=3)))
    minnormal<-range5[,1]-1.5*(range5[,2]-range5[,1])
    maxnormal<-range5[,2]+1.5*(range5[,2]-range5[,1])
    rangenormal<-cbind(minnormal,maxnormal)
    for(j in 1:length(data_nonzero[[i]][,1]))
    {   
        m<-0
        for(k in 13:19)
		{
			if(data_nonzero[[i]][j,k]<rangenormal[k-12,1]||data_nonzero[[i]][j,k]>rangenormal[k-12,2]) {m<-m+1}
		}
		if(m>0){data_abnormal[[i]]<-rbind(data_abnormal[[i]],data_nonzero[[i]][j,])}else {
		data_normal[[i]]<-rbind(data_normal[[i]],data_nonzero[[i]][j,])}
    }
}
#定义列表，data_firstbox，data_emgrp，分别用来存储第一次分盒与第二次分盒的数据。
data_firstbox<-list()
data_emgrp<-list()
for(i in 1:3)#对三个规模段循环，进行第一次分盒
{   
    num<-length(data_normal[[i]]$NSRDM)+length(data_abnormal[[i]]$NSRDM)#计算该规模段中正常企业与离群点企业数目合计值，储存在变量num中。
    if(num<100) next#如果num小于100,则退出后续计算
    d1<-ceiling(num/150)#定义分盒的盒子数目，d2为最终的分盒数目。
    if(d1>5) {d1<-5}
	d2<-d1+1
    data_firstbox[[i]]<-list()#指定data_firstbox[[i]]为一个列表，它的组件用来存储该规模段中，第一次分盒后d2个盒子的数据。
    for(m in 1:d2)#指定data_firstbox[[i]][[m]]为数据框格式。
    {data_firstbox[[i]][[m]]<-data.frame()}
    data_emgrp[[i]]<-list()#指定data_emgrp[[i]]为一个列表，它的组件用来存储该规模段中，第二次分盒后d2个盒子的数据。   
    for(m in 1:d2)
    {data_emgrp[[i]][[m]]<-data.frame()}#指定data_emgrp[[i]][[m]]为数据框格式。
    pca<-princomp(~p1+p2+p3+p4+p5+p6+p7,data=data_normal[[i]])#调用princomp，进行主成分计算。
    load<-pca$load
    pcabox<-as.matrix(data_normal[[i]][,13:19])%*%as.matrix(load)
    center<-vector();itvl<-vector()#定义中心向量center，和间隔向量itvl。
    for(m in 1:7)#共七维主成分，因此从1循环到7，分别计算每个维度的center和itvl。
    {
        center[m]<-(max(pcabox[,m])+min(pcabox[,m]))/2
		itvl[m]<-(max(pcabox[,m])-min(pcabox[,m]))/(2*d1)
    }
    for(j in 1:length(data_normal[[i]][,1]))#对正常企业进行循环，分配到1到d1个盒子里。
    {   
        range<-matrix(,1,7)	
        for(k in 1:7)
		{
			range[k]<-ceiling(abs(pcabox[j,k]-center[k])/itvl[k])
			if(range[k]==0) range[k]<-range[k]+1
		}
        data_firstbox[[i]][[max(range)]]<-rbind(data_firstbox[[i]][[max(range)]],data_normal[[i]][j,])
    }
    data_firstbox[[i]][[d2]]<-rbind(data_firstbox[[i]][[d2]],data_abnormal[[i]])##离群点数据直接放到第d2个盒子中。
    data_grp<-data_firstbox#通过随机抽样的方式，进行第二次分盒。
    for(j in 1:d2)
    {
        t<-length(data_grp[[i]][[j]]$NSRDM)
		iteration<-0
		while (t>0)
		{
			iteration<-iteration+1
			tag<-round(t*runif(1))
			if(tag==0) tag<-1
			q<-iteration-floor(iteration/d2)*d2
			if(q==0)q<-d2
			data_emgrp[[i]][[q]]<-rbind(data_emgrp[[i]][[q]],data_grp[[i]][[j]][tag,])
			data_grp[[i]][[j]]<-data_grp[[i]][[j]][-tag,]
			t<-length(data_grp[[i]][[j]][,1])
		}
    }
}
for(i in 1:3)#对每个规模段进行循环，如果该规模段num小于100，不进行分盒
{   
    num<-length(data_normal[[i]]$NSRDM)+length(data_abnormal[[i]]$NSRDM)
    if(num<100)
    next
    d2<-length(data_emgrp[[i]])
    for(j in 1:d2)#对每个规模段的每个第二次分盒后的盒子的数据，定义变量gailv，初值为0
    {
    data_emgrp[[i]][[j]]$gailv<-0
    }
}
for(i in 1:3)#对每个规模段进行循环，通过em算法进行高斯混合模型计算。
{
    num<-length(data_normal[[i]]$NSRDM)+length(data_abnormal[[i]]$NSRDM)
    if(num<100) next#同样的如果该规模段的企业数目小于100，则不选样，后续直接进行回归计算。
    d2<-length(data_emgrp[[i]])#对第二次分盒后的每个盒子的数据分别进行em计算。
    for(j in 1:d2)
    {
        newemgrp<-data_emgrp[[i]][[j]]#通过计算企业两两间欧式距离，将企业预先分为两组，然后计算每组的均值，协方差，赋先验概率值。
		num<-length(newemgrp[,1])
		dis<-matrix(0,num,num)
		for(l in 1:(num-1))
		{
			for(k in (l+1):num)
			{
				dis[l,k]<-sqrt(sum((newemgrp[l,13:19]-newemgrp[k,13:19])^2))
			}
		}
		whichmax<-which.max(dis)
		maxcol<-floor(whichmax/num)+1
		maxrow<-whichmax-floor(whichmax/num)*num
		newemgrp$distance<-0
		for(h in 1:num)
		{
			newemgrp$distance[h]<-sqrt(sum((newemgrp[h,13:19]-newemgrp[maxcol,13:19])^2))-sqrt(sum((newemgrp[h,13:19]-newemgrp[maxrow,13:19])^2))    
		}
		o<-order(newemgrp$distance)
		newemgrp<-newemgrp[o,]
		newemgrp$level<-c("a")
		newemgrp$level[1:round(length(newemgrp[,1]))/2]<-c("b")
		z<-unmap(newemgrp[,23]);newemgrp[1,16]<-newemgrp[1,16]-0.001#调用em算法
		msEst <- mstep(modelName = "EEE", data = newemgrp[,13:19], z = z)
		emjg<-em(modelName = msEst$modelName, data = newemgrp[,13:19],parameters = msEst$parameters)
		if(length(emjg$z[emjg$z[,1]<emjg$z[,2],])>=length(emjg$z[emjg$z[,1]>emjg$z[,2],])) data_emgrp[[i]][[j]]$gailv<-emjg$z[,2]
		if(length(emjg$z[emjg$z[,1]<emjg$z[,2],])<length(emjg$z[emjg$z[,1]>emjg$z[,2],])) data_emgrp[[i]][[j]]$gailv<-emjg$z[,1]
    }
}
#定义列表data_allsvm，data_samsvm，分别储存全部数据和样本数据。
data_allsvm<-list()
data_samsvm<-list()
for(i in 1:3)#对每个规模段进行循环计算。
{
    num<-length(data_normal[[i]]$NSRDM)+length(data_abnormal[[i]]$NSRDM)
    if(num<100)#如果该规模段企业数目小于100，则样本数据等于全部数据。
    {
        data_allsvm[[i]]<-data_onedalei[data_onedalei$NSRDM%in%data_afterscale[[i]]$NSRDM,]
        data_samsvm[[i]]<-data_allsvm[[i]][data_allsvm[[i]]$NSRDM%in%lzdata$NSRDM,]
        next 
    }
    d2<-length(data_emgrp[[i]])
    data_allsvm[[i]]<-data_onedalei[data_onedalei$NSRDM%in%data_afterscale[[i]]$NSRDM,]
    sample<-data.frame()
    for(j in 1:d2)
    {
	sample<-rbind(sample,data_emgrp[[i]][[j]])
    }
    nsrdm_subset<-sample[sample$gailv>0.5&is.na(sample$gailv)==FALSE,]$NSRDM
    data_samsvm[[i]]<-data_onedalei[data_onedalei$NSRDM%in%nsrdm_subset,]
    data_samsvm[[i]]<-data_samsvm[[i]][data_samsvm[[i]]$NSRDM%in%lzdata$NSRDM,]
}


for(i in 1:3)#对每个规模段进行svm回归计算。预测值保存在gsznum中。
{
	gsznum<-paste("gsz",1:modelnum,sep="")
	data_allsvm[[i]][,gsznum]<-0
	for (j in 1:modelnum)
	{
		innum<-as.matrix(data_samsvm[[i]][,formulastep[[1]][[j]][-1]])
		outnum<-data_samsvm[[i]][,formulastep[[1]][[j]][1]]
		regmnum<-ksvm(innum,outnum,epsilon=0.01,kernel ="rbfdot",cross=3)
		prenum<-as.matrix(data_allsvm[[i]][,formulastep[[1]][[j]][-1]])
		data_allsvm[[i]][,gsznum[j]]<-as.numeric(predict(regmnum,prenum))
	}
}
hyjgdata<-rbind(data_allsvm[[1]],data_allsvm[[2]],data_allsvm[[3]])#将三个规模段的总体数据合并保存在hyjgdata中。
#计算税款差额，二表模型中包含15个经济模型，计算公式不同，通过if语句控制。
hyjgdata$sd1<-(hyjgdata$gsz1-hyjgdata$gsz2-hyjgdata$gsz3)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd2<-(hyjgdata$gsz4-hyjgdata$gsz5-hyjgdata$gsz6)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$sd3<-(hyjgdata$gsz7-hyjgdata$gsz8-hyjgdata$gsz9)*0.25-(hyjgdata$YYSR-hyjgdata$YYCB-hyjgdata$SXFY)*0.25
hyjgdata$zz1<-(hyjgdata$gsz10-hyjgdata$gsz11)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
hyjgdata$zz2<-(hyjgdata$gsz12-hyjgdata$gsz13)-(hyjgdata$XXSE-hyjgdata$SJDKSE)
if(modelnum==15){hyjgdata$zz3<-(hyjgdata$gsz14-hyjgdata$gsz15)-(hyjgdata$XXSE-hyjgdata$SJDKSE)}else{
hyjgdata$zz3<-hyjgdata$gsz14-hyjgdata$YNSE.ZZ}
#将计算结果保存在工作空间中。这里的name和method_featureselection为sjwl_alllabels函数的三个输入变量之二。
write.table(hyjgdata,paste(name,method_featureselection,"神经网络结果",sep=""),sep="\t")
}
#---------------------------------------------------------------------------------------
#定义疑点科目计算函数，ydkm_calculate，该函数只有一个参数，hydmset_xiaolei代表行业相对小类行业代码集合。
ydkm_calculate<-function(hydmset_xiaolei)
{
oridata<-subset(shdata,shdata$HY_DM%in%hydmset_xiaolei)#将该行业相对小类的数据储存在oridata中。
#定义sd_robust，进行稳健性标准差计算。
sd_robust<-function(data_vector)
{
    limit_quantile<-quantile(data_vector,probs=c(0.2,0.9))
    sd_robust<-sd(data_vector[data_vector>limit_quantile[1]&data_vector<limit_quantile[2]])
    sd_robust
}
pnum<-paste("p",1:14,sep="")#定义变量名称p1~p14，用于存储14个疑点会计科目相关的比率
resultnum<-paste("result",1:14,sep="")#定义变量名称result1~result14，用于标识该科目是否为疑点，1代表疑点，0代表不是疑点。
#计算疑点会计科目相关的比率
oridata$p1<-oridata$YYSR*2/(oridata$ZCHJ.x+oridata$ZCHJ.y)
oridata$p2<-oridata$SXFY/oridata$YYCB
oridata$p3<-oridata$SXFY/oridata$YYSR
oridata$p4<-(oridata$YYSR-oridata$YYCB)/oridata$YYSR
oridata$p5<-oridata$XXSE/oridata$SYSL_YSHW_XSE
oridata$p6<-oridata$JXSE/oridata$SYSL_YSHW_XSE
oridata$p7<-oridata$SJDKSE/oridata$JXSE
oridata$p8<-(oridata$YSZK.xy+oridata$QTYSK.xy+oridata$YUFZK.xy)/(oridata$YSZK.x+oridata$QTYSK.x+oridata$YUFZK.x)
oridata$p9<-(oridata$LDZCHJ.y-oridata$CH.y)/oridata$LDFZHJ.y
oridata$p10<-oridata$HBZJ.xy/oridata$HBZJ.x
oridata$p11<-oridata$YYSR*2/(oridata$LDZCHJ.x+oridata$LDZCHJ.y)
oridata$p12<-oridata$GDZC.xy/oridata$GDZC.x
oridata$p13<-oridata$YYSR*2/(oridata$ZCHJ.x+oridata$ZCHJ.y)
oridata$p14<-(oridata$YFZK.xy+oridata$QTYFK.xy+oridata$YUSZK.xy)/(oridata$YFZK.x+oridata$QTYFK.x+oridata$YUSZK.x)

for(i in 1:length(pnum))
{
oridata[which(is.na(oridata[,pnum[i]])==TRUE),pnum[i]]<-0
}
oridata[,resultnum]<-0
meanresult<-apply(oridata[,pnum],2,median,na.rm = FALSE)#计算p1~p14,每个变量的中位数。
sdresult<-apply(oridata[,pnum],2,sd_robust)#计算p1~p14,每个变量的稳健性标准差。
warning<-qnorm(1-12*0.01)#按照12%异常水平，计算预警上限。
for(i in 1:length(oridata[,1]))#对每个企业的每个疑点科目，分别计算其是否为疑点。
{
    for(j in 1:14)
    {
        value_sd<-as.numeric((oridata[,pnum[j]][i]-meanresult[j])/sdresult[j])
		if(abs(value_sd)>warning|is.na(value_sd)==TRUE) oridata[,resultnum[j]][i]<-1		
	}
}
#4个强制变量随机选择3个与全部指标计算得到的疑点科目取并集，这样，4个强制变量有可能全部进入疑点科目。这四个强制变量分别为："三项费用合计(销售费用、管理费用、财务费用)","存货","货币资金","应付合计(应付账款、其他应付款、预收账款)"
for(i in 1:length(oridata[,1]))
{ 
    qiangzhi<-sample(c(3,9,10,14),3)    
    oridata[i,resultnum][qiangzhi]<-1
}
oridata$sum_ydnum<-apply(oridata[,resultnum],1,sum)#加入一列，命名为sum_ydnum，用于统计每个企业的疑点数目。
#■■需要根据需要设置疑点会计科目的中文名称,避免展示的时候，出现的疑点科目所在的表不存在■■
chnames_ydkm<-c("营业收入","营业成本","三项费用合计(销售费用、管理费用、财务费用)","应税货物销售额","销项税额","进项税额","实际抵扣税额",
"应收合计(应收账款、其他应收款、预付账款)","存货","货币资金","流动资产合计","固定资产","资产合计","应付合计(应付账款、其他应付款、预收账款)")
oridata$ydkm<-0#加入一列，命名为ydkm，用来记录疑点会计科目中文名称，中间用,间隔。
for(i in 1:length(oridata[,1]))
{    
    y<-which(oridata[i,resultnum]==1)
    y<-y[sample(1:length(y),length(y))]
    oridata$ydkm[i]<-paste(chnames_ydkm[y], collapse= ",")    
}
oridata#oridata作为函数ydkm_calculate的输出。
}
#定义一个函数，regression_kf，用于卡方法计算。该函数只有一个输入参数，hydmset_onedalei代表一个行业相对大类行业代码集合。
regression_kf<-function(hydmset_onedalei)
{
    data_total_kf<-shdata[shdata$HY_DM%in%hydmset_onedalei,]#保存该相对大类的总体数据
	data_sample_kf<-lzdata[lzdata$HY_DM%in%hydmset_onedalei,]#保存该相对大类的样本数据
	svalue_quantile<-quantile(data_sample_kf$s,prob=c(0.05,0.35))#提取s值的5%与35%分位数。
	data_finalsample<-data_sample_kf[data_sample_kf$s>svalue_quantile[1]&data_sample_kf$s<svalue_quantile[2],]#s值的5%与35%分位数之间的数据作为样本，最终样本保存在data_finalsample中。
	formula_kf<-list()#定义卡方法模型公式。储存在列表formula_kf中。
	formula_kf[[1]]<-YNSE.SD~YYSR+YYCB+SXFY
	formula_kf[[2]]<-YNSE.SD~HBZJ.xy+CH.xy+YSZK.xy+QTYSK.xy+LDZCHJ.xy
	formula_kf[[3]]<-YNSE.SD~YFZK.xy+QTYFK.xy+LDFZHJ.xy+SYZQYHJ.xy
	formula_kf[[4]]<-YNSE.ZZ~YYSR+YYCB+SXFY
	formula_kf[[5]]<-YNSE.ZZ~HBZJ.xy+CH.xy+YSZK.xy+QTYSK.xy+LDZCHJ.xy
	formula_kf[[6]]<-YNSE.ZZ~YFZK.xy+QTYFK.xy+LDFZHJ.xy+SYZQYHJ.xy
	lmjg_kf<-list()#定义列表lmjg_kf用来存储卡方法模型的回归结果。
	for(i in 1:6)
	{
	    lmjg_kf[[i]]<-lm(formula_kf[[i]],data=data_finalsample)
	}
	kfjg_predict_num<-paste("kfjg_predict",1:6,sep="")#定义kfjg_predict1~kfjg_predict6,用来储存卡方法模型的预测结果。
	kfjg_chae_num<-paste("kfjg_chae",1:6,sep="")#定义kfjg_chae1~kfjg_chae6用来储存卡方法模型的税款差额。
	data_total_kf[,kfjg_predict_num]<-0
	for(i in 1:6)
	{
	    data_total_kf[,kfjg_predict_num[i]]<-predict(lmjg_kf[[i]],data_total_kf)#对全部数据进行预测。
		data_total_kf[,kfjg_chae_num[i]]<-data_total_kf[,kfjg_predict_num[i]]-data_total_kf[,names(lmjg_kf[[i]]$model)[1]]
	}
	data_total_kf
}
#-------------------------------------------------------------------------------------------------


#定义函数zhpg_alltables,用于计算二表、三表、四表的综合评估结果。该函数只有一个参数，method_featureselection代表特征选择方法。可以输入"step"或者"alasso"。
zhpg_alltables<-function(method_featureselection)
{
jgdata_sltj<-data.frame()#定义数据框jgdata_sltj，用于储存导入的数理统计模型结果。
jgdata_sjwl<-data.frame()#定义数据框jgdata_sjwl，用于储存导入的神经网络模型结果。
for(i in 1:length(hydmset))#导入工作空间中的数理统计、神经网络模型结果，method_featureselection用于指定"step"或者"alasso",导入时调用unit_10000函数，用于将所有数值处理为以万元为单位。
{
    jgdata_sltj<-rbind(jgdata_sltj,unit_10000(read.table(paste(names(hydmset)[i],method_featureselection,"数理统计结果",sep=""))))	
	jgdata_sjwl<-rbind(jgdata_sjwl,unit_10000(read.table(paste(names(hydmset)[i],method_featureselection,"神经网络结果",sep=""))))
}
#定义一系列变量sd1~sd6;zz1~zz6;sltjjg1~sltjjg12;sjwljg1~sjwljg6。
sdnum<-paste("sd",1:6,sep="");zznum<-paste("zz",1:6,sep="");sltjjgnum<-paste("sltjjg",1:12,sep="");sjwljgnum<-paste("sjwljg",1:6,sep="")
jgdata_sltj[,sltjjgnum]<-jgdata_sltj[,c(sdnum,zznum)]#将数理统计的6个增值税结果、6个所得税结果复制给sltjjg1~sltjjg12，前6是增值税、后6是所得税。
jgdata_sjwl[,sjwljgnum]<-jgdata_sjwl[,c(sdnum[1:3],zznum[1:3])]#将神经网络的3个增值税结果、3个所得税结果复制给sjwljg1~sjwljg6，前3是增值税、后3是所得税。
names_sltjjg<-names(jgdata_sltj)[-1]#提取数据集jgdata_sltj的名称。这样合并jgdata_sltj和jgdata_sjwl的时候重名字段不再重复合并。
jgdata<-merge(jgdata_sltj,jgdata_sjwl[!(names(jgdata_sjwl)%in%names_sltjjg)],by.x="NSRDM",by.y="NSRDM",all.x=TRUE,all.y=TRUE)
#定义一些新的变量，用来存储综合评估结果。
jgdata[,c("zz_sltj_lower","zz_sltj_upper","zz_sltj_pgz_lower","zz_sltj_pgz_upper","zz_sjwl_lower","zz_sjwl_upper","zz_sjwl_pgz_lower","zz_sjwl_pgz_upper","zz_two_lower","zz_two_upper","zz_two_pgz_lower","zz_two_pgz_upper",
"sd_sltj_lower","sd_sltj_upper","sd_sltj_pgz_lower","sd_sltj_pgz_upper","sd_sjwl_lower","sd_sjwl_upper","sd_sjwl_pgz_lower","sd_sjwl_pgz_upper","sd_two_lower","sd_two_upper","sd_two_pgz_lower","sd_two_pgz_upper")]<-0
#模型的原始结果需要调用negative函数，实现正值*0.7，负值取绝对值*0.3，然后+1万操作，之后调用interval_generate函数，生成综合评估区间。
for(i in 1:length(jgdata[,1]))
{
    jgdata[i,c("sd_sltj_lower","sd_sltj_upper","sd_sltj_pgz_lower","sd_sltj_pgz_upper")]<-interval_generate(negative(jgdata[i,sltjjgnum[1:6]]))
	jgdata[i,c("sd_sjwl_lower","sd_sjwl_upper","sd_sjwl_pgz_lower","sd_sjwl_pgz_upper")]<-interval_generate(negative(jgdata[i,sjwljgnum[1:3]]))
	jgdata[i,c("sd_two_lower","sd_two_upper","sd_two_pgz_lower","sd_two_pgz_upper")]<-interval_generate(jgdata[i,c("sd_sltj_pgz_lower","sd_sltj_pgz_upper","sd_sjwl_pgz_lower","sd_sjwl_pgz_upper")])
    jgdata[i,c("zz_sltj_lower","zz_sltj_upper","zz_sltj_pgz_lower","zz_sltj_pgz_upper")]<-interval_generate(negative(jgdata[i,sltjjgnum[7:12]]))
	jgdata[i,c("zz_sjwl_lower","zz_sjwl_upper","zz_sjwl_pgz_lower","zz_sjwl_pgz_upper")]<-interval_generate(negative(jgdata[i,sjwljgnum[4:6]]))
	jgdata[i,c("zz_two_lower","zz_two_upper","zz_two_pgz_lower","zz_two_pgz_upper")]<-interval_generate(jgdata[i,c("zz_sltj_pgz_lower","zz_sltj_pgz_upper","zz_sjwl_pgz_lower","zz_sjwl_pgz_upper")])
}
#如果modelnum=14，那么代表是三表、四表模型，需要加入疑点科目结果和卡方法结果。使用merge函数合并。
if(modelnum==14){
    data_ydjg<-read.table("data_ydjg",sep="\t")
	data_kfjg<-read.table("卡方法结果",sep="\t")
	jgdata<-merge(jgdata,data_ydjg[c("NSRDM","ydkm")],by.x="NSRDM",by.y="NSRDM",all.x=TRUE,all.y=TRUE)
	jgdata<-merge(jgdata,data_kfjg[c("NSRDM","sd_kf_pgz_lower","sd_kf_pgz_upper","zz_kf_pgz_lower","zz_kf_pgz_upper")],by.x="NSRDM",by.y="NSRDM",all.x=TRUE,all.y=TRUE)
	}
#interval_generate(c(1:3,6,100,102))
#定义生成所得税表结果函数，对于三表、四表模型，需要增加疑点科目字段。
createtable_sdsb<-function(data_onexiaolei)
{
    sdsb_xiaolei<-data_onexiaolei[,c("NSRDM","NSRMC","HY_DM","sltjjg1","sltj_gsz1.x", "sltj_gsz2.x", "sltj_gsz3.x","sltjjg2","sltj_gsz1.y", "sltj_gsz2.y", "sltj_gsz3.y",
    "sltjjg3","sltj_gsz4.x", "sltj_gsz5.x", "sltj_gsz6.x","sltjjg4","sltj_gsz4.y", "sltj_gsz5.y", "sltj_gsz6.y","sltjjg5","sltj_gsz7.x", "sltj_gsz8.x", "sltj_gsz9.x",
    "sltjjg6","sltj_gsz7.y", "sltj_gsz8.y", "sltj_gsz9.y","sd_sltj_lower","sd_sltj_upper","sd_sltj_pgz_lower","sd_sltj_pgz_upper",
    "sjwljg1","gsz1", "gsz2", "gsz3","sjwljg2","gsz4", "gsz5", "gsz6","sjwljg3","gsz7", "gsz8", "gsz9","sd_sjwl_lower","sd_sjwl_upper",
    "sd_sjwl_pgz_lower","sd_sjwl_pgz_upper","YYSR","SJYNSDSE","sd_two_lower","sd_two_upper","sd_two_pgz_lower","sd_two_pgz_upper")]
	sdsb_xiaolei$hysds_lsv_lower<-sum(sdsb_xiaolei$sd_two_pgz_lower)/(sum(sdsb_xiaolei$SJYNSDSE)+sum(sdsb_xiaolei$sd_two_pgz_upper))
	sdsb_xiaolei$hysds_lsv_upper<-sum(sdsb_xiaolei$sd_two_pgz_upper)/(sum(sdsb_xiaolei$SJYNSDSE)+sum(sdsb_xiaolei$sd_two_pgz_upper))
	names(sdsb_xiaolei)<-c("纳税人代码", "纳税人名称", "企业行业代码", "数理统计1大类所得税偷漏税额", "数理统计1大类营业收入", "数理统计1大类营业成本", "数理统计1大类三项费用", 
	"数理统计1小类所得税偷漏税额", "数理统计1小类营业收入", "数理统计1小类营业成本", "数理统计1小类三项费用", "数理统计2大类所得税偷漏税额", "数理统计2大类营业收入", 
	"数理统计2大类营业成本", "数理统计2大类三项费用", "数理统计2小类所得税偷漏税额", "数理统计2小类营业收入", "数理统计2小类营业成本", "数理统计2小类三项费用", 
	"数理统计3大类所得税偷漏税额", "数理统计3大类营业收入", "数理统计3大类营业成本", "数理统计3大类三项费用", "数理统计3小类所得税偷漏税额", "数理统计3小类营业收入", 
	"数理统计3小类营业成本", "数理统计3小类三项费用", "数理统计评估偷漏税下限_评估值", "数理统计评估偷漏税上限_评估值", "数理统计评估偷漏税下限_调整值", 
	"数理统计评估偷漏税上限_调整值", "神经网络1所得税偷漏税额", "神经网络1营业收入", "神经网络1营业成本", "神经网络1三项费用", "神经网络2所得税偷漏税额", 
	"神经网络2营业收入", "神经网络2营业成本", "神经网络2三项费用", "神经网络3所得税偷漏税额", "神经网络3营业收入", "神经网络3营业成本", 
	"神经网络3三项费用", "神经网络评估偷漏税下限_评估值", "神经网络评估偷漏税上限_评估值", "神经网络评估偷漏税下限_调整值", "神经网络评估偷漏税上限_调整值", 
	"企业申报营业收入", "企业申报实际应纳所得税额", "二类模型评估企业所得税偷漏税额下限_评估值", "二类模型评估企业所得税偷漏税额上限_评估值", "二类模型评估企业所得税偷漏税额下限_补税值", 
	"二类模型评估企业所得税偷漏税额上限_补税值", "行业所得税税收流失率下限", "行业所得税税收流失率上限")
	if(modelnum==14) sdsb_xiaolei$偏离行业经营状态部分疑点会计科目<-data_onexiaolei$ydkm
	sdsb_xiaolei
}
#定义生成增值税表结果函数，三表、四表的增值税结果表需要展示："数理统计6大类应纳税额"，"数理统计6小类应纳税额"，"神经网络6应纳税额"，"偏离行业经营状态部分疑点会计科目"。
#二表的增值税结果表需要展示："数理统计6大类销项税额"，"数理统计6大类实际抵扣税额"，"数理统计6小类销项税额"， "数理统计6小类实际抵扣税额"，"神经网络6销项税额"，"神经网络6实际抵扣税额"。

createtable_zzsb<-function(data_onexiaolei)
{
    if(modelnum==14){
	zzsb_xiaolei<-data_onexiaolei[,c("NSRDM","NSRMC","HY_DM","sltjjg7","sltj_gsz10.x", "sltj_gsz11.x","sltjjg8","sltj_gsz10.y", "sltj_gsz11.y",
	"sltjjg9","sltj_gsz12.x", "sltj_gsz13.x","sltjjg10","sltj_gsz12.y", "sltj_gsz13.y","sltjjg11","sltj_gsz14.x","sltjjg12","sltj_gsz14.y",
	"zz_sltj_lower","zz_sltj_upper","zz_sltj_pgz_lower","zz_sltj_pgz_upper","sjwljg4","gsz10", "gsz11","sjwljg5","gsz12", "gsz13","sjwljg6","gsz14",
	"zz_sjwl_lower", "zz_sjwl_upper", "zz_sjwl_pgz_lower", "zz_sjwl_pgz_upper","XXSE","SJDKSE","YYSR","YNSEHJ","zz_two_lower", "zz_two_upper", 
	"zz_two_pgz_lower", "zz_two_pgz_upper")]
	zzsb_xiaolei$hyzzs_lsv_lower<-sum(zzsb_xiaolei$zz_two_pgz_lower)/(sum(zzsb_xiaolei$YNSEHJ)+sum(zzsb_xiaolei$zz_two_pgz_upper))
	zzsb_xiaolei$hyzzs_lsv_upper<-sum(zzsb_xiaolei$zz_two_pgz_upper)/(sum(zzsb_xiaolei$YNSEHJ)+sum(zzsb_xiaolei$zz_two_pgz_upper))
	zzsb_xiaolei$hy_lsv_lower<-(sum(data_onexiaolei$zz_two_pgz_lower)+sum(data_onexiaolei$sd_two_pgz_lower))/(sum(data_onexiaolei$YNSEHJ)+sum(data_onexiaolei$SJYNSDSE)+sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))
	zzsb_xiaolei$hy_lsv_upper<-(sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))/(sum(data_onexiaolei$YNSEHJ)+sum(data_onexiaolei$SJYNSDSE)+sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))
	zzsb_xiaolei$ydkm<-data_onexiaolei$ydkm
	names(zzsb_xiaolei)<-c("纳税人代码", "纳税人名称", "企业行业代码", "数理统计4大类增值税偷漏税额", 
	"数理统计4大类销项税额", "数理统计4大类实际抵扣税额", "数理统计4小类增值税偷漏税额", "数理统计4小类销项税额", "数理统计4小类实际抵扣税额", "数理统计5大类增值税偷漏税额", 
	"数理统计5大类销项税额", "数理统计5大类实际抵扣税额", "数理统计5小类增值税偷漏税额", "数理统计5小类销项税额", "数理统计5小类实际抵扣税额", "数理统计6大类增值税偷漏税额", 
	"数理统计6大类应纳税额", "数理统计6小类增值税偷漏税额", "数理统计6小类应纳税额", "数理统计评估偷漏税下限_评估值", "数理统计评估偷漏税上限_评估值", 
	"数理统计评估偷漏税下限_调整值", "数理统计评估偷漏税上限_调整值", "神经网络4增值税偷漏税额", "神经网络4销项税额", "神经网络4实际抵扣税额", 
	"神经网络5增值税偷漏税额", "神经网络5销项税额", "神经网络5实际抵扣税额", "神经网络6增值税偷漏税额", "神经网络6应纳税额", "神经网络评估偷漏税下限_评估值", 
	"神经网络评估偷漏税上限_评估值", "神经网络评估偷漏税下限_调整值", "神经网络评估偷漏税上限_调整值", "企业申报年销项税额合计", "企业申报年实际抵扣税额合计", 
	"企业申报营业收入", "企业增值税应纳税额合计年申报值", "二类模型评估企业增值税偷漏税额下限_评估值", "二类模型评估企业增值税偷漏税额上限_评估值", "二类模型评估企业增值税偷漏税额下限_补税值", 
	"二类模型评估企业增值税偷漏税额上限_补税值", "行业增值税税收流失率下限", "行业增值税税收流失率上限", "行业总税收流失率下限", "行业总税收流失率上限", "偏离行业经营状态部分疑点会计科目")
	}
	if(modelnum==15){
	zzsb_xiaolei<-data_onexiaolei[,c("NSRDM","NSRMC","HY_DM","sltjjg7","sltj_gsz10.x", "sltj_gsz11.x","sltjjg8","sltj_gsz10.y", "sltj_gsz11.y",
	"sltjjg9","sltj_gsz12.x", "sltj_gsz13.x","sltjjg10","sltj_gsz12.y", "sltj_gsz13.y","sltjjg11","sltj_gsz14.x","sltj_gsz15.x","sltjjg12","sltj_gsz14.y","sltj_gsz15.y",
	"zz_sltj_lower","zz_sltj_upper","zz_sltj_pgz_lower","zz_sltj_pgz_upper","sjwljg4","gsz10", "gsz11","sjwljg5","gsz12", "gsz13","sjwljg6","gsz14","gsz15",
	"zz_sjwl_lower", "zz_sjwl_upper", "zz_sjwl_pgz_lower", "zz_sjwl_pgz_upper","XXSE","SJDKSE","YYSR","YNSEHJ","zz_two_lower", "zz_two_upper", 
	"zz_two_pgz_lower", "zz_two_pgz_upper")]
	zzsb_xiaolei$hyzzs_lsv_lower<-sum(zzsb_xiaolei$zz_two_pgz_lower)/(sum(zzsb_xiaolei$YNSEHJ)+sum(zzsb_xiaolei$zz_two_pgz_upper))
	zzsb_xiaolei$hyzzs_lsv_upper<-sum(zzsb_xiaolei$zz_two_pgz_upper)/(sum(zzsb_xiaolei$YNSEHJ)+sum(zzsb_xiaolei$zz_two_pgz_upper))
	zzsb_xiaolei$hy_lsv_lower<-(sum(data_onexiaolei$zz_two_pgz_lower)+sum(data_onexiaolei$sd_two_pgz_lower))/(sum(data_onexiaolei$YNSEHJ)+sum(data_onexiaolei$SJYNSDSE)+sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))
	zzsb_xiaolei$hy_lsv_upper<-(sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))/(sum(data_onexiaolei$YNSEHJ)+sum(data_onexiaolei$SJYNSDSE)+sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))
	names(zzsb_xiaolei)<-c("纳税人代码", "纳税人名称", "企业行业代码", "数理统计4大类增值税偷漏税额", 
	"数理统计4大类销项税额", "数理统计4大类实际抵扣税额", "数理统计4小类增值税偷漏税额", "数理统计4小类销项税额", "数理统计4小类实际抵扣税额", "数理统计5大类增值税偷漏税额", 
	"数理统计5大类销项税额", "数理统计5大类实际抵扣税额", "数理统计5小类增值税偷漏税额", "数理统计5小类销项税额", "数理统计5小类实际抵扣税额", "数理统计6大类增值税偷漏税额", 
	"数理统计6大类销项税额","数理统计6大类实际抵扣税额", "数理统计6小类增值税偷漏税额", "数理统计6小类销项税额", "数理统计6小类实际抵扣税额","数理统计评估偷漏税下限_评估值", "数理统计评估偷漏税上限_评估值", 
	"数理统计评估偷漏税下限_调整值", "数理统计评估偷漏税上限_调整值", "神经网络4增值税偷漏税额", "神经网络4销项税额", "神经网络4实际抵扣税额", 
	"神经网络5增值税偷漏税额", "神经网络5销项税额", "神经网络5实际抵扣税额", "神经网络6增值税偷漏税额", "神经网络6销项税额", "神经网络6实际抵扣税额","神经网络评估偷漏税下限_评估值", 
	"神经网络评估偷漏税上限_评估值", "神经网络评估偷漏税下限_调整值", "神经网络评估偷漏税上限_调整值", "企业申报年销项税额合计", "企业申报年实际抵扣税额合计", 
	"企业申报营业收入", "企业增值税应纳税额合计年申报值", "二类模型评估企业增值税偷漏税额下限_评估值", "二类模型评估企业增值税偷漏税额上限_评估值", "二类模型评估企业增值税偷漏税额下限_补税值", 
	"二类模型评估企业增值税偷漏税额上限_补税值", "行业增值税税收流失率下限", "行业增值税税收流失率上限", "行业总税收流失率下限", "行业总税收流失率上限")	
	}
	zzsb_xiaolei
}
#定义生成流失率表结果，三表、四表因为会进行疑点科目与卡方法计算，因此模型组提供给数据组表需要增加的字段包括：卡方法增值税偷漏税下限_补税值，
#卡方法增值税偷漏税上限_补税值，卡方法所得税偷漏税下限_补税值，卡方法所得税偷漏税上限_补税值，卡方法偷漏税合计下限，卡方法偷漏税合计上限，偏离行业经营状态部分疑点会计科目。
createtable_lsvb<-function(data_onexiaolei)
{
    
	data_onexiaolei$qyzz_lsv_lower<-data_onexiaolei$zz_two_pgz_lower/(data_onexiaolei$YNSEHJ+data_onexiaolei$zz_two_pgz_upper)
	data_onexiaolei$qyzz_lsv_upper<-data_onexiaolei$zz_two_pgz_upper/(data_onexiaolei$YNSEHJ+data_onexiaolei$zz_two_pgz_upper)
	data_onexiaolei$qysd_lsv_lower<-data_onexiaolei$sd_two_pgz_lower/(data_onexiaolei$SJYNSDSE+data_onexiaolei$sd_two_pgz_upper)
	data_onexiaolei$qysd_lsv_upper<-data_onexiaolei$sd_two_pgz_upper/(data_onexiaolei$SJYNSDSE+data_onexiaolei$sd_two_pgz_upper)
	data_onexiaolei$qy_sbz<-data_onexiaolei$YNSEHJ+data_onexiaolei$SJYNSDSE
	data_onexiaolei$qy_lse_lower<-data_onexiaolei$zz_two_pgz_lower+data_onexiaolei$sd_two_pgz_lower
	data_onexiaolei$qy_lse_upper<-data_onexiaolei$zz_two_pgz_upper+data_onexiaolei$sd_two_pgz_upper
	data_onexiaolei$qy_lsv_lower<-(data_onexiaolei$zz_two_pgz_lower+data_onexiaolei$sd_two_pgz_lower)/(data_onexiaolei$YNSEHJ+data_onexiaolei$zz_two_pgz_upper+data_onexiaolei$SJYNSDSE+data_onexiaolei$sd_two_pgz_upper)
	data_onexiaolei$qy_lsv_upper<-(data_onexiaolei$zz_two_pgz_upper+data_onexiaolei$sd_two_pgz_upper)/(data_onexiaolei$YNSEHJ+data_onexiaolei$zz_two_pgz_upper+data_onexiaolei$SJYNSDSE+data_onexiaolei$sd_two_pgz_upper)
	data_onexiaolei$hy_lsv_lower<-(sum(data_onexiaolei$zz_two_pgz_lower)+sum(data_onexiaolei$sd_two_pgz_lower))/(sum(data_onexiaolei$YNSEHJ)+sum(data_onexiaolei$SJYNSDSE)+sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))
	data_onexiaolei$hy_lsv_upper<-(sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))/(sum(data_onexiaolei$YNSEHJ)+sum(data_onexiaolei$SJYNSDSE)+sum(data_onexiaolei$zz_two_pgz_upper)+sum(data_onexiaolei$sd_two_pgz_upper))
	lsvb_xiaolei<-data_onexiaolei[,c("NSRDM","NSRMC","HY_DM","YYSR","YNSEHJ","zz_two_pgz_lower","zz_two_pgz_upper","qyzz_lsv_lower","qyzz_lsv_upper",
	"SJYNSDSE","sd_two_pgz_lower","sd_two_pgz_upper","qysd_lsv_lower","qysd_lsv_upper","qy_sbz","qy_lse_lower","qy_lse_upper","qy_lsv_lower","qy_lsv_upper",
	"hy_lsv_lower","hy_lsv_upper")]
	names(lsvb_xiaolei)<-c("纳税人代码", "纳税人名称", "行业代码", "企业营业收入", "企业增值税纳税申报值", "增值税偷漏税下限_补税值", "增值税偷漏税上限_补税值", "企业增值税税收流失率下限", 
	"企业增值税税收流失率上限", "企业所得税纳税申报值", "所得税偷漏税下限_补税值", "所得税偷漏税上限_补税值", "企业所得税税收流失率下限", "企业所得税税收流失率上限", "增值税申报值加所得税申报值",
	"企业税收流失额下限", "企业税收流失额上限", "企业税收流失率下限", "企业税收流失率上限", "行业总税收流失率下限", "行业总税收流失率上限")
	if(modelnum==14){lsvb_xiaolei$卡方法增值税偷漏税下限_补税值<-data_onexiaolei$zz_kf_pgz_lower
	lsvb_xiaolei$卡方法增值税偷漏税上限_补税值<-data_onexiaolei$zz_kf_pgz_upper
	lsvb_xiaolei$卡方法所得税偷漏税下限_补税值<-data_onexiaolei$sd_kf_pgz_lower
	lsvb_xiaolei$卡方法所得税偷漏税上限_补税值<-data_onexiaolei$sd_kf_pgz_upper
	lsvb_xiaolei$卡方法偷漏税合计下限<-data_onexiaolei$zz_kf_pgz_lower+data_onexiaolei$sd_kf_pgz_lower
	lsvb_xiaolei$卡方法偷漏税合计上限<-data_onexiaolei$zz_kf_pgz_upper+data_onexiaolei$sd_kf_pgz_upper
	lsvb_xiaolei$偏离行业经营状态部分疑点会计科目<-data_onexiaolei$ydkm
	}
	lsvb_xiaolei
}
#定义createtable函数，保存所得税表结果、增值税表结果、流失率表结果
createtable<-function(hydm_xiaolei)
{
	data_xiaolei<-subset(jgdata,HY_DM==hydm_xiaolei)
	sdsb_xiaolei<-createtable_sdsb(data_xiaolei)
	write.table(sdsb_xiaolei,paste(hydm_xiaolei,"所得税表.xls",sep=""),sep="\t",row.names=F)
	write.table(sdsb_xiaolei,paste(hydm_xiaolei,"所得税表",sep=""),sep="\t",row.names=F)
	zzsb_xiaolei<-createtable_zzsb(data_xiaolei)
	write.table(zzsb_xiaolei,paste(hydm_xiaolei,"增值税表.xls",sep=""),sep="\t",row.names=F)
	write.table(zzsb_xiaolei,paste(hydm_xiaolei,"增值税表",sep=""),sep="\t",row.names=F)
	lsvb_xiaolei<-createtable_lsvb(data_xiaolei)
	write.table(lsvb_xiaolei,paste(hydm_xiaolei,"流失率表.xls",sep=""),sep="\t",row.names=F)
	write.table(lsvb_xiaolei,paste(hydm_xiaolei,"流失率表",sep=""),sep="\t",row.names=F)
	table_list<-list(sdsb_xiaolei,zzsb_xiaolei,lsvb_xiaolei)
	table_list
}
#将各行业小类的结果合并，并保存到工作空间中。
sdsb_all<-data.frame()
zzsb_all<-data.frame()
lsvb_all<-data.frame()
hydm<-sort(unique(jgdata$HY_DM))
for(i in 1:length(hydm))
{
   tabel.list.onexiaolei<-createtable(hydm[i])
   sdsb_all<-rbind(sdsb_all,tabel.list.onexiaolei[[1]])
   zzsb_all<-rbind(zzsb_all,tabel.list.onexiaolei[[2]])
   lsvb_all<-rbind(lsvb_all,tabel.list.onexiaolei[[3]])
}

write.table(sdsb_all,paste(method_featureselection,"所得税表结果",sep=""),sep="\t",row.names=F)
write.table(zzsb_all,paste(method_featureselection,"增值税表结果",sep=""),sep="\t",row.names=F)
write.table(lsvb_all,paste(method_featureselection,"模型组提供给数据组表结果",sep=""),sep="\t",row.names=F)
}

#---------------------------------------------------------------------------
#定义tables234_generate函数，合并二三四表结果。
tables234_generate<-function(method)
{
#method="step"
twojg<-read.table(paste(path,Date,city,"_model","\\",names_dir[2],method,"模型组提供给数据组表结果",sep=""),sep="\t",header = TRUE)
threejg<-read.table(paste(path,Date,city,"_model","\\",names_dir[3],method,"模型组提供给数据组表结果",sep=""),sep="\t",header = TRUE)
fourjg<-read.table(paste(path,Date,city,"_model","\\",names_dir[4],method,"模型组提供给数据组表结果",sep=""),sep="\t",header = TRUE)
names_addmethod<-c("增值税偷漏税下限_补税值", "增值税偷漏税上限_补税值", "企业增值税税收流失率下限", "企业增值税税收流失率上限",  "所得税偷漏税下限_补税值", 
"所得税偷漏税上限_补税值", "企业所得税税收流失率下限", "企业所得税税收流失率上限", "企业税收流失额下限", "企业税收流失额上限", 
"企业税收流失率下限", "企业税收流失率上限", "行业总税收流失率下限", "行业总税收流失率上限")
add_method<-function(data_addmethod)
{
	names(data_addmethod)[names(data_addmethod)%in%names_addmethod]<-paste(method,names(data_addmethod)[names(data_addmethod)%in%names_addmethod],sep="")
	data_addmethod
}
twojg<-add_method(twojg);threejg<-add_method(threejg);fourjg<-add_method(fourjg)
names_addtablenum<-c(paste(method,names_addmethod,sep=""),"卡方法增值税偷漏税下限_补税值", "卡方法增值税偷漏税上限_补税值", "卡方法所得税偷漏税下限_补税值", "卡方法所得税偷漏税上限_补税值", 
"卡方法偷漏税合计下限", "卡方法偷漏税合计上限", "偏离行业经营状态部分疑点会计科目")
add_tablenum<-function(data_addtablenum,tablenum)
{
    
	names(data_addtablenum)[names(data_addtablenum)%in%names_addtablenum]<-paste(names(data_addtablenum)[names(data_addtablenum)%in%names_addtablenum],tablenum,sep="")	
    data_addtablenum
	}
twojg<-add_tablenum(twojg,"(二表)")
threejg<-add_tablenum(threejg,"(三表)")
fourjg<-add_tablenum(fourjg,"(四表)")
table23_merge<-merge(twojg,threejg[,c("纳税人代码",grep(method,names(threejg),value=TRUE),grep("卡方法",names(threejg),value=TRUE))],by.x="纳税人代码",by.y="纳税人代码",all.x=TRUE,all.y=FALSE)
table234_merge<-merge(table23_merge,fourjg[,c("纳税人代码",grep(method,names(fourjg),value=TRUE),grep("卡方法",names(fourjg),value=TRUE))],by.x="纳税人代码",by.y="纳税人代码",all.x=TRUE,all.y=FALSE)
#grep(method,names(twojg),value=TRUE)
colnames_hb<-grep("补税值",names_addtablenum,value=TRUE)
colnames_show<-c("纳税人代码", "纳税人名称", "行业代码", "企业营业收入", "企业增值税纳税申报值","企业所得税纳税申报值","增值税申报值加所得税申报值")
for(i in 1:length(colnames_hb))
{
    table234_merge[,paste(colnames_hb[i],"(表合并)",sep="")]<-apply(table234_merge[,grep(colnames_hb[i],names(table234_merge))],1,mean,na.rm = TRUE)
	colnames_show<-c(colnames_show,grep(colnames_hb[i],names(table234_merge),value=TRUE))
}
table234_merge[,paste(method,"偷漏税合计值下限_补税值(表合并)",sep="")]<-apply(table234_merge[,grep(paste(method,"\\w+下限\\w+\\(表合并",sep=""),colnames_show,value=TRUE)],1,sum)
table234_merge[,paste(method,"偷漏税合计值上限_补税值(表合并)",sep="")]<-apply(table234_merge[,grep(paste(method,"\\w+上限\\w+\\(表合并",sep=""),colnames_show,value=TRUE)],1,sum)
table234_merge[,"卡方法偷漏税合计值下限(表合并)"]<-apply(table234_merge[,grep("卡方法\\w+下限\\w+\\(表合并",colnames_show,value=TRUE)],1,sum)
table234_merge[,"卡方法偷漏税合计值上限(表合并)"]<-apply(table234_merge[,grep("卡方法\\w+上限\\w+\\(表合并",colnames_show,value=TRUE)],1,sum)
colnames_show<-c(colnames_show,grep("\\w+合计值\\w+\\(表合并",names(table234_merge),value=TRUE),"偏离行业经营状态部分疑点会计科目(二表)")
#grep("(卡方法+\\.)*\\w+",text)#grep("\\w+合计值\\w+\\(表合并",names(table234_merge),value=TRUE)
#grep(paste(method,"\\w+上限\\w+\\(表合并",sep=""),colnames_show,value=TRUE)#grep("(\\w+\\.)*\\w+@(\\w+\\.)+[a-zA-Z]+",text)#grep("(\\卡方法+\\.)*",text)
#edit(table234_merge[,colnames_show])#edit(table234_merge[1:10,grep(colnames_hb[i],names(table234_merge))])
if(city=="qingdao") table234_merge$纳税人代码<-sub("8","3702",table234_merge$纳税人代码)
write.table(table234_merge[,colnames_show],paste(method,"二三四表合并结果",sep=""),sep="\t",row.names=F)
}

