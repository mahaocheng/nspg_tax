rm(list=ls())
#Sys.Date()
#dir.create("d:\\work\\model_nspg\\2016\\",recursive = TRUE)
Date<-"2016.04.14";city<-"qingdao";path="d:\\work\\model_nspg\\2016\\"
names_dir<-c("programmes\\","twotables_wd\\","threetables_wd\\","fourtables_wd\\","results\\")

path_list<-list()
for(i in 1:length(names_dir))
{
    path_list[[i]]<-paste(path,Date,city,"_model","\\",names_dir[i],sep="")
	dir.create(path_list[[i]],recursive = TRUE)
}
