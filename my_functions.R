library(jsonlite)
library(stringr)
library(tidyverse)

URL2data<-function(x){
  if(length(fromJSON(x)$people$pose_keypoints_2d)>0){
    data<-fromJSON(x) %$% people %$% pose_keypoints_2d %>% 
      map_df(function(x){                        # リスト各要素に以下を適用してbind_rows()
        matrix(x, 18, byrow = T) %>%             #   ・ 行列に整形
          data.frame() %>%                       #   ・ data.frameに変換
          rename(X = X1, Y = X2, prob = X3) %>%  #   ・ カラム名変更
          mutate(pos = 1:nrow(.))}               #   ・ 公式と同じポイント名+1をつける
      )%>% 
      mutate(pose = ifelse(pos == 1, 1, 0) %>% cumsum()) %>%     # 骨格の番号カラムを作る
      nest(-pose)
    return(data[[2]])
  }
  else{
    d<-list(data.frame(X=1,Y=1,prob=0,pos=1),data.frame(X=1,Y=1,prob=0,pos=1),data.frame(X=1,Y=1,prob=0,pos=1))
    return(d)
  }
}



na_sum<-function(t,data_s){
  pts<-c(0,1,2,5,8,11)+1 
  xyp_t<-matrix(0,ncol=3,nrow=18)
  if(t>0){
  for(i in 1:3){
    xyp_t[,i]<-c(data_s[[t]][[i]][pts,1],data_s[[t]][[i]][pts,2],data_s[[t]][[i]][pts,3])
  }
  ans<-sum(is.na(xyp_t))
  }
  else{
  ans<-100
  }
  return(ans)
}



xy_cm<-function(t,data_s){ 
  pts<-c(1,8,11)+1 
  xyp_t<-matrix(0,ncol=3,nrow=9)
  if(t>0){
  for(i in 1:3){
  xyp_t[,i]<-c(data_s[[t]][[i]][pts,1],data_s[[t]][[i]][pts,2],data_s[[t]][[i]][pts,3])
  }
  if(mean(xyp_t[7:9,1])>0){
  cmx_1<-mean(xyp_t[1:3,1]*xyp_t[7:9,1])/mean(xyp_t[7:9,1])
  cmy_1<-mean(xyp_t[4:6,1]*xyp_t[7:9,1])/mean(xyp_t[7:9,1])
  }
  else{
  cmx_1<-0
  cmy_1<-0
  }
  if(mean(xyp_t[7:9,2])>0){  
  cmx_2<-mean(xyp_t[1:3,2]*xyp_t[7:9,2])/mean(xyp_t[7:9,2])
  cmy_2<-mean(xyp_t[4:6,2]*xyp_t[7:9,2])/mean(xyp_t[7:9,2])
  }
  else{
  cmx_2<-0
  cmy_2<-0
  }
  if(mean(xyp_t[7:9,3])>0){    
  cmx_3<-mean(xyp_t[1:3,3]*xyp_t[7:9,3])/mean(xyp_t[7:9,3])
  cmy_3<-mean(xyp_t[4:6,3]*xyp_t[7:9,3])/mean(xyp_t[7:9,3])
  }
  else{
  cmx_3<-0
  cmy_3<-0
  }
  ans<-c(cmx_1,cmy_1,cmx_2,cmy_2,cmx_3,cmy_3)
  }
  else{
  ans<-rep(0,6)
  }
  return(ans)
}  



data_trans<-function(xyp,ann){
T<-length(xyp)
xyp_n<-list()
xyp_zero<-cbind(X=rep(0,18),Y=rep(0,18),prob=rep(0,18),pos=1:18)
xyp_zeros<-list(xyp_zero,xyp_zero,xyp_zero)
s<-0
for(t in 1:T){
  ijk<-ann[t,2]
  
  if(ijk!=0){

  if(ijk==-1 & t>1){
  ijk_p<-ann[t-1,2]
  if(ijk_p!=-1){
  s<-s+1
  xyp_n[[s]]<-xyp_zeros
  }
  }
  if(ijk==12){
  s<-s+1
  xyp_n[[s]]<-list(xyp[[t]][[1]],xyp[[t]][[2]],xyp[[t]][[3]])  
  }
  if(ijk==13){
  s<-s+1
  xyp_n[[s]]<-list(xyp[[t]][[1]],xyp[[t]][[3]],xyp[[t]][[2]])  
  }
  if(ijk==21){
  s<-s+1
  xyp_n[[s]]<-list(xyp[[t]][[2]],xyp[[t]][[1]],xyp[[t]][[3]])  
  }
  if(ijk==23){
  s<-s+1
  xyp_n[[s]]<-list(xyp[[t]][[2]],xyp[[t]][[3]],xyp[[t]][[1]])  
  }
  if(ijk==31){
  s<-s+1
  xyp_n[[s]]<-list(xyp[[t]][[3]],xyp[[t]][[1]],xyp[[t]][[2]])  
  }
  if(ijk==32){
  s<-s+1
  xyp_n[[s]]<-list(xyp[[t]][[3]],xyp[[t]][[2]],xyp[[t]][[1]])  
  }
}
  if(ijk==0 & t>1 & t<T & ann[t+1,2]!=0){
  s<-s+1
  xyp_n[[s]]<-xyp_n[[(s-1)]]  
}

}
return(xyp_n)
}



show<-function(t1,t2,data_s,WAIT=0){
  L<-1920
  H<-1080
  ss<-c(0, 0, 0,14,15,1,1,2,3,5,6,1,8,9,  1,11,12)+1
  es<-c(1,14,15,16,17,2,5,3,4,6,7,8,9,10,11,12,13)+1
  pts<-c(1,8,11)+1 

  for(t in t1:t2){
    t2<-t%/%30
    
    if(sum(data_s[[t]][[1]][,3])>0.1){
      
      p1<-1
      p2<-2
      p3<-3
      
      X1<-data_s[[t]][[p1]]$X
      Y1<-data_s[[t]][[p1]]$Y
      P1<-data_s[[t]][[p1]]$prob
      X2<-data_s[[t]][[p2]]$X
      Y2<-data_s[[t]][[p2]]$Y
      P2<-data_s[[t]][[p2]]$prob
      X3<-data_s[[t]][[p3]]$X
      Y3<-data_s[[t]][[p3]]$Y
      X1_cm<-mean(X1[pts]*P1[pts])/mean(P1[pts])
      Y1_cm<-mean(Y1[pts]*P1[pts])/mean(P1[pts])
      X2_cm<-mean(X2[pts]*P2[pts])/mean(P2[pts])
      Y2_cm<-mean(Y2[pts]*P2[pts])/mean(P2[pts])
      
      plot(1,1,xlim=c(1,(L-1)),ylim=c(100,1000),cex=0.0000)
      
      points(X1,H-Y1,col=1,pch=20)
	points(X1[5],H-Y1[5],col=3,pch=20,cex=2)
	points(X1[11],H-Y1[11],col=3,pch=20,cex=2)
	points(X1[8],H-Y1[8],col=4,pch=20,cex=2)
	points(X1[14],H-Y1[14],col=4,pch=20,cex=2)
	points(X1_cm,H-Y1_cm,col=1,pch=20,cex=3)
      for(i in 1:17){
        x1<-X1[ss[i]]
        y1<-Y1[ss[i]]
        x2<-X1[es[i]]
        y2<-Y1[es[i]]
	
        if(x1 >0 & x2>0 & y1>0 & y2>0){
          segments(x1,H-y1,x2,H-y2,col=1,lwd=2)
        }
      }
      
      points(X2,H-Y2,col=2,pch=20)
	points(X2[5],H-Y2[5],col=3,pch=20,cex=2)
	points(X2[11],H-Y2[11],col=3,pch=20,cex=2)
	points(X2[8],H-Y2[8],col=4,pch=20,cex=2)
	points(X2[14],H-Y2[14],col=4,pch=20,cex=2)
	points(X2_cm,H-Y2_cm,col=2,pch=20,cex=3)
      for(i in 1:17){
        x1<-X2[ss[i]]
        y1<-Y2[ss[i]]
        x2<-X2[es[i]]
        y2<-Y2[es[i]]
        if(x1 >0 & x2>0 & y1>0 & y2>0){
          segments(x1,H-y1,x2,H-y2,col=2,lwd=2)
        }
      }
      
      
      times<-paste("File No.=",t)
      legend("topright",legend=c(times,"No.1","No.2"),col=c(1,1,2),lty=-1,pch=c(-1,20,20),cex=1.0)
      Sys.sleep(WAIT)
    }
    else{
      Sys.sleep(5)
    }
  }
}



Check<-function(t1,t2,data_s,info,WAIT=0){
  L<-1920
  H<-1080
  ss<-c(0, 0, 0,14,15,1,1,2,3,5,6,1,8,9,  1,11,12)+1
  es<-c(1,14,15,16,17,2,5,3,4,6,7,8,9,10,11,12,13)+1

  for(t in t1:t2){
    
    if(dim(data_s[[t]][[1]])[1]>1){
    
      p1<-1
      p2<-2
      p3<-3
      c1<-1
      c2<-2
      c3<-3
      if(info[t,2]==13){
      p1<-1
      p2<-3
      p3<-2
      }
      if(info[t,2]==21){
      p1<-2
      p2<-1
      p3<-3
      }
      if(info[t,2]==23){
      p1<-2
      p2<-3
      p3<-1
      }
      if(info[t,2]==31){
      p1<-3
      p2<-1
      p3<-2
      }
      if(info[t,2]==32){
      p1<-3
      p2<-2
      p3<-1
      }
      if(info[t,2]==0){
      c1<-4
      c2<-4
      c3<-4
      }
      
      X1<-data_s[[t]][[p1]]$X
      Y1<-data_s[[t]][[p1]]$Y
      X2<-data_s[[t]][[p2]]$X
      Y2<-data_s[[t]][[p2]]$Y
      X3<-data_s[[t]][[p3]]$X
      Y3<-data_s[[t]][[p3]]$Y
      
      plot(0,0,xlim=c(0,L),ylim=c(0,H))
      
      points(X1,H-Y1,col=c1,pch=20)
      for(i in 1:17){
        x1<-X1[ss[i]]
        y1<-Y1[ss[i]]
        x2<-X1[es[i]]
        y2<-Y1[es[i]]
        if(x1 >0 & x2>0){
          segments(x1,H-y1,x2,H-y2,col=c1,lwd=2)
        }
      }
      
      points(X2,H-Y2,col=c2,pch=20)
      for(i in 1:17){
        x1<-X2[ss[i]]
        y1<-Y2[ss[i]]
        x2<-X2[es[i]]
        y2<-Y2[es[i]]
        if(x1 >0 & x2>0){
          segments(x1,H-y1,x2,H-y2,col=c2,lwd=2)
        }
      }
     
      points(X3,H-Y3,col=c3,pch=20)
      for(i in 1:17){
        x1<-X3[ss[i]]
        y1<-Y3[ss[i]]
        x2<-X3[es[i]]
        y2<-Y3[es[i]]
        if(x1 >0 & x2>0){
          segments(x1,H-y1,x2,H-y2,col=c3,lwd=2)
        }
      }
      
      times<-paste("File No.=",t)
      no1<-paste("No.1=",p1)
      no2<-paste("No.2=",p2)
      no3<-paste("No.3=",p3)
      legend("topright",legend=c(times,no1,no2,no3),col=c(1,c1,c2,c3),lty=-1,pch=c(-1,20,20,20),cex=1.0)
      if(WAIT==0){
      readline("Enter to restart !")
      }
      else{
      Sys.sleep(WAIT)
      }
    }
    else{
      print(t)
      readline("Enter to restart 2!")
    }
  }
}



t_interrupt<-function(xyp_123){
T<-length(xyp_123)
ans<-c()
s<-1
for(t in 1:T){
if(mean(xyp_123[[t]][[1]][,1])==0){
ans[s]<-t
s<-s+1
}
}
return(ans)
}






data_12<-function(xyp_123,Data){
t_c<-t_interrupt(xyp_123)
T<-length(xyp_123)
T_c<-length(t_c)

pts<-c(1,8,11)+1 
pts2<-c(4,10,7,13)+1

ans<-matrix(0,nrow=(T+1),ncol=24)

for(t in 1:T){

rnd<-sum(t>t_c)+1
if(rnd==1){
Pts_L<-0
Pts_R<-0
}
if(rnd>1){
Pts_L<-cumsum(Data[,2])[rnd-1]
Pts_R<-cumsum(Data[,3])[rnd-1]
}
if(sum(t==t_c)==1){
Pts_L<-Data[rnd,2]
Pts_R<-Data[rnd,3]
}

cm_p_1<-mean(xyp_123[[t]][[1]][pts,3])
cm_p_2<-mean(xyp_123[[t]][[2]][pts,3])
if(cm_p_1>0.0 & cm_p_2>0.0){
cm_x_1<-mean(xyp_123[[t]][[1]][pts,1]*xyp_123[[t]][[1]][pts,3])/cm_p_1
cm_x_2<-mean(xyp_123[[t]][[2]][pts,1]*xyp_123[[t]][[2]][pts,3])/cm_p_2
cm_y_1<-mean(xyp_123[[t]][[1]][pts,2]*xyp_123[[t]][[1]][pts,3])/cm_p_1
cm_y_2<-mean(xyp_123[[t]][[2]][pts,2]*xyp_123[[t]][[2]][pts,3])/cm_p_2
}
else{
cm_x_1<-0
cm_x_2<-0
}
ans[t,]<-c(round(cm_x_1,1),round(xyp_123[[t]][[1]][pts2,1],1),round(cm_x_2,1),round(xyp_123[[t]][[2]][pts2,1],1),round(cm_y_1,1),round(xyp_123[[t]][[1]][pts2,2],1),round(cm_y_2,1),round(xyp_123[[t]][[2]][pts2,2],1),round(t),round(rnd),round(Pts_L),round(Pts_R))
}

t<-T+1
rnd<-sum(t>t_c)+1
Pts_L<-Data[rnd,2]
Pts_R<-Data[rnd,3]
ans[t,]<-c(rep(0,20),round(t),round(rnd),round(Pts_L),round(Pts_R))

colnames(ans)<-c("cm_x.L","4_x.L","10_x.L","7_x.L","13_x.L","cm_x.R","4_x.R","10_x.R","7_x.R","13_x.R","cm_y.L","4_y.L","10_y.L","7_y.L","13_y.L","cm_y.R","4_y.R","10_y.R","7_y.R","13_y.R","t","rnd","Pts.L","Pts.R")



return(ans)
}



show_data_12<-function(t1,t2,data_12_i,i,WAIT=0){
      ss<-c(1,1)
      es<-c(2,3)

for(t in t1:t2){
      X1<-data_12_i[[i]][t,c(1,3,5)]
      X2<-data_12_i[[i]][t,c(6,8,10)]
      Y1<-data_12_i[[i]][t,c(11,13,15)]*(-1)
      Y2<-data_12_i[[i]][t,c(16,18,20)]*(-1)
      x1<-X1[1]
      y1<-Y1[1]
      x2<-X2[1]
      y2<-Y2[1]
  if(x1!=0.0 & x2!=0.0){
      plot(X1,Y1-Y1[1],cex=5,col=1,pch=20,xlim=c(-650,1650),ylim=c(-400,100))
      for(j in 1:2){
      x1<-X1[1]
      y1<-Y1[1]
      x2<-X1[es[j]]
      y2<-Y1[es[j]]
      if(abs(x1)<500 & abs(x2)<500 & abs(y1)<500 & abs(y2)<500){
      segments(x1,0,x2,y2-y1,col=1,lwd=2)
      }
      }
      points(X2+1000,Y2-Y2[1],cex=5,col=2,pch=20)
      for(j in 1:2){
      x1<-X2[1]
      y1<-Y2[1]
      x2<-X2[es[j]]
      y2<-Y2[es[j]]
      if(abs(x1)<500 & abs(x2)<500 & abs(y1)<500 & abs(y2)<500){
      segments(x1+1000,0,x2+1000,y2-y1,col=2,lwd=2)
      }
      }
      times<-paste("File No.=",t)
      no1<-paste("Player L")
      no2<-paste("Player R")
      legend("topright",legend=c(times,no1,no2),col=c(1,1,2),lty=-1,pch=c(-1,20,20),cex=1.0)

      if(WAIT==0){
      readline("Enter to restart !")
      }
      else{
      Sys.sleep(WAIT)
      }
}  
else{
      readline("Enter to restart !")
}
}   # t-loop
}
