#Выйти из игры можно, набрав quit

#Ниже установлены размеры поля, цвета игроков и длина стороны ячейки
n <- 12   #кол-во строк
m <- 12   #кол-во столбцов
col.player1 <- 'blue'
col.player2 <- 'green'
cell.size <- 0.6
symbol.size <- cell.size*4

#Матрица matr задает поле в начале игры
a <- rep(0,m*n)
matr <- matrix(a,n,m)


#Возвращает TRUE, если ход сделан успешно
make.turn <- function(player.id, pos.row, pos.col) {
  if (pos.row>n || pos.col>m || pos.row<1 || pos.col<1 || matr[pos.row,pos.col]!=0){
    print("error1")
    return(FALSE)
  }
  if (player.id == 1){
    next.turn <- max(matr) + 1
  } else if (player.id == 2) {
    next.turn <- min(matr) - 1
  } else {
    print("error2")
    return(FALSE)
  }
  matr[pos.row,pos.col] <<- next.turn
  return(TRUE)
}


mark.win <- function(pos.row,pos.col,r,l,dir){
  segments(pos.col + dir[2]*r - .5, pos.row + dir[1]*r - .5, pos.col - dir[2]*l - .5, 
           pos.row - dir[1]*l - .5, lwd=4)
}


#Возвращает TRUE, если по одному из направлений (type) есть пять или более
#подряд идущих ячеек одного игрока
check.win <- function(pos.row, pos.col,type){
  r <- 0  #по направлению
  l <- 0  #против направления
  flag1 <- 1
  flag2 <- 1
  player.sign <- sign(matr[pos.row,pos.col])
  dir <- matrix(c(0,1,1,0,1,1,1,-1),2,4)
  
  for (i in 1:4){
    a <- (pos.row + dir[1,type]*i <= n) && 
      (pos.row + dir[1,type]*i > 0) &&
      (pos.col + dir[2,type]*i <= m) && 
      (pos.col + dir[2,type]*i > 0) &&
      (matr[pos.row + dir[1,type]*i, pos.col + dir[2,type]*i] * player.sign * flag1 > 0) 
    b <- (pos.row - dir[1,type]*i > 0) &&
      (pos.row - dir[1,type]*i <= n) &&
      (pos.col-dir[2,type]*i <= m) &&
      (pos.col-dir[2,type]*i > 0) &&
      (matr[pos.row - dir[1,type]*i, pos.col - dir[2,type]*i] * player.sign * flag2 > 0)
    
    if (a&&b) {
      r <- r+1
      l <- l+1
    } else if (a){
      r <- r+1
      flag2<-0
    } else if (b){
      l <- l+1
      flag1<-0
    } else if (r+l+1 >= 5){
      mark.win(pos.row,pos.col,r,l,dir[,type])
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if (r+l+1>=5) {
    mark.win(pos.row,pos.col,r,l,dir[,type])
    return(TRUE)
  } else {
    return(FALSE)
  }
}



#Рисует игровое поле
draw.field <- function(matr){
  n.turns2 <- abs(min(matr))
  n.turns1 <- max(matr)
  n.zeroes <- m*n - (n.turns1 + n.turns2)
  
  if ((n.turns2 == 0) && (n.turns1 ==1)){
    palette <- c(col.player1,col.player1)
  } else {
    palette <- c(colorRampPalette(c(col.player2, 'white'))(n.turns2+1)[1:n.turns2], 'white',
                 colorRampPalette(c('white', col.player1))(n.turns1+1)[2:(n.turns1+1)])
  }
  
  par(pin = c(cell.size*m,cell.size*n), xaxs='i', yaxs='i')
  plot(c(0,m),c(0,n),type = 'n', xlab = NA, ylab = NA, axes = F)
  grid(m, n, lwd = 2)
  box()
  axis(1, 1:m-0.5, labels=1:m, tick=F)
  axis(2, 1:n-0.5, labels=1:n, tick=F, las=2)
  
  for(i in 1:n){
    for (j in 1:m){
      if (matr[i,j]!=0){
        #text(j-0.5, i-0.5, pos=1, offset=-.7, if (matr[i,j]>0) '✖' else '◯', cex=symbol.size,
        #     col=palette[matr[i,j]-min(matr)+1], font=2)
        text(j-0.5, i-0.5, if (matr[i,j]>0) '✖' else '◯', cex=symbol.size,
             col=palette[matr[i,j]-min(matr)+1], font=2, adj=c(0.5,0.3))
      }
    }
  }
}


#Возвращает TRUE в случае победы одного из игроков
win.is.true <- function(pos.row, pos.col){
  win <- FALSE
  for (i in 1:4){
    h = check.win(pos.row, pos.col,i)
    win <- win || h
  }
  return(win)
}


cur.player <- 1
draw.field(matr)

while (TRUE){
  cat("player",cur.player,"turn\n")
  
  x = readline("col = ")
  if (x == 'quit'){
    break
  } 
  pos.col <- as.numeric(x)
  
  y = readline("row = ")
  if (y == 'quit'){
    break
  } 
  pos.row <- as.numeric(y)
  
  if (is.na(pos.row) || is.na(pos.col)){
    next
  } 
  
  if (make.turn(cur.player,pos.row,pos.col) == TRUE){
    draw.field(matr)
    
    if (win.is.true(pos.row,pos.col)){
      cat("PLAYER", cur.player, "WON")
      break
    }
    
    cur.player <- cur.player %% 2 + 1
  }
}

