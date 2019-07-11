package webtest.yahoo2019

object Practice {
  def main(args: Array[String]): Unit = {
    var cnt = 0
    for(c1 <- 'A' to 'Z'; c2 <- 'A' to 'Z';c3 <- 'A' to 'Z'){
      if(c1 != c2 && c2 != c3){
        cnt += 1
      }
    }
    println(cnt)
  }
}
