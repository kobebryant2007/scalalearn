package com.ln.chapter1

object HelloWorld {
  def main(args: Array[String])= {
    println("hello scala world");
    println(formatAbs(-6));
    println(factorial(16))
  }
  def abs(n: Int):Int={
    if(n < 0) -n
    else n
  }
  
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x));
  }
  
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = 
      if(n <= 0) acc
      else go(n-1, n * acc)
     
    go(n, 1)
  }
  
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = 
      if(n >= as.length) -1
      else if(p(as(n))) n
      else loop(n + 1)
    loop(0)
  }
}