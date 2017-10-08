package com.ln.chapter1


sealed trait List[+A]
case object Nil extends List[Nothing]//用于表现空List的List数据构造器
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {// List伴生对象。包含创建List和List操作的一些函数
  def sum(ints: List[Int]): Int = ints match{
    case Nil=>0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def product(ds: List[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] ={
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}