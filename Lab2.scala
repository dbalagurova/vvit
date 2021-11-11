/** Option представляет собой контейнер, который хранит какое-то значение
 * или не хранит ничего совсем, указывает, вернула ли операция результат или нет.
 * Это часто используется при поиске значений или когда операции могут потерпеть неудачу,
 * и вам не важна причина.

 * Комбинаторы называются так потому, что они созданы, чтобы объединять результаты.
 * Результат одной функции часто используется в качестве входных данных для другой.

 * Наиболее распространенным способом, является использование их со стандартными структурами данных.
 * Функциональные комбинаторы `map` и` flatMap` являются контекстно-зависимыми.
 * map - применяет функцию к каждому элементу из списка, возвращается список с тем же числом элементов.
 * flatMap берет функцию, которая работает с вложенными списками и объединяет результаты.
 */

/** Напишите ваши решения в тестовых функциях.  */
object Compositions {

  sealed trait Option[A] {

    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
  }
  case class Some[A](a: A) extends Option[A] {

    def map[B](f: A => B): Option[B] = Some(f(a))
    def flatMap[B](f: A => Option[B]): Option[B] = f(a)
  }
  case class None[A]()     extends Option[A] {

    def map[B](f: A => B): Option[B] = None()
    def flatMap[B](f: A => Option[B]): Option[B] = None()
  }

  // a) Используйте данные функции. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры

  def testCompose[A, B, C, D](f: A => B)
                             (g: B => C)
                             (h: C => D): A => D = { h compose g compose f }

  // b) Напишите функции с использованием `map` и `flatMap`. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры

  def testMapFlatMap[A, B, C, D](f: A => Option[B])
                                (g: B => Option[C])
                                (h: C => D): Option[A] => Option[D] = { a => a flatMap f flatMap g map h }

  // c) Напишите функцию используя for. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры

  def testForComprehension[A, B, C, D](f: A => Option[B])
                                      (g: B => Option[C])
                                      (h: C => D): Option[A] => Option[D] = { v =>
    for {
      first <- v
      second <- f(first)
      third <- g(second)
    } yield h(third)
  }
}

/** Напишите свои решения в виде функций. */
object RecursiveData {

  sealed trait List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case class Nil[A]() extends List[A]

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.
  def ListIntEmpty(list: List[Int]): Boolean = list match{
    case Nil() => true
    case _ => false
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = ListIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
  def ListIntHead(list: List[Int]): Int = list match {
    case list: Nil[Int] => -1
    case list: Cons[Int] => list.head
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = ListIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?
  //

  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */
  sealed trait Tree[A]
  case class Leaf[A](leaf: A) extends Tree[A]
  case class Node[A](leaf: A, left_node: Tree[A], right_node: Tree[A]) extends Tree[A]

}

import scala.annotation.tailrec

/** Реализуйте функции для решения следующих задач.
 * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
 * рекурсия будет хвостовой если:
 *   1. рекурсия реализуется в одном направлении
 *   2. вызов рекурсивной функции будет последней операцией перед возвратом
 */
object RecursiveFunctions {

  sealed trait List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case class Nil[A]() extends List[A]

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }
    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(rlist: List[A], vlist: List[A]): List[A] = vlist match {
      case Nil() => rlist
      case Cons(head, tail) => loop(Cons(head, rlist), tail)
    }
    loop(Nil(), list)
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)

  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(rem: List[A], acc: List[B]): List[B] = rem match {
      case Nil() => reverse(acc)
      case Cons(head, tail) => loop(tail, Cons(f(head), acc))
    }
    loop(list, Nil())
  }

  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)

  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */
  def append[A](l: List[A], r: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], r: List[A]): List[A] = r match {
      case Nil() => reverse(l)
      case Cons(head, tail) => loop(Cons(head, l), tail)
    }
    loop(reverse(l), r)
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l, r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   *
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в
   *    списке. Поэтому вы создаете List[List[B]].
   */
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[List[B]] = {
    @tailrec
    def loop(rem: List[A], acc: List[List[B]]): List[List[B]] = rem match {
      case Nil() => reverse(acc)
      case Cons(head, tail) => loop(tail, Cons(f(head), acc))
    }
    loop(list, Nil())
  }

  // используйте функцию из пункта  (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[List[B]] = flatMap(list)(f)

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
}

object Lab3 {

  def main(args: Array[String]) = {

    val elist: RecursiveData.List[Int] = RecursiveData.Nil()
    val flist: RecursiveData.List[Int] = RecursiveData.Cons(0, RecursiveData.Cons(1, RecursiveData.Cons(2, RecursiveData.Nil())))

    println(RecursiveData.testListIntEmpty(elist))
    println(RecursiveData.testListIntEmpty(flist))

    println(RecursiveData.testListIntHead(elist))
    println(RecursiveData.testListIntHead(flist))

    val rlist: RecursiveFunctions.List[Int] = RecursiveFunctions.Cons(0, RecursiveFunctions.Cons(1, RecursiveFunctions.Cons(2, RecursiveFunctions.Nil())))
    println(RecursiveFunctions.testReverse(rlist))
    def mul2(i: Int):Int = i * 2
    println(RecursiveFunctions.testMap(rlist, mul2))
    println(RecursiveFunctions.testAppend(rlist, rlist))
    def btolistb(b: Int):RecursiveFunctions.List[Int] = RecursiveFunctions.Cons(b, RecursiveFunctions.Nil())
    println(RecursiveFunctions.testFlatMap(rlist, btolistb))
  }
}
