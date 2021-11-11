object Lab3 extends App {
  import scala.util.{Try, Failure, Success}

  /** Реализуйте следующие функции.
   *
   * List(1, 2) match {
   *   case head :: tail => ???
   *   case Nil          => ???
   *   case l            => ???
   * }
   *
   * Option(1) match {
   *   case Some(a) => ???
   *   case None    => ???
   * }
   *
   * Either.cond(true, 1, "right") match {
   *   case Left(i)  => ???
   *   case Right(s) => ???
   * }
   *
   * Try(impureExpression()) match {
   *   case Success(a)     => ???
   *   case Failure(error) => ???
   * }
   *
   * Try(impureExpression()).toEither
   *
   */
  object Adts {

    // a) Дан List[Int], верните элемент с индексом n
    def getNth(list: List[Int], n: Int): Option[Int] = list match {
      case head::tail => Some(list(n))
      case Nil => null
      case l => Option(l(n))
    }

    // примените функцию из пункта (a) здесь, не изменяйте сигнатуру
    def testGetNth(list: List[Int], n: Int): Option[Int] = getNth(list, n)

    // b) Напишите функцию, увеличивающую число в два раза.

    def double(num: Option[Int]): Option[Int] = num match {
      case Some(n) => Option(n * 2)
      case None => null
    }

    // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
    def testDouble(n: Option[Int]): Option[Int] = double(n)

    // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").

    def isEven(n: Int): Either[String, Int] = Either.cond(n % 2 == 0, n, "Нечетное число") match {
        case Left(i)  => Left(i)
        case Right(s) => Right(s)
    }

    // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
    def testIsEven(n: Int): Either[String, Int] = isEven(n)

    // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").

    def safeDivide(a: Int, b: Int): Either[String, Int] = {
      Either.cond(b != 0, a / b, "Вы не можете делить на ноль") match {
        case Left(i)  => Left(i)
        case Right(s) => Right(s)
      }
    }

    // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
    def testSafeDivide(a: Int, b: Int): Either[String, Int] = safeDivide(a,b)

    // e) Обработайте исключения функции с побочным эффектом вернув 0.

    def goodOldJava(impure: String => Int, str: String): Try[Int] = Try(impure(str)) match{
      case Success(a) => Success(a)
      case Failure(error) => Failure(error)
    }

    // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
    def testGoodOldJava(impure: String => Int, str: String): Try[Int] = goodOldJava(impure, str)

  }



  /** Напишите вашу реализацию в тестовые функции.
   *
   * https://docs.scala-lang.org/overviews/collections/maps.html
   */
  object Maps {

    case class User(name: String, age: Int)

    /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
     *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
     */
    def testGroupUsers(users: Seq[User]): Map[String, Int] = {
      users.groupBy(_.name).mapValues(s => s.map(_.age).sum / s.length)
    }

    /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
     *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
     */
    def testNumberFrodos(map: Map[String, User]): Int = map.count(v => v._2.name.contains("Adam")) //(_._1 == "Adam") //contains

    /* c) Удалите всех пользователей возраст которых менее 35 лет.
     *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
     */
    def testUnderaged(map: Map[String, User]): Map[String, User] = map.filter(_._2.age > 35)
  }


  import scala.annotation.tailrec

  /** Напишите свои решения в тестовых функциях.
   *
   * Seq(1, 2) match {
   *   case head +: tail => ???
   *   case Nil          => ???
   *   case s            => ???
   * }
   *
   * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
   */
  // Примечание: напишите функции с хвостовой рекурсией

  object Sequence {

    /* a) Найдите последний элемент Seq.
     *
     */
    def testLastElement[A](seq: Seq[A]): Option[A] = {
      @tailrec
      def loop(seq: Seq[A], hd : A): Option[A] =
        seq match {
          case Nil => Option(hd)
          case head +: tail => loop(tail, head)
          case Seq(x) => Some(x)
        }
      loop(seq.tail, seq.head)
    }

    /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
     *
     */

    def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = a.zip(b)

    /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
     *
     */
    def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = seq.forall(cond)

    /* d) Проверьте, является ли Seq палиндромом
     *
     */

    def testPalindrom[A](seq: Seq[A]): Boolean = {
      @tailrec
      def loop(a: Seq[A], b: Seq[A]): Boolean = {
        a match {
          case head +: tail => loop(tail, b = head +: b)
          case Nil => seq.equals(b)
        }
      }
      loop(seq, Nil)
    }

    /* e) Реализуйте flatMap используя foldLeft.
     *
     */
    def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = {
      seq.foldLeft[Seq[B]](Seq())((enum, i) => enum ++: f(i) )
    }
  }


  /** Напишите ваши решения в тестовых функциях.
   *
   * https://www.scala-lang.org/api/2.12.3/scala/collection/immutable/StringOps.html
   */
  object Strings {

    /* a) Преобразуйте все символы типа Char в верхний регистр (не используйте заглавные буквы).
     *
     */
    def testUppercase(str: String): String = str.toUpperCase

    /* b) Вставьте следующие значения в строку:
     *       Hi my name is <name> and I am <age> years old.
     *
     */
    def testInterpolations(name: String, age: Int): String = s"Hi, my name is $name and I am $age years old."

    /* c) Добавьте два числа в следующую строку:
     *       Hi,
     *       now follows a quite hard calculation. We try to add:
     *         a := <value of a>
     *         b := <value of b>
     *
     *         result is <a + b>
     *
     *
     */
    def testComputation(a: Int, b: Int): String =
      s"Hi,\nnow follows a quite hard calculation. We try to add:\na := $a \nb := $b \nresult is ${a + b}"

    /* d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.
     */
    def testTakeTwo(str: String): String = if (str.length == 2) str else str.take(2)
  }

  override def main(args: Array[String]): Unit = {
    println("Adts\na)")
    println(Adts.testGetNth(List(1, 2, 3, 4, 5), 3))
    println(Adts.testGetNth(Nil, 10))
    println("b)")
    println(Adts.testDouble(Option(6)))
    println("c)")
    println(Adts.testIsEven(2))
    println(Adts.testIsEven(7))
    println("d)")
    println(Adts.testSafeDivide(8, 2))
    println(Adts.testSafeDivide(6, 0))
    println("e)")
    def fail(str: String): Int = str.toInt / 0
    def success(str: String): Int = str.toInt
    println(Adts.testGoodOldJava(success, "123"))
    println(Adts.testGoodOldJava(fail, "123"))

    println("\nMaps\na)")
    println(Maps.testGroupUsers(Seq(Maps.User("Diana", 20), Maps.User("Diana", 18), Maps.User("Alexandra", 22), Maps.User("Samuel", 19))))
    println("b)")
    val map = Map("First" -> Maps.User("George", 20), "Second" -> Maps.User("Adam", 30), "Third" -> Maps.User("Adamir", 63))
    println(Maps.testNumberFrodos(map))
    println("c)")
    println(Maps.testUnderaged(map))

    println("\nSequence\na)")
    println(Sequence.testLastElement(Seq(1, 2, 3, 4, 5)))
    println("b)")
    println(Sequence.testZip(Seq(1, 2), Seq(3, 4)))
    println(Sequence.testZip(Seq(3, 4, 5), Seq(6, 7, 8, 9)))
    println("c)")
    def test(num:Int): Boolean = num < 10
    println(Sequence.testForAll(Seq(1, 5, 9))(test))
    println(Sequence.testForAll(Seq(1, 13, 5))(test))
    println("d)")
    println(Sequence.testPalindrom(Seq(1, 2, 5, 2, 1)))
    println(Sequence.testPalindrom(Seq(1, 2, 10, 12, 1)))
    println("e)")
    def func(in: String): Seq[Char] = in.toUpperCase
    println(Sequence.testFlatMap(Seq("One", "Two", "Three"))(func))

    println("\nStrings\na)")
    println(Strings.testUppercase("upper"))
    println("b)")
    println(Strings.testInterpolations("Diana", 20))
    println("c)")
    println(Strings.testComputation(6, 4))
    println("d)")
    println(Strings.testTakeTwo("to"))
    println(Strings.testTakeTwo("test"))
  }
}
