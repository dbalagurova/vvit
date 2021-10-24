//import Classes.Animal.{cat, goldfish, parrot}
import PatternMatching.{Animal, Food}

/*=============================================================================*/

/** Напишите ваши решения в виде функций. */
object HigherOrder {

  val plus: (Int, Int) => Int = _ + _
  val multiply: (Int, Int) => Int = _ * _

  /* a) Напишите функцию, которая принимает `f: (Int, Int) => Int`, параменты `a` и `b`
   *    и коэффициент умножения `n` и возвращает n * f(a, b). Назовите `nTimes`.
   */
  def nTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = n * f(a, b)

  // примените вашу функцию (a) здесь, не изменяйте сигнатуру
  def testNTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = nTimes(f, a, b, n)

  /* b) Напишите анонимную функцию, функцию без идентификатора ((a, b) => ???) для `nTimes` которая
   *    выполняет следующее:
   *          if (a > b) a else b
   */
  val more: (Int, Int) => Int = (_).max(_)

  def testAnonymousNTimes(a: Int, b: Int, n: Int): Int = nTimes(more, a, b, n)
}

/*=============================================================================*/

/** Напишите отдельные функции, решающие поставленную задачу. 
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
object Functions {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  def Circle(r: Double): Double = r * r * Math.PI

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = Circle(r)


  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def RectangleCurried(a: Double) = (b: Double) => a * b

  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = RectangleCurried(a)(b)

  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  def RectangleUc(a: Double, b: Double): Double = a * b

  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = RectangleUc(a, b)
}

/*=============================================================================*/

/* 
 * 
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 * 
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 *
 * b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
 *      - cat, mammal, meat
 *      - parrot, bird, vegetables
 *      - goldfish, fish, plants
 * 
 *    Синтаксис: object MyClass {
 *              // статические поля и методы
 *            }
 * 
 * c) Добавьте следующие метод в Animals:
 *      def eats(food: String): Boolean
 *    
 *     который проверяет ест ли животное определенную пищу
 * 
 * d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
 *    Вам все еще нужно поле `species`?
 * 
 * e) Добавьте следующие функции в объект-компаньон Animal:
 *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
 *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
 * 
 * f) Создайте трейт Food со следующими классами-образцами:
 *      - Meat
 *      - Vegetables
 *      - Plants
 *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
 *      def apply(food: String): Option[Food]
 */
//object Classes {

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  object Food {
    def apply(food: String): Option[Food] = food match {
      case "meat" => Some(Meat)
      case "vegetables" => Some(Vegetables)
      case "plants" => Some(Plants)
      case _ => None()
    }
  }

  sealed trait Animal {

    val name: String
    val species: String
    val food: Food

    def eats(food: Food): Boolean = food == this.food

    override def toString: String = {
      "Name: " + name + ", Food: " + food
    }
  }
  case class Mammal(name: String, species: String = "mammal", food: Food) extends Animal
  case class Fish(name: String, species: String = "fish", food: Food) extends Animal
  case class Bird(name: String, species: String = "bird", food: Food) extends Animal

  object Animal {
    val cat: Animal = new Animal {
      override val name: String = "cat"
      override val species: String = "mammal"
      override val food: Food = Meat
    }
    val parrot: Animal = new Animal {
      override val name: String = "parrot"
      override val species: String = "bird"
      override val food: Food = Vegetables
    }
    val goldfish: Animal = new Animal {
      override val name: String = "goldfish"
      override val species: String = "fish"
      override val food: Food = Plants
    }
    def knownAnimal(name: String): Boolean = name match {
      case "cat" | "parrot" | "goldfish" => true
      case _ => false
    }
    def apply(name: String): Option[Animal] = name match {
      case "cat" => Some(cat)
      case "parrot" => Some(parrot)
      case "goldfish" => Some(goldfish)
      case _ => None()
    }
  }

  sealed trait Option[A] {
    def isEmpty: Boolean
  }

  case class Some[A](a: A) extends Option[A] {
    val isEmpty = false
  }

  case class None[A]() extends Option[A] {
    val isEmpty = true
  }
//}
/*=============================================================================*/

/** Напишите решение в виде функции. 
  * 
  * Синтаксис:
  *   val a: Int = ???
  * 
  *   a match {
  *     case 0 => true
  *     case _ => false
  *   }
  */
object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    val food: Food

    override def toString: String = {
      "Name: " + name + ", Food: " + food
    }
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal

  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */
  def IntToString(value: Int): String = value match {
      case 1 => "it is one"
      case 2 => "it is two"
      case 3 => "it is three"
      case _ => "what's that"
  }

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = IntToString(value)

  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max
   *     "moritz" или "Moritz"
   */
  def IsMaxAndMoritz(value: String): Boolean = value match {
      case "max" | "Max" |  "moritz" | "Moritz" => true
      case _ => false
  }
  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = IsMaxAndMoritz(value)

  // c) Напишите функцию проверки является ли `value` четным 
  def IsEven(value: Int): Boolean = (value%2) match {
      case 0 => true
      case 1 => false
  }
  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = IsEven(value)

    /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */
  def WinsA(a: Hand, b: Hand): Result = a match {
      case Rock => b match {
          case Rock => Draw
          case Paper => Lose
          case Scissor => Win
      }
      case Paper => b match {
        case Rock => Win
        case Paper => Draw
        case Scissor => Lose
      }
      case Scissor => b match {
        case Rock => Lose
        case Paper => Win
        case Scissor => Draw
      }
  }
  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = WinsA(a, b)

  // Примечание: используйте определение Animals
  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.
  def ExtractMammalWeight(animal: Animal): Int = animal match {
      case animal: Mammal => animal.weight
      case _ => -1
  }
  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = ExtractMammalWeight(animal)

  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.
  def UpdateFood(animal: Animal): Animal = animal match {
    case animal: Fish => new Animal {
      override val name: String = animal.name
      override val food: Food = Plants
    }
    case animal: Bird => new Animal {
      override val name: String = animal.name
      override val food: Food = Plants
    }
    case animal: Mammal => animal
  }
  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = UpdateFood(animal)

}


object Lab2 {

  def main(args: Array[String]) = {

    println(HigherOrder.testNTimes(HigherOrder.plus, 6,5,2))
    println(HigherOrder.testAnonymousNTimes(6,5,2))

    println(Functions.testCircle(1.75))
    println(Functions.testRectangleCurried(3, 4))
    println(Functions.testRectangleUc(4, 3))

    println(PatternMatching.testIntToString(3))
    println(PatternMatching.testIsMaxAndMoritz("Moritz"))
    println(PatternMatching.testIsEven(3))
    println(PatternMatching.testWinsA(PatternMatching.Scissor, PatternMatching.Paper))
    println(PatternMatching.testExtractMammalWeight(new PatternMatching.Mammal("I", PatternMatching.Vegetables, 60)))
    println(PatternMatching.testUpdateFood(new PatternMatching.Fish("Akula", PatternMatching.Meat)))
  }
}
