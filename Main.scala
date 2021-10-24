object Lab1{
	def main(args: Array[String]): Unit = {

	z1()
	z2()
	z3()
	z6()
	z7()
	z8()
	z9(3)
	z9(-7)
	z9(0)
	z10()
	z11()
	z12(10)
	z13()
	z16("Hello")
	z17(5, 0)
	z17(5, 2)
	z18(10, 20)
	z19(List(List(1, 1), 2, List(3, List(5, 8))))
	z20(110)
	z21(List(17,18,19),3)
	z24(15,20)
	z25(List(1,2,3,4,5,6,7,8,9),4)
	z26(3,2)
	z27(List(1,2,3,4,5,6,7,8,9),-1)
	z28(100)
	z29(List(1,2,3,4,5,6,7,8,9))
	z30(100)
	z31(List(List(1,"a"),List(2,"b"),List(3,"c")))
	}
	
	import scala.BigInt.probablePrime
	import scala.util.Random
	
	def z1()={
		println("N 1")
		println("val res = 1" +"\n"+"res = 2"+"\n"+"[error]   |     ^^^^^^^"+"\n"+"[error]   |     Reassignment to val res" + "\n"+"[error] one error found")
	}

	def z2()={
		println("N 2")
		println("crazy" * 3)
	}
	
	def z3()={
		println("N 3")
		println("A max B return A if A > B else return B" + "\n" + "prinadlezhit RichInt")
	}
	
	def z4()={
		println("N 4")
		println(BigInt(2).pow(1024)) 
	}
	
	def z5()={
		println("N 5")
		println(probablePrime(100, Random))
	}
	
	def z6()={
		println("N 6")
		println(probablePrime(100, Random).toString(36))
	}
	
	def z7()={	
		println("N 7")
		val s = "Diana"
		println(s.head)
		println( s.last)
	}
	
	def z8()={
		println("N 8")
		val d = "Diana"
		println(d.take(2))
		println(d.drop(2))
		println(d.takeRight(2))
		println(d.dropRight(2))
		println("преимущество: соединение методов")
		println(d.drop(1).take(3))
	}
	
	def z9(n: Int): Int = {
		println("N 9")
		if (n > 0) 1 else if (n < 0) -1 else 0
	}
	
	def z10() = {
		println("N 10")
		println("значение одно - ()"+"\n"+"assert( {} == () )"+"\n"+"тип Unit"+"\n"+"assert({}.isInstanceOf[Unit])")
	}
	
	def z11() = {
		println("N 11")
		println("Java："+"\n"+"for(int i=10;i>=0;i-)System.out.println(i);"+"\n"+"Scala:"+"\n"+"for (i <- 10.to(0, -1)) println(i)")
		for (i <- 10.to(0, -1)) println(i)
	}
	
	def z12(n: Int)={
		println("N 12")
		for(i <- n.to(0, -1)) println(i)
    }
	

	def z13() = {
		println("N 13")
		var result: Long = 1
		for (c <- "Hello") result *= c.toLong
		println(result)
	}
	
	def z14() = {
		println("N 14")
		println("Hello".map(_.toLong).product)
	}
	
	def z16(s: String): Long = {
		println("N 16")
		if (s.tail != "") s.head.toLong * z16(s.tail) else s.head.toLong
	}
	
	def z17(x:Double,n:Int):Double={
		println("N 17")
		if(n == 0) 1
		else if(n>0) x * z17(x,n-1)
		else 1/z17(x,-n)
	}
	
	def distinctDigits(n: Int): Boolean = {
		val s = n.toString
		s.length == s.distinct.length
	}

	def z18(m: Int, n: Int) = {
		println("N 18")
		println((m to n).filter(distinctDigits).sum)
	}

	def flatlist[U](l: List[U]): List[U] = l match {
		case Nil => Nil
		case (x: List[U]) :: tail => flatlist(x) ::: flatlist(tail)
		case x :: tail => x :: flatlist(tail)
	}

	def z19(l: List[Any]) = {
		println("N 19")
		println(flatlist(l))
	}
	
	def z20(s: Int) = {
		println("N 20")
		var s1:Int = s
		var i:Int = 2
		while(i != s1)
		if (s1 % i == 0) s1 /= i else i += 1
		var sum = 0
		while(s1 > 10){
		sum += (s1 % 10)
		s1 /= 10
		}
		sum += s1
		println(sum)
	}

	def z21(l: List[Int], c: Int)={
		println("N 21")
		var biglist:List[Int] = List()
		var ind:Int = 0
		for (el <- l)
		biglist = biglist:::List.fill(c)(el)
		println(biglist)
	}
	
	def nod(a: Int,b: Int): Int = {
		if(b ==0) a else nod(b, a%b)
	}

	def z24(m:Int, n:Int)={
		println("N 24")
		println((m * n) / nod(m, n))
	}
	
	def z25(l: List[Int], c: Int)={
		println("N 25")
		var l1:List[Int] = List()
		for ((x, i) <- l.zipWithIndex)
		if ((i+1)%c !=0)
		l1 = l1:+x
		println(l1)
	}
	
	def z26(n:Int, k:Int) = {
		println("N 26")
		var a:Int = 1
		for (i <- 2 to n) a*=i
		for (i <- 2 to (n-k)) a/=i
		println(a)
	}
	
	def z27(l: List[Int], c: Int)={
		println("N 27")
		var l1:List[Int] = l
		var c1:Int = c
		if (c1 < 0) c1 = l1.length + c1
		for (i <- 1 to c1) l1 = l1.tail:+l1.head
		println(l1)
	}
	
	def z28(n:Int) = {
		println("N 28")
		var m:Int = 0
		for (i <- 1 to n){
		var s:Int = 0
		for (j <- 1 to i-1) if (i%j == 0) s+=j
		if (s == i) m = i
		}
		println(m)
	}
	
	def z29(l: List[Int])={
		println("N 29")
		var c:List[Int] = List()
		var n:List[Int] = List()
		for ((x, i) <- l.zipWithIndex)
		if (i%2 ==0) c = c:+x else n = n:+x
		println(c)
		println(n)
	}
	
	def sc(n:Int): Int = {
		var sum:Int = 0
		var i:Int = n
		while(i >= 10){
		sum += (i % 10)
		i /= 10
		}
		sum += i
		sum
	}
	
	def z30(n:Int) = {
		println("N 30")
		var m:Int = 0
		for (i <- 1 to n){
		var s:Int = sc(i)
		if (s != 1) {
			while(s<i)
			s*=s
			if (s == i) m = i
			}
		}
		println(m)
	}
	

	def z31(l: List[List[Any]])={
		println("N 31")
		val (ln, ls) = l.unzip(el => (el(0), el(1)))
		println(ln)
		println(ls)
	}
}

