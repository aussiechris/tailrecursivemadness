package cosc250.recursion

import scala.annotation.tailrec



object TailRecursiveMadness {

  /**
    * Let's start off with some non-tail recursive exercises.
    *
    * Just implement these as ordinary recusions
    */
  object NonTail {

    /**
      * triangular number 4 is 1 + 2 + 3 + 4
      * triangular number n is 1 + 2 + 3 + ... n
      */
    def triangular(n:Int):Int = {
      if (n <= 1) 1 else n + triangular(n - 1)
    }

    /**
      * factorial(4) is 1 * 2 * 3 * 4
      * factorial(n) is 1 * 2 * 3 * ... n
      */
    def factorial(n:Int):Int = {
      if (n <= 1) 1 else n * factorial (n-1)
    }

    /**
      * The Fibonacci sequence begins
      * 0 1 1 2 3 5 8 13 21 36
      * with fib(n) = fib(n-1) + fib(n-2) for n > 1
      *
      * It gets big very quickly, so I'm using BigInt
      * BigInt(3) produces a BigInt containing 3
      * BigInt(3) + BigInt(2) = BigInt(5)
      * But unlike Int, summing BigInts has no maximum value.
      */
    def fibonacci(n:Int):BigInt = {
      if (n<=0) {
        BigInt(0)
      } else if (n==1) {
        BigInt(1)
      } else {
        fibonacci(n-1) + fibonacci(n-2)
      }
    }

    // Puzzle: try calling it with a large number - say 100. What breaks?
    // (Comment out this line, and then run the tests. The println will be run as part of the object
    // initialisation for the NonTail object)
    // println(fibonacci(10))

    /**
      * Let's do one using Lists
      *
      * Pascal's triangle goes
      *     1
      *    1 1
      *   1 2 1
      *  1 3 3 1
      * 1 4 6 4 1
      *
      * To calculate the next line, take
      * left = 0 +: pascal(n-1)     // +: is append
      * right = pascal(n=1) :+ 0    // :+ is append at the other end
      *
      * That gives the list twice, offset by one.
      * Now add them up element by element to get pascal(n)
      */
    def pascal(n:Int):List[Int] = {

      // You'll need a function that can sum tuples in a list
      // Let's define that recursively for you
      def sumPairs(l:List[(Int, Int)]):List[Int] = {
        l match {
          case (a,b) :: t => (a + b) :: sumPairs(t)
          case Nil => Nil
        }
      }

      // Now write the body of pascal(n)
      if (n<=0) {
        List(1)
      } else {
        val prev = pascal(n - 1)
        val left = 0 +: prev
        val right = prev :+ 0
        val zipped = left.zip(right)
        sumPairs(zipped)
      }
    }
  }

  object Tail {

    /**
      * Let's now make factorial use an inner tail recursive function
      */
    def triangular(n:Int) = {

      /**
        * The key is realising that as we can't do the calculation on the returned
        * value, so we have to do it on the arguments that are passed in.
        */
      @tailrec
      def innerTri(n:Int, accum:Int = 0):Int = {
        if (n <= 0) accum else innerTri(n - 1, accum + n)
      }

      innerTri(n, 0)
    }

    /**
      * Your turn - try making a tail recursive factorial
      * @param n
      * @return
      */
    def factorial(n:Int):Int = {

      @tailrec
      def fac(n:Int, accum:Int):Int = {
        if (n <= 0) accum else fac(n-1,accum*n)
      }
      fac(n,1)
    }

    /**
      * Now for a tail-recursive fibonacci
      *
      */
    def fibonacci(n:Int):BigInt = {

      // For this one, we have to count down but calculate upwards
      // ie, start at n=k but a=0 b=1
      // then n=k-1, a=1 b=1
      // then n=k-2, a=1 b=2
      // then n=k-3, a=2 b=3
      // until you reach n=0
      @tailrec
      def fibInt(n:Int, a:BigInt=0, b:BigInt=1):BigInt = {
        if (n<=0) {
          a
        } else if (n==1) {
          b
        } else {
          fibInt(n-1,b,a+b)
        }
      }
      fibInt(n)
    }

    // Try fibonacci(60), but not too large or it will take a really long time.
    // println(fibonacci(1000000))

    /**
      * Now for a tail recursive Pascal's triangle
      */
    def pascal(n:Int):List[Int] = {


      /**
        * You're going to need a function that can sum a List[(Int, Int)].
        * Let's do that tail recursively for you
        */
      @tailrec
      def sumPairs(source: List[(Int, Int)], dest: List[Int] = Nil): List[Int] = {
        // Notice we build up the destination by adding to the head.
        // This gives the list in reverse, so we have to reverse it in the terminal case
        source match {
          case (a, b) :: tail => sumPairs(tail, (a + b) :: dest)
          case Nil => dest.reverse
        }
      }

      @tailrec
      def pas(n: Int,prev:List[Int]=List(1)): List[Int] = {
        if (n <= 0) {
          prev
        } else {
          val left = 0 +: prev
          val right = prev :+ 0
          val zipped = left.zip(right)
          pas(n-1,sumPairs(zipped))
        }
      }

      pas(n)

    }

  }

  /**
    * Oh no, it's the Roman numerals again.
    *
    * But that means I can put them in the solution!
    * Let's define a tail recursive function for producing Roman numerals
    */
  object Roman {

    /*
     * I've put these in a List so we can recursively take the head off it, and work
     * from big numerals to small.
     */
    val allNumerals = List(
      "M" -> 1000, "CM" -> 900,
      "C" -> 100, "XC" -> 90,
      "L" -> 50, "XL" -> 40,
      "X" -> 10, "IX" -> 9,
      "V" -> 5, "IV" -> 4, "I" -> 1
    )

    /**
      * First, define a tail recursive function that will select the first (numeral, number) pair
      * from our list of numerals that is smaller than n.
      *
      * Keep the (numeral, value)s that are yet to be considered in the second argument.
      *
      * This is going to be our function for choosing the nextNumeral
      */
    @tailrec
    def nextNumeral(n:Int, numerals:List[(String, Int)] = allNumerals):(String, Int) = {
      // either the head pair is the one we want, or call ourselves recursively for the tail
      if (numerals.head._2<=n) numerals.head else nextNumeral(n,numerals.tail)
    }

    /**
      * Now we'll define our Roman numerals function
      */
    def roman(n:Int):String = {

      // Now define an inner tail recursive function that will build up our Roman numeral
      // We keep the string we've built so far in s
      @tailrec
      def intRom(n:Int, s:String = ""):String = {
        if (n<=0) s else {
          val currentNumerals = nextNumeral(n,allNumerals)
          intRom(n-currentNumerals._2,s+currentNumerals._1)
        }
      }
      intRom(n)
    }


  }


  /**
    * A puzzle list
    *
    * 1
    * 1 1
    * 2 1
    * 1 2 1 1
    * 1 1 1 2 2 1
    *
    * What's happening?
    * Well, the top line has "one 1"
    * And the next has "two 1s"
    * And the next has "one 2, one 1"
    * And the next has "one 1, one 2, two 1s"
    *
    */
  object Puzzle {

    /**
      * Start off by defining a tail-recursive function that can produce the next line from the
      * previous one. It's going to consume a "source" list and produce a "dest" list.
      * The source list will get shorter each step until we've worked our way through it.
      * We'll build up the dest list *backwards* because it's easier to append to the head of a list.
      *
      * Our terminal condition is if the source list is empty.
      * If not, have two outer cases. Is the destination list empty or not?
      * If not, does the number at the head of the source list match or not match dest(1)?
      */
    @tailrec
    def nextLine(source:List[Int], dest:List[Int] = Nil):List[Int] = {
//      source match {
//        case Nil => dest.reverse
//        case sh :: st => dest match {
//          case dh :: dn :: dt if (dh==sh) => nextLine(st,dh::dn+1::dt)
//          case _ => nextLine(st,sh::1::dest)
//        }
//      }
      if (source.isEmpty) dest.reverse
      else if (dest.isEmpty || source.head!=dest.head) nextLine(source.tail, source.head :: 1 :: dest)
      else nextLine(source.tail, dest.head :: dest(1)+1 :: dest.tail.tail)
    }

    /**
      * And now we write our puzzle call
      */
    def puzzle(n:Int):List[Int] = {

      @tailrec
      def intPuz(n:Int, line:List[Int] = List(1)):List[Int] = {
        if (n<=0){
          line
        } else {
          intPuz(n-1,nextLine(line))
        }
      }

      intPuz(n)
    }


  }


}
