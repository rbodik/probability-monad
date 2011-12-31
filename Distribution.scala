import scala.util.Random
import scala.annotation.tailrec

trait Distribution[A] {
  self =>
  protected def get: A

  override def toString = "<distribution>"

  def map[B](f: A => B): Distribution[B] = new Distribution[B] {
    override def get = f(self.get)
  }

  def flatMap[B](f: A => Distribution[B]): Distribution[B] = new Distribution[B] {
    override def get = f(self.get).get
  }

  def filter(pred: A => Boolean): Distribution[A] = new Distribution[A] {
    @tailrec
    override def get = {
      val s = self.get
      if (pred(s)) s else this.get
    }
  }

  def given(pred: A => Boolean): Distribution[A] = filter(pred)

  def until(pred: List[A] => Boolean): Distribution[List[A]] = new Distribution[List[A]] {
    override def get = {
      @tailrec
      def helper(sofar: List[A]): List[A] = {
	if (pred(sofar)) sofar
	else helper(self.get :: sofar)
      }
      helper(Nil)
    }
  }

  def repeat(n: Int): Distribution[List[A]] = until(_.length == n)
  
  private val nn = 1000

  def p(pred: A => Boolean, given: A => Boolean = (a: A) => true): Double = 1.0 * filter(given).sample(nn).count(pred) / nn

  // NB: Expected value only makes sense for real-valued distributions. If you want to find the expected
  // value of a die roll, for example, you have to do die.map(_.toDouble).ev.
  def ev(implicit f: Fractional[A]): A = f.div(sample(nn).foldLeft(f.zero)(f.plus), f.fromInt(nn))

  def sample(n: Int = nn): List[A] = List.fill(n)(self.get)

  def zip[B](d: Distribution[B]): Distribution[(A, B)] = new Distribution[(A, B)] {
    override def get = (self.get, d.get)
  }

  def zipWith[B, C](d: Distribution[B])(f: (A, B) => C): Distribution[C] = new Distribution[C] {
    override def get = f(self.get, d.get)
  }

  def +(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.plus(self.get, d.get)
  }
  def -(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.minus(self.get, d.get)
  }
  def *(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.times(self.get, d.get)
  }
  def /(d: Distribution[A])(implicit f: Fractional[A]): Distribution[A] = new Distribution[A] {
    override def get = f.div(self.get, d.get)
  }

  def hist = {
    this.sample(nn).groupBy(x=>x).mapValues(_.length.toDouble / nn)
  }
}

object Distribution {
  val rand = new Random()

  def always[A](value: A) = new Distribution[A] {
    override def get = value
  }

  object u extends Distribution[Double] {
    override def get = rand.nextDouble()
  }

  object normal extends Distribution[Double] {
    override def get = rand.nextGaussian()
  }

  sealed abstract class Coin
  case object H extends Coin
  case object T extends Coin
  def coin = discreteUniform(List(H, T))
  def biasedCoin(p: Double) = discrete(List(H -> p, T -> (1-p)))

  def d(n: Int) = discreteUniform(1 to n)
  def die = d(6)
  def dice(n: Int) = die.repeat(n)
  
  def tf(p: Double = 0.5) = discrete(List(true -> p, false -> (1-p)))

  def uniform(lo: Double, hi: Double) = u.map(x => (x * (hi - lo)) + lo)

  def discreteUniform[A](values: Iterable[A]): Distribution[A] = new Distribution[A] {
    private val vec = Vector() ++ values
    override def get = vec(rand.nextInt(vec.length))
  }

  def discrete[A](weightedValues: Iterable[(A, Double)]): Distribution[A] = new Distribution[A] {
    val len = weightedValues.size
    val scale = len / weightedValues.map(_._2).sum
    val scaled = weightedValues.map{ case (a, p) => (a, p * scale) }.toList
    val (smaller, bigger) = scaled.partition(_._2 < 1.0)
    private def alias(smaller: List[(A, Double)], bigger: List[(A, Double)]): List[(A, Double, Option[A])] = {
      smaller match {
	case Nil => bigger.map{ case (a, _) => (a, 1.0, None) }
	case (s, sp)::ss => {
	  val (b, pb)::bb = bigger
	  val remainder = (b, pb - (1.0 - sp))
	  val rest = if (remainder._2 < 0.9999) alias(remainder :: ss, bb) else alias(ss, remainder :: bb)
	  (s, sp, Some(b)) :: rest
	}
      }
    }
    val table = alias(smaller, bigger)
    private def select(p1: Double, p2: Double, table: List[(A, Double, Option[A])]): A = {
      table((p1 * len).toInt) match {
	case (a, _, None) => a
	case (a, p, Some(b)) => if (p2 <= p) a else b
      }
    }
    override def get = {
      select(u.get, u.get, table)
    }
  }

  def poisson(lambda: Double): Distribution[Int] = new Distribution[Int] {
    override def get = {
      val m = math.exp(-lambda)
      val s = Stream.iterate(1.0)(_ * u.get)
      s.tail.takeWhile(_ > m).length
    }
  }

  def chi2(n: Int) = List.fill(n)(normal).map(x => x*x).reduceLeft[Distribution[Double]](_ + _)

  lazy val cauchy = normal / normal
}