package fp.example

object Functions {

  /**
    * Extension methdodology or syntax. This allows
    * to call like this:
    * {{
	*	(1.0).sin
    *}}
    */
  implicit class DoupleOps1(x: Double) {
    def sin: Double = java.lang.Math.sin(x)
  }

  /**
    * Implicit class is the same as implicit converions:
    */
  implicit def DoubleOps2(x: Double): DoubleOps2 = new DoubleOps2(x)
  class DoubleOps2(x: Double) {
    def sin: Double = java.lang.Math.sin(x)
  }

  /**
    * This avoids creating new class and reduce GC pressure.
    */
  implicit final class DoubleOps(private val x: Double) extends AnyVal {
    def sin: Double = java.lang.Math.sin(x)
  }

  /**
    * Ordering typeclass with `compare` being primitive combinator.
    */
  trait Ordering[T] {
    def compare(x: T, y: T): Int

    def lt(x: T, y: T): Boolean = compare(x, y) < 0
    def gt(x: T, y: T): Boolean = compare(x, y) > 0
  }

  /**
    * Numeric typeclass extending Ordering.
    */
  trait Numeric[T] extends Ordering[T] {
    def plus(x: T, y: T): T
    def times(x: T, y: T): T
    def negate(x: T): T
    def zero: T

    def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  }

  object Numeric {
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

    /**
  	* This can be generated by `simulacrum.@typeclass`.
  	*/
    object ops {
      implicit class NumericOps[T](t: T)(implicit N: Numeric[T]) {
        def +(o: T): T = N.plus(t, o)
        def *(o: T): T = N.times(t, o)
        def unary_- : T = N.negate(t)
        def abs: T = N.abs(t)

        def <(o: T): Boolean = N.lt(t, o)
        def >(o: T): Boolean = N.gt(t, o)
      }
    }
  }

  /**
    * Function for type, that **have** a Numeric typeclass.
    */
  def signOfTheTimes[T](t: T)(implicit N: Numeric[T]): T = {
    import N._
    times(negate(abs(t)), t)
  }

  /**
    * Context bound demand to have implicit Numeric[T]
    * available. But this forces the usage of `implicitly`.
	*/
  def signWithContextBounds[T: Numeric](t: T) = {
    val N = implicitly[Numeric[T]]
    import N._
    times(negate(abs(t)), t)
  }

  def signWithCompanionObject[T: Numeric](t: T) = {
    // This forces to use `apply` from companion
    // object, which expects implicit Numeric instance.
    val N = Numeric[T]
    import N._
    times(negate(abs(t)), t)
  }

  /**
    * Usage of implicit conversions gives much cleaner API.
    */
  def signWithOps[T: Numeric](t: T): T = {
    import Numeric.ops._
    -(t.abs) * t
  }

  /**
    * The instance of the typeclass for the Double. The
    * operators defined in the class takes preference over
    * the extension methods that we defined in ops.
    */
  implicit val NumericDouble: Numeric[Double] = new Numeric[Double] {
    def plus(x: Double, y: Double) = x + y
    def times(x: Double, y: Double) = x * y
    def negate(x: Double) = -x
    def zero = 0.0
    def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)

    // optimised
    override def lt(x: Double, y: Double) = x < y
    override def gt(x: Double, y: Double) = x > y
    override def abs(x: Double): Double = java.lang.Math.abs(x)
  }

  import java.math.{BigDecimal => BD}

  implicit val NumbericBD: Numeric[BD] = new Numeric[BD] {
    def plus(x: BD, y: BD): BD = x.add(y)
    def times(x: BD, y: BD): BD = x.multiply(y)
    def negate(x: BD): BD = x.negate
    def zero: BD = BD.ZERO
    def compare(x: BD, y: BD): Int = x.compareTo(y)
  }

  final case class Complex[T](r: T, i: T)

  /**
    * Derive Numeric instance for Complex if Numeric[T] exists.
    */
  implicit def numericComplex[T: Numeric]: Numeric[Complex[T]] =
    new Numeric[Complex[T]] {
      type CT = Complex[T]
      import Numeric.ops._
      def plus(x: CT, y: CT) = Complex(x.r + y.r, x.i + y.i)
      def times(x: CT, y: CT) =
        Complex(x.r * y.r + (-x.i * y.i), x.r * y.i + x.i * y.r)
      def negate(x: CT) = Complex(-x.r, -x.i)
      def zero = Complex(Numeric[T].zero, Numeric[T].zero)
      def compare(x: CT, y: CT): Int = {
        val real = (Numeric[T].compare(x.r, y.r))
        if (real != 0) real
        else Numeric[T].compare(x.i, y.i)
      }
    }
}
