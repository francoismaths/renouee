import org.apache.commons.math3.random._

import scala.util.Random

package object renouee {

  implicit def scalaRandomToApache(random: Random) = new RandomGenerator {
    override def nextFloat(): Float = random.nextFloat()
    override def nextInt(): Int = random.nextInt()
    override def nextInt(n: Int): Int = random.nextInt(n)
    override def nextLong(): Long = random.nextLong()
    override def nextGaussian(): Double = random.nextGaussian()
    override def nextDouble(): Double = random.nextDouble()
    override def nextBytes(bytes: Array[Byte]): Unit = random.nextBytes(bytes)
    override def nextBoolean(): Boolean = random.nextBoolean()
    override def setSeed(seed: Int): Unit = random.setSeed(seed)
    override def setSeed(seed: Array[Int]): Unit = ???
    override def setSeed(seed: Long): Unit = random.setSeed(seed)
  }


  def random(seed: Long) = new util.Random(new RandomAdaptor(new Well44497b(seed)))

}
