package renouee

import better.files.File
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

object test_Main extends App {


  //val res = createInitialPop.createPopIni(10, Plant.tau, Plant.proportionMowing, 10.0, Plant.L, 0.6,0.4).plants.length

  //println(res)

  /*
  val res = createInitialPop.createInitialPopEvolution(10,Plant.tau, Plant.proportionMowing, 10.0, Plant.L, 0.3,0.7)

  val f = File("test")
  createfileforR.writeFinalPop(f,res)
   */

  val rng = new RandomAdaptor(new Well44497b(43))
  implicit val rnd = new scala.util.Random(7)


  val res1 = fieldutil.mowing_effect(0.5)(0.4)(3)(rng)

    val res2 = fieldutil.mowing_effect(0.5)(0.4)(3)(rng)

    println(res1,res2)

}