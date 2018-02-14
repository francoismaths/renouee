package renouee

import better.files.File
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

object test_Main extends App {

  implicit val rng = random(2)  // function created in the package object renouee



  //val res = createInitialPop.createPopIni(10, Plant.tau, Plant.proportionMowing, 10.0, Plant.L, 0.6,0.4).plants.length

  //println(res)

/*
  val res = createInitialPop.createInitialPopEvolution(10,Plant.tau, Plant.proportionMowing, 10.0, Plant.L, 0.3,0.7)

  val f = File("test4")
  createfileforR.writeFinalPop(f,res)
 */

  /*
  val rng = new RandomAdaptor(new Well44497b(43))
  implicit val rnd = new scala.util.Random(7)


  val res1 = fieldutil.mowing_effect(0.5)(0.4)(3)(rng)

    val res2 = fieldutil.mowing_effect(0.5)(0.4)(3)(rng)

    println(res1,res2)
  */

/*
  val res = test.foncTest(TestClassParam(bidule = Seq(12)),Result1.All)
  println(res)
*/


/*
  val res = createInitialPop.createPopIni(500,Plant.tau, Plant.proportionMowing,Plant.K,Plant.L,0.5)

  val f = File("test5")
  createfileforR.writeFinalPop(f,res)
*/


  ///////////////  test management technique   ////////////////

/*

  val b2 =  PlantEvolution(Seq( Plant(0,0,1),  Plant(-1,0,2)  , Plant(10,12,3)  , Plant(2,2,4)).toVector
    , Seq(InfosEvolution())  )

  //val res = ManagementTechniqueUtil.mowingAlea(b2,Management(proportionMowing = 0.5), PlantGrowth(), ResultType.Last, 2 )

  //val res = ManagementTechniqueUtil.mowingSideProportion(b2,Management(proportionMowing = 0.5), PlantGrowth(), ResultType.Last, 2 )

  //val res = Evolution.nextEvolution(b2,Management(),PlantGrowth(),ResultType.Last,ManagementTechnique.Alea)

  val b = createInitialPop.createInitialPopEvolution(5,parameter.Nmax,Management(),PlantGrowth())

  val res = Run.simu(b,Management(),PlantGrowth(),ResultType.Last,ManagementTechnique.Alea)

  val testH = File("ttini.txt")
  createfileforR.writeFinalPop(testH,b)

  val testF = File("tttt.txt")
  createfileforR.writeFinalPop(testF,res)

  println(res)


  val popIni = createInitialPop.createInitialPopEvolution(5,parameter.Nmax, Management(proportionMowing = 0.9, tau = 1.0 ),PlantGrowth())

  //val resAlea = ManagementTechniqueUtil.mowingAlea(popIni,Management(proportionMowing = 0.7),PlantGrowth(),ResultType.Last,1)

  val resEvolAleaT = Run.simu(popIni,Management(T = 8.0),PlantGrowth(),ResultType.Last,ManagementTechnique.Alea)

  ////////////////////////

  val f = File("popIni.txt")
  createfileforR.writeFinalPop(f,popIni)

  val g= File("resEvolAlea.txt")
  createfileforR.writeFinalPop(g,resEvolAleaT)

 */
  ///////////////  test pop ini avec modif du temps dans seqevol...   ////////////////

/*
  val res = createInitialPop.createInitialPopEvolution(5,parameter.Nmax, Management(), PlantGrowth() )(rng)

  println(res)
*/

  ///////////////  test NmaxPopIni pour                ////////////////

  val initialPopSize = 1000
  val res = createInitialPop.createPopIni2(initialPopSize ,math.round(2*initialPopSize/0.005).toInt,2,Management(),PlantGrowth())

  println(res.plants.length)

/*
  val res = createInitialPop.createPopIni(1000,50000,2,Management(),PlantGrowth())
  println(res.plants.length)
*/
}