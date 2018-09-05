package renouee

import java.io.{BufferedReader, FileReader}

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

  /*
  val initialPopSize = 1000
  val res = createInitialPop.createPopIni(initialPopSize ,math.round(2*initialPopSize/0.005).toInt,2,Management(),PlantGrowth())

  println(res.plants.length)
*/

/*
  val res = createInitialPop.createPopIni(1000,50000,2,Management(),PlantGrowth())
  println(res.plants.length)
*/

  ///////////////  test NmaxPopIni avec la nouvelle fonction               ////////////////
  //  but : faire dépendre le nombre d'iter de la taille de la pop que l'on souhaite créer.

/*
  val proportionMowingForSeq = 0.8

  lazy val p = slaveArgumentsFiles.fileToSeq("data_allegee/p_allegee").map(p=> if(p==1)(1) else(proportionMowingForSeq))
  lazy val tau = slaveArgumentsFiles.fileToSeq("data_allegee/tau_allegee")
  lazy val popSizes = slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2008_allegee")


  val NmaxPopsInis = popSizes.map(s => (2*s / 0.005).toInt)

  val res = createInitialPop.createSeveralInitialPop(popSizes)(NmaxPopsInis,3,Management(),PlantGrowth())


  println(popSizes)
  println(res.map(p=>p.plants.length))
*/


  ////////////////////////////////////////
  // IMPORTER LES DONNER DE CALIBRATION DE R

  //val test= slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2015_allegee").map(t => (5*t).toInt) : Seq[Int]
/*
  import better.files._

  val nameFile : String = "resCalibrate/resParam_"
  val numeroFile : Int = 1
  val r = File( nameFile + numeroFile.toString + ".csv")
  val lines = r.lines.toVector

  def doubleQuoteFilter(c: Char) = c != '"'

  //val tempNames = lines(1).filter(doubleQuoteFilter).split(",").toList   //.map(_.toDouble)

  val tempNames = lines.map(p => p.split(",").toList(0) )
  val tempVal = lines.map(p => p.split(",").toList(1).toDouble )

  //println(lines.map(p => p.split(",").toList(0)   ) )
  println(tempVal)
  println(tempNames)

  val plantGrowth = PlantGrowth(
    K = tempVal(0),
    L = tempVal(1),
    distanceCompetition = tempVal(2),
    distanceParent = tempVal(3),
    shape = tempVal(4),
    scale = tempVal(5),
    deathParameterDecrease = tempVal(6),
    deathParameterScaling = tempVal(7),
    mowingParameter = tempVal(8),
    bbar = tempVal(9),
    a0 = tempVal(10),
  )

*/





  // teste de Side dans la calibration

  /*
  val initialPopulationSize = 30


  val NmaxPopIni = (2/0.005 * initialPopulationSize).toInt
  val NmaxEvol= 2000000
  val compteurMax = 3

  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)


  val nameFile : String = "ParamMin"
  val r = File( nameFile  + ".csv")
  val lines = r.lines.toVector
  def doubleQuoteFilter(c: Char) = c != '"'

  val tempNames = lines.map(p => p.split(",").toList(0) )
  val tempVal = lines.map(p => p.split(",").toList(1).toDouble )
  println(tempVal)
  println(tempNames)


  val plantGrowth = PlantGrowth(
    K = tempVal(0),
    L = tempVal(1),
    distanceCompetition = tempVal(2),
    distanceParent = tempVal(3),
    shape = tempVal(4),
    scale = tempVal(5),
    deathParameterDecrease = tempVal(6),
    deathParameterScaling = tempVal(7),
    mowingParameter = tempVal(8),
    bbar = tempVal(9),
    a0 = tempVal(10),
  )
  */

  /*
  val initialPopulation = createInitialPop.createPopIni(initialPopulationSize,NmaxPopIni ,compteurMax,
    managementPopIni,plantGrowth)(rng) : PlantEvolution


  println(initialPopulation.plants.length)
  println(initialPopulation.plants.map(p => p.x).sorted)

  val res = renouee.RunCalibrationSide.findXLimitPositionToMow(initialPopulation,0.65)
  println(res)


  val temp_initial_pop =  createInitialPop.createSeveralInitialPop(Seq(5,3))(Seq(200,200), compteurMax, managementPopIni, plantGrowth )(rng)

  println( temp_initial_pop(0).plants.map(p => p.x).sorted )
  println( temp_initial_pop(1).plants.map(p => p.x).sorted )

  val proportionMowing = Seq(0.6,1)
  val SeqxAxisMowLimit = (temp_initial_pop zip proportionMowing).map(p => renouee.RunCalibrationSide.findXLimitPositionToMow(p._1,p._2))
  println(SeqxAxisMowLimit)
  */








  // test side 2

  val proportionMowingForSeq = 0.9
  val numberOfPops = 3 : Int


  //  Pour tester rapidement
  lazy val p = Seq(0.9)
  lazy val tau = Seq(0).map(_.toDouble)
  lazy val popSizes = Seq(1500.0)


  lazy val path_Dir1 = "simu_aire2008"
  lazy val path_Dir2 = "simu_aire2015"

  lazy val dir1 = new java.io.File(path_Dir1)
  lazy val dir2 = new java.io.File(path_Dir2)

  dir1.mkdir()
  dir2.mkdir()


  val NmaxPopsInis = popSizes.map(s => (2*s / 0.005).toInt)

  lazy val taillePopFinaleMaxVect = List.fill(numberOfPops)(2000)

  println(popSizes.sorted)
  println(NmaxPopsInis.sorted)


  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)


  //////////////////////////////////////////////////////
  //////////   Enter the parameter of the plant
  ////////////////////////////////////////////////////

  //////////////////////////////////////////////////////
  //////////   WITH A FILE (param that make the min in the exploration)  ////////////
  ////////////////////////////////////////////////////

  // if we want to  use a file for the value of the paameter (plantGrowth), for example the result of nsga caliration openmole

  val nameFile : String = "ParamMin2"
  val r = File( nameFile  + ".csv")
  val lines = r.lines.toVector

  def doubleQuoteFilter(c: Char) = c != '"'

  val tempNames = lines.map(p => p.split(",").toList(0) )
  val tempVal = lines.map(p => p.split(",").toList(1).toDouble )
  println(tempVal)
  println(tempNames)


  val plantGrowth = PlantGrowth(
    K = tempVal(0),
    L = tempVal(1),
    distanceCompetition = tempVal(2),
    distanceParent = tempVal(3),
    shape = tempVal(4),
    scale = tempVal(5),
    deathParameterDecrease = tempVal(6),
    deathParameterScaling = tempVal(7),
    mowingParameter = tempVal(8),
    bbar = tempVal(9),
    a0 = tempVal(10),
    biomassFirstIndiv = tempVal(11)
  )

  ///////////////////////////////////////////////////

  // function that creates and makes evolve initial pops, and creates files

/*
  val temp_initial_pop =  createInitialPop.createSeveralInitialPop(popSizes)(NmaxPopsInis, 5, managementPopIni, plantGrowth )(rng)

  println( temp_initial_pop(0).plants.map(p => p.x).sorted )
  println( temp_initial_pop(1).plants.map(p => p.x).sorted )


  val SeqxAxisMowLimit = (temp_initial_pop zip p ).map(k =>RunCalibrationSide.findXLimitPositionToMow(k._1,k._2))
  println(p)
  println(SeqxAxisMowLimit)


  val final_pop = Run.severalFinalPop(temp_initial_pop)(parameter.Nmax,taillePopFinaleMaxVect, ManagementSeveralEvolution(T = 8,tau = tau, proportionMowing = p, xAxisMowLimit = SeqxAxisMowLimit), plantGrowth,ManagementTechnique.SideXPosition)(rng)

  println(tau)
  println( final_pop(0).plants.length)
  println( final_pop(1).plants.length)
*/


  /*
  RunCalibrationSide.resultEvolutionABCSide(popSizes)(NmaxPopsInis, 3, parameter.Nmax,taillePopFinaleMaxVect,
    managementPopIni,
    8, tau, p,
    plantGrowth)(rng)
*/



    renouee.RunCalibrationSide.evolutionWriteABCSide(popSizes)( dir1, dir2)(NmaxPopsInis, 3, parameter.Nmax,taillePopFinaleMaxVect  )(
      managementPopIni,
      8, tau, p,
      plantGrowth)(rng)


}
