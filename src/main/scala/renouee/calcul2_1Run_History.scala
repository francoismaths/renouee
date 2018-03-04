package renouee

import better.files.File
import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

object calcul2_1Run_History extends App {


  //////////////////////////////////////////////////////////
  ////////////   Single experience
  /////////////////////////////////////////////////////////

  /*
  Produce four file in the directory 1run :
  - a file with the parameter
  - a file with the initial pop (biomass and position)
  - a file with the final pop (biomass and position)
  - a file with the history (time, popsize, area(scala, rectangle), event)
  */

  val rng = new RandomAdaptor(new Well44497b(4))

  val initialPopulationSize = 2000


  val NmaxPopIni = (2/0.005 * initialPopulationSize).toInt
  val NmaxEvol= 1000000
  val compteurMax = 3

  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)
  val management = Management(T=10, tau = 5, proportionMowing = 0.8)

  /*
  val plantGrowth = PlantGrowth(
    distanceCompetition = 0.5      ,
    distanceParent = 0.5 ,
    shape = 45.0,
    scale = 4.5  ,
    K = 5.0 ,
    L= 0.7428989           ,
    mowingParameter= 1.0 ,
    deathParameterDecrease=  1.0,
    deathParameterScaling = 2.065402,
    a0 = 0.5  ,
    bbar =  1.0 ,     //2.734778 ,
    biomassFirstIndiv = 3.0 )
  */

  //////////////////////////////////////////////////////
  ////////     WITH A FILE  (eg from openmole via R    /////////
  ////////////////////////////////////////////////////

  // if we want to  use a file for the value of the paameter (plantGrowth), for example the result of nsga caliration openmole

  /*
  val nameFile : String = "resCalibrate/resParam_"
  val numeroFile : Int = 1

  val r = File( nameFile + numeroFile.toString + ".csv")
*/

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

  ///////////////////////////////////////////////////
  ///////////////////////////////////////////////////


  val managementTechnique = ManagementTechnique.Periphery

  val initialPopulation = createInitialPop.createPopIni(initialPopulationSize,NmaxPopIni ,compteurMax,
    managementPopIni,plantGrowth)(rng) : PlantEvolution
  val taillePopFinaleMax = 20000 : Int


  val res = Run.simu(initialPopulation,NmaxEvol, taillePopFinaleMax, management,plantGrowth,ResultType.All,managementTechnique)(rng)


  lazy val name_Dir1 = "1run"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1,plantGrowth,management, managementTechnique )


}
