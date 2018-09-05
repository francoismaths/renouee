package renouee

import better.files.File
import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

object calcul5_1Run_History_SIDE  extends App {


  // Do the same that calcul2_1Run_History, but for SIDE
  // the difference  is that the parameter of the management technique (xposition) depends on the intiale population
  // so we have to add this between the creation of the initial population and it's evolution
  // SideXPosition

  //////////////////////////////////////////////////////////
  ////////////   Single experience
  /////////////////////////////////////////////////////////

  /*
  Produce four file in the directory 1run :
  - a file with the parameter
  - a file with the initial pop (biomass and position)
  - a file with the final pop (biomass and position)
  - a file with the history (time, popsize, area(scala, rectangle), event)
  - a file with the FULL history (not clear event, even when 'nothing' happens, ie the eventnot happens cause of alea)
  */

  val rng = new RandomAdaptor(new Well44497b(3))

  val initialPopulationSize = 1500


  val NmaxPopIni = (2/0.005 * initialPopulationSize).toInt
  val NmaxEvol= 2000000
  val compteurMax = 3

  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)

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
  val nameFile : String = "ParamMin2Mean"  // from the calibration in openMole withe side
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


  val initialPopulation = createInitialPop.createPopIni(initialPopulationSize,NmaxPopIni ,compteurMax,
    managementPopIni,plantGrowth)(rng) : PlantEvolution

  println(initialPopulation.infosEvolution)



  val managementTechnique = ManagementTechnique.SideXPosition
  // management est temporaire, on a besoin proportionMowing  pour trouver la limite xposition
  val management = Management(T=8, tau = 3.0, proportionMowing = 0.75)
  val taillePopFinaleMax = 2000 : Int


  //Slave function for the x limit position to mow given the initial position
  def findXLimitPositionToMow(initialPop : PlantEvolution, proportionMowing:Double) : Double = {
    // trie les x par ordre croissant
    val temp = (initialPop.plants.map(p=>p.x)).sorted
    // sélectionne le bon
    if (temp.length > 0){
      temp(math.max(math.floor((1-proportionMowing)*(temp.length)).toInt -1,0)) } else {0}
  }


  val xAxisMowLimit = findXLimitPositionToMow(initialPopulation,management.proportionMowing)
  println(xAxisMowLimit)

  val res = Run.simu(initialPopulation,NmaxEvol, taillePopFinaleMax, Management(T= management.T, tau = management.tau, xAxisMowLimit = xAxisMowLimit),plantGrowth,ResultType.All,managementTechnique)(rng)



  // save file
  lazy val name_Dir1 = "1run"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1,plantGrowth,management, managementTechnique )


}
