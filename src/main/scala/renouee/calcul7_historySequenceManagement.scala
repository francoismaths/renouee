package renouee

import better.files.File
import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}
import better.files._



object calcul7_historySequenceManagement extends App {


  // Do the same that calcul2_1Run_History, but for SIDE   : WITH A SEQUENCE OF MANAGEMENT TECHNIQUE
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

  val initialPopulationSize = 100


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


  //////////////////////////////////////////////////////
  ////////     SEQUENCE OF MANAGEMENT to apply   /////////
  ////////////////////////////////////////////////////

  val T = Seq(10,3,10) : Seq[Double]
  val tau = Seq(3,1,2) : Seq[Double]
  val proportionMowing = Seq(0.75,0.5,0.9) : Seq[Double]

  val managementTechnique  = ManagementTechnique.Alea: ManagementTechnique

  val taillePopFinaleMax = 2000 : Int


  val res = sequenceManagementTechniques.slaveSequenceMT(initialPopulation,NmaxEvol,taillePopFinaleMax, T, tau, proportionMowing, managementTechnique, ResultType.All, plantGrowth)(rng)


  // save file
  lazy val name_Dir1 = "simuSeveralManagement"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1,plantGrowth,Management(), ManagementTechnique.SideXPosition)
  val f = dir1.toScala / "Management.txt"
  renouee.createfileforR.slaveWriteManagemement(f,T,tau,proportionMowing,managementTechnique)



}
