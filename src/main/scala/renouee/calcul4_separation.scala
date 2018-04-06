package renouee

import better.files.File
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

object calcul4_separation extends App {


  val rng = new RandomAdaptor(new Well44497b(4))

  val initialPopulationSize = 1500


  val NmaxPopIni = (2/0.005 * initialPopulationSize).toInt
  val NmaxEvol= 1000000
  val compteurMax = 3
  val taillePopFinaleMax = 8000 : Int

  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)


  //////////////////////////////////////////////////////
  ////////   Plant param  WITH A FILE  (eg from openmole via R)    /////////
  ////////////////////////////////////////////////////

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

  val popIni = createInitialPop.createPopIni(initialPopulationSize,NmaxPopIni ,compteurMax,
    managementPopIni,plantGrowth)(rng) : PlantEvolution

  ///////////////////////////////////////////////////
  ///////     Parameter Management            ///////
  ///////////////////////////////////////////////////

  val tau = 7.0 : Double
  val T = 5.0 : Double
  val  proportionMowing = 0.8 : Double
  val xAxisMowLimit = 0

  val management = Management(T = T,proportionMowing = proportionMowing,tau = tau, xAxisMowLimit = xAxisMowLimit )

  ///////////////////////////////////////////////////
  // EVOLUTION

  val resEvolAleaT = Run.simu(popIni,NmaxEvol, taillePopFinaleMax, management , plantGrowth,ResultType.Last,ManagementTechnique.Alea)(rng)

  val resEvolPerypheryT = Run.simu(popIni,NmaxEvol, taillePopFinaleMax, management, plantGrowth,ResultType.Last,ManagementTechnique.Periphery)(rng)

  val resEvolSideT = Run.simu(popIni,NmaxEvol, taillePopFinaleMax, management, plantGrowth,ResultType.Last,ManagementTechnique.Side)(rng)

  val resEvolSideXPositionT = Run.simu(popIni,NmaxEvol, taillePopFinaleMax, management, plantGrowth,ResultType.Last,ManagementTechnique.SideXPosition)(rng)


  ////////////////////////
  // Write in Files

  lazy val name_Dir1 = "resultFilesComparaisonMowingTechnique_Evol"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()


  createfileforR.writeResultComparaisonTechniqueEvol(dir1,popIni, resEvolAleaT, resEvolPerypheryT, resEvolSideT, resEvolSideXPositionT,
    plantGrowth, management)


}
