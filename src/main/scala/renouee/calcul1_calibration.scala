package renouee

import better.files.File
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

/* AIM : Evolution of the model for several population, given initial pop size (creation and evolution),
according to mowing parameter for each population (here in vectors).
Creation of directories to record positions (for R analysis).

*/

object calcul1_calibration extends App {


  val proportionMowingForSeq = 0.9

  lazy val p = slaveArgumentsFiles.fileToSeq("data_allegee/p_allegee").map(p=> if(p==1)(1) else(proportionMowingForSeq))
  lazy val tau = slaveArgumentsFiles.fileToSeq("data_allegee/tau_allegee")
  lazy val popSizes = slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2008_allegee")

  val id = 2 : Long
  val rng = new RandomAdaptor(new Well44497b(id))


  lazy val path_Dir1 = "simu_aire2008"
  lazy val path_Dir2 = "simu_aire2015"

  lazy val dir1 = new java.io.File(path_Dir1)
  lazy val dir2 = new java.io.File(path_Dir2)

  dir1.mkdir()
  dir2.mkdir()




  /*
  //  Pour tester rapidement
  lazy val p = Seq(1,0,0,1,0,1,1).map(p=> if(p==1)(1) else(proportionMowingForSeq))  // because the file just says if fullMow or not
  lazy val tau = Seq(3,0,1,2,1,1,3).map(_.toDouble)
  lazy val popSizes = Seq(3.0,10.0,18.0,35.0,10.0,10.0,40.0)
*/

  val NmaxPopsInis = popSizes.map(s => (2*s / 0.005).toInt)

  lazy val taillePopFinaleMaxVect = (slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2008_allegee") zip  slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2015_allegee") ).map(t => math.max(10*t._1,10*t._2).toInt) : Seq[Int]


  println(popSizes.sorted)
  println(NmaxPopsInis.sorted)


  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)
  val managementSeveralEvolution = ManagementSeveralEvolution(tau=tau,proportionMowing = p)
/*
  val plantGrowth = PlantGrowth(
    distanceCompetition = 0.2476456      ,
    distanceParent = 0.3861672 ,
    shape = 36.46628,
    scale = 4.27974  ,
    K = 6.205428 ,
    L= 0.7428989           ,
    mowingParameter= 1.435171 ,
    deathParameterDecrease=  1.682868,
    deathParameterScaling = 2.065402,
    a0 = 1.385761  ,
    bbar =  1.0 ,     //2.734778 ,
    biomassFirstIndiv = 3 )
*/


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
  val managementTechnique = ManagementTechnique.Alea


    RunCalibration.evolutionWriteABC(popSizes)( dir1, dir2)(NmaxPopsInis, 3, parameter.Nmax,taillePopFinaleMaxVect  )(
      managementPopIni,managementSeveralEvolution,plantGrowth,managementTechnique)(rng)



  }


