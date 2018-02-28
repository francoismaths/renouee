package renouee

import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

/* AIM : Evolution of the model for several population, given initial pop size (creation and evolution),
according to mowing parameter for each population (here in vectors).
Creation of directories to record positions (for R analysis).

*/

object calcul1_calibration_forR {



  def evolutionWriteABCforR(params : Array[Double]) : Array[Double] = {

    val rng = new RandomAdaptor(new Well44497b(43))

    val proportionMowingForSeq = 0.8
    lazy val p = slaveArgumentsFiles.fileToSeq("data_allegee/p_allegee").map(p=> if(p==1)(1) else(proportionMowingForSeq))
    lazy val tau = slaveArgumentsFiles.fileToSeq("data_allegee/tau_allegee")
    lazy val popSizes = slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2008_allegee")


    val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)
    val managementSeveralEvolution = ManagementSeveralEvolution(tau=tau,proportionMowing = p)
    val managementTechnique = ManagementTechnique.Alea
    //  PlantGrowth c'est la liste des param pour R


    lazy val path_Dir1 = "simu_aire2008"
  lazy val path_Dir2 = "simu_aire2015"

  lazy val dir1 = new java.io.File(path_Dir1)
  lazy val dir2 = new java.io.File(path_Dir2)

  dir1.mkdir()
  dir2.mkdir()

  //  Pour tester rapidement
    /*
  lazy val p = Seq(1,0,0,1,0,1,1).map(p=> if(p==1)(1) else(Plant.proportionMowing))
  lazy val tau = Seq(3,0,1,2,1,1,3).map(_.toDouble)
  lazy val popSizes = Seq(3.0,10.0,18.0,35.0,10.0,10.0,40.0)
*/

      ////////////////////////////////////////////////////////////////////////////////
      //////////////////    Attention à être raccord avec R sur l'odre des paramètres      /////////////////
      ////////////////////////////////////////////////////////////////////////////////



    val NmaxPopsInis = popSizes.map(s => (2*s / 0.005 ).toInt) : Seq[Int]
    lazy val taillePopFinaleMaxVect = slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2015_allegee").map(t => (5*t).toInt) : Seq[Int]


    RunCalibration.evolutionWriteABC(popSizes)( dir1, dir2)(NmaxPopsInis, parameter.compteurMax,parameter.Nmax,taillePopFinaleMaxVect)(
      managementPopIni,managementSeveralEvolution,
    PlantGrowth(  distanceCompetition = params(0), distanceParent = params(1), shape = params(2), scale = params(3), K = params(4), L= params(5),
      mowingParameter= params(6), deathParameterDecrease= params(7), deathParameterScaling = params(8),
      a0 =params(9), bbar = params(10), biomassFirstIndiv = params(11) ),managementTechnique)(rng)

    Array(12.0)
  }


}


