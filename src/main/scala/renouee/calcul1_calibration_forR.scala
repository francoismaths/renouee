package renouee

import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

/* AIM : Evolution of the model for several population, given initial pop size (creation and evolution),
according to mowing parameter for each population (here in vectors).
Creation of directories to record positions (for R analysis).

*/

object calcul1_calibration_forR {



  def evolutionWriteABCforR(params : Array[Double]) : Array[Double] = {

    val rng = new RandomAdaptor(new Well44497b(43))



    lazy val p = slaveArgumentsFiles.fileToSeq("data_allegee/p_allegee").map(p=> if(p==1)(1) else(Plant.proportionMowing))
  lazy val tau = slaveArgumentsFiles.fileToSeq("data_allegee/tau_allegee")
  lazy val popSizes = slaveArgumentsFiles.fileToSeq("data_allegee/taillePop2008_allegee")


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

  lazy val parameters : Array[Double] = Array(Plant.K,  Plant.L, Plant.d1, Plant.b1, Plant.shape,
  Plant.scale, Plant.deathParameterDecrease,  Plant.deathParameterScaling, Plant.mowingParameter,
  Plant.bbar, Plant.a_0(Plant.K))


      ////////////////////////////////////////////////////////////////////////////////
      //////////////////    Attention à être raccord avec R sur l'odre des paramètres      /////////////////
      ////////////////////////////////////////////////////////////////////////////////

    RunCalibration.evolutionWriteABC(popSizes)( dir1, dir2)(parameter.Nmax, parameter.compteurMax,
    Management(tau= 1.0, proportionMowing = 0.9),ManagementSeveralEvolution(T=parameter.T,tau=tau,proportionMowing = p),
    PlantGrowth(  d1 = params(0), b1 = params(1), shape = params(2), scale = params(3), K = params(4), L= params(5),
      mowingParameter= params(6), deathParameterDecrease= params(7), deathParameterScaling = params(8),
      a0 =params(9), bbar = params(10), biomassFirstIndiv = params(11) ),ManagementTechnique.Alea)(rng)

    Array(12.0)
  }


}


