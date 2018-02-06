package renouee

/* AIM : Evolution of the model for several population, given initial pop size (creation and evolution),
according to mowing parameter for each population (here in vectors).
Creation of directories to record positions (for R analysis).

*/

object calcul1_calibration_forR {



  def evolutionWriteABCforR(params : Array[Double]) : Array[Double] = {


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
  Plant.scale, Plant.deathParameterDecrease,  Plant.deathParameterScaling, Plant.mowing_parameter,
  Plant.bbar, Plant.a_0(Plant.K))


  RunCalibration.evolutionWriteABC(popSizes)(dir1, dir2)(parameter.T , tau, p)(params(0),
    params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10))

    Array(12.0)
  }


}


