package renouee


import java.io.File
import org.apache.commons.math3.distribution.UniformRealDistribution
import scala.annotation.tailrec
import better.files._



case class ResultCalibrateABC(popinis: Seq[Seq[Plant]], popsT: Seq[Seq[Plant]], popsSizeIni : Seq[Int] , popsSizeT : Seq[Int] )



object RunCalibration {
  /*
  Aim : each population has it's own management (p, tau, T)
   */



  //////////////////////////////////////////////////
  ///////////////  creation of inital pop (given pop sizes) AND evolution /////////////////
  ///////////////   for calibration, so the result is of type ResultCalibrateABC
  ////////////////////////////////////////////////

  /*
 Function that takes as first argument a sequence of initial population size (Seq), then takes the control parameter (p, tau) : Seq,
 T is the same for all (but not complicated to the same as tau and p)
 and finally all the other parameter (plant).
 Return 3 things (Seq) :  in the class ResultCalibrateABC
 initial positions, (area to be calculed with R (chullarea...)),
 positions at time T (for the area)  and
 final pop size to calculate density (easy to do if we have the list of positions).
  */

  def resultEvolutionABC(initialPopSizes: Seq[Double])(T: Double = parameter.T, tau: Seq[Double], proportionMowing: Seq[Double] )
                        (K : Double = Plant.K, L : Double = Plant.L, d1: Double = Plant.d1, b1 : Double = Plant.b1,
                                    shape : Double = Plant.shape, scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                    deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter  : Double = Plant.mowing_parameter,
                                    bbar:Double = Plant.bbar, a_0 : Double ) : ResultCalibrateABC ={
    // recall that the creation of all initial pop is done with the same mowing param (not the same during the evolution)
    val temp_initial_pop =  createInitialPop.createSeveralInitialPop(initialPopSizes)(Plant.tau, Plant.proportionMowing,
      K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar, a_0)
    val final_pop = Run.severalFinalPop(temp_initial_pop)(T, tau, proportionMowing)(K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar,a_0)
    ResultCalibrateABC(temp_initial_pop.map(p => p.plants) , final_pop.map(p => p.plants) , temp_initial_pop.map(p => p.plants.length) , final_pop.map(p => p.plants.length))
  }





  //////////////////////////////////////////////////
  ///////////////  Create files from the  class ResultCalibrateABC created by the function ResultEvolutionABC  /////////////////
  ////////////////////////////////////////////////

  /*
  This function is OK for calibration openMole ! (add size obtained by the simulation to check that
  it is ok + penalisation in the objective otherwise)
  creation of inital pop (given pop size) AND evolution thanks to  resultEvolutionABC
  use the slave functions in slaveArgumentsFiles
  Remark : we don't use the Seq[Int] of the class ResultCalibrateABC (R does it also)
  */

  def evolutionWriteABC(initialPopSizes: Seq[Double])(file1 : java.io.File, file2 : java.io.File)
                        (T: Double = parameter.T , tau: Seq[Double] , p: Seq[Double] )
                        (K : Double = Plant.K, L : Double = Plant.L, d1: Double = Plant.d1, b1 : Double = Plant.b1,
                        shape : Double = Plant.shape, scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                         deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter  : Double = Plant.mowing_parameter,
                         bbar:Double = Plant.bbar, a_0 : Double ) = {

    val res = resultEvolutionABC(initialPopSizes)(T, tau , p ) (K , L , d1, b1 , shape ,
      scale , deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar,a_0)
    // record positions of initial population (2008)
    slaveArgumentsFiles.createFilesPositionsABC(res.popinis, file1 , "pos_popIni_")

    // record positions of final population (2015)
    slaveArgumentsFiles.createFilesPositionsABC(res.popsT , file2, "pos_popFinal_")

  }


}

