package renouee


import java.io.File

import org.apache.commons.math3.distribution.UniformRealDistribution

import scala.annotation.tailrec
import better.files._

import scala.util.Random



case class ResultCalibrateSide(popinis: Seq[Seq[Plant]], popsT: Seq[Seq[Plant]], popsSizeIni : Seq[Int] , popsSizeT : Seq[Int] )



object RunCalibrationSide {
  /*
  special for the side mowing technique, after creating all the initial pop, we compute the "x" limit of the mow for each stand
  according to p and the positions of the indiv in the initials pops.
  we mow at the right of this position
  Aim : each population has it's own management (p, tau), T is common
   */



  //////////////////////////////////////////////////
  ///////////////  creation of inital pop (given pop sizes) AND evolution /////////////////
  ///////////////   for calibration, so the result is of type ResultCalibrateABC
  ////////////////////////////////////////////////

  /*
 Function that takes as first argument a sequence of initial population size (Seq), then takes the control parameter (p, tau) : Seq,
 T is the same for all (but not complicated to the same as tau and p)
 //  new : For side : calculate the xlimit position to mow from the initial pop (slave function)
 and finally all the other parameter (plant).
 Return 3 things (Seq) :  in the class ResultCalibrateABC
 initial positions, (area to be calculed with R (chullarea...)),
 positions at time T (for the area)  and
 final pop size to calculate density (easy to do if we have the list of positions).
  */

  /*
  Slave function for the x limit position to mow given the initial position
   */

  def findXLimitPositionToMow(initialPop : PlantEvolution, proportionMowing:Double) : Double = {
      // trie les x par ordre croissant
    val temp = (initialPop.plants.map(p=>p.x)).sorted
    // sélectionne le bon
    if (temp.length > 0){
    temp(math.max(math.floor((1-proportionMowing)*(temp.length)).toInt -1,0)) } else {0}
  }





  def resultEvolutionABCSide(initialPopSizes: Seq[Double])(NmaxPopsInis : Seq[Int] , compteurMax : Int = parameter.compteurMax, NmaxEvol : Int = parameter.Nmax , taillePopFinaleMaxVect : Seq[Int],
                                                       managementInitialPops: Management,
                                                           T:Double, tau : Seq[Double], proportionMowing: Seq[Double],
                                                        plantGrowth: PlantGrowth)
                        (implicit random: Random): ResultCalibrateSide  ={
    // recall that the creation of all initial pop is done with the same mowing param (not the same during the evolution)
    val temp_initial_pop =  createInitialPop.createSeveralInitialPop(initialPopSizes)(NmaxPopsInis, compteurMax,managementInitialPops,plantGrowth )(random)

    val SeqxAxisMowLimit = (temp_initial_pop zip proportionMowing).map(p =>findXLimitPositionToMow(p._1,p._2))

    val final_pop = Run.severalFinalPop(temp_initial_pop)(NmaxEvol,taillePopFinaleMaxVect, ManagementSeveralEvolution(T = T,tau = tau, proportionMowing = proportionMowing, xAxisMowLimit = SeqxAxisMowLimit), plantGrowth,ManagementTechnique.SideXPosition)(random)

    ResultCalibrateSide(temp_initial_pop.map(p => p.plants) , final_pop.map(p => p.plants) , temp_initial_pop.map(p => p.plants.length) , final_pop.map(p => p.plants.length))
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

  def evolutionWriteABCSide(initialPopSizes: Seq[Double])(file1 : java.io.File, file2 : java.io.File)(NmaxPopsInis : Seq[Int] , compteurMax : Int = parameter.compteurMax, NmaxEvol : Int = parameter.Nmax, taillePopFinaleMaxVect : Seq[Int])(
    managementInitialPops: Management,
    T:Double, tau : Seq[Double], proportionMowing: Seq[Double],
    plantGrowth: PlantGrowth)
                       (implicit random: Random) = {

    val res = resultEvolutionABCSide(initialPopSizes)(NmaxPopsInis,compteurMax,NmaxEvol, taillePopFinaleMaxVect, managementInitialPops, T, tau, proportionMowing, plantGrowth)(random)
    // record positions of initial population (2008)
    slaveArgumentsFiles.createFilesPositionsABC(res.popinis, file1 , "pos_popIni_")

    // record positions of final population (2015)
    slaveArgumentsFiles.createFilesPositionsABC(res.popsT , file2, "pos_popFinal_")

  }


}

