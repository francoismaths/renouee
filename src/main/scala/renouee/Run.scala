package renouee


import org.apache.commons.math3.distribution.UniformRealDistribution
import scala.annotation.tailrec
import scala.util.Random

case class ManagementSeveralEvolution(T: Double = parameter.T, tau: Seq[Double], proportionMowing: Seq[Double])


object Run {

  implicit val rnd = new scala.util.Random(7)

  ////////////////////////////    simu      //////////////////////
  ////////////////  Chose NO HISTORY (last), or HISTORY (all)   //////////////////////
  @tailrec
  def slaveSimu(plantEvolution: PlantEvolution, iter2: Int = 0,
                       management: Management,
                       plantGrowth: PlantGrowth,
                       resultType: ResultType,
                       managementTechnique: ManagementTechnique) (implicit random: Random): PlantEvolution = {
    /* gives just the last population, and a summary of the history (evolution of population size, time of events,...)
     */
    if ((iter2 == parameter.Nmax) || plantEvolution.plants.isEmpty || (plantEvolution.infosEvolution.last.time > management.T)) plantEvolution
    else slaveSimu(Evolution.nextEvolution(plantEvolution,management,plantGrowth,resultType,managementTechnique)(random),
      iter2 + 1,
      management,plantGrowth,resultType,managementTechnique)(random)
  }


  // to have not the iter in argument of the function
  def simu(initialPop: PlantEvolution ,
                  management: Management,
                  plantGrowth: PlantGrowth,
                  resultType: ResultType,
                  managementTechnique: ManagementTechnique) (implicit random: Random): PlantEvolution = {
    slaveSimu(initialPop,0,management,plantGrowth,resultType,managementTechnique)(random)

  }




  //////////////////////////////////////////////////
  /////////////// simu No history (LAST), SEVERAL pop /////////////////
  ////////////////////////////////////////////////
  //////////////////////////////////////////////////
  ///////////////  Evolution of several initial pop (Seq given as argument) until time T /////////////////
  ////////////////////////////////////////////////


  def slaveSeveralFinalPop(initialPops: Seq[PlantEvolution])(managementSeveralEvolution: ManagementSeveralEvolution,
                                                             plantGrowth: PlantGrowth,
                                                             managementTechnique: ManagementTechnique,
                                                             res: Seq[PlantEvolution] = Seq(PlantEvolution(Nil.toVector,Nil.toVector)) )
                                                             (implicit random: Random) : Seq[PlantEvolution] = {
    if (initialPops.isEmpty) res
    else {
      val temp = initialPops.head // The initial pop that will evolve during this iteration
      val tempres = Run.simu(temp, Management(T = managementSeveralEvolution.T,
        tau = managementSeveralEvolution.tau(managementSeveralEvolution.tau.length - initialPops.length),
        proportionMowing = managementSeveralEvolution.proportionMowing(managementSeveralEvolution.tau.length - initialPops.length)),
        plantGrowth, ResultType.Last, managementTechnique)(random)

      slaveSeveralFinalPop(initialPops.tail)(managementSeveralEvolution, plantGrowth, managementTechnique, res :+ tempres)(random)
    }
  }



  // with tail
  def severalFinalPop(initialPops: Seq[PlantEvolution] )(managementSeveralEvolution: ManagementSeveralEvolution,
                                                         plantGrowth: PlantGrowth,
                                                         managementTechnique: ManagementTechnique)(implicit random: Random): Seq[PlantEvolution] ={
    slaveSeveralFinalPop(initialPops)(managementSeveralEvolution, plantGrowth, managementTechnique, Seq(PlantEvolution(Nil.toVector,Nil.toVector)))(random).tail
  }



}

