package renouee


import org.apache.commons.math3.distribution.UniformRealDistribution
import scala.annotation.tailrec
import scala.util.Random



object Run {

  implicit val rnd = new scala.util.Random(7)

  ////////////////////////////    simu  HISTORY //////////////////////
  @tailrec
  def slaveSimuHistory(plantEvolution: PlantEvolution, iter2: Int = 0, T:Double = parameter.T, tau: Double = Plant.tau, p: Double = Plant.proportionMowing,
                       K: Double = Plant.K, L: Double = Plant.L,
                       d1: Double = Plant.d1, b1: Double = Plant.b1, shape: Double = Plant.shape,
                       scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                       deathParameterScaling: Double = Plant.deathParameterScaling,
                       mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double ): PlantEvolution = {
    /* gives just the last population, and a summary of the history (evolution of population size, time of events,...)
     */
    if ((iter2 == parameter.Nmax) || plantEvolution.plants.isEmpty || (plantEvolution.infosEvolution.last.time > T)) plantEvolution
    else slaveSimuHistory(Evolution.nextEvolutionHistory(plantEvolution,tau,p,K,L,d1,b1,shape,scale,deathParameterDecrease,deathParameterScaling,mowing_parameter,bbar,a_0),
      iter2 + 1, T,
      tau, p , K , L, d1, b1 , shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar,a_0 )
  }


  // to have not the iter in argument of the function
  def simuHistory(initialPop: PlantEvolution , T: Double = parameter.T ,
              tau : Double = Plant.tau, p : Double = Plant.proportionMowing ,  K : Double = Plant.K, L : Double = Plant.L,
              d1: Double = Plant.d1, b1 : Double = Plant.b1, shape : Double = Plant.shape,
              scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
              deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter: Double = Plant.mowing_parameter,
              bbar:Double = Plant.bbar, a_0 : Double): PlantEvolution = {
    slaveSimuHistory(initialPop,0,T,tau,p,K,L,d1,b1,shape,scale,deathParameterDecrease,deathParameterScaling,mowing_parameter,bbar,a_0 )

  }



  //////////////////////////////////////////////////
  /////////////// simu No history, 1 pop /////////////////
  ////////////////////////////////////////////////


  @tailrec
  def slaveSimuLastPop(plantEvolution: PlantEvolution, iter2: Int = 0, T: Double = parameter.T,
                       tau : Double = Plant.tau, p : Double = Plant.proportionMowing, K : Double = Plant.K, L : Double = Plant.L,
                       d1: Double = Plant.d1, b1 : Double = Plant.b1, shape : Double = Plant.shape,
                       scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                       deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter  : Double = Plant.mowing_parameter,
                       bbar:Double = Plant.bbar, a_0 : Double): PlantEvolution = {
    /* gives just the last population
     */
    if ((iter2 == parameter.Nmax) || plantEvolution.plants.isEmpty || (plantEvolution.infosEvolution.last.time > T)) plantEvolution
    else slaveSimuLastPop(Evolution.nextEvolution(plantEvolution,tau,p,K,L,d1,b1,shape,scale,deathParameterDecrease,deathParameterScaling,
      mowing_parameter, bbar, a_0),
      iter2 + 1, T ,
      tau , p , K , L, d1, b1 , shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar, a_0)
  }

  // to have not the iter in argument of the function
  def SimuLastPop(initialPop: PlantEvolution, T: Double = parameter.T,
                  tau : Double = Plant.tau, p : Double = Plant.proportionMowing, K : Double = Plant.K, L : Double = Plant.L,
                  d1: Double = Plant.d1, b1 : Double = Plant.b1, shape : Double = Plant.shape,
                  scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                  deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter  : Double = Plant.mowing_parameter,
                  bbar:Double = Plant.bbar, a_0 : Double): PlantEvolution = {
    slaveSimuLastPop(initialPop,0,T,tau,p,K,L,d1,b1,shape,scale,deathParameterDecrease,deathParameterScaling,mowing_parameter,bbar,a_0)

  }


  //////////////////////////////////////////////////
  /////////////// simu No history, severall pop /////////////////
  ////////////////////////////////////////////////
  //////////////////////////////////////////////////
  ///////////////  Evolution of several initial pop (given as argument) until time T /////////////////
  ////////////////////////////////////////////////


  def slaveSeveralFinalPop(initialPops: Seq[PlantEvolution])(T: Double = parameter.T, tau: Seq[Double], p: Seq[Double],
                                                                         K: Double = Plant.K, L: Double = Plant.L, d1: Double = Plant.d1, b1: Double = Plant.b1,
                                                                         shape: Double = Plant.shape, scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                                                         deathParameterScaling: Double = Plant.deathParameterScaling,
                                                                         mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double,
                                                                         res: Seq[PlantEvolution] = Seq(PlantEvolution(Nil,Nil)) ): Seq[PlantEvolution] =
    if (initialPops.isEmpty) res
    else {
      val temp = initialPops.head // The initial pop that will evolve during this iteration
      val tempres = Run.SimuLastPop(temp, T, tau(tau.length - initialPops.length), p(tau.length - initialPops.length),
        K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar,a_0)
      slaveSeveralFinalPop(initialPops.tail)(T, tau, p, K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar ,a_0 ,res :+ tempres)
    }



  // avec tail
  def severalFinalPop(initialPops: Seq[PlantEvolution] )(T: Double = parameter.T, tau: Seq[Double], p: Seq[Double] )
                                 (K: Double = Plant.K, L: Double = Plant.L, d1: Double = Plant.d1, b1: Double = Plant.b1,
                                  shape: Double = Plant.shape, scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                  deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter: Double = Plant.mowing_parameter,
                                  bbar:Double = Plant.bbar, a_0 : Double ): Seq[PlantEvolution] ={
    slaveSeveralFinalPop(initialPops)(T, tau, p, K, L, d1, b1, shape, scale, deathParameterDecrease,
      deathParameterScaling, mowing_parameter, bbar, a_0, Seq(PlantEvolution(Nil,Nil))).tail
  }



}

