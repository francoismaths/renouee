package renouee

import org.apache.commons.math3.distribution.UniformRealDistribution

import scala.annotation.tailrec

object createInitialPop {


  implicit val rnd = new scala.util.Random(7)

  //////////////////////////////////////////////////
  /////////////// create an initial pop , with evolution //////////////
  ////////////////////////////////////////////////

  @tailrec
  def slaveSimuInitialPopEvolution(plantEvolution: PlantEvolution, iter2: Int = 0, initialPopSize : Double = Plant.initialPopulationSize ,
                                   tau : Double = Plant.tau, p : Double = Plant.proportionMowing, K : Double = Plant.K, L : Double = Plant.L,
                                   d1: Double = Plant.d1, b1 : Double = Plant.b1, shape : Double = Plant.shape,
                                   scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                   deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter  : Double = Plant.mowing_parameter,
                                   bbar:Double = Plant.bbar, a_0 : Double = Plant.a_0(Plant.K)): PlantEvolution = {
    /* create initial pop from the evolution of one indiv (until reach the desired pop size), or extinction, or Nmax reach
      with birth/death rate, mow
     */
    if ((iter2 == parameter.Nmax) || plantEvolution.plants.isEmpty || (plantEvolution.plants.length == initialPopSize )) plantEvolution
    else slaveSimuInitialPopEvolution(Evolution.nextEvolution(plantEvolution,tau,p,K,L,d1,b1,shape,scale,deathParameterDecrease,deathParameterScaling,
      mowing_parameter, bbar, a_0),
      iter2 + 1, initialPopSize  ,
      tau , p , K , L, d1, b1 , shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar, a_0)
  }

  // create initial pop from one indiv, this function to have not the iter in argument of the function
  def createInitialPopEvolution(initialPopSize : Double = Plant.initialPopulationSize ,
                                tau : Double = Plant.tau, p : Double = Plant.proportionMowing, K : Double = Plant.K, L : Double = Plant.L,
                                d1: Double = Plant.d1, b1 : Double = Plant.b1, shape : Double = Plant.shape,
                                scale : Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                deathParameterScaling: Double = Plant.deathParameterScaling, mowing_parameter  : Double = Plant.mowing_parameter,
                                bbar:Double = Plant.bbar, a_0 : Double = Plant.a_0(Plant.K) ): PlantEvolution = {


    val dist = new UniformRealDistribution(Plant.bound_initial_bimass_min(Plant.K), Plant.bound_initial_bimass_max(Plant.K))
    val pop0 = PlantEvolution(Seq(Plant(0.0,0.0,dist.sample())) , Seq(InfosEvolution(0,0,0.0,""))  ) : PlantEvolution

    slaveSimuInitialPopEvolution(pop0,0,initialPopSize ,tau,p,K,L,d1,b1,shape,scale,deathParameterDecrease,deathParameterScaling,mowing_parameter,bbar,a_0)

  }


  ///////////////////////////////
  //////////////  Allways initial pop, with evolution   //////////////
  //////////////  no time,  with compteurMax
  /////////     just require a pop size to reach (if no succeess, cause of parameters for example)
  ////////////  it returns Nil : PlantEvolution (there are mow, birth, death)
  ///////////////////////////////

  def slaveCreatePopIni(initialPopSize : Double = Plant.initialPopulationSize, compteur : Int = 1,
                        tau: Double = Plant.tau, proportionMowing: Double = Plant.proportionMowing,
                        K: Double = Plant.K, L: Double = Plant.L, d1: Double = Plant.d1, b1: Double = Plant.b1,
                        shape: Double = Plant.shape, scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                        deathParameterScaling: Double = Plant.deathParameterScaling,
                        mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double = Plant.a_0(Plant.K))  : PlantEvolution = {

    if (compteur > parameter.compteurMax) PlantEvolution(Nil,Nil)
    else {

      val temp  = createInitialPop.createInitialPopEvolution(initialPopSize ,tau, proportionMowing,
        K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter,bbar,a_0)


      if (temp.plants.length == initialPopSize) {
        temp

      }

      else {
        slaveCreatePopIni(initialPopSize, compteur+1,
          tau, proportionMowing,
          K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar, a_0)
      }
    }
  }


  //  Function without the iter
  def createPopIni(initialPopSize : Double = Plant.initialPopulationSize,
                   tau: Double = Plant.tau, proportionMowing: Double = Plant.proportionMowing,
                   K: Double = Plant.K, L: Double = Plant.L, d1: Double = Plant.d1, b1: Double = Plant.b1,
                   shape: Double = Plant.shape, scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                   deathParameterScaling: Double = Plant.deathParameterScaling,
                   mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double = Plant.a_0(Plant.K))  : PlantEvolution = {

    slaveCreatePopIni(initialPopSize, 1,
      tau, proportionMowing,
      K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar, a_0)
  }


  ///////////////////////////////  Create several initial population  ///////////////////
  ///////////////////////////////  different pop sizes, but the same management (mowing)
  ///////////////  cause if mowing is too intense, it is harder to create an initial pop /////
  ////////////////////////////////////////////////////////////////////////////////////////

  def slaveCreateSeveralInitialPop(initialPopSizes: Seq[Double])(tau: Double = Plant.tau, proportionMowing: Double = Plant.proportionMowing,
                                                                 K: Double = Plant.K, L: Double = Plant.L, d1: Double = Plant.d1, b1: Double = Plant.b1,
                                                                 shape: Double = Plant.shape, scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                                                 deathParameterScaling: Double = Plant.deathParameterScaling,
                                                                 mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double = Plant.a_0(Plant.K),
                                                                 res: Seq[PlantEvolution] = Seq(PlantEvolution(Nil,Nil)): Seq[PlantEvolution] ) : Seq[PlantEvolution] = {
    if (initialPopSizes.isEmpty) res
    else {
      val temp = math.floor(initialPopSizes.head)
      val tempres = createInitialPop.createPopIni(temp, tau, proportionMowing,
        K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar, a_0)
      slaveCreateSeveralInitialPop(initialPopSizes.tail)(tau, proportionMowing,
        K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar, a_0, res :+ tempres)
    }
  }




  ///////////////////////    Several initial population  (different pop size (seq))  ///////////////
  //////////////   Always with the  compteurMax
  /////////////////////////////////////
  //  With this technic, we have the List() at the begining  that's why we take the tail
  def createSeveralInitialPop(initialPopSizes: Seq[Double])(tau: Double = Plant.tau, proportionMowing: Double = Plant.proportionMowing,
                                                               K: Double = Plant.K, L: Double = Plant.L, d1: Double = Plant.d1, b1: Double = Plant.b1,
                                                               shape: Double = Plant.shape, scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                                                               deathParameterScaling: Double = Plant.deathParameterScaling,
                                                               mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double = Plant.a_0(Plant.K)) : Seq[PlantEvolution]  = {

    slaveCreateSeveralInitialPop(initialPopSizes)(tau, proportionMowing,
      K, L, d1, b1, shape, scale, deathParameterDecrease, deathParameterScaling, mowing_parameter, bbar, a_0 , Seq(PlantEvolution(Nil,Nil))).tail
  }



}
