package renouee

import org.apache.commons.math3.distribution.UniformRealDistribution

import scala.annotation.tailrec
import scala.util.Random
import monocle.function.Each
import monocle.macros._


object createInitialPop {


  //implicit val rnd = new scala.util.Random(7)

  //////////////////////////////////////////////////
  /////////////// Create an initial pop , with evolution //////////////
  ///////////////  For the management (alea), prefer use tau = 1, proportionMowing = 0.9    //////////////

  ///////////////  Rq: the biomass of the first individual is an argument    //////////////
  ////////////////////////////////////////////////

  @tailrec
  def slaveSimuInitialPopEvolution(plantEvolution: PlantEvolution, iter2: Int = 0, Nmax : Int,
                                   initialPopSize : Double,
                                   management: Management,
                                   plantGrowth: PlantGrowth)(implicit random: Random): PlantEvolution = {
    /* create initial pop from the evolution of one indiv (until reach the desired pop size), or extinction, or Nmax reach
      with birth/death rate, mow
     */
    if ((iter2 == Nmax) || plantEvolution.plants.isEmpty || (plantEvolution.plants.length == initialPopSize )) plantEvolution
    else slaveSimuInitialPopEvolution(Evolution.nextEvolution(plantEvolution, management, plantGrowth, ResultType.Last, ManagementTechnique.Alea),
      iter2 + 1, Nmax,
      initialPopSize, management, plantGrowth)(random)
  }


  // create initial pop from one indiv, this function to have not the iter in argument of the function
  def createInitialPopEvolution(initialPopSize : Double = Plant.initialPopulationSize , Nmax : Int = parameter.Nmax,
                                management: Management,
                                plantGrowth: PlantGrowth  )(implicit random: Random): PlantEvolution = {

    val pop0 = PlantEvolution(Seq(Plant(0.0,0.0, plantGrowth.biomassFirstIndiv)).toVector , Vector(InfosEvolution(0,0,0.0,""))  ) : PlantEvolution
    val res = slaveSimuInitialPopEvolution(pop0,0,Nmax, initialPopSize, management,plantGrowth)(random)

    //  en modifiant avec Lens
    val timeToModify = PlantEvolution.infosEvolution  composeTraversal Each.each composeLens InfosEvolution.time
    val allBiomases = PlantEvolution.plants composeTraversal Each.each composeLens Plant.biomass
    //timeToModify.modify(0.0)(res) : PlantEvolution

    PlantEvolution(res.plants,Vector(InfosEvolution(popSize = res.plants.length,
      area = fieldutil.area(res.plants.map(p => p.x),res.plants.map(p => p.y) ))))

  }


  //////////////////////////////
  //////////////  Always initial pop, with evolution   //////////////
  //////////////  no time,  with compteurMax (to avoid the pop created is Nil cause of bad luck (death))
  /////////     just require a pop size to reach (if no success, cause of parameters for example)
  ////////////  it returns Nil : PlantEvolution (there are mow, birth, death)
  ///////////////////////////////

  def slaveCreatePopIni(initialPopSize : Double = Plant.initialPopulationSize , Nmax : Int = parameter.Nmax,
                        compteur : Int = 1, compteurMax : Int = parameter.compteurMax,
                        management: Management,
                        plantGrowth: PlantGrowth  )(implicit random: Random) : PlantEvolution = {

    if (compteur > compteurMax)   PlantEvolution(Nil.toVector,Nil.toVector)
    else {

      val temp  = createInitialPop.createInitialPopEvolution(initialPopSize, Nmax, management, plantGrowth )(random)

      if (temp.plants.length == initialPopSize) {
        temp
      }

      else {
        slaveCreatePopIni(initialPopSize, Nmax,
          compteur+1, compteurMax, management, plantGrowth)(random)
      }
    }
  }


  //  Function without the iter
  def createPopIni(initialPopSize : Double = Plant.initialPopulationSize , Nmax : Int = parameter.Nmax, compteurMax : Int = parameter.compteurMax,
                   management: Management,
                   plantGrowth: PlantGrowth  )(implicit random: Random)  : PlantEvolution = {

    slaveCreatePopIni(initialPopSize, Nmax, 1, compteurMax, management, plantGrowth)(random)
  }


  ///////////////////////////////  Create several initial population  ///////////////////
  ///////////////////////////////  different pop sizes, but the same management (mowing)
  ///////////////  cause if mowing is too intense, it is harder to create an initial pop /////
  ////////////////////////////////////////////////////////////////////////////////////////

  def slaveCreateSeveralInitialPop(initialPopSizes: Seq[Double])(Nmax : Int = parameter.Nmax, compteurMax : Int = parameter.compteurMax,
                                                                 management: Management,
                                                                 plantGrowth: PlantGrowth,
                                                                 res: Seq[PlantEvolution] = Seq(PlantEvolution(Nil.toVector,Nil.toVector)) )
                                                                 (implicit random: Random) : Seq[PlantEvolution] = {
    if (initialPopSizes.isEmpty) res
    else {
      val temp = math.floor(initialPopSizes.head)
      val tempres = createInitialPop.createPopIni(temp, Nmax, compteurMax, management, plantGrowth)(random)

      slaveCreateSeveralInitialPop(initialPopSizes.tail)(Nmax, compteurMax, management, plantGrowth, res :+ tempres)(random)
    }
  }



  ///////////////////////    Several initial population  (different pop size (seq))  ///////////////
  //////////////   Always with the  compteurMax
  /////////////////////////////////////
  //  With this technique, we have the List() at the beginning  that's why we take the tail
  def createSeveralInitialPop(initialPopSizes: Seq[Double])(Nmax : Int = parameter.Nmax, compteurMax : Int = parameter.compteurMax,
                                                            management: Management,
                                                            plantGrowth: PlantGrowth)
                                                            (implicit random: Random): Seq[PlantEvolution] = {

    slaveCreateSeveralInitialPop(initialPopSizes)(Nmax,compteurMax, management, plantGrowth, Seq(PlantEvolution(Nil.toVector,Nil.toVector)))(random).tail
  }



}
