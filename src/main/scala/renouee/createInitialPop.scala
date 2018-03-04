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
  def slaveSimuInitialPopEvolution(plantEvolution: PlantEvolution, iter2: Int = 0, NmaxPopIni : Int,
                                   initialPopSize : Double,
                                   management: Management,
                                   plantGrowth: PlantGrowth)(implicit random: Random): PlantEvolution = {
    /* create initial pop from the evolution of one indiv (until reach the desired pop size), or extinction, or Nmax reach
      with birth/death rate, mow
     */
    if ((iter2 == NmaxPopIni) || plantEvolution.plants.isEmpty || (plantEvolution.plants.length == initialPopSize )) plantEvolution
    else slaveSimuInitialPopEvolution(Evolution.nextEvolution(plantEvolution, management, plantGrowth, ResultType.Last, ManagementTechnique.Alea),
      iter2 + 1, NmaxPopIni,
      initialPopSize, management, plantGrowth)(random)
  }


  // create initial pop from one indiv, this function to have not the iter in argument of the function
  def createInitialPopEvolution(initialPopSize : Double = Plant.initialPopulationSize, NmaxPopIni : Int,
                                management: Management,
                                plantGrowth: PlantGrowth  )(implicit random: Random): PlantEvolution = {

    val pop0 = PlantEvolution(Seq(Plant(0.0,0.0, plantGrowth.biomassFirstIndiv)).toVector , Vector(InfosEvolution(0,0,0.0,""))  ) : PlantEvolution
    val res = slaveSimuInitialPopEvolution(pop0,0,NmaxPopIni, initialPopSize, management,plantGrowth)(random)

    /*
    //  en modifiant avec Lens
    val timeToModify = PlantEvolution.infosEvolution  composeTraversal Each.each composeLens InfosEvolution.time
    val allBiomases = PlantEvolution.plants composeTraversal Each.each composeLens Plant.biomass
    //timeToModify.modify(0.0)(res) : PlantEvolution
    */

    PlantEvolution(res.plants,Vector(InfosEvolution(popSize = res.plants.length,
      area = fieldutil.area(res.plants.map(p => p.x),res.plants.map(p => p.y) ))))

  }






  //////////////////////////////
  //////////////  Always initial pop, with evolution   //////////////
  //////////////  Test : si la premiere taille de pop est nulle (morte) ie rapide : on retente, (ie pas de chance a cause de la fauche pex)
  //   on retente aussi si la taille de pop est plus grande qu'un seuil : si on est plus petit, c'est que le jeu de paramètre ne parmet
  //   pas d'avoir la pop souhaitée (retenter avec un Nmax plus grand ? double ?)
  ////////////  on garde en mémoire le meilleur essai si plus grand que le seuil
  // intialiser memory avec une réaliation et pas par NULL
  ///////////////////////////////


  @tailrec
  def slaveCreatePopIni(initialPopSize : Double = Plant.initialPopulationSize, NmaxPopIni : Int,
                         compteur : Int = 1, compteurMax : Int = parameter.compteurMax,
                         management: Management,
                         plantGrowth: PlantGrowth,
                         memory : PlantEvolution ,
                         seuil : Int)(implicit random: Random) : PlantEvolution = {

    if (compteur > compteurMax) memory

    else if (memory.plants.length == initialPopSize) memory

      else if (memory.plants.length > seuil)  {
        val temp = createInitialPop.createInitialPopEvolution(initialPopSize, NmaxPopIni, management, plantGrowth)(random)

          slaveCreatePopIni(initialPopSize, math.floor(1.5 *NmaxPopIni).toInt,
            compteur + 1,  compteurMax , management, plantGrowth, temp, seuil)(random)
        }

      else if  (memory.plants.length == 0) {
        val temp = createInitialPop.createInitialPopEvolution(initialPopSize, NmaxPopIni, management, plantGrowth)(random)

        slaveCreatePopIni(initialPopSize, NmaxPopIni,
          compteur + 1, compteurMax, management, plantGrowth, temp, seuil)(random)
    }

      else PlantEvolution(Nil.toVector, Nil.toVector)
  }

    //  Function without the iter
    def createPopIni(initialPopSize : Double = Plant.initialPopulationSize, NmaxPopIni : Int, compteurMax : Int = parameter.compteurMax,
                      management: Management,
                      plantGrowth: PlantGrowth  )(implicit random: Random)  : PlantEvolution = {

      val temp  = createInitialPop.createInitialPopEvolution(initialPopSize, NmaxPopIni, management, plantGrowth )(random)
      slaveCreatePopIni(initialPopSize, NmaxPopIni, 1, compteurMax, management, plantGrowth,temp,(initialPopSize/2).toInt)(random)
    }






  ///////////////////////////////  Create several initial population  ///////////////////
  ///////////////////////////////  different pop sizes, but the same management (mowing)
  ///////////////  cause if mowing is too intense, it is harder to create an initial pop /////
  ////////////////////////////////////////////////////////////////////////////////////////

  def slaveCreateSeveralInitialPop(initialPopSizes: Seq[Double])(NmaxPopsInis : Seq[Int], compteurMax : Int = parameter.compteurMax,
                                                                 management: Management,
                                                                 plantGrowth: PlantGrowth,
                                                                 res: Seq[PlantEvolution] = Seq(PlantEvolution(Nil.toVector,Nil.toVector)) )
                                                                 (implicit random: Random) : Seq[PlantEvolution] = {
    if (initialPopSizes.isEmpty) res
    else {
      val temp = math.floor(initialPopSizes.head)
      val tempres = createInitialPop.createPopIni(temp, NmaxPopsInis.head , compteurMax, management, plantGrowth)(random)

      slaveCreateSeveralInitialPop(initialPopSizes.tail)(NmaxPopsInis.tail, compteurMax, management, plantGrowth, res :+ tempres)(random)
    }
  }



  ///////////////////////    Several initial population  (different pop size (seq))  ///////////////
  //////////////   Always with the  compteurMax
  /////////////////////////////////////
  //  With this technique, we have the List() at the beginning  that's why we take the tail
  def createSeveralInitialPop(initialPopSizes: Seq[Double])(NmaxPopIni : Seq[Int], compteurMax : Int = parameter.compteurMax,
                                                            management: Management,
                                                            plantGrowth: PlantGrowth)
                                                            (implicit random: Random): Seq[PlantEvolution] = {

    slaveCreateSeveralInitialPop(initialPopSizes)(NmaxPopIni,compteurMax, management, plantGrowth, Seq(PlantEvolution(Nil.toVector,Nil.toVector)))(random).tail
  }



}
