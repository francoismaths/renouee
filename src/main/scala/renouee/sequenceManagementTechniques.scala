package renouee

import scala.util.Random
import better.files._

case class ResultSequenceMT(popinis: Seq[Plant], popsT: Seq[Plant])


object sequenceManagementTechniques {


  // slave function to find the x position of mowing given the proportion to mow
  def findXLimitPositionToMow(initialPop : PlantEvolution, proportionMowing:Double) : Double = {
    // trie les x par ordre croissant
    val temp = (initialPop.plants.map(p=>p.x)).sorted
    // sélectionne le bon
    if (temp.length > 0){
      temp(math.max(math.floor((1-proportionMowing)*(temp.length)).toInt -1,0)) } else {0}
  }


///////////
//  RQ T: C'EST LA DURÉE DE CHAQUE SOUS PROJET, ex (1,2,1) (ie au total 4 ans), ce n'est pas les temps successifs.
///////////

  // keep the history of the evolution (seq of management), to apply with res = initialpop (:plantEvol)
  def slaveSequenceMTSide(res : PlantEvolution,  NmaxEvol : Int = parameter.Nmax , taillePopFinaleMax: Int,
                                             T: Seq[Double], tau : Seq[Double], xAxisMowLimit:Double,
                                             resultType: ResultType, plantGrowth: PlantGrowth)
                    (implicit random: Random): PlantEvolution  ={

    if (T.isEmpty) res
    else {
      val tempT = T.head // The T that will evolve during this iteration
      val temptau = tau.head // The tau that will evolve during this iteration

      val tempres = Run.simu(PlantEvolution(res.plants,Vector(InfosEvolution(0,res.infosEvolution.last.popSize,res.infosEvolution.last.area,""))),
        NmaxEvol, taillePopFinaleMax, Management(T = tempT,
        tau = temptau,
        xAxisMowLimit = xAxisMowLimit),
        plantGrowth, resultType, ManagementTechnique.SideXPosition)(random)

      slaveSequenceMTSide(PlantEvolution(tempres.plants, res.infosEvolution ++ tempres.infosEvolution), NmaxEvol, taillePopFinaleMax, T.tail, tau.tail, xAxisMowLimit, resultType, plantGrowth)(random)
    }
  }



  // keep the history of the evolution (seq of management), to apply with res = initialpop (:plantEvol)
  def slaveSequenceMT(res : PlantEvolution,  NmaxEvol : Int = parameter.Nmax , taillePopFinaleMax: Int,
                          T: Seq[Double], tau : Seq[Double], proportionMowing :Seq[Double], managementTechnique: ManagementTechnique,
                          resultType: ResultType, plantGrowth: PlantGrowth)
                         (implicit random: Random): PlantEvolution  ={

    if (T.isEmpty) res
    else {
      val tempT = T.head // The T that will evolve during this iteration
      val temptau = tau.head // The tau that will evolve during this iteration
      val tempProportionMowing= proportionMowing.head // The tau that will evolve during this iteration


      val tempres = Run.simu(PlantEvolution(res.plants,Vector(InfosEvolution(0,res.infosEvolution.last.popSize,res.infosEvolution.last.area,""))),
        NmaxEvol, taillePopFinaleMax, Management(T = tempT,
          tau = temptau, proportionMowing = tempProportionMowing),
        plantGrowth, resultType, managementTechnique)(random)

      slaveSequenceMT(PlantEvolution(tempres.plants, res.infosEvolution ++ tempres.infosEvolution), NmaxEvol, taillePopFinaleMax, T.tail, tau.tail, proportionMowing.tail, managementTechnique, resultType, plantGrowth)(random)
    }
  }





/*
  def sequenceMTSide(initialPopSize: Double)(NmaxPopsIni : Int , compteurMax : Int = parameter.compteurMax, NmaxEvol : Int = parameter.Nmax , taillePopFinaleMax: Int,
                                                           managementInitialPops: Management,
                                                           T: Seq[Double], tau : Seq[Double], proportionMowing: Double,
                                                           plantGrowth: PlantGrowth)
                            (implicit random: Random) ={

    val temp_initial_pop =  createInitialPop.createPopIni(initialPopSize,NmaxPopsIni, compteurMax,managementInitialPops,plantGrowth)(random)

    val xAxisMowLimit = findXLimitPositionToMow(temp_initial_pop,proportionMowing)

    val final_pop = slaveSequenceMTSide(temp_initial_pop,NmaxEvol,taillePopFinaleMax, T, tau, xAxisMowLimit, ResultType.Last, plantGrowth)(random)

  }
  */

  // couple the creation and the evolution + writting
  def evolutionWriteSequenceSide(initialPopSize: Double)(file1 : java.io.File, file2 : java.io.File)
                                (NmaxPopsIni : Int , compteurMax : Int = parameter.compteurMax, NmaxEvol : Int = parameter.Nmax , taillePopFinaleMax: Int,
                                                         managementInitialPops: Management,
                                                         T: Seq[Double], tau : Seq[Double], proportionMowing: Double,
                                                         plantGrowth: PlantGrowth)
                                (implicit random: Random) ={

    val temp_initial_pop =  createInitialPop.createPopIni(initialPopSize,NmaxPopsIni, compteurMax,managementInitialPops,plantGrowth)(random)

    val xAxisMowLimit = findXLimitPositionToMow(temp_initial_pop,proportionMowing)

    val final_pop = slaveSequenceMTSide(temp_initial_pop,NmaxEvol,taillePopFinaleMax, T, tau, xAxisMowLimit, ResultType.Last, plantGrowth)(random)

    // record positions of initial population (2008)
    val f = file1.toScala /   "pos_popIni_0.txt"
    slaveArgumentsFiles.writeFinalPop(f, temp_initial_pop.plants)

    // record positions of final population (2015)
    val g = file2.toScala /   "pos_popFinal_0.txt"
    slaveArgumentsFiles.writeFinalPop(g,final_pop.plants)

  }





  def sequenceMT(initialPopSize: Double)(NmaxPopsIni : Int , compteurMax : Int = parameter.compteurMax, NmaxEvol : Int = parameter.Nmax , taillePopFinaleMax: Int,
                                             managementInitialPops: Management,
                                             T: Seq[Double], tau : Seq[Double], proportionMowing: Seq[Double], managementTechnique: ManagementTechnique,
                                             plantGrowth: PlantGrowth)
                    (implicit random: Random)  ={

    val temp_initial_pop =  createInitialPop.createPopIni(initialPopSize,NmaxPopsIni, compteurMax,managementInitialPops,plantGrowth)(random)

    val final_pop = slaveSequenceMT(temp_initial_pop,NmaxEvol,taillePopFinaleMax, T, tau, proportionMowing, managementTechnique, ResultType.Last, plantGrowth)(random)

    final_pop
  }

}
