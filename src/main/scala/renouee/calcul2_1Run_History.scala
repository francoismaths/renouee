package renouee

import better.files.File
import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

object calcul2_1Run_History extends App {


  //////////////////////////////////////////////////////////
  ////////////   Single experience
  /////////////////////////////////////////////////////////

  /*
  Produce four file in the directory 1run :
  - a file with the parameter
  - a file with the initial pop (biomass and position)
  - a file with the final pop (biomass and position)
  - a file with the history (time, popsize, area(scala, rectangle), event)
  */

  val rng = new RandomAdaptor(new Well44497b(3))

  val NmaxPopIni = 100000
  val NmaxEvol= 1000000
  val initialPopulationSize = 1
  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)
  val management = Management(T=1000)
  val plantGrowth = PlantGrowth()
  val managementTechnique = ManagementTechnique.Alea



  val initialPopulation = createInitialPop.createPopIni(initialPopulationSize,NmaxPopIni ,parameter.compteurMax,
    managementPopIni,plantGrowth)(rng) : PlantEvolution


  val res = Run.simu(initialPopulation,NmaxEvol, management,plantGrowth,ResultType.All,managementTechnique)(rng)


  lazy val name_Dir1 = "1run"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1,plantGrowth,management, managementTechnique )


  /*
  TO DO : faire une évolution d'une population initiale de 1 indiv avec un temps long, et un grand nombre d'iter
  But : voir comment évolue la taille de la pop en fonction du nb d'iter ( faire sans clear event? et avec)
  comme ca on a vraiment les iter
   */






}
