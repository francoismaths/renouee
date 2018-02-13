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

  val rng = new RandomAdaptor(new Well44497b(1))

  val initialPopulationSize = 1000
  val managementPopIni = Management(tau= 1.0, proportionMowing = 0.9)
  val management = Management(tau=3)
  val plantGrowth = PlantGrowth()
  val managementTechnique = ManagementTechnique.Alea



  val initialPopulation = createInitialPop.createPopIni(initialPopulationSize,parameter.Nmax,parameter.compteurMax,
    managementPopIni,plantGrowth)(rng) : PlantEvolution

  val res = Run.simu(initialPopulation,management,plantGrowth,ResultType.All,managementTechnique)(rng)


  lazy val name_Dir1 = "1run"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1,plantGrowth,management, managementTechnique )








}
