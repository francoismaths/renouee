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

  val rng = new RandomAdaptor(new Well44497b(43))

  val initialPopulation = createInitialPop.createPopIni(Plant.initialPopulationSize,parameter.Nmax,parameter.compteurMax,
    Management(tau= 1.0, proportionMowing = 0.9),PlantGrowth())(rng) : PlantEvolution

  val res = Run.simu(initialPopulation,Management(),PlantGrowth(),ResultType.Last,ManagementTechnique.Alea)(rng)


  lazy val name_Dir1 = "1run"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1)








}
