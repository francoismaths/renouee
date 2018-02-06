package renouee

import better.files.File
import org.apache.commons.math3.distribution.UniformRealDistribution

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

  val initialPopulation = createInitialPop.createPopIni(Plant.initialPopulationSize) : PlantEvolution

  val res = Run.simuHistory(initialPopulation,parameter.T , Plant.tau, Plant.proportionMowing ,Plant.K,  Plant.L, Plant.d1, Plant.b1, Plant.shape,
    Plant.scale, Plant.deathParameterDecrease,  Plant.deathParameterScaling, Plant.mowing_parameter,
    Plant.bbar, Plant.a_0(Plant.K))


  lazy val name_Dir1 = "1run"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultSingleExperiment(initialPopulation,res,dir1)








}
