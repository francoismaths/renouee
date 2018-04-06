package renouee

import better.files.File

object calcul3_comparaison_Mowing_Technique extends App {

  /*
  Il n'y a pas d'évolution, c'est ça la différence avec calcul 4
   */

  implicit val rng = random(2)

  val popIni = createInitialPop.createInitialPopEvolution(1000,parameter.Nmax, Management(proportionMowing = 0.9, tau = 1.0 ),PlantGrowth())

  val resAlea = ManagementTechniqueUtil.mowingAlea(popIni,Management(proportionMowing = 0.7),PlantGrowth(),ResultType.Last,1)

  val resPeryphery = ManagementTechniqueUtil.mowingPeriphery(popIni, Management(proportionMowing = 0.8), PlantGrowth(),ResultType.Last,1)

  val resSide = ManagementTechniqueUtil.mowingSideProportion(popIni,Management(proportionMowing = 0.8),PlantGrowth(),ResultType.Last,1)

  val resSideXPosition = ManagementTechniqueUtil.mowingSidePosition(popIni,Management(xAxisMowLimit = 0),PlantGrowth(),ResultType.Last,1)



  ////////////////////////

  lazy val name_Dir1 = "resultFilesComparaisonMowingTechnique_NoEvol"
  lazy val dir1 = new java.io.File(name_Dir1)
  dir1.mkdir()

  createfileforR.writeResultComparaisonTechnique(dir1,popIni,resAlea,resPeryphery,resSide,resSideXPosition)



}
