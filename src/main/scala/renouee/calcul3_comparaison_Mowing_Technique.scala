package renouee

import better.files.File

object calcul3_comparaison_Mowing_Technique extends App {

  implicit val rng = random(2)

  val popIni = createInitialPop.createInitialPopEvolution(1000,parameter.Nmax, Management(proportionMowing = 0.9, tau = 1.0 ),PlantGrowth())

  val resAlea = ManagementTechniqueUtil.mowingAlea(popIni,Management(proportionMowing = 0.7),PlantGrowth(),ResultType.Last,1)

  val resPeryphery = ManagementTechniqueUtil.mowingPeriphery(popIni, Management(proportionMowing = 0.8), PlantGrowth(),ResultType.Last,1)

  val resSide = ManagementTechniqueUtil.mowingSideProportion(popIni,Management(proportionMowing = 0.8),PlantGrowth(),ResultType.Last,1)

  val resSideXPosition = ManagementTechniqueUtil.mowingSidePosition(popIni,Management(xAxisMowLimit = 0),PlantGrowth(),ResultType.Last,1)



  ////////////////////////

  val f = File("popIni.txt")
  createfileforR.writeFinalPop(f,popIni)

  val g= File("resAlea.txt")
  createfileforR.writeFinalPop(g,resAlea)

  val h = File("resPeryphery.txt")
  createfileforR.writeFinalPop(h,resPeryphery)

  val i= File("resSide.txt")
  createfileforR.writeFinalPop(i,resSide)

  val j= File("resSideXPosition.txt")
  createfileforR.writeFinalPop(j,resSideXPosition)



}
