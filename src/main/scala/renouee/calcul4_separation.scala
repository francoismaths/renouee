package renouee

import better.files.File

object calcul4_separation extends App {

  implicit val rng = random(2)

  val popIni = createInitialPop.createInitialPopEvolution(1000, parameter.Nmax, Management(proportionMowing = 0.9, tau = 1.0 ),PlantGrowth())

  //val resAlea = ManagementTechniqueUtil.mowingAlea(popIni,Management(proportionMowing = 0.7),PlantGrowth(),ResultType.Last,1)

  val resEvolAleaT = Run.simu(popIni,Management(tau = 3,T = 8.0),PlantGrowth(),ResultType.Last,ManagementTechnique.Alea)


  val resEvolPerypheryT = Run.simu(popIni,Management(tau = 3,proportionMowing = 0.8),PlantGrowth(),ResultType.Last,ManagementTechnique.Periphery)

  val resEvolSideT = Run.simu(popIni,Management(tau = 3,proportionMowing = 0.8),PlantGrowth(),ResultType.Last,ManagementTechnique.Side)

  val resEvolSideXPositionT = Run.simu(popIni,Management(tau = 3,xAxisMowLimit = 0),PlantGrowth(),ResultType.Last,ManagementTechnique.SideXPosition)

  //println(resEvolSideXPositionT)

  ////////////////////////

  val f = File("popIni.txt")
  createfileforR.writeFinalPop(f,popIni)

  val g= File("resEvolAlea.txt")
  createfileforR.writeFinalPop(g,resEvolAleaT)


  val h = File("resEvolPeryphery.txt")
  createfileforR.writeFinalPop(h,resEvolPerypheryT)

  val i= File("resEvolSide.txt")
  createfileforR.writeFinalPop(i,resEvolSideT)

  val j= File("resEvolSideXPosition.txt")
  createfileforR.writeFinalPop(j, resEvolSideXPositionT)

}
