package renouee


import org.apache.commons.math3._
import org.apache.commons.math3.stat.descriptive.moment.Mean

import math._
import scala.util.Random

import monocle.function.Each
import monocle.macros._



object ManagementTechniqueUtil {

  /////////////////////////////////////////////////////////
  //  Periphery
  /////////////////////////////////////////////////////


  def mowingPeriphery(plantEvolution: PlantEvolution, management: Management, plantGrowth: PlantGrowth, resultType: ResultType, t:Double): PlantEvolution ={

    // already calculated in evolution
    val xPos = plantEvolution.plants.map(p => p.x)
    val yPos = plantEvolution.plants.map(p => p.y)
    val popSize = xPos.length

    val xCenterStand = xPos.sum / xPos.length
    val yCenterStand = yPos.sum / yPos.length

    // list of the distances from the centre, squared
    val distanceToCenter =
      (xPos.map{ a => math.pow(a - xCenterStand, 2)} zip yPos.map{ a => math.pow(a - yCenterStand, 2) })
        .map { z => z._1 + z._2 }

    val numberToMow = math.floor(management.proportionMowing * popSize ).toInt
    // permet de trouver la distance au centre au dela de laquelle on fauche

    val distanceSorted = distanceToCenter.sorted   // trie croissant
    val distanceLimitMow = distanceSorted(popSize - numberToMow -1)

    //  on a dejà fait évoluer la biomasse dans evolution. (le plantEvolution fourni : tempEvolBiomass,
    // pour la fonction est le bon), juste à Faucher


    /*
    val allBiomases = PlantEvolution.plants composeTraversal Each.each composeLens Plant.biomass

    val allBiomases2 = PlantEvolution composeTraversal Each.each composeLens Plant
    //val tempEvolBiomass = plantEvolution.plants.map(p => Plant.biomassVonBert(plantGrowth.K, plantGrowth.L)(t, t0)(p.biomass))


    val tempEvolBiomass =
        allBiomases.modify(Plant.biomassVonBert(plantGrowth.K, plantGrowth.L)(t, t0))(plantEvolution)

    val tempEvolBiomass2 =
      (allBiomases.modify(Plant.biomassVonBert(plantGrowth.K, plantGrowth.L)(t, t0)) compose
        allBiomases.modify(Plant.biomassVonBert(plantGrowth.K, plantGrowth.L)(t, t0)))
    (plantEvolution)
    val tempAfterMow = allBiomases.modify(fieldutil.mowingEffectPeriphery(plantGrowth.mowingParameter)(distanceLimitMow,xCenterStand,yCenterStand)())(tempEvolBiomass2)
    */


    val allPlants= (PlantEvolution.plants composeTraversal Each.each )

    val tempAfterMow = allPlants.modify(fieldutil.slaveMowingEffectPeriphery(plantGrowth.mowingParameter)(distanceLimitMow,xCenterStand,yCenterStand))(plantEvolution)  : PlantEvolution



    //val tempAfterMow = plantEvolution.plants.map(q => q.copy(biomass = (fieldutil.mowingEffectPeriphery(plantGrowth.mowingParameter)(distanceLimitMow,xCenterStand,yCenterStand)(q))(q.biomass)))

    resultType match {
      case ResultType.Last => PlantEvolution(tempAfterMow.plants, Vector(InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowPeriphery")) )

      case ResultType.All => PlantEvolution(tempAfterMow.plants, plantEvolution.infosEvolution :+ InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowPeriphery") )

    }




  }

  /*  si on veut tester
  val b =  PlantEvolution(Seq( Plant(0,0,1),  Plant(-1,-0,2)  , Plant(10,12,3)  , Plant(2,2,4) ,
    Plant(-7,-6,5),  Plant(4,5,6)  , Plant(-2,-3,7)  , Plant(8,9,8)  )
    , Seq(InfosEvolution())  )
  */



  ////////////////////////////////////////////
  // Side (porportion)
  //////////////////////////////////////////

  // mow the right side of the stand (proportion chosen)
  def mowingSideProportion(plantEvolution: PlantEvolution, management: Management, plantGrowth: PlantGrowth, resultType: ResultType, t:Double ): PlantEvolution ={

    // already calculated in evolution
    val xPos = plantEvolution.plants.map(p => p.x)
    val yPos = plantEvolution.plants.map(p => p.y)
    val popSize = xPos.length
    val numberToMow = math.floor(management.proportionMowing * popSize).toInt


    val distanceSorted = xPos.sorted   // trie croissant
    val distanceLimitMowX = distanceSorted(popSize - numberToMow -1)

    //val tempAfterMow = plantEvolution.plants.map(q => q.copy(biomass = fieldutil.mowingEffectSide(plantGrowth.mowingParameter)(distanceLimitMowX)(q)(q.biomass)))

    val allPlants= (PlantEvolution.plants composeTraversal Each.each )

    val tempAfterMow = allPlants.modify(fieldutil.slaveMowingEffectSide(plantGrowth.mowingParameter)(distanceLimitMowX))(plantEvolution)  : PlantEvolution



    resultType match {
      case ResultType.Last => PlantEvolution(tempAfterMow.plants, Vector(InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowSide")) )

      case ResultType.All => PlantEvolution(tempAfterMow.plants, plantEvolution.infosEvolution :+ InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowSide") )
    }



  }


  ////////////////////////////////////////////
  // Alea
  //////////////////////////////////////////

  // Mow a proportion  chosen of the stand (uniformly at random)
  def mowingAlea(plantEvolution: PlantEvolution, management: Management, plantGrowth: PlantGrowth, resultType: ResultType, t:Double )(implicit random: Random): PlantEvolution ={

    // already calculated in evolution
    val xPos = plantEvolution.plants.map(p => p.x)
    val yPos = plantEvolution.plants.map(p => p.y)
    val popSize = xPos.length


    val allBiomases = PlantEvolution.plants composeTraversal Each.each composeLens Plant.biomass
    val tempAfterMow2 = allBiomases.modify(fieldutil.mowingEffect(management.proportionMowing)(plantGrowth.mowingParameter))(plantEvolution) : PlantEvolution


    //val tempAfterMow = plantEvolution.plants.map(q => q.copy(biomass = fieldutil.mowingEffect(management.proportionMowing)(plantGrowth.mowingParameter)(q.biomass)(random)))

    resultType match {
      case ResultType.Last => PlantEvolution(tempAfterMow2.plants, Vector(InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowAlea")))

      case ResultType.All => PlantEvolution(tempAfterMow2.plants, plantEvolution.infosEvolution :+ InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowAlea"))
    }
  }




  ////////////////////////////////////////////
  // Side (position)
  //////////////////////////////////////////

  // mow the right side of the stand (proportion chosen)
  def mowingSidePosition(plantEvolution: PlantEvolution, management: Management, plantGrowth: PlantGrowth, resultType: ResultType, t:Double ): PlantEvolution ={

    // already calculated in evolution
    val xPos = plantEvolution.plants.map(p => p.x)
    val yPos = plantEvolution.plants.map(p => p.y)
    val popSize = xPos.length

    //val tempAfterMow = plantEvolution.plants.map(q => q.copy(biomass = fieldutil.mowingEffectSide(plantGrowth.mowingParameter)(distanceLimitMowX)(q)(q.biomass)))

    val allPlants= (PlantEvolution.plants composeTraversal Each.each )

    val tempAfterMow = allPlants.modify(fieldutil.slaveMowingEffectSide(plantGrowth.mowingParameter)(management.xAxisMowLimit))(plantEvolution)  : PlantEvolution



    resultType match {
      case ResultType.Last => PlantEvolution(tempAfterMow.plants, Vector(InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowSideXPosition")) )

      case ResultType.All => PlantEvolution(tempAfterMow.plants, plantEvolution.infosEvolution :+ InfosEvolution(t, popSize, fieldutil.area(xPos, yPos), "MowSideXPosition") )
    }



  }





}
