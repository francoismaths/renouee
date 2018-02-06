package renouee


import org.apache.commons.math3._
import org.apache.commons.math3.stat.descriptive.moment.Mean

import math._


object spatialisation_mowing {

  // à mettre dans une nouvelle fonction evolution : evolution_spatialisation_mowing_périphérie
  // périphérie


  // distance au centre de la tache :
  /*  On a :
    val xpos = plantEvolution.plants.map(p => p.x)
    val ypos = plantEvolution.plants.map(p => p.y)
    val pop_size = xpos.length
   */

  // pour avoir une pop sous la main
  //val b = createInitialPop.createPopIni()  // type : PlantEvolution

  /*
  val b =  PlantEvolution(Seq( Plant(0,0,1),  Plant(-1,-0,2)  , Plant(10,12,3)  , Plant(2,2,4) ,
    Plant(-7,-6,5),  Plant(4,5,6)  , Plant(-2,-3,7)  , Plant(8,9,8)  )
              , Seq(InfosEvolution())  )


  val xpos = b.plants.map(p => p.x)
  val ypos = b.plants.map(p => p.y)
  val pop_size = xpos.length

  val xcenter_stand = xpos.sum / xpos.length
  val ycenter_stand = ypos.sum / ypos.length

  // au carré
  val distanceToCenter =
    (xpos.map{ a => math.pow(a - xcenter_stand, 2)} zip ypos.map{ a => math.pow(a - ycenter_stand, 2) })
     .map { z => z._1 + z._2 }

  val proportionToMow : Double = 0.5  // entre 0 et 1  // équivalent du  Plant.proportionMowing
  // a mettre en param de la grande fonction

  val numberToMow = math.floor(proportionToMow * pop_size ).toInt
  // donne l'indice de ou s'arréter sur la population triée, s'en servir pour accéder à la distance de l'indiv
  // par distance croissante/décroissante ?

  val distance_sorted = distanceToCenter.sorted   // trie croissant
  val distance_limite_mow = distance_sorted(numberToMow)

  def distancePlantToCenter(q:Plant,xcentre : Double, ycentre : Double)={
    math.pow(q.x- xcentre, 2)  + math.pow(q.y- ycentre, 2)
  }

  // A mettre dans fieldutil, équivalent de mowing_effect
  def mowing_effect_PERIPHERIE( mowing_parameter : Double )(distance_repere : Double = distance_limite_mow)
                              (q : Plant)(a : Double) : Double = {
    if ( distancePlantToCenter(q,xcenter_stand,ycenter_stand) < distance_repere) a
    else a * Plant.mowing(mowing_parameter)(a)
  }


  PlantEvolution(b.plants.map(q => q.copy(biomass = (mowing_effect_PERIPHERIE(Plant.mowing_parameter)(distance_limite_mow)(q)(Plant.biomasse_von_bert(5, 0.2)(1, 0)(q.biomass)))))
    , Seq(InfosEvolution(1, pop_size, fieldutil.area(xpos, ypos), "Mowing")))

*/

/////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////

def mowingPeriphery(plantEvolution: PlantEvolution,proportionToMow : Double = Plant.proportionMowing): PlantEvolution ={

  // dejà calculé dans evolution
  val xpos = plantEvolution.plants.map(p => p.x)
  val ypos = plantEvolution.plants.map(p => p.y)
  val pop_size = xpos.length

  val xcenter_stand = xpos.sum / xpos.length
  val ycenter_stand = ypos.sum / ypos.length

  // liste des distances au centre,au carré
  val distanceToCenter =
    (xpos.map{ a => math.pow(a - xcenter_stand, 2)} zip ypos.map{ a => math.pow(a - ycenter_stand, 2) })
      .map { z => z._1 + z._2 }

  val numberToMow = math.floor(proportionToMow * pop_size ).toInt
  // permet de trouver la distance au centre au dela de laquelle on fauche

  val distance_sorted = distanceToCenter.sorted   // trie croissant
  val distance_limite_mow = distance_sorted(pop_size - numberToMow -1)


  // to use in the map, give the distance to center of the plant q
  def distancePlantToCenter(q:Plant,xcentre : Double, ycentre : Double)={
    math.pow(q.x- xcentre, 2)  + math.pow(q.y- ycentre, 2)
  }


  // A mettre dans fieldutil, équivalent de mowing_effect
  def mowing_effect_PERIPHERIE( mowing_parameter : Double )(distance_repere : Double = distance_limite_mow)
                              (q : Plant)(a : Double) : Double = {
    if ( distancePlantToCenter(q,xcenter_stand,ycenter_stand) <= distance_repere) a
    else a * Plant.mowing(mowing_parameter)(a)
  }

  // REMETTRE LES BONS PARAMETRES :K,L t time dans infoevol
  PlantEvolution(plantEvolution.plants.map(q => q.copy(biomass = (mowing_effect_PERIPHERIE(Plant.mowing_parameter)(distance_limite_mow)(q)(Plant.biomasse_von_bert(5, 0.2)(1, 0)(q.biomass)))))
    , Seq(InfosEvolution(time =1, pop_size, fieldutil.area(xpos, ypos), "Mowing")))
}

  /*  si on veut tester
  val b =  PlantEvolution(Seq( Plant(0,0,1),  Plant(-1,-0,2)  , Plant(10,12,3)  , Plant(2,2,4) ,
    Plant(-7,-6,5),  Plant(4,5,6)  , Plant(-2,-3,7)  , Plant(8,9,8)  )
    , Seq(InfosEvolution())  )
  */



////////////////////////////////////////////
  //////////////////////////////////////////


  def mowingSide(plantEvolution: PlantEvolution,proportionToMow : Double = Plant.proportionMowing): PlantEvolution ={
    // mow the right side of the stand (porportion chosen)
    val xpos = plantEvolution.plants.map(p => p.x)
    val ypos = plantEvolution.plants.map(p => p.y)  // pour l'aire
    val pop_size = xpos.length

    val numberToMow = math.floor(proportionToMow * pop_size ).toInt


    val distance_sorted = xpos.sorted   // trie croissant
    val distance_limite_mow_x = distance_sorted(pop_size - numberToMow -1)



    // A mettre dans fieldutil, équivalent de mowing_effect
    def mowing_effect_PERIPHERIE( mowing_parameter : Double )(distance_repere : Double = distance_limite_mow_x)
                                (q : Plant)(a : Double) : Double = {
      if ( q.x <= distance_repere) a  // those not mowed, in the left
      else a * Plant.mowing(mowing_parameter)(a)
    }

    // REMETTRE LES BONS PARAMETRES :K,L t time dans infoevol
    PlantEvolution(plantEvolution.plants.map(q => q.copy(biomass = (mowing_effect_PERIPHERIE(Plant.mowing_parameter)(distance_limite_mow_x)(q)(Plant.biomasse_von_bert(5, 0.2)(1, 0)(q.biomass)))))
      , Seq(InfosEvolution(time =1, pop_size, fieldutil.area(xpos, ypos), "Mowing")))
  }




}
