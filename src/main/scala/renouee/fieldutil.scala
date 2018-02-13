package renouee

import org.apache.commons.math3.random.RandomGenerator

import scala.util.Random

package object fieldutil {


  //  delete le nth element of a list
  def delete[T](n: Int, Z: List[T]): List[T] = {
    /*  used in order to model the effect of the death of an idividual in the population (list)
     */
    List.concat(Z.dropRight(Z.length - n), Z.drop(n + 1))
  }


  ////////////////////////////////////////////

  def intraSpecificCompetition(d1: Double )(j: Int, x: Double, y: Double, l1: List[Double], l2: List[Double]): Boolean = {
    /* intra specific competition : the new inidividual is not created if he birth to close from an other individual already present in the population
      return T or F : T if the new individual can be created
      require j : the position of the parent in the lists;  l1,l2 of the positions of every individual in the population
      x = new_x : the position of the individual the would like to be created
     */

    val deleteAndSquare1 = delete(j, l1).map { a => math.pow(a - x, 2) }
    val deleteAndSquare2 = delete(j, l2).map { a => math.pow(a - y, 2) }
    val distance = (deleteAndSquare1 zip deleteAndSquare2).map { z => z._1 + z._2 }
    distance.filter(_ < math.pow(d1, 2)).length == 0

    //(((delete(j, l1).map { a => math.pow(a - x, 2) } zip delete(j, l2).map { a => math.pow(a - y, 2) }).map { z => z._1 + z._2 }).filter(_ < math.pow(d1, 2))).length == 0

  }

  /////////////////////////////////////////
  // birth rate function : = 1 if not already given birth to 2 children (still present in the pop), 0 otherwise

  def birth(b1: Double )(x: Double, y: Double, l1: List[Double], l2: List[Double]): Boolean = {
    /* return T or F,
    require : l1,l2 : positions of all the indiv in the population, require x,y : the position of the parent in the lists;
    birth returns false if the individual selected in the population (j) has already other indiv next to him :
    to model the fact that he has already gave birth to his 2 children.
    the comparison to 4 in the function is because in the pop x, the parent of the individual is present and also the individual itself
     */

    val xSquared = l1.map { a => math.pow(a - x, 2) }
    val ySquared = l2.map { a => math.pow(a - y, 2)}
    val distance = ( xSquared zip ySquared).map { z => z._1 + z._2 }
    distance.filter(_ < math.pow(b1, 2)).length < 4

    /*
    ((l1.map { a => math.pow(a - x, 2) } zip l2.map { a => math.pow(a - y, 2)
    }).map { z => z._1 + z._2 }).filter(_ < math.pow(b1, 2)).length < 4
    */
  }

  //////////////////////////////   Mowinf effect function, use in ManagementTechnique

  // alea
  def mowingEffect(p : Double)(mowingParameter : Double )(a : Double)(implicit random: Random) : Double = {
    /* require the function mowing (effect of mowing)
    model the effect of  mowing a proportion p of the pop for a plant (effect on the biomass a)
     */
    if (random.nextDouble() > p) a
    else a * Plant.mowing(mowingParameter)(a)
  }


  // periphery
  // give the distance to the centre (of the stand) of the plant q
  def distancePlantToCenter(q:Plant,xcentre : Double, ycentre : Double)={
    math.pow(q.x- xcentre, 2)  + math.pow(q.y- ycentre, 2)
  }
  
  def mowingEffectPeriphery(mowingParameter : Double )(distanceRepere : Double, xCenterStand : Double, yCenterStand : Double)
                           (q : Plant)(a : Double) : Double = {
    if ( distancePlantToCenter(q,xCenterStand,yCenterStand) <= distanceRepere) a
    else a * Plant.mowing(mowingParameter)(a)
  }


  def slaveMowingEffectPeriphery(mowingParameter : Double )(distanceRepere : Double, xCenterStand : Double, yCenterStand : Double)(plant: Plant): Plant = {
    Plant.biomass.modify(mowingEffectPeriphery(mowingParameter)(distanceRepere,xCenterStand,yCenterStand)(plant))(plant)
  }




  // Side
  def mowingEffectSide(mowingParameter : Double )(distanceRepere : Double )
                      (q : Plant)(a : Double) : Double = {
    if ( q.x <= distanceRepere) a  // those not mowed, in the left
    else a * Plant.mowing(mowingParameter)(a)
  }

  def slaveMowingEffectSide(mowingParameter : Double )(distanceRepere : Double )(q : Plant) : Plant= {
    Plant.biomass.modify(mowingEffectSide(mowingParameter)(distanceRepere)(q))(q)
  }







  // R is better (but available just for the last pop)
  def area (l1 : Seq[Double], l2 : Seq[Double]) : Double = {
    if (l1.isEmpty) { 0 }
    else {
      val minx = l1.min
      val miny = l2.min
      val maxx = l1.max
      val maxy = l2.max ;
      (maxx-minx)*(maxy-miny)  }
  }



}
