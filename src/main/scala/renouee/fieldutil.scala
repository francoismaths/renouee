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


  /////////////////////////////////////////////
  //  okbirth_competition

  def okbirth_competition(d1: Double = Plant.d1)(j: Int, x: Double, y: Double, l1: List[Double], l2: List[Double]): Boolean = {
    /* intra specific competition : the new inidividual is not created if he birth to close from an other individual already present in the population
      return T or F : T if the new individual can be created
      require j : the position of the parent in the lists;  l1,l2 of the positions of every individual in the population
      x = new_x : the position of the individual the would like to be created
     */

    (((delete(j, l1).map { a => math.pow(a - x, 2) } zip delete(j, l2).map { a => math.pow(a - y, 2) }).map { z => z._1 + z._2 }).filter(_ < math.pow(d1, 2))).length == 0

  }

  /////////////////////////////////////////
  // birth rate function : = 1 if not already given birth to 2 children (still present in the pop), 0 otherwise

  def birth(b1: Double = Plant.b1)(x: Double, y: Double, l1: List[Double], l2: List[Double]): Boolean = {
    /* return T or F,
    require : l1,l2 : positions of all the indiv in the population, require x,y : the position of the parent in the lists;
    birth returns false if the individual selected in the population (j) has already other indiv next to him :
    to model the fact that he has already gave birth to his 2 children.
    the comparison to 4 in the function is because in the pop x, the parent of the individual is present and also the individual itself
     */
    ((l1.map { a => math.pow(a - x, 2) } zip l2.map { a => math.pow(a - y, 2)
    }).map { z => z._1 + z._2 }).filter(_ < math.pow(b1, 2)).length < 4

  }

  //////////////////////////////

  def mowing_effect(p : Double)( mowing_parameter : Double )(a : Double)(implicit random: Random) : Double = {
    /* require the function mowing (effect of mowing)
    model the effect of  mowing a proportion p of the pop for a plant (effect on the biomass a)
     */
    if (random.nextDouble() > p) a
    else a * Plant.mowing(mowing_parameter)(a)
  }


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
