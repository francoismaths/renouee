package renouee

import org.apache.commons.math3.distribution._
import math._


case class InfosEvolution(time : parameter.Time =0 , popSize : Int = 0, area : Double =0.0, event : String = "")

case class PlantEvolution(plants: Seq[Plant], infosEvolution: Seq[InfosEvolution] )



object Evolution {

  import scala.util.Random


  ///////////////////////////////////////////////////////////////////
  ///////////   Next for the evolution, all the arguments (for a calibration)   /////////////////
  /////////// allow to keep JUST the information about the last population (position and infos) /////////////////
  /////////////////////////////////////////////////////////////////


  def nextEvolution(plantEvolution: PlantEvolution, tau: Double, p: Double, K: Double = Plant.K, L: Double = Plant.L,
            d1: Double = Plant.d1, b1: Double = Plant.b1, shape: Double = Plant.shape,
            scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
            deathParameterScaling: Double = Plant.deathParameterScaling,
            mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double )(implicit random: Random): PlantEvolution = {


    val xpos = plantEvolution.plants.map(p => p.x)
    val ypos = plantEvolution.plants.map(p => p.y)
    val pop_size = xpos.length

    //rate of event
    val rate1 = pop_size * bbar // rate of a birth event in the pop
    val rate2 = tau // rate of mowing
    val mbar: Double = Plant.mbar(K, deathParameterDecrease, deathParameterScaling)
    val rate3 = pop_size * mbar // rate of a death event in the pop
    val total_rate = rate1 + rate2 + rate3 // rate of an event in the pop

    val t0 = plantEvolution.infosEvolution.last.time // current time
    val dist_expo_rate = new ExponentialDistribution(random, 1 / total_rate)
    val t: Double = dist_expo_rate.sample() + t0 // time of the (next) event


    val u = random.nextDouble()

    if (u < (rate1 / total_rate)) { // birth event
      val dist_unif_birth = new UniformIntegerDistribution(0, pop_size - 1)
      val i = dist_unif_birth.sample() // select the potential parent

      val dist_gamma = new GammaDistribution(random, shape, scale) // dist of the dispersion law of the child around the parent
      val rayon_naissance = dist_gamma.sample() / 100 // in meter
      val xParent = plantEvolution.plants(i).x
      val yParent = plantEvolution.plants(i).y // position coordinate of the parent

      val dist_angle = new UniformRealDistribution(random, 0, 2 * math.Pi) //angle of dispersion around the parent
      val theta: Double = dist_angle.sample()
      val newx: Double = xParent + cos(theta) * rayon_naissance // potential position of the child
      val newy: Double = yParent + sin(theta) * rayon_naissance

      if (fieldutil.birth(b1)(xParent, yParent, xpos.toList, ypos.toList) && fieldutil.okbirth_competition(d1)(i, newx, newy, xpos.toList, ypos.toList)) {
        /*   say if the potential child really appear in the pop (if he is not "killed" immediately by intra psecific competition or if the parent
        has alreay given birth to his children
         */
        // evolution of biomasse and add a new plant in the pop
        PlantEvolution(plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass)))
          :+ Plant(newx, newy, a_0), Seq(InfosEvolution(t, pop_size + 1, fieldutil.area(xpos :+ newx, ypos :+ newy), "birth")))

      }
      else {
        // if no birth, evolution of the biomass
        PlantEvolution(plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass)))
          , Seq(InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nobirth")))

      }
    }

    else if (u < ((rate1 + rate2) / total_rate)) { // Mowing event
      PlantEvolution(plantEvolution.plants.map(q => q.copy(biomass = (fieldutil.mowing_effect(p)(mowing_parameter)(Plant.biomasse_von_bert(K, L)(t, t0)(q.biomass)))))
        , Seq(InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Mowing")))

    }

    else { // death event

      val dist_death = new UniformIntegerDistribution(0, pop_size - 1) // select the potential death individual
      val j = dist_death.sample()
      if (Random.nextDouble() < plantEvolution.plants(j).mortalityPlant(deathParameterDecrease, deathParameterScaling) / mbar) { // if the individual selected dies
        // evolution of biomass and then kill the individual
        PlantEvolution(fieldutil.delete(j, plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass))).toList)
          , Seq(InfosEvolution(t, pop_size - 1, fieldutil.area(fieldutil.delete(j, xpos.toList), fieldutil.delete(j, ypos.toList)), "death")))
      }

      else {
        PlantEvolution(plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass)))
          , Seq(InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nodeath")))
      } // evolution of biomass

    }
  }







  ///////////////////////////////////////////////////////////////////
  ///////////   Next for the evolution, all the argumnents (for a calibration)   /////////////////
  /////////// allow to keep HISTORY of informations about populations + final pop(position and infos) /////////////////
  /////////////////////////////////////////////////////////////////


  def nextEvolutionHistory(plantEvolution: PlantEvolution, tau: Double, p: Double, K: Double = Plant.K, L: Double = Plant.L,
                   d1: Double = Plant.d1, b1: Double = Plant.b1, shape: Double = Plant.shape,
                   scale: Double = Plant.scale, deathParameterDecrease: Double = Plant.deathParameterDecrease,
                   deathParameterScaling: Double = Plant.deathParameterScaling,
                   mowing_parameter: Double = Plant.mowing_parameter, bbar:Double = Plant.bbar, a_0 : Double, result: Result = Result.Last)(implicit random: Random): PlantEvolution = {


    val xpos = plantEvolution.plants.map(p => p.x)
    val ypos = plantEvolution.plants.map(p => p.y)
    val pop_size = xpos.length

    //rate of event
    val rate1 = pop_size * bbar // rate of a birth event in the pop
    val rate2 = tau // rate of mowing
    val mbar: Double = Plant.mbar(K, deathParameterDecrease, deathParameterScaling)
    val rate3 = pop_size * mbar // rate of a death event in the pop
    val total_rate = rate1 + rate2 + rate3 // rate of an event in the pop

    val t0 = plantEvolution.infosEvolution.last.time // current time
    val dist_expo_rate = new ExponentialDistribution(1 / total_rate)
    val t: Double = dist_expo_rate.sample() + t0 // time of the (next) event


    val u = Random.nextDouble()

    if (u < (rate1 / total_rate)) { // birth event
      val dist_unif_birth = new UniformIntegerDistribution(0, pop_size - 1)
      val i = dist_unif_birth.sample() // select the potential parent

      val dist_gamma = new GammaDistribution(shape, scale) // dist of the dispersion law of the child around the parent
      val rayon_naissance = dist_gamma.sample() / 100 // in meter
      val xParent = plantEvolution.plants(i).x
      val yParent = plantEvolution.plants(i).y // position coordinate of the parent

      val dist_angle = new UniformRealDistribution(0, 2 * math.Pi) //angle of dispersion around the parent
      val theta: Double = dist_angle.sample()
      val newx: Double = xParent + cos(theta) * rayon_naissance // potential position of the child
      val newy: Double = yParent + sin(theta) * rayon_naissance

      if (fieldutil.birth(b1)(xParent, yParent, xpos.toList, ypos.toList) && fieldutil.okbirth_competition(d1)(i, newx, newy, xpos.toList, ypos.toList)) {
        /*   say if the potential child really appear in the pop (if he is not "killed" immediately by intra psecific competition or if the parent
        has alreay given birth to his children
         */
        // evolution of biomasse and add a new plant in the pop
        PlantEvolution(plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass)))
          :+ Plant(newx, newy, a_0),
          plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size + 1, fieldutil.area(xpos :+ newx, ypos :+ newy), "birth"))

      }
      else {
        // if no birth, evolution of the biomass
        PlantEvolution(plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass)))
          , plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nobirth"))

      }
    }

    else if (u < ((rate1 + rate2) / total_rate)) { // Mowing event
      PlantEvolution(plantEvolution.plants.map(q => q.copy(biomass = (fieldutil.mowing_effect(p)(mowing_parameter)(Plant.biomasse_von_bert(K, L)(t, t0)(q.biomass)))))
        , plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Mowing"))

    }

    else { // death event

      val dist_death = new UniformIntegerDistribution(0, pop_size - 1) // select the potential death individual
      val j = dist_death.sample()
      if (Random.nextDouble() < plantEvolution.plants(j).mortalityPlant(deathParameterDecrease, deathParameterScaling) / mbar) { // if the individual selected dies
        // evolution of biomass and then kill the individual
        PlantEvolution(fieldutil.delete(j, plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass))).toList)
          , plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size - 1, fieldutil.area(fieldutil.delete(j, xpos.toList), fieldutil.delete(j, ypos.toList)), "death"))
      }

      else {
        result match {
          case Result.Last =>
          case Result.All =>
        }
        PlantEvolution(plantEvolution.plants.map(p => p.copy(biomass = Plant.biomasse_von_bert(K, L)(t, t0)(p.biomass)))
          , plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nodeath"))
      } // evolution of biomass

    }
  }

//  type Result = (Seq[InfosEvolution], InfosEvolution) => Seq[InfosEvolution]
//  def lastResult(history: Seq[InfosEvolution], info: InfosEvolution) = Seq(info)
//  def allResults()

  sealed trait Result

  object Result {
    case object Last extends Result
    case object All extends Result
  }


}