package renouee


import renouee.Plant._
import scala.math._
import monocle.function.Each
import monocle.macros._

case class PlantGrowth(
                        // distance
                        d1: Double = 0.15, // distance of itra specific competition (in function okbirth_competition) (meter)
                        b1: Double = 0.15, // distance of apical dominance (in function birth) (meter)

                        // parameters of the dispersion law of children  : gamma(shape,scale)
                        shape: Double = 10,
                        scale: Double = 1,

                        // parameters of the deterministic equation of evolution of biomass (Von Bertalanffy)
                        K: Double = 5.0, //   maximum biomass of the rhizome
                        L: Double = 0.3, //  seen as the evolution rate at low biomass

                        // Mowing parameters
                        mowingParameter : Double = 0.6, // in function mowing

                        //mortality
                        deathParameterDecrease: Double = 1.5, // in function mortality
                        deathParameterScaling: Double = 1.0, // in function mortality

                        //initial biomass (when born)
                        a0: Double = 0.5,
                        bbar : Double = 1,


                        // biomass of the first individual creates
                        biomassFirstIndiv : Double = 4
                            )



case class Management( T: Double = 8.0,
                       proportionMowing: Double = 0.9, // proportion of the plant in the population we mow (between 0 and 1)
                       tau: Double = 1, // we mow tau tau times a year (on average)
                       xAxisMowLimit : Double = 0 // for the Management technique = SideXPosition
                     )




object Plant {

  val d1: Double = 0.15 // distance of itra specific competition (in function okbirth_competition) (meter)
  val b1: Double = 0.15 // distance of apical dominance (in function birth) (meter)

  // parameters of the dispersion law of children  : gamma(shape,scale)
  val shape: Double = 10
  val scale: Double = 1

  // parameters of the deterministic equation of evolution of biomass (Von Bertalanffy)
  val K: Double = 5.0 //   maximum biomass of the rhizome
  val L: Double = 0.3 //  seen as the evolution rate at low biomass

  def a_0(K: Double) = {
    K / 10
  }

  // Mowing parameters
  val tau: Double = 1 // we mow tau tau times a year (on average)
  val proportionMowing: Double = 0.9 // proportion of the plant in the population we mow (between 0 and 1)
  val mowingParameter = 0.6 // in function mowing

  def mowing(mowingParameter: Double = mowingParameter)(a: Double) = 1 - math.exp(-mowingParameter * a)
  // effect of the mowing on the biomass of the plant


  //mortality
  val deathParameterDecrease: Double = 1.5 // in function mortality
  val deathParameterScaling: Double = 1.0 // in function mortality
  def mortality(deathParameterDecrease: Double = deathParameterDecrease, deathParameterScaling: Double = deathParameterScaling)(biomass: Double) = {
    deathParameterScaling * exp(-deathParameterDecrease * biomass)
  } // death rate of a plant (depends on its biomass)

  // Parameters about the initial population
  def bound_initial_bimass_min(K: Double) = K / 3 // lower bound of the law Uniform(lower_bound, upper_bound)
  def bound_initial_bimass_max(K: Double) = 2 * K / 3 // upper bound


  val initialPopulationSize: Double = 40


  // In oder to find the max of the mortality function : create a list of possible biomass with a step
  val step: Double = 0.1

  def biomasse_0(K: Double) = (0.0 to K by step).toList

  def mbar(K: Double, deathParameterDecrease: Double = deathParameterDecrease, deathParameterScaling: Double = deathParameterScaling) = {
    biomasse_0(K).map(mortality(deathParameterDecrease, deathParameterScaling)).max
  } // max of mortality function


  val bbar: Double = 1.0 // max of birth rate function

  // evolution of biomass
  def biomassVonBert(K: Double = K, L: Double = L)(t: Double, t0: Double)(a0: Double) =
  /*   flot de l'équa diff*/
    a0 * math.exp((-L * (t - t0))) + K * (1 - math.exp(-L * (t - t0)))


}



@Lenses case class Plant(x: Double, y: Double, biomass: Double) {
  def mortalityPlant(deathParameterDecrease: Double = deathParameterDecrease, deathParameterScaling: Double = deathParameterScaling) =
    Plant.mortality(deathParameterDecrease, deathParameterScaling)(biomass)
}


