package renouee

import org.apache.commons.math3.distribution._
import math._
import monocle.function.Each
import monocle.macros._

import monocle._
import monocle.syntax._
import monocle.std._




@Lenses case class InfosEvolution(time : parameter.Time =0 , popSize : Int = 0, area : Double =0.0, event : String = "")

//@Lenses case class PlantEvolution(plants: Vector[Plant], infosEvolution: Seq[InfosEvolution] )
@Lenses case class PlantEvolution(plants: Vector[Plant], infosEvolution: Vector[InfosEvolution] )



sealed trait ResultType

object ResultType {
  case object Last extends ResultType
  case object All extends ResultType
}


sealed trait ManagementTechnique

object ManagementTechnique{
  case object Alea extends ManagementTechnique
  case object Periphery  extends ManagementTechnique
  case object Side extends ManagementTechnique
  case object SideXPosition extends ManagementTechnique

}



object Evolution {

  import scala.util.Random


  ///////////////////////////////////////////////////////////////////
  ///////////   Next for the evolution, all the arguments (for a calibration)   /////////////////
  /////////// Chose to keep all (history : time,event,size) OR last (population) : resultType      /////////////////
  /////////// Chose the management technique : alea, periphery, side  /////////////////
  /////////////////////////////////////////////////////////////////


  def nextEvolution(plantEvolution: PlantEvolution,
                    management: Management,
                    plantGrowth: PlantGrowth,
                    resultType: ResultType,
                    managementTechnique: ManagementTechnique) (implicit random: Random): PlantEvolution = {

    val xpos = plantEvolution.plants.map(p => p.x)
    val ypos = plantEvolution.plants.map(p => p.y)
    val pop_size = xpos.length

    //rate of event
    val rate1 = pop_size * plantGrowth.bbar // rate of a birth event in the pop
    val rate2 = management.tau // rate of mowing
    val mbar: Double = Plant.mbar(plantGrowth.K, plantGrowth.deathParameterDecrease, plantGrowth.deathParameterScaling)
    val rate3 = pop_size * mbar // rate of a death event in the pop
    val total_rate = rate1 + rate2 + rate3 // rate of an event in the pop

    val t0 = plantEvolution.infosEvolution.last.time // current time
    val dist_expo_rate = new ExponentialDistribution(random, 1 / total_rate)
    // use the implicit def de random in the package object
    //  conversion of org.apache.math to scala ?  // le random qui est dans l'implicit en argument de la fonction
    val t: Double = dist_expo_rate.sample() + t0 // time of the (next) event


    // evolution of the biomasses of the current population, then the event appends
    val allBiomasses = (PlantEvolution.plants composeTraversal Each.each composeLens Plant.biomass)
    val tempEvolBiomass = allBiomasses.modify(Plant.biomassVonBert(plantGrowth.K, plantGrowth.L)(t, t0))(plantEvolution)


    val u = random.nextDouble()

    if (u < (rate1 / total_rate)) { // birth event
      val dist_unif_birth = new UniformIntegerDistribution(random, 0, pop_size - 1)
      val i = dist_unif_birth.sample() // select the potential parent

      val dist_gamma = new GammaDistribution(random, plantGrowth.shape, plantGrowth.scale) // dist of the dispersion law of the child around the parent
      val rayon_naissance = dist_gamma.sample() / 100 // in meter
      val xParent = plantEvolution.plants(i).x
      val yParent = plantEvolution.plants(i).y // position coordinate of the parent

      val dist_angle = new UniformRealDistribution(random, 0, 2 * math.Pi) //angle of dispersion around the parent
      val theta: Double = dist_angle.sample()
      val newx: Double = xParent + cos(theta) * rayon_naissance // potential position of the child
      val newy: Double = yParent + sin(theta) * rayon_naissance




      if (fieldutil.birth(plantGrowth.distanceParent)(xParent, yParent, xpos.toList, ypos.toList) && fieldutil.intraSpecificCompetition(plantGrowth.distanceCompetition)(i, newx, newy, xpos.toList, ypos.toList)) {
        /*   say if the potential child really appear in the pop (if he is not "killed" immediately by intra psecific competition or if the parent
        has alreay given birth to his children
         */
        // evolution of biomasse and add a new plant in the pop

        resultType match {
          case ResultType.Last => PlantEvolution( tempEvolBiomass.plants :+ Plant(newx, newy, plantGrowth.a0), Vector(InfosEvolution(t, pop_size + 1, fieldutil.area(xpos :+ newx, ypos :+ newy), "birth")))

          case ResultType.All =>
            PlantEvolution( tempEvolBiomass.plants :+ Plant(newx, newy, plantGrowth.a0),
              plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size + 1, fieldutil.area(xpos :+ newx, ypos :+ newy), "birth"))

        }
      }
      else {
        // if no birth, evolution of the biomass

        resultType match {
          case ResultType.Last => PlantEvolution(tempEvolBiomass.plants,
            Vector(InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nobirth")))

          case ResultType.All => PlantEvolution(tempEvolBiomass.plants,
            plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nobirth"))

        }
      }
    }

    else if (u < ((rate1 + rate2) / total_rate)) { // Mowing event

      managementTechnique match {
        case ManagementTechnique.Alea => ManagementTechniqueUtil.mowingAlea(tempEvolBiomass,management,plantGrowth,resultType,t)(random)

        case ManagementTechnique.Periphery => ManagementTechniqueUtil.mowingPeriphery(tempEvolBiomass,management,plantGrowth,resultType,t)

        case ManagementTechnique.Side =>  ManagementTechniqueUtil.mowingSideProportion(tempEvolBiomass,management,plantGrowth,resultType,t)

        case ManagementTechnique.SideXPosition =>  ManagementTechniqueUtil.mowingSidePosition(tempEvolBiomass,management,plantGrowth,resultType,t)
      }
    }

    else { // death event

      val dist_death = new UniformIntegerDistribution(random,0, pop_size - 1) // select the potential death individual
      val j = dist_death.sample()
      if (random.nextDouble() < tempEvolBiomass.plants(j).mortalityPlant(plantGrowth.deathParameterDecrease, plantGrowth.deathParameterScaling) / mbar) { // if the individual selected dies
        // evolution of biomass and then kill the individual

        resultType match {
          case ResultType.Last =>   PlantEvolution( fieldutil.delete(j, tempEvolBiomass.plants.toList).toVector
            , Vector(InfosEvolution(t, pop_size - 1, fieldutil.area(fieldutil.delete(j, xpos.toList), fieldutil.delete(j, ypos.toList)), "death")))

          case ResultType.All =>    PlantEvolution(fieldutil.delete(j, tempEvolBiomass.plants.toList).toVector
            , plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size - 1, fieldutil.area(fieldutil.delete(j, xpos.toList), fieldutil.delete(j, ypos.toList)), "death"))

        }

      }

      else {  // no death
        resultType match {
          case ResultType.Last =>  PlantEvolution(tempEvolBiomass.plants, Vector(InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nodeath")))

          case ResultType.All =>          PlantEvolution( tempEvolBiomass.plants
            , plantEvolution.infosEvolution :+ InfosEvolution(t, pop_size, fieldutil.area(xpos, ypos), "Nodeath"))

        }
      }

    }

  }





//  type Result = (Seq[InfosEvolution], InfosEvolution) => Seq[InfosEvolution]
//  def lastResult(history: Seq[InfosEvolution], info: InfosEvolution) = Seq(info)
//  def allResults()




}