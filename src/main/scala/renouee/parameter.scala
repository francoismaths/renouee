package renouee

package object parameter {
  type Time = Double

  // simulation parameter
  val Nmax : Int =  100000  // maximal number of iteration
  val T: Double = 8  // maximal time (year)
  val compteurMax : Int = 10  // number of tentative to have an initial pop (otherwise return Nil : PlantEvolution)

  val n = 300 // number of experiences
}
