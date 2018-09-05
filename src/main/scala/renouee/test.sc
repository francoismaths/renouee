math.min(2,3)

val a = List(1,8,4,2,7,1,6)


val p :Double = 0.5 // la poportion a faucher
val size_a = a.length

val number = math.floor(p*size_a)

a(2)


// pourquoi ca ne marche pas ?
// a.sorted(4) ou (a.sorted)(4)
// car il attend un ordering, mais le résultat c'ets une
// liste, ca serait bien de pouvoir accéder à ces éléments



val b = a.sorted
b(4)


math.floor(5.5)

(List.fill(5)("stand")zipWithIndex).map(s => s._1+"_"+(s._2 +1)).mkString(",")
//val l = (List.fill(5)(2)zipWithIndex ).map(p => p._1)

Seq(1,5,7)(0)


List.fill(5)(7) //.map(p => math.floor(p/2.5))

//math.abs( z._1 - z._2)/z._1

//math.abs( 12 - 4)/3.toDouble



val prop : Double = 0.66
val ttemp = (Seq(1,5,3,7,2,18,13,15,4,8).sorted)
ttemp(7)


Vector(1,5,-7,3,8.6) ++ Vector(4,6)

val T = Seq(1,5,2)

T.mkString(",")
T.length


math.abs(math.cos(math.Pi))



List(1,4,7)

List(1,4,7).map( p=> List(p))

val testVect =( 0.0 to 12.0 by  2.0)
testVect(4)



( List(2.0, 4.0) zip Array(3.0,7.0)).map(z => ( math.abs( z._1 - z._2)/z._1 ))

(3.0 to 4.0 by  1.0)



(List(3.0,5.0) zip Array(4.0,6.0)).map(z => ( math.abs( z._1 - z._2) ).toDouble ).sum


val l = List(3.0,5.0)

l.map(p => math.max(p,4))






import org.apache.commons.math3.distribution._
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}

val rng = new RandomAdaptor(new Well44497b(1))
val dist_normal= new NormalDistribution(rng,0, 1)
val rayon_naissance = dist_normal.sample()




val i = 2
val name = "toto"+i+".csv"

