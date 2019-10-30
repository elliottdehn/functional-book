import fpinscala.testing._

val smallInt = Gen.choose(-10,10)
val maxProp = Prop.forAll(SGen.listOf(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}