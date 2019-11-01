import fpinscala.testing.{Gen, Prop}

val smallInt = Gen.choose(-10,10)
val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
  val max = ns.max'
  !ns.exists(_ > max)
}