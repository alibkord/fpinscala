package fpinscala.state

import fpinscala.state.RNG._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Arbitrary, Properties}

object RNGTest extends Properties("RNG") {

  val rngGen: Gen[RNG] = for {
    s <- Arbitrary.arbitrary[Long]
  } yield Simple(s)

  property("nonNegativeInt") =
    forAll(rngGen) {
      rng => nonNegativeInt(rng)._1 >= 0
    }

  property("double") =
    forAll(rngGen) {
      rng => {
        val result: (Double, RNG) = double(rng)
        result._1 >= 0 && result._1 < 1
      }
    }

  property("ints") =
    forAll(rngGen, Gen.chooseNum(0, 99)) {
      (rng, count) => {
        val result: (List[Int], RNG) = ints(count)(rng)
        val list: List[Int] = result._1
        list.size == count
      }
    }

  property("sequence") =
    forAll(rngGen, Gen.chooseNum(0, 99)) {
      (rng, listSize) => {
        val rands: List[Rand[Int]] = List.fill(listSize)(int)
        val seq: Rand[List[Int]] = sequence(rands)
        val expected: List[Int] = {
          @annotation.tailrec
          def looper(ir: List[Rand[Int]], il: List[Int], rng: RNG): List[Int] = ir match {
            case List() => il.reverse
            case h :: t =>
              val (nextInt, nextRNG) = h(rng)
              looper(t, nextInt :: il, nextRNG)
          }
          looper(rands, List(), rng)
        }
        val actual: List[Int] = seq(rng)._1
        collect(s"Expected: $expected, Actual: $actual") {
          expected == actual
        }
      }
    }

  property("flatMap") =
    forAll(rngGen) {
      rng => {
        val aRand: Rand[Int] = int
        val g: (Int => Rand[Double]) = (i: Int) => rng => (i.toDouble, rng)
        val combined: Rand[Double] = RNG.flatMap(aRand)(g)
        val (expectedInt: Int, _) = rng.nextInt
        val result: (Double, RNG) = combined.apply(rng)
        collect(s"expected: ${expectedInt.toDouble}, result: ${result._1}") {
          expectedInt.toDouble == result._1
        }
      }
    }

  property("nonNegativeLessThan") =
    forAll(rngGen, Gen.chooseNum(9999999, 999999999)) {
      (rng, bound) => {
        val rand = RNG.nonNegativeLessThan(bound)
        val (intResult, _) = rand.apply(rng)
        collect(s"$intResult is less than $bound") {
          intResult < bound && intResult >= 0
        }
      }
    }

  property("map2UsingFlatMap") =
    forAll(rngGen) {
      rng => {
        // build expected:
        val aRand: Rand[Int] = int
        val bRand: Rand[Double] =
          rng => {
            val (int1, rng1) = rng.nextInt
            (int1.toDouble, rng1)
          }
        val g: ((Int, Double) => String) = (i, d) => s"Int: $i, Double: $d"
        val (expectedInt1, rng1) = rng.nextInt
        val (expectedInt2, rng2) = rng1.nextInt
        val expectedStr = g(expectedInt1, expectedInt2.toDouble)
        // build result:
        val mapped: Rand[String] = RNG.map2UsingFlatMap(aRand, bRand)(g)
        val (resultStr, resultRNG) = mapped.apply(rng)
        // verify:
        collect(s"expected: $expectedStr, result: $resultStr") {
          expectedStr == resultStr && rng2 == resultRNG
        }
      }
    }

  property("state_rand") =
    forAll(rngGen) {
      rng => {
        val run: RNG => (Int, RNG) = rng => {
          rng.nextInt
        }
        val rand: RNGState.Rand[Int] = State[RNG, Int](run)
        val (i, r) = rand.run(rng)
        val (expected, rr) = rng.nextInt
        collect(s"Expected: $expected, Actual: $i") {
          expected == i && r == rr
        }
      }
    }

  property("state_map") =
    forAll(rngGen) {
      rng => {
        val run: RNG => (Int, RNG) = rng => {
          rng.nextInt
        }
        val rand: RNGState.Rand[Int] = State[RNG, Int](run)
        val (initial, r) = rand.run(rng)
        val mapped: State[RNG, Double] = rand.map(i => (i * i).toDouble)
        val expected = (initial * initial).toDouble
        val (actual, rr) = mapped.run(rng)
        collect(s"Expected: $expected, Actual: $actual") {
          expected == actual && r == rr
        }
      }
    }

  property("state_map2") =
    forAll(rngGen) {
      rng => {
        val run1: RNG => (Int, RNG) = rng => {
          rng.nextInt
        }
        val run2: RNG => (Double, RNG) = rng => {
          val (nextInt, rng1) = rng.nextInt
          (nextInt.toDouble, rng1)
        }
        val rand1: RNGState.Rand[Int] = State[RNG, Int](run1)
        val rand2: RNGState.Rand[Double] = State[RNG, Double](run2)
        val (init1: Int, rng1) = rand1.run(rng)
        val (init2: Double, rng2) = rand2.run(rng1)
        val expected = s"Int: $init1, Double: $init2"
        val mapped = rand1.map2(rand2)((i: Int, d: Double) => s"Int: $i, Double: $d")
        val (result, r) = mapped.run(rng)
        collect(s"Expected: $expected, Actual: $result") {
          expected == result && rng2 == r
        }
      }
    }

  property("state_flatMap") =
    forAll(rngGen) {
      rng => {
        val run: RNG => (Int, RNG) = rng => {
          rng.nextInt
        }
        val rand: RNGState.Rand[Int] = State[RNG, Int](run)
        val f: (Int => State[RNG, Double]) =
          i => State[RNG, Double](rng => ((i * i).toDouble, rng))
        val flatMapped: State[RNG, Double] = rand.flatMap(f)
        val (initial, r) = rand.run(rng)
        val expected = (initial * initial).toDouble
        val (actual, rr) = flatMapped.run(rng)
        collect(s"Expected: $expected, Actual: $actual") {
          expected == actual && r == rr
        }
      }
    }

  property("state_sequence") =
    forAll(rngGen, Gen.chooseNum(0, 99)) {
      (rng, listSize) => {
        val raw: List[State[RNG, Int]] =
          List.fill(listSize)(State[RNG, Int](rng => rng.nextInt))
        val sequenced: State[RNG, List[Int]] = State.sequence(raw)
        val expected: (List[Int], RNG) = {
          @annotation.tailrec
          def looper(curr: List[Int], r: RNG): (List[Int], RNG) = {
            if (curr.size < listSize) {
              val (nextInt, nextRNG) = r.nextInt
              looper(nextInt :: curr, nextRNG)
            } else {
              (curr, r)
            }
          }
          looper(List(), rng)
        }
        val actual: (List[Int], RNG) = sequenced.run(rng)
        collect(s"Expected: ${expected._1}, Actual: ${actual._1}") {
          expected._1 == actual._1 && expected._2 == actual._2
        }
      }
    }

  val coinGen = Gen.const(Coin)
  val turnGen = Gen.const(Turn)
  val inputGen = Gen.oneOf(coinGen, turnGen)
  val countGen = Gen.chooseNum(0, 100)

  val inputsGen: Gen[List[Input]] =
    for {
      count <- countGen
      inputs <- Gen.containerOfN[List, Input](count, inputGen)
    } yield inputs

  property("state_machine_1") =
    forAll(Gen.chooseNum(0, 100), inputsGen) {
      (candies, inputs) =>
        val machine = Machine(locked = true, candies, 0)

        val expectedMachine: Machine = manualMachine(inputs)(machine)
        val expectedCandies: Int = expectedMachine.candies
        val expectedCoins: Int = expectedMachine.coins

        val actual: ((Int, Int), Machine) = State.simulateMachine(inputs).run(machine)

        collect(s"Inputs: $inputs") {
          expectedMachine == actual._2 &&
            expectedCandies == actual._1._1 &&
            expectedCoins == actual._1._2
        }
    }

  def manualMachine(inputs: List[Input])(m: Machine): Machine = {
    @annotation.tailrec
    def looper(inps: List[Input], state: Machine): Machine = inps match {
      case List() => state
      case h :: t =>
        looper(t, Machine.handle.apply(h)(state))
    }
    looper(inputs, m)
  }
}
