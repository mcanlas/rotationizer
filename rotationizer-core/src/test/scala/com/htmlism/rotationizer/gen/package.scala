package com.htmlism.rotationizer

import org.scalacheck.Arbitrary

package object gen:
  given Arbitrary[Rotation.Rotation6] =
    Arbitrary {
      Arbitrary
        .arbitrary[Int]
        .map(scala.util.Random(_))
        .map(implicitly[RandomInstance[Rotation.Rotation6]].fromRng(_))
    }

  given Arbitrary[Rotation.Rotation7] =
    Arbitrary {
      Arbitrary
        .arbitrary[Int]
        .map(scala.util.Random(_))
        .map(implicitly[RandomInstance[Rotation.Rotation7]].fromRng(_))
    }

  given Arbitrary[Rotation.Rotation10] =
    Arbitrary {
      Arbitrary
        .arbitrary[Int]
        .map(scala.util.Random(_))
        .map(implicitly[RandomInstance[Rotation.Rotation10]].fromRng(_))
    }
