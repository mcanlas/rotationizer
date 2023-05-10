package com.htmlism.rotationizer

import cats.effect._
import weaver._
import weaver.scalacheck._

import com.htmlism.rotationizer.gen.given

object RotationSuite extends SimpleIOSuite with Checkers:
  test("A rotation 6 generates 6 cycles") {
    forall { (r: Rotation.Rotation6) =>
      val courts =
        Rotation
          .courts(r)

      val numCyclesEqualsRosterSize =
        expect.eql(6, courts.length)

      numCyclesEqualsRosterSize
    }
  }

  test("A rotation 7 generates 7 cycles") {
    forall { (r: Rotation.Rotation7) =>
      val courts =
        Rotation
          .courts(r)

      val numCyclesEqualsRosterSize =
        expect.eql(7, courts.length)

      numCyclesEqualsRosterSize
    }
  }
