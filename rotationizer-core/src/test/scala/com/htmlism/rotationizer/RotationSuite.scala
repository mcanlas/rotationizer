package com.htmlism.rotationizer

import cats.effect._
import weaver._
import weaver.scalacheck._

import com.htmlism.rotationizer.gen.given

object RotationSuite extends SimpleIOSuite with Checkers:
  test("A rotation 6 generates 6 cycles") {
    forall { (r: Rotation.Rotation6) =>
      expect.eql(
        6,
        Rotation
          .courts(r)
          .length
      )
    }
  }
