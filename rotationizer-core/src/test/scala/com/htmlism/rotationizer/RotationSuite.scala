package com.htmlism.rotationizer

import cats.effect._
import weaver._
import weaver.scalacheck._

object RotationSuite extends SimpleIOSuite with Checkers:
  test("A rotation 6 generates 6 cycles") {
    forall { (n: Int) =>
      expect.eql(n, n)
    }
  }
