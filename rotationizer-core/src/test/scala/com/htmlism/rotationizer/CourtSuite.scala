package com.htmlism.rotationizer

import weaver.*

object CourtSuite extends FunSuite:
  // TODO
  test("a court has a setter"):
    implicitly[RandomInstance[Rotation.Rotation6]]

    expect.eql(1, 1)
