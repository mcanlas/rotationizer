package com.htmlism.rotationizer

import weaver._

object RandomInstanceSuite extends FunSuite:
  test("supports a rotation of six players") {
    implicitly[RandomInstance[Rotation.Rotation6]]

    expect.eql(1, 1)
  }

  test("supports a rotation of seven players") {
    implicitly[RandomInstance[Rotation.Rotation7]]

    expect.eql(1, 1)
  }
