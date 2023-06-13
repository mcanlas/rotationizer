package com.htmlism.rotationizer

import scala.util.chaining._

import cats.data._
import cats.effect._
import weaver._
import weaver.scalacheck._

import com.htmlism.rotationizer.gen.given

object RotationSuite extends SimpleIOSuite with Checkers:
  pureTest("cycle maker 1"):
    val xs =
      Rotation.cycles(1)

    expect.eql(1, xs.length) and
      expect.same(NonEmptyVector.one(CycleIndex(0)), xs.head)

  pureTest("cycle maker 2"):
    val xs =
      Rotation.cycles(2)

    expect.eql(2, xs.length) and
      expect.same(NonEmptyVector.of(CycleIndex(0), CycleIndex(1)), xs.getUnsafe(0)) and
      expect.same(NonEmptyVector.of(CycleIndex(1), CycleIndex(0)), xs.getUnsafe(1))

  test("A rotation 6 generates 6 cycles"):
    forall { (r: Rotation.Rotation6) =>
      val courts =
        Rotation
          .courts(r)

      val positionZeroRotates =
        expect.same(CycleIndex(0), courts.getUnsafe(0).position1) and
          expect.same(CycleIndex(0), courts.getUnsafe(1).position6) and
          expect.same(CycleIndex(0), courts.getUnsafe(2).position5) and
          expect.same(CycleIndex(0), courts.getUnsafe(3).position4) and
          expect.same(CycleIndex(0), courts.getUnsafe(4).position3) and
          expect.same(CycleIndex(0), courts.getUnsafe(5).position2)

      val numCyclesEqualsRosterSize =
        expect.eql(6, courts.length)

      numCyclesEqualsRosterSize and
        positionZeroRotates
    }

  test("A rotation 7 generates 7 cycles"):
    forall { (r: Rotation.Rotation7) =>
      val courts =
        Rotation
          .courts(r)

      val positionZeroRotates =
        expect.same(CycleIndex(0), courts.getUnsafe(0).position1) and
          expect.same(CycleIndex(0), courts.getUnsafe(1).position6) and
          expect.same(CycleIndex(0), courts.getUnsafe(2).position5)

      val numCyclesEqualsRosterSize =
        expect.eql(7, courts.length)

      numCyclesEqualsRosterSize and
        positionZeroRotates
    }

  test("A rotation 10 generates 10 cycles"):
    forall { (r: Rotation.Rotation10) =>
      val courts =
        Rotation
          .courts(r)

      val positionZeroRotates =
        expect.same(CycleIndex(0), courts.getUnsafe(0).position1) and
          expect.same(CycleIndex(0), courts.getUnsafe(1).position6) and
          expect.same(CycleIndex(0), courts.getUnsafe(2).position5)

      val numCyclesEqualsRosterSize =
        expect.eql(10, courts.length)

      numCyclesEqualsRosterSize and
        positionZeroRotates
    }
