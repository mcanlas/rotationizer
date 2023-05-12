package com.htmlism.rotationizer

import scala.util.Random
import scala.util.chaining._

import cats.Show
import cats.data.*
import cats.syntax.all.*

import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnHitterSide
import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnServerSide

sealed trait Rotation:
  def cycleRanks: NonEmptyList[Int]

  def positionRanks: NonEmptyList[Int]

object Rotation:
  def cycles[A](ranks: NonEmptyList[Int], indices: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] =
    val xsSorted =
      indices
        .zip(ranks)
        .sortBy(_._2)
        .map(_._1)

    xsSorted
      .asList {
        _.indices
          .map { i =>
            cycleFromOffset(xsSorted, i)
          }
          .toList
      }

  def cycleFromOffset[A](xs: NonEmptyList[A], offset: Int): NonEmptyList[A] =
    xs.asList { ys =>
      val (left, right) =
        ys.splitAt(offset)

      left ::: right
    }

  def courts(rotation: Rotation): NonEmptyList[Court[CycleIndex]] =
    rotation match
      case Rotation6(xs, _) =>
        val cycleIndices =
          (0 until xs.length)
            .map(CycleIndex(_))
            .toList
            .pipe(NonEmptyList.fromListUnsafe)

        cycles(xs, cycleIndices)
          .map(cy => Court(cy, Nil, Nil))

      case Rotation7(xs, _, mod) =>
        val cycleIndices =
          (0 until xs.length)
            .map(CycleIndex(_))
            .toList
            .pipe(NonEmptyList.fromListUnsafe)

        cycles(xs, cycleIndices)
          .map { cy =>
            mod match
              case Rotation7.OffCourtModifier.OutOnServerSide =>
                val onCourt =
                  cy.asList { xs =>
                    xs(0) :: xs.slice(2, xs.length)
                  }

                Court(onCourt, List(cy.toList(1)), Nil)

              case Rotation7.OffCourtModifier.OutOnHitterSide =>
                val onCourt =
                  cy.asList { xs =>
                    xs.slice(0, 4) ::: xs.slice(5, 7)
                  }

                Court(onCourt, Nil, List(cy.toList(4)))
          }

      case Rotation10(xs, _) =>
        val cycleIndices =
          (0 until xs.length)
            .map(CycleIndex(_))
            .toList
            .pipe(NonEmptyList.fromListUnsafe)

        cycles(xs, cycleIndices)
          .map { cy =>
            val onCourt =
              cy.asList { xs =>
                (xs(0) :: xs.slice(3, 6)) ::: xs.slice(8, 10)
              }

            Court(onCourt, cy.toList.slice(1, 3), cy.toList.slice(6, 8))
          }

  case class Rotation6(
      cycleRanks: NonEmptyList[Int],
      positionRanks: NonEmptyList[Int]
  ) extends Rotation

  case class Rotation10(
      cycleRanks: NonEmptyList[Int],
      positionRanks: NonEmptyList[Int]
  ) extends Rotation

  object Rotation6:
    given RandomInstance[Rotation6] with
      def fromRng: Reader[Random, Rotation6] =
        (
          RandomInstance.ranks(6),
          RandomInstance.ranks(6)
        )
          .mapN(Rotation6.apply)

    given Show[Rotation6] =
      Show.fromToString

  object Rotation10:
    given RandomInstance[Rotation10] with
      def fromRng: Reader[Random, Rotation10] =
        (
          RandomInstance.ranks(10),
          RandomInstance.ranks(10)
        )
          .mapN(Rotation10.apply)

    given Show[Rotation10] =
      Show.fromToString

  case class Rotation7(
      cycleRanks: NonEmptyList[Int],
      positionRanks: NonEmptyList[Int],
      offCourtModifier: Rotation7.OffCourtModifier
  ) extends Rotation

  object Rotation7:
    enum OffCourtModifier:
      case OutOnServerSide
      case OutOnHitterSide

    given RandomInstance[Rotation7] with
      def fromRng: Reader[Random, Rotation7] =
        (
          RandomInstance.ranks(7),
          RandomInstance.ranks(7),
          RandomInstance.oneOf(OutOnServerSide, OutOnHitterSide)
        )
          .mapN(Rotation7.apply)

    given Show[Rotation7] =
      Show.fromToString
