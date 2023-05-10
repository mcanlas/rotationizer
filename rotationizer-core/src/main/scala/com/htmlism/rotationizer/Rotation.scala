package com.htmlism.rotationizer

import scala.util.Random

import cats.data.*
import cats.syntax.all.*

import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnHitterSide
import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnServerSide

sealed trait Rotation:
  def cycleRanks: NonEmptyList[Int]

  def positionRanks: NonEmptyList[Int]

object Rotation:
  def cycles[A](ranks: NonEmptyList[Int], indices: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] =
    NonEmptyList.one {
      indices
        .zip(ranks)
        .sortBy(_._2)
        .map(_._1)
    }

  def courts(rotation: Rotation): NonEmptyList[Court[Int]] =
    rotation match
      case Rotation6(_, _) =>
        NonEmptyList.one(Court(NonEmptyList.one(123), Nil, Nil))

      case Rotation7(_, _, _) =>
        NonEmptyList.one(Court(NonEmptyList.one(123), Nil, Nil))

  case class Rotation6(
      cycleRanks: NonEmptyList[Int],
      positionRanks: NonEmptyList[Int]
  ) extends Rotation

  object Rotation6:
    given RandomInstance[Rotation6] with
      def reader: Reader[Random, Rotation6] =
        (
          RandomInstance.ranks(6),
          RandomInstance.ranks(6)
        )
          .mapN(Rotation6.apply)

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
      def reader: Reader[Random, Rotation7] =
        (
          RandomInstance.ranks(7),
          RandomInstance.ranks(7),
          RandomInstance.oneOf(OutOnServerSide, OutOnHitterSide)
        )
          .mapN(Rotation7.apply)
