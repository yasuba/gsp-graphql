// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import scala.annotation.{ nowarn, tailrec }

import cats.Monad
import cats.implicits._
import io.circe.{ Encoder, Json }
import shapeless.{ Coproduct, Generic, ::, HList, HNil, Inl, Inr, LabelledGeneric }
import shapeless.ops.hlist.{ LiftAll => PLiftAll }
import shapeless.ops.coproduct.{ LiftAll => CLiftAll }
import shapeless.ops.record.Keys

import Cursor.Env
import QueryInterpreter.{ mkErrorResult, mkOneError }
import ShapelessUtils._
import org.tpolecat.sourcepos.SourcePos

abstract class GenericMapping[F[_]: Monad] extends Mapping[F] {
  case class GenericRoot[T](val tpe: Type, val fieldName: String, t: T, cb: () => CursorBuilder[T])(
    implicit val pos: SourcePos
  ) extends RootMapping {
    lazy val cursorBuilder = cb()
    def cursor(query: Query, env: Env): F[Result[Cursor]] = cursorBuilder.build(Nil, t, None, env).pure[F]
    def withParent(tpe: Type): GenericRoot[T] =
      new GenericRoot(tpe, fieldName, t, cb)
  }

  def GenericRoot[T](fieldName: String, t: T)(implicit cb: => CursorBuilder[T]): GenericRoot[T] =
    new GenericRoot(NoType, fieldName, t, () => cb)
}

object semiauto {
  final def deriveObjectCursorBuilder[T](tpe: Type)
    (implicit mkBuilder: => MkObjectCursorBuilder[T]): ObjectCursorBuilder[T] = mkBuilder(tpe)
  final def deriveInterfaceCursorBuilder[T](tpe: Type)
    (implicit mkBuilder: => MkInterfaceCursorBuilder[T]): CursorBuilder[T] = mkBuilder(tpe)
}

trait ObjectCursorBuilder[T] extends CursorBuilder[T] {
  def renameField(from: String, to: String): ObjectCursorBuilder[T]
  def transformFieldNames(f: String => String): ObjectCursorBuilder[T]
  def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T]
}

trait MkObjectCursorBuilder[T] {
  def apply(tpe: Type): ObjectCursorBuilder[T]
}

object MkObjectCursorBuilder {
  implicit def productCursorBuilder[T <: Product, R <: HList, L <: HList]
    (implicit
      @nowarn gen: Generic.Aux[T, R],
      elems0: => PLiftAll[CursorBuilder, R],
      @nowarn lgen: LabelledGeneric.Aux[T, L],
      keys0: Keys[L]
    ): MkObjectCursorBuilder[T] =
    new MkObjectCursorBuilder[T] {
      def apply(tpe: Type): ObjectCursorBuilder[T] = {
        def fieldMap: Map[String, (List[String], T, Option[Cursor], Env) => Result[Cursor]] = {
          val keys: List[String] = unsafeToList[Symbol](keys0()).map(_.name)
          val elems = unsafeToList[CursorBuilder[Any]](elems0.instances)
          keys.zip(elems.zipWithIndex).map {
            case (fieldName, (elem, idx)) => (fieldName, (p: List[String], t: T, c: Option[Cursor], e: Env) => elem.build(p, t.productElement(idx), c, e))
          }.toMap
        }
        new Impl[T](tpe, fieldMap)
      }
    }

  class Impl[T](tpe0: Type, fieldMap0: => Map[String, (List[String], T, Option[Cursor], Env) => Result[Cursor]]) extends ObjectCursorBuilder[T] {
    lazy val fieldMap = fieldMap0

    val tpe = tpe0

    def build(path: List[String], focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
      CursorImpl(path, tpe, focus, fieldMap, parent, env).rightIor

    def renameField(from: String, to: String): ObjectCursorBuilder[T] =
      transformFieldNames { case `from` => to ; case other => other }
    def transformFieldNames(f: String => String): ObjectCursorBuilder[T] =
      new Impl(tpe, fieldMap0.map { case (k, v) => (f(k), v) })
    def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T] =
      new Impl(tpe, fieldMap0.updated(fieldName, (path: List[String], focus: T, parent: Option[Cursor], env: Env) => f(focus).flatMap(f => cb.build(path, f, parent, env))))
  }

  case class CursorImpl[T](path: List[String], tpe: Type, focus: T, fieldMap: Map[String, (List[String], T, Option[Cursor], Env) => Result[Cursor]], parent: Option[Cursor], env: Env)
    extends AbstractCursor[Product] {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def hasField(fieldName: String): Boolean = fieldMap.contains(fieldName)

    override def field(fieldName: String): Result[Cursor] = {
      fieldMap.get(fieldName).toRightIor(mkOneError(s"No field '$fieldName' for type $tpe")).flatMap(f => f(fieldName :: path, focus, Some(this), Env.empty))
    }
  }
}

trait MkInterfaceCursorBuilder[T] {
  def apply(tpe: Type): CursorBuilder[T]
}

object MkInterfaceCursorBuilder {
  implicit def coproductCursorBuilder[T, R <: Coproduct]
    (implicit
      gen: Generic.Aux[T, R],
      elems0: => CLiftAll[CursorBuilder, R]
    ): MkInterfaceCursorBuilder[T] =
    new MkInterfaceCursorBuilder[T] {
      def apply(tpe: Type): CursorBuilder[T] = {
        def elems = unsafeToList[CursorBuilder[T]](elems0.instances).toArray
        def sel(t: T) = {
          @tailrec
          def loop(c: Coproduct, acc: Int): Int = c match {
            case Inl(_) => acc
            case Inr(tl) => loop(tl, acc+1)
          }
          loop(gen.to(t), 0)
        }
        new Impl[T](tpe, sel, elems)
      }
    }

  class Impl[T](tpe0: Type, sel: T => Int, elems: => Array[CursorBuilder[T]]) extends CursorBuilder[T] {
    val tpe = tpe0
    def build(path: List[String], focus: T, parent: Option[Cursor], env: Env): Result[Cursor] = {
      val builder = elems(sel(focus))
      builder.build(path, focus, parent, env).map(cursor => CursorImpl(tpe, builder.tpe, cursor, parent, env))
    }
  }

  case class CursorImpl[T](tpe: Type, rtpe: Type, cursor: Cursor, parent: Option[Cursor], env: Env)
    extends AbstractCursor[T] {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def focus: Any = cursor.focus
    def path: List[String] = cursor.path

    override def hasField(fieldName: String): Boolean = cursor.hasField(fieldName)

    override def field(fieldName: String): Result[Cursor] = cursor.field(fieldName)

    override def narrowsTo(subtpe: TypeRef): Boolean =
      subtpe <:< tpe && rtpe <:< subtpe

    override def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe)) copy(tpe = subtpe).rightIor
      else mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
  }
}

trait CursorBuilder[T] { outer =>
  def tpe: Type
  def build(path: List[String], focus: T, parent: Option[Cursor] = None, env: Env = Env.empty): Result[Cursor]
}

object CursorBuilder {
  def apply[T](implicit cb: CursorBuilder[T]): CursorBuilder[T] = cb

  import ScalarType._

  implicit val stringCursorBuilder: CursorBuilder[String] = {
    case class StringCursor(path: List[String], focus: String, parent: Option[Cursor], env: Env) extends PrimitiveCursor[String] {
      val tpe = StringType
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromString(focus).rightIor
    }
    new CursorBuilder[String] {
      val tpe = StringType
      def build(path: List[String], focus: String, parent: Option[Cursor], env: Env): Result[Cursor] =
        StringCursor(path, focus, parent, env).rightIor
    }
  }

  implicit val intCursorBuilder: CursorBuilder[Int] = {
    case class IntCursor(path: List[String], focus: Int, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Int] {
      val tpe = IntType
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromInt(focus).rightIor
    }
    new CursorBuilder[Int] {
      val tpe = IntType
      def build(path: List[String], focus: Int, parent: Option[Cursor], env: Env): Result[Cursor] =
        IntCursor(path, focus, parent, env).rightIor
    }
  }

  implicit val longCursorBuilder: CursorBuilder[Long] = {
    case class LongCursor(path: List[String], focus: Long, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Long] {
      val tpe = IntType
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromLong(focus).rightIor
    }
    new CursorBuilder[Long] {
      val tpe = IntType
      def build(path: List[String], focus: Long, parent: Option[Cursor], env: Env): Result[Cursor] =
        LongCursor(path, focus, parent, env).rightIor
    }
  }

  implicit val floatCursorBuilder: CursorBuilder[Float] = {
    case class FloatCursor(path: List[String], focus: Float, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Float] {
      val tpe = FloatType
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] =
        Json.fromFloat(focus).toRightIor(mkOneError(s"Unrepresentable float %focus"))
    }
    new CursorBuilder[Float] {
      val tpe = FloatType
      def build(path: List[String], focus: Float, parent: Option[Cursor], env: Env): Result[Cursor] =
        FloatCursor(path, focus, parent, env).rightIor
    }
  }

  implicit val doubleCursorBuilder: CursorBuilder[Double] = {
    case class DoubleCursor(path: List[String], focus: Double, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Double] {
      val tpe = FloatType
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] =
        Json.fromDouble(focus).toRightIor(mkOneError(s"Unrepresentable double %focus"))
    }
    new CursorBuilder[Double] {
      val tpe = FloatType
      def build(path: List[String], focus: Double, parent: Option[Cursor], env: Env): Result[Cursor] =
        DoubleCursor(path, focus, parent, env).rightIor
    }
  }

  implicit val booleanCursorBuilder: CursorBuilder[Boolean] = {
    case class BooleanCursor(path: List[String], focus: Boolean, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Boolean] {
      val tpe = BooleanType
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromBoolean(focus).rightIor
    }
    new CursorBuilder[Boolean] {
      val tpe = BooleanType
      def build(path: List[String], focus: Boolean, parent: Option[Cursor], env: Env): Result[Cursor] =
        BooleanCursor(path, focus, parent, env).rightIor
    }
  }

  implicit def deriveEnumerationCursorBuilder[T <: Enumeration#Value](tpe0: Type): CursorBuilder[T] = {
    case class EnumerationCursor(path: List[String], focus: T, parent: Option[Cursor], env: Env) extends PrimitiveCursor[T] {
      val tpe = tpe0
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromString(focus.toString).rightIor
    }
    new CursorBuilder[T] {
      val tpe = tpe0
      def build(path: List[String], focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
        EnumerationCursor(path, focus, parent, env).rightIor
    }
  }

  implicit def enumerationCursorBuilder[T <: Enumeration#Value]: CursorBuilder[T] =
    deriveEnumerationCursorBuilder(NoType)

  implicit def optionCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[Option[T]] = {
    case class OptionCursor(path: List[String], tpe: Type, focus: Option[T], parent: Option[Cursor], env: Env) extends AbstractCursor[Option[T]] {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      override def isNullable: Boolean = true
      override def asNullable: Result[Option[Cursor]] = {
        focus.traverse(elem => elemBuilder.build(path, elem, Some(this), env))
      }
    }

    new CursorBuilder[Option[T]] { outer =>
      val tpe = NullableType(elemBuilder.tpe)
      def build(path: List[String], focus: Option[T], parent: Option[Cursor], env: Env): Result[Cursor] =
        OptionCursor(path, tpe, focus, parent, env).rightIor
    }
  }

  implicit def listCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[List[T]] = {
    case class ListCursor(path: List[String], tpe: Type, focus: List[T], parent: Option[Cursor], env: Env) extends AbstractCursor[List[T]] {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      override def isList: Boolean = true
      override def asList: Result[List[Cursor]] = {
        focus.traverse(elem => elemBuilder.build(path, elem, Some(this), env))
      }
    }

    new CursorBuilder[List[T]] { outer =>
      val tpe = ListType(elemBuilder.tpe)
      def build(path: List[String], focus: List[T], parent: Option[Cursor], env: Env): Result[Cursor] =
        ListCursor(path, tpe, focus, parent, env).rightIor
    }
  }

  case class LeafCursor[T](path: List[String], tpe: Type, focus: T, encoder: Encoder[T], parent: Option[Cursor], env: Env) extends AbstractCursor[T] {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def isLeaf: Boolean = true
    override def asLeaf: Result[Json] = encoder(focus).rightIor
  }

  def deriveLeafCursorBuilder[T](tpe0: Type)(implicit encoder: Encoder[T]): CursorBuilder[T] =
    new CursorBuilder[T] {
      val tpe = tpe0
      def build(path: List[String], focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
        new LeafCursor(path, tpe, focus, encoder, parent, env).rightIor
    }

  implicit def leafCursorBuilder[T](implicit encoder: Encoder[T]): CursorBuilder[T] =
    deriveLeafCursorBuilder(NoType)
}

abstract class AbstractCursor[T] extends Cursor {
  def isLeaf: Boolean = false

  def asLeaf: Result[Json] =
    mkErrorResult(s"Expected Scalar type, found $tpe for focus ${focus}")

  def isList: Boolean = false

  def asList: Result[List[Cursor]] =
    mkErrorResult(s"Expected List type, found $tpe")

  def isNullable: Boolean = false

  def asNullable: Result[Option[Cursor]] =
    mkErrorResult(s"Expected Nullable type, found $focus for $tpe")

  def narrowsTo(subtpe: TypeRef): Boolean = false

  def narrow(subtpe: TypeRef): Result[Cursor] =
    mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

  def hasField(fieldName: String): Boolean = false

  def field(fieldName: String): Result[Cursor] =
    mkErrorResult(s"No field '$fieldName' for type $tpe")

  def hasAttribute(attributeName: String): Boolean = false

  def attribute(attributeName: String): Result[Any] =
    mkErrorResult(s"No attribute '$attributeName' for type $tpe")
}

abstract class PrimitiveCursor[T] extends AbstractCursor[T] {
  val focus: T
  override def isLeaf: Boolean = true
}

object ShapelessUtils {
  def unsafeToList[U](l: HList): List[U] = {
    @tailrec
    def loop(l: HList, acc: List[U]): List[U] = l match {
      case HNil => acc.reverse
      case hd :: tl => loop(tl, hd.asInstanceOf[U] :: acc)
    }
    loop(l, Nil)
  }
}
