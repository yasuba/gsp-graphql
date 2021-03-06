// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package jsonb

import cats.effect.Sync
import cats.implicits._

import edu.gemini.grackle._, skunk._
import Query._, Predicate._, Value._
import QueryCompiler._
import _root_.skunk.codec.all._
import _root_.skunk.circe.codec.all._
import cats.effect.Resource
import _root_.skunk.Session

trait JsonbMapping[F[_]] extends SkunkMapping[F] {
  val schema =
    Schema(
      """
        type Query {
          record(id: Int!): Row
          records: [Row!]!
        }
        type Row {
          id: Int!
          record: Record
          nonNullRecord: Record!
        }
        type Record {
          bool: Boolean
          int: Int
          float: Float
          string: String
          id: ID
          choice: Choice
          arrary: [Int!]
          object: A
          children: [Child!]!
        }
        enum Choice {
          ONE
          TWO
          THREE
        }
        interface Child {
          id: ID
        }
        type A implements Child {
          id: ID
          aField: Int
        }
        type B implements Child {
          id: ID
          bField: String
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val RowType = schema.ref("Row")
  val RecordType = schema.ref("Record")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("record"),
            SqlRoot("records")
          )
      ),
      ObjectMapping(
        tpe = RowType,
        fieldMappings =
          List(
            SqlField("id", ColumnRef("records", "id", int4), key = true),
            SqlJson("record", ColumnRef("records", "record", jsonb.opt))
          )
      ),
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("record", List(Binding("id", IntValue(id))), child) =>
        Select("record", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))
}

object JsonbMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with JsonbMapping[F]

}
