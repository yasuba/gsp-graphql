// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Value._
import QueryCompiler._

final class VariablesSuite extends CatsSuite {
  test("simple variables query") {
    val text = """
      query getZuckProfile($devicePicSize: Int) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val variables = json"""
      {
        "devicePicSize": 60
      }
    """

    val expected =
      Select("user", List(Binding("id", IDValue("4"))),
        Group(List(
          Select("id", Nil, Empty),
          Select("name", Nil, Empty),
          Select("profilePic", List(Binding("size", IntValue(60))), Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("list variable query") {
    val text = """
      query getProfile($ids: [ID!]) {
        users(ids: $ids) {
          name
        }
      }
    """

    val variables = json"""
      {
        "ids": [1, 2, 3]
      }
    """

    val expected =
      Select("users",
        List(Binding("ids", ListValue(List(IDValue("1"), IDValue("2"), IDValue("3"))))),
        Select("name", Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("object variable query") {
    val text = """
      query doSearch($pattern: Pattern) {
        search(pattern: $pattern) {
          name
          id
        }
      }
    """

    val variables = json"""
      {
        "pattern": {
          "name": "Foo",
          "age": 23,
          "id": 123
        }
      }
    """

    val expected =
      Select("search",
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123"))
          ))
        )),
        Group(List(
          Select("name", Nil, Empty),
          Select("id", Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }
}

object VariablesMapping {
  val schema =
    Schema(
      """
        type Query {
          user(id: ID!): User!
          users(ids: [ID!]!): [User!]!
          search(pattern: Pattern!): [User!]!
        }
        type User {
          id: String!
          name: String!
          profilePic(size: Int): String!
        }
        input Pattern {
          name: String
          age: Int
          id: ID
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> PartialFunction.empty
  ))

  val compiler = new QueryCompiler(schema, List(selectElaborator), Nil)
}
