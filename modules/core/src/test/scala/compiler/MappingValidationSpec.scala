// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.data.Ior
import cats.tests.CatsSuite
import composed.CountryData
import edu.gemini.grackle.{Schema, SimpleMapping, ValueMapping}

class MappingValidationSpec extends CatsSuite {

  test("validate schema missing defined type mapping") {
    val text = """
      query {
        component {
          fielda2 {
            componentb
          }
        }
      }
    """

    val expected = "Mappings validation failed. The following mappings were not found in the schema: FieldA2"

    UnvalidatedTypeMapping.compiler.compile(text) match {
      case Ior.Left(a) => assert(a.head.noSpaces.contains(expected))
      case unexpected  => fail(s"This was unexpected: $unexpected")
    }
  }

  test("validate schema missing defined fieldname mapping") {
    val text = """
      query {
        country(code: "GBR") {
          name
        }
      }
    """

    val expected = "Mappings validation failed. The following mappings were not found in the schema: FieldA2"

    UnvalidatedFieldMapping.compiler.compile(text) match {
      case Ior.Left(a) => assert(a.head.noSpaces.contains(expected))
      case unexpected  => fail(s"This was unexpected: $unexpected")
    }
  }
}

object Component1 extends DummyComponent

object UnvalidatedTypeMapping extends SimpleMapping[Id] {
  val schema =
    Schema(
      """
        type Query {
          component1: Component1!
        }
        type Component1 {
          fielda1: String!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val FieldA2Type = schema.ref("FieldA2")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("component1", Component1)
          )
      ),
      ObjectMapping(
        tpe = FieldA2Type,
        fieldMappings =
          Nil
      )
    )
}

object UnvalidatedFieldMapping extends ValueMapping[Id] {

  import CountryData._

  val schema =
    Schema(
      """
        type Query {
          country(code: String): Country
        }
        type Country {
          code: String!
          name: String!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("country", countries)
          )
      ),
      ValueObjectMapping[Country](
        tpe = CountryType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("name", _.name),
            ValueField("currencyCode", _.currencyCode)
          )
      )
    )
}
