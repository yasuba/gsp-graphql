// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.data.Ior
import cats.effect.Sync
import doobie.Transactor
import io.circe.literal.JsonStringContext
import io.circe.optics.JsonPath.root
import io.chrisdavenport.log4cats.Logger
import utils.DatabaseSuite

final class WorldSpec extends DatabaseSuite {
  lazy val mapping = WorldMapping.fromTransactor(xa)

  test("simple query") {
    val query = """
      query {
        countries {
          name
        }
      }
    """

    val expected = 239

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    val resSize = root.data.countries.arr.getOption(res).map(_.size)

    assert(resSize == Some(expected))
  }

  test("simple restricted query") {
    val query = """
      query {
        country(code: "GBR") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "country": {
            "name": "United Kingdom"
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("simple restricted nested query") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
            languages {
              language
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "cities": [
            {
              "name": "Amersfoort",
              "country": {
                "name": "Netherlands",
                "languages": [
                  {
                    "language": "Arabic"
                  },
                  {
                    "language": "Dutch"
                  },
                  {
                    "language": "Fries"
                  },
                  {
                    "language": "Turkish"
                  }
                ]
              }
            },
            {
              "name": "Americana",
              "country": {
                "name": "Brazil",
                "languages": [
                  {
                    "language": "German"
                  },
                  {
                    "language": "Indian Languages"
                  },
                  {
                    "language": "Italian"
                  },
                  {
                    "language": "Japanese"
                  },
                  {
                    "language": "Portuguese"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("multiple aliased root queries") {
    val query = """
      query {
        gbr: country(code: "GBR") {
          name
        }
        fra: country(code: "FRA") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "gbr" : {
            "name" : "United Kingdom"
          },
          "fra" : {
            "name" : "France"
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (1)") {
    val query = """
      query {
        cities(namePattern: "Monte-Carlo") {
          name
          country {
            name
            cities {
              name
              country {
                name
                cities {
                  name
                  country {
                    name
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "cities" : [
          {
            "name" : "Monte-Carlo",
            "country" : {
              "name" : "Monaco",
              "cities" : [
                {
                  "name" : "Monte-Carlo",
                  "country" : {
                    "name" : "Monaco",
                    "cities" : [
                      {
                        "name" : "Monte-Carlo",
                        "country" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "country" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                },
                {
                  "name" : "Monaco-Ville",
                  "country" : {
                    "name" : "Monaco",
                    "cities" : [
                      {
                        "name" : "Monte-Carlo",
                        "country" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "country" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                }
              ]
            }
          }
        ]
      }
    }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (2)") {
    val query = """
      query {
        country(code: "ESP") {
          name
          languages {
            language
            countries {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Spain",
            "languages" : [
              {
                "language" : "Basque",
                "countries" : [
                  {
                    "name" : "Spain"
                  }
                ]
              },
              {
                "language" : "Catalan",
                "countries" : [
                  {
                    "name" : "Andorra"
                  },
                  {
                    "name" : "Spain"
                  }
                ]
              },
              {
                "language" : "Galecian",
                "countries" : [
                  {
                    "name" : "Spain"
                  }
                ]
              },
              {
                "language" : "Spanish",
                "countries" : [
                  {
                    "name" : "Aruba"
                  },
                  {
                    "name" : "Andorra"
                  },
                  {
                    "name" : "Argentina"
                  },
                  {
                    "name" : "Belize"
                  },
                  {
                    "name" : "Bolivia"
                  },
                  {
                    "name" : "Canada"
                  },
                  {
                    "name" : "Chile"
                  },
                  {
                    "name" : "Colombia"
                  },
                  {
                    "name" : "Costa Rica"
                  },
                  {
                    "name" : "Cuba"
                  },
                  {
                    "name" : "Dominican Republic"
                  },
                  {
                    "name" : "Ecuador"
                  },
                  {
                    "name" : "Spain"
                  },
                  {
                    "name" : "France"
                  },
                  {
                    "name" : "Guatemala"
                  },
                  {
                    "name" : "Honduras"
                  },
                  {
                    "name" : "Mexico"
                  },
                  {
                    "name" : "Nicaragua"
                  },
                  {
                    "name" : "Panama"
                  },
                  {
                    "name" : "Peru"
                  },
                  {
                    "name" : "Puerto Rico"
                  },
                  {
                    "name" : "Paraguay"
                  },
                  {
                    "name" : "El Salvador"
                  },
                  {
                    "name" : "Sweden"
                  },
                  {
                    "name" : "Uruguay"
                  },
                  {
                    "name" : "United States"
                  },
                  {
                    "name" : "Venezuela"
                  },
                  {
                    "name" : "Virgin Islands, U.S."
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (3)") {
    val query = """
      query {
        cities(namePattern: "Lausanne") {
          name
          country {
            name
            cities {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "cities" : [
            {
              "name" : "Lausanne",
              "country" : {
                "name" : "Switzerland",
                "cities" : [
                  {
                    "name" : "Zürich"
                  },
                  {
                    "name" : "Geneve"
                  },
                  {
                    "name" : "Basel"
                  },
                  {
                    "name" : "Bern"
                  },
                  {
                    "name" : "Lausanne"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (4)") {
    val query = """
      query {
        country(code: "CHE") {
          name
          cities {
            name
            country {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Switzerland",
            "cities" : [
              {
                "name" : "Zürich",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Geneve",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Basel",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Bern",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Lausanne",
                "country" : {
                  "name" : "Switzerland"
                }
              }
            ]
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (5)") {
    val query = """
      query {
        language(language: "Estonian") {
          language
          countries {
            name
            languages {
              language
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "language" : {
            "language" : "Estonian",
            "countries" : [
              {
                "name" : "Estonia",
                "languages" : [
                  {
                    "language" : "Belorussian"
                  },
                  {
                    "language" : "Estonian"
                  },
                  {
                    "language" : "Finnish"
                  },
                  {
                    "language" : "Russian"
                  },
                  {
                    "language" : "Ukrainian"
                  }
                ]
              },
              {
                "name" : "Finland",
                "languages" : [
                  {
                    "language" : "Estonian"
                  },
                  {
                    "language" : "Finnish"
                  },
                  {
                    "language" : "Russian"
                  },
                  {
                    "language" : "Saame"
                  },
                  {
                    "language" : "Swedish"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (6)") {
    val query = """
      query {
        cities(namePattern: "Monte-Carlo") {
          name
          a: country {
            name
            b: cities {
              name
              c: country {
                name
                d: cities {
                  name
                  e: country {
                    name
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "cities" : [
          {
            "name" : "Monte-Carlo",
            "a" : {
              "name" : "Monaco",
              "b" : [
                {
                  "name" : "Monte-Carlo",
                  "c" : {
                    "name" : "Monaco",
                    "d" : [
                      {
                        "name" : "Monte-Carlo",
                        "e" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "e" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                },
                {
                  "name" : "Monaco-Ville",
                  "c" : {
                    "name" : "Monaco",
                    "d" : [
                      {
                        "name" : "Monte-Carlo",
                        "e" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "e" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                }
              ]
            }
          }
        ]
      }
    }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
  test("country with no cities") {
    val query = """
      query {
        country(code: "ATA") {
          name
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "country": {
            "name": "Antarctica",
            "cities": []
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  // Outer join in which some parents have children and others do not.
  test("countries, some with no cities") {
    val query = """
      query {
        countries {
          name
          cities {
            name
          }
        }
      }
    """
    val cquery    = mapping.compiler.compile(query).right.get
    val json      = mapping.interpreter.run(cquery, mapping.schema.queryType).unsafeRunSync
    val countries = root.data.countries.arr.getOption(json).get
    val map       = countries.map(j => root.name.string.getOption(j).get -> root.cities.arr.getOption(j).get.length).toMap
    assert(map("Kazakstan")  == 21)
    assert(map("Antarctica") == 0)
  }

  test("no such country") {
    val query = """
      query {
        country(code: "XXX") {
          name
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : null
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("multiple missing countries") {
    val query = """
      query {
        xxx: country(code: "xxx") {
          name
        }
        yyy: country(code: "yyy") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "xxx" : null,
          "yyy" : null
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query)
    val res = mapping.interpreter.run(compiledQuery.right.get, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("nullable column (null)") {
    val query = """
    query {
        country(code: "ANT") {
          name
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Netherlands Antilles",
            "indepyear" : null
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("nullable column (non-null)") {
    val query = """
    query {
        country(code: "USA") {
          name
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United States",
            "indepyear" : 1776
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("query with introspection") {
    val query = """
      query {
        country(code: "GBR") {
          __typename
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "__typename" : "Country",
            "name" : "United Kingdom"
          }
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("structured predicates") {
    val query = """
      query {
        search(minPopulation: 20000000, indepSince: 1980) {
          name
          population
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "search" : [
            {
              "name" : "Russian Federation",
              "population" : 146934000,
              "indepyear" : 1991
            },
            {
              "name" : "Ukraine",
              "population" : 50456000,
              "indepyear" : 1991
            },
            {
              "name" : "Uzbekistan",
              "population" : 24318000,
              "indepyear" : 1991
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("simple query with limit") {
    val query = """
      query {
        countries(limit: 3) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Aruba"
            },
            {
              "name" : "Afghanistan"
            },
            {
              "name" : "Angola"
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("simple query with limit, filter and ordering") {
    val query = """
      query {
        countries(limit: 3, minPopulation: 1, byPopulation: true) {
          name
          population
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Pitcairn",
              "population" : 50
            },
            {
              "name" : "Cocos (Keeling) Islands",
              "population" : 600
            },
            {
              "name" : "Holy See (Vatican City State)",
              "population" : 1000
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("validate mappings for simple query") {
    object UnvalidatedWorldMapping {
      def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): WorldMapping[F] =
        new WorldMapping[F](transactor, Logger[F]) {
          override val typeMappings =
            List(
              ObjectMapping(
                tpe = QueryType,
                fieldMappings =
                  List(
                    DoobieRoot("country")
                  )
              ),
              ObjectMapping(
                tpe = CountryType,
                fieldMappings =
                  List(
                    DoobieAttribute[String]("code", ColumnRef("country", "code")),
                    DoobieFieldMapping.DoobieField("name", ColumnRef("country", "name"))
                  )
              )
            )
        }
    }

    lazy val unvalidatedMapping = UnvalidatedWorldMapping.fromTransactor(xa)

    val query = """
      query {
        countries {
          name
        }
      }
    """

    unvalidatedMapping.compiler.compile(query) match {
      case Ior.Left(a) => assert(a.head.noSpaces.contains("hi"))
      case unexpected => fail(s"This was unexpected $unexpected")
    }
  }
}
