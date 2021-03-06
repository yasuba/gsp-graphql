// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini

import cats.data.IorNec
import io.circe.Json

package object grackle {
  /**
   * A result value.
   *
   * A result of type `T`, a non-empty collection of errors encoded as
   * Json, or both.
   */
  type Result[T] = IorNec[Json, T]
}
