// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue

import org.bson.Document

trait RogueReadSerializer[R] {
  def fromDocument(dbo: Document): R
}

trait RogueWriteSerializer[R] {
  def toDocument(record: R): Document
}