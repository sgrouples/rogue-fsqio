package io.fsq.rogue
import io.fsq.rogue.index.IndexModifier
import io.fsq.field.Field

sealed trait Hint
final class NamedHint(val name:String) extends Hint
final class NaturalHint(val ord: IndexModifier) extends Hint
final class FieldsHint(val seq:Seq[(Field[_,_], IndexModifier)]) extends Hint