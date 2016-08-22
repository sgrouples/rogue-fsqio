# Rogue

Rogue is a type-safe internal Scala DSL for constructing and executing find and modify commands against
MongoDB in the Lift web framework. Originally developed by [Foursquare](http://github.com/foursquare/rogue), then re-published 
as part of Foursquare [monorepo]((http://github.com/foursquare/fsqio)

#Sgrouples changes to the original

1. Mongo java [driver](http://mongodb.github.io/mongo-java-driver/) updated to version 3.2.x to support MongoDB version 3.2 and up
2. Support for non-blocking IO via async functions like `getAsync`, `fetchAsync` etc.
   Note that for really non-blocking io, you need to use MongoDB version 3.2 and higher, as earlier versions of Mongo don't have non-blocking protocol implemented, and in effect connection pool will be exhausted anyway
3. Spindle support is not implemented, as we don't use it internally
4. Build is standard sbt instead of pants

#Building and installing
Standard sbt build, so just `sbt package; sbt publish-local` is enough. 


#Usage
1. define your model as a case class `CC`
supported types:
 `Boolean`, `Int`, `Long`, `String`, `ObjectId`, `Tagged ObjectId` (shapeless `@@`), `Double`, `UUID`, `java.time.LocalDateTime`, `java.time.Instant`,
 `Map[String, _]` , `List[_]`, `Option[_]`, `Array[_]`

inside `_` can be eny of supported types or a case class - so nesting is allowed.

Enumerations are handled separately - you need to import either `me.sgrouples.rogue.EnumNameFormats._` if you want to serialize Enumeration names or
 `me.sgrouples.rogue.EnumValueFormats._` if you want to serialize integers.


Default values handling: if database lacks value that can be deserialized into required format, a default value is provided, those are, 0 of specific type or empty. This is
a heritage from Lift Active record, and might be changed in the future.

2. Create `RCcMeta[CC]` object with definition of your fields. They must match case class fields

3. Create connection to Mongo and put it on