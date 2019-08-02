package com.mongodb.reactivestreams.client.internal

import com.mongodb.async.client.MongoDatabase

/*
 * hacky way to wrap async => reactive - since all other construction methods are sealed
 */
class ReactiveDbWrapper(db: MongoDatabase) {
  def wrap: MongoDatabaseImpl = new MongoDatabaseImpl(db)
}
