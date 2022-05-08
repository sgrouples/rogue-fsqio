package me.sgrouples.rogue

object tags {
  opaque type Tagged[+V, +Tag] = Any
  type @@[+V, +Tag] = V & Tagged[V, Tag]

  def tag[Tag]: [V] => V => V @@ Tag = 
    [V] => (v: V) => v
}
