package me.sgrouples.rogue.enums

trait ReflectEnumInstance[T <: Enumeration] {


  /*protected def enumeration(implicit typeTag: TypeTag[T]): T = {

    val runtimeClass: ClassSymbol = universe.typeOf[T].typeSymbol.asClass

    val runtimeMirror: Mirror = universe
      .runtimeMirror(implicitly[TypeTag[T]].mirror.classLoader)

    val module: ModuleSymbol = runtimeMirror
      .staticModule(runtimeClass.fullName)

    runtimeMirror
      .reflectModule(module)
      .instance
      .asInstanceOf[T]

  }*/
}
