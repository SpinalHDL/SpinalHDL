package spinal.lib.misc.database

import spinal.core._
import spinal.core.fiber._

import scala.collection.mutable

/**
 * Provide a API to access a HashMap which uses Element[_ <: Any] as keys
 * The Database object provide a SpinalHDL ScopeProperty[DataBase] allowing to have one globally accessible implicit database
 * That globally shared database can be used as a way to exchange "global" variable in a given context (ex : VexiiRiscv core 3)
 */
class Database{
  // User API
  def update[T](key: Element[T], value: T) = key.set(this, value)
  def apply[T](key: Element[T]): T = key.getOn(this)
  def on[T](body: => T) = Database(this).on(body)

  // "private" API
  val storage = mutable.LinkedHashMap[Element[_ <: Any], Any]()
  def storageUpdate[T](key : Element[T], value : T) = storage.update(key, value)
  def storageGet[T](key : Element[T]) : T = storage.apply(key).asInstanceOf[T]
  def storageGetElseUpdate[T](key: Element[T], create: => T): T = storage.getOrElseUpdate(key, create).asInstanceOf[T]
  def storageExists[T](key : Element[T]) = storage.contains(key)
}

/**
 * Provide a global implicit database
 */
object Database extends ScopeProperty[Database] {
  def on[T](body: => T) = this (new Database).on(body) // Set the implicit database to be used ojn the given body of code.

  //Here is a few ways you can define new Element instances (which can be used as a key to the data base)
  def value[T]() = new ElementValue[T]()
  def blocking[T]() = new ElementBlocking[T]()
  def landa[T](body : => T) = new ElementLanda(body)
}

object Element{
  implicit def toValue[T](p : Element[T]) : T = p.get

  class ThingIntPimper(p: Element[Int]) {
    def bits = BitCount(p.get)
    def bit = BitCount(p.get)
  }

  implicit def thingIntPimperFunc(p: Element[Int]): ThingIntPimper = new ThingIntPimper(p)
}

/**
 * Represent a thing which can be in a data base (this is the key)
 */
abstract class Element[T](sp: ScopeProperty[Database] = Database) extends Nameable {
  // user API
  def get : T = getOn(sp.get)
  def apply: T = getOn(sp.get)
  def set(value: T): Unit = set(sp.get, value)
  def isEmpty : Boolean = isEmpty(sp.get)

  // private API
  def isEmpty(db: Database) : Boolean
  def getOn(db: Database) : T
  def set(db: Database, value: T) : Unit
}

/**
 * Simple implementation, which allow to get/set a value
 * Will throw an exception if we try to get something which isn't set.
 */
class ElementValue[T](sp : ScopeProperty[Database] = Database) extends Element[T](sp) {
  def getOn(db: Database): T = db.storageGet(this)
  def set(db: Database, value: T) = db.storageUpdate(this, value)
  override def isEmpty(db: Database): Boolean = ???
}

/**
 * Same as ElementValue, but based on the SpinalHDL Fiber API.
 * Meaning that when we get something which isn't set, it will put the current fiber thread in sleep until the thing is set.
 */
class ElementBlocking[T](sp : ScopeProperty[Database] = Database) extends Element[T](sp) with Area{
  val thing = new ElementValue[Handle[T]]()
  def getHandle(db : Database) : Handle[T] = db.storageGetElseUpdate(thing, new Handle[T].setCompositeName(this))
  def getOn(db: Database) : T = getHandle(db).get
  def set(db: Database, value : T) = {
    val handle = getHandle(db)
    if (handle.isLoaded) {
      assert(handle.get == value, s"DB was set to ${handle.get} before, can't overwrite to $value")
    }
    handle.load(value)
  }

  def soon(): Unit = {
    getHandle(sp.get).willBeLoadedBy = AsyncThread.current
    AsyncThread.current.willLoadHandles += getHandle(sp.get)
  }
  override def isEmpty(db: Database): Boolean = !getHandle(db).isLoaded
}

/**
 * The body provide the processing to generate the value
 */
class ElementLanda[T](body : => T, sp : ScopeProperty[Database] = Database) extends ElementValue[T](sp){
  override def getOn(db: Database) : T = {
    if(!db.storageExists(this)){
      db.storageUpdate(this, body)
    }
    super.getOn(db)
  }

  override def set(db: Database, value: T) = ???
}
