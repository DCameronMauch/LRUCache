package com.github.dcameronmauch

trait Cache[K, V] {
  def getCurrSize: Int
  def getMaxSize: Int
  def setMaxSize(newSize: Int): Unit
  def getValue(key: K): Option[V]
  def setValue(key: K, value: V): Unit
  def delValue(key: K): Unit
}
