package com.github.dcameronmauch

import java.util.concurrent.locks.{Lock, ReadWriteLock, ReentrantReadWriteLock}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class LRUCache[K, V](private val initSize: Int) {
  require(initSize > 0)

  // locks

  private val masterLock: ReadWriteLock = new ReentrantReadWriteLock()
  private val readLock: Lock = masterLock.readLock()
  private val writeLock: Lock = masterLock.writeLock()

  // double linked list

  private class Node(val key: K, var value: V, var prev: Option[Node] = None, var next: Option[Node] = None)

  // state variables

  private val map: MMap[K, Node] = MMap.empty
  private var maxSize: Int = initSize
  private var head: Option[Node] = None
  private var tail: Option[Node] = None

  // size methods

  def getCurrSize: Int = try {
    readLock.lock()
    map.size
  } finally readLock.unlock()

  def getMaxSize: Int = try {
    readLock.lock()
    maxSize
  } finally readLock.unlock()

  def setMaxSize(newSize: Int): Unit = try {
    writeLock.lock()
    require(newSize > 0)
    if (newSize < map.size) {
      val nodesToRemove: Int = map.size - newSize
      (1 to nodesToRemove).foreach(_ => evictNode())
    }
    maxSize = newSize
  } finally writeLock.unlock()

  // value methods

  def getValue(key: K): Option[V] = try {
    writeLock.lock()
    map.get(key).map(node => {
      moveNode(node)
      node.value
    })
  } finally writeLock.unlock()

  def setValue(key: K, value: V): Unit = try {
    writeLock.lock()
    if (map.contains(key)) {
      val node: Node = map(key)
      node.value = value
      moveNode(node)
    }
    else {
      if (map.size == maxSize) evictNode()
      val node: Node = new Node(key, value)
      map.addOne(key, node)
      addNode(node)
    }
  } finally writeLock.unlock()

  def getKeys(page: Int = 0, size: Int = 1000): List[K] = try {
    readLock.lock()

    @tailrec
    def recurse(nodeOpt: Option[Node], skip: Int, remain: Int, acc: List[K]): List[K] =
      if (remain == 0 || nodeOpt.isEmpty) acc.reverse
      else if (skip > 0) recurse(nodeOpt.get.next, skip - 1, remain, acc)
      else recurse(nodeOpt.get.next, skip, remain - 1, nodeOpt.get.key :: acc)

    recurse(head, page * size, size, List.empty)
  } finally readLock.unlock()

  // private methods

  private def moveNode(node: Node): Unit = {
    removeNode(node)
    addNode(node)
  }

  private def removeNode(node: Node): Unit = {
    node.prev.foreach(_.next = node.next)
    node.next.foreach(_.prev = node.prev)
    if (node.next.isEmpty) tail = node.prev
  }

  private def addNode(node: Node): Unit = {
    node.prev = None
    node.next = head
    head.foreach(_.prev = Some(node))
    head = Some(node)
    if (tail.isEmpty) tail = Some(node)
  }

  private def evictNode(): Unit =
    tail.foreach(node => {
      map.remove(node.key)
      val prev: Option[Node] = node.prev
      if (prev.isEmpty) {
        head = None
        tail = None
      }
      else {
        prev.foreach(_.next = None)
        tail = prev
      }
    })
}
