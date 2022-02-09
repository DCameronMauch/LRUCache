package com.github.dcameronmauch

import java.util.concurrent.locks.{Lock, ReadWriteLock, ReentrantReadWriteLock}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class LRWCache[K, V](private val initSize: Int) {
  require(initSize > 0)

  // locks

  private val masterLock: ReadWriteLock = new ReentrantReadWriteLock()
  private val readLock: Lock = masterLock.readLock()
  private val writeLock: Lock = masterLock.writeLock()

  // double linked list

  private class Node(val key: K, var value: V, var prev: Option[Node] = None, var next: Option[Node] = None) {
    override def toString: String =
      s"Node(${key}, ${value}, ${prev.map(_.key)}, ${next.map{_.key}})"
  }

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
    readLock.lock()
    map.get(key).map(_.value)
  } finally readLock.unlock()

  def setValue(key: K, value: V): Unit = try {
    writeLock.lock()
    map.get(key) match {
      case Some(node) =>
        node.value = value
        moveNode(node)
      case None =>
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

  private def moveNode(node: Node): Unit =
    if (node.prev.nonEmpty) {
      removeNode(node)
      addNode(node)
    }

  private def removeNode(node: Node): Unit = {
    node.prev match {
      case Some(prev) =>
        prev.next = node.next
      case None =>
        head = node.next
    }

    node.next match {
      case Some(next) =>
        next.prev = node.prev
      case None =>
        tail = node.prev
    }

    node.prev = None
    node.next = None
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
      removeNode(node)
    })
}
