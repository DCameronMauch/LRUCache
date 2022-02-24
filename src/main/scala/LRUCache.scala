package com.github.dcameronmauch

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class LRUCache[K, V](private val initSize: Int) extends Cache[K, V] {
  require(initSize > 0)

  // double linked list

  private class Node(val key: K, var value: V, var prev: Option[Node] = None, var next: Option[Node] = None)

  // state variables

  private val map: MMap[K, Node] = MMap.empty
  private var maxSize: Int = initSize
  private var head: Option[Node] = None
  private var tail: Option[Node] = None

  // size methods

  def getCurrSize: Int = synchronized {
    map.size
  }

  def getMaxSize: Int = synchronized {
    maxSize
  }

  def setMaxSize(newSize: Int): Unit = synchronized {
    require(newSize > 0)
    if (newSize < map.size) {
      val nodesToRemove: Int = map.size - newSize
      (1 to nodesToRemove).foreach(_ => evictNode())
    }
    maxSize = newSize
  }

  // value methods

  def getValue(key: K): Option[V] = synchronized {
    map.get(key).map(node => {
      moveNode(node)
      node.value
    })
  }

  def setValue(key: K, value: V): Unit = synchronized {
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
  }

  def getKeys(page: Int = 0, size: Int = 1000): List[K] = synchronized {
    @tailrec
    def recurse(nodeOpt: Option[Node], skip: Int, remain: Int, acc: List[K]): List[K] =
      if (remain == 0 || nodeOpt.isEmpty) acc.reverse
      else if (skip > 0) recurse(nodeOpt.get.next, skip - 1, remain, acc)
      else recurse(nodeOpt.get.next, skip, remain - 1, nodeOpt.get.key :: acc)

    recurse(head, page * size, size, List.empty)
  }

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
