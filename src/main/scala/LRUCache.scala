package com.github.dcameronmauch

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class LRUCache[K, V](private val initSize: Int) {
  require(initSize > 0)

  // double linked list

  private class Node(val key: K, var value: V, var prev: Option[Node] = None, var next: Option[Node] = None)

  // state variables

  private val map: MMap[K, Node] = MMap.empty
  private var maxSize: Int = initSize
  private var head: Option[Node] = None
  private var tail: Option[Node] = None

  // size methods

  def getCurrSize: Int = synchronized { map.size }

  def getMaxSize: Int = synchronized { maxSize }

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
  }

  def getKeys: List[K] = synchronized {
    @tailrec
    def recurse(nodeOpt: Option[Node], acc: List[K]): List[K] =
      if (nodeOpt.isEmpty) acc.reverse
      else recurse(nodeOpt.get.next, nodeOpt.get.key :: acc)

    recurse(head, List.empty)
  }

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