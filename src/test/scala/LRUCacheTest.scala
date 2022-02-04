package com.github.dcameronmauch

import org.scalatest.funsuite.AnyFunSuite

class LRUCacheTest extends AnyFunSuite {
  test("empty cache") {
    val cache: LRUCache[Int, String] = new LRUCache(4)

    assertResult(0)(cache.getCurrSize)
    assertResult(List.empty)(cache.getKeys())
  }

  test("adding max elements") {
    val cache: LRUCache[Int, String] = new LRUCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")

    assertResult(4)(cache.getCurrSize)
    assertResult(List(4, 3, 2, 1))(cache.getKeys())
  }

  test("adding more than max elements") {
    val cache: LRUCache[Int, String] = new LRUCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")
    cache.setValue(5, "mno")

    assertResult(4)(cache.getCurrSize)
    assertResult(List(5, 4, 3, 2))(cache.getKeys())
  }

  test("get moves to head") {
    val cache: LRUCache[Int, String] = new LRUCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")

    assertResult(Some("abc"))(cache.getValue(1))
    assertResult(4)(cache.getCurrSize)
    assertResult(List(1, 4, 3, 2))(cache.getKeys())
  }

  test("expanding/shrinking cache") {
    val cache: LRUCache[Int, String] = new LRUCache(5)

    cache.setValue(1, "abc")
    assertResult(1)(cache.getCurrSize)
    assertResult(List(1))(cache.getKeys())

    cache.setValue(2, "def")
    assertResult(2)(cache.getCurrSize)
    assertResult(List(2, 1))(cache.getKeys())

    cache.setValue(3, "ghi")
    assertResult(3)(cache.getCurrSize)
    assertResult(List(3, 2, 1))(cache.getKeys())

    cache.setValue(4, "jkl")
    assertResult(4)(cache.getCurrSize)
    assertResult(List(4, 3, 2, 1))(cache.getKeys())

    cache.setValue(5, "mno")
    assertResult(5)(cache.getCurrSize)
    assertResult(List(5, 4, 3, 2, 1))(cache.getKeys())

    cache.getValue(1)
    assertResult(5)(cache.getCurrSize)
    assertResult(List(1, 5, 4, 3, 2))(cache.getKeys())

    cache.setMaxSize(3)
    assertResult(3)(cache.getCurrSize)
    assertResult(List(1, 5, 4))(cache.getKeys())
  }

  test("overwrite element") {
    val cache: LRUCache[Int, String] = new LRUCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")
    cache.setValue(2, "zzz")

    assertResult(4)(cache.getCurrSize)
    assertResult(List(2, 4, 3, 1))(cache.getKeys())
    assertResult(Some("zzz"))(cache.getValue(2))
  }
  
  test("get keys paging") {
    val cache: LRUCache[Int, String] = new LRUCache(5)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")
    cache.setValue(5, "mno")

    assertResult(List(5, 4))(cache.getKeys(0, 2))
    assertResult(List(3, 2))(cache.getKeys(1, 2))
    assertResult(List(1))(cache.getKeys(2, 2))
  }
}
