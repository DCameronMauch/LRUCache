package com.github.dcameronmauch

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class LRWCacheTest extends AnyFunSuite {
  test("empty cache") {
    val cache: LRWCache[Int, String] = new LRWCache(4)

    assertResult(0)(cache.getCurrSize)
    assertResult(List.empty)(cache.getKeys())
  }

  test("adding max elements") {
    val cache: LRWCache[Int, String] = new LRWCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")

    assertResult(4)(cache.getCurrSize)
    assertResult(List(4, 3, 2, 1))(cache.getKeys())
  }

  test("adding more than max elements") {
    val cache: LRWCache[Int, String] = new LRWCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")
    cache.setValue(5, "mno")

    assertResult(4)(cache.getCurrSize)
    assertResult(List(5, 4, 3, 2))(cache.getKeys())
  }

  test("get does not move to head") {
    val cache: LRWCache[Int, String] = new LRWCache(4)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")

    assertResult(Some("abc"))(cache.getValue(1))
    assertResult(4)(cache.getCurrSize)
    assertResult(List(4, 3, 2, 1))(cache.getKeys())
  }

  test("expanding/shrinking cache") {
    val cache: LRWCache[Int, String] = new LRWCache(5)

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
    assertResult(List(5, 4, 3, 2, 1))(cache.getKeys())

    cache.setMaxSize(3)
    assertResult(3)(cache.getCurrSize)
    assertResult(List(5, 4, 3))(cache.getKeys())
  }

  test("overwrite element") {
    val cache: LRWCache[Int, String] = new LRWCache(4)

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
    val cache: LRWCache[Int, String] = new LRWCache(5)

    cache.setValue(1, "abc")
    cache.setValue(2, "def")
    cache.setValue(3, "ghi")
    cache.setValue(4, "jkl")
    cache.setValue(5, "mno")

    assertResult(List(5, 4))(cache.getKeys(0, 2))
    assertResult(List(3, 2))(cache.getKeys(1, 2))
    assertResult(List(1))(cache.getKeys(2, 2))
  }

  test("multi-threaded writes") {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(9))

    val cache: LRWCache[Int, String] = new LRWCache(9)

    val futures: Future[List[Unit]] = Future.sequence(
      List(
        Future(cache.setValue(1, "abc")),
        Future(cache.setValue(2, "def")),
        Future(cache.setValue(3, "ghi")),
        Future(cache.setValue(4, "jkl")),
        Future(cache.setValue(5, "mno"))
      )
    )

    futures.onComplete {
      case Success(_) =>
        assertResult(5)(cache.getCurrSize)
      case Failure(e) =>
        fail(e)
    }
  }
}
