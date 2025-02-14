/**
 * CONCURRENCY
 *
 * ZIO has pervasive support for concurrency, including parallel versions of
 * operators like `zip`, `foreach`, and many others.
 *
 * More than just enabling your applications to be highly concurrent, ZIO
 * gives you lock-free, asynchronous structures like queues, hubs, and pools.
 * Sometimes you need more: you need the ability to concurrently update complex
 * shared state across many fibers.
 *
 * Whether you need the basics or advanced custom functionality, ZIO has the
 * toolset that enables you to solve any problem you encounter in the
 * development of highly-scalable, resilient, cloud-native applications.
 *
 * In this section, you will explore more advanced aspects of ZIO concurrency,
 * so you can learn to build applications that are low-latency, highly-
 * scalable, interruptible, and free from deadlocks and race conditions.
 */
package advancedzio.concurrency

import zio._
import zio.stm._
import zio.test._
import zio.test.TestAspect._

/**
 * ZIO queues are high-performance, asynchronous, lock-free structures backed
 * by hand-optimized ring buffers. ZIO queues come in variations for bounded,
 * which are doubly-backpressured for producers and consumers, sliding (which
 * drops earlier elements when capacity is reached), dropping (which drops
 * later elements when capacity is reached), and unbounded.
 *
 * Queues work well for multiple producers and multiple consumers, where
 * consumers divide work between themselves.
 */
object QueueBasics extends ZIOSpecDefault {
  def spec =
    suite("QueueBasics") {

      /**
       * EXERCISE
       *
       * Using `.take` and `.offer`, create two fibers, one which places
       * 12 inside the queue, and one which takes an element from the
       * queue and stores it into the provided `ref`.
       */
      test("offer/take") {
        for {
          queue <- Queue.bounded[Int](1)
          offerFiber <- queue.offer(12).fork
          ref <- Ref.make(0)
          takeFiber <- queue.take.flatMap(ref.set).fork
          // Zip the two fibers and wait for them to finish
          _ <- offerFiber.zip(takeFiber).join
          v   <- ref.get
        } yield assertTrue(v == 12)
      }  +
        /**
         * EXERCISE
         *
         * Create a consumer of this queue that adds each value taken from the
         * queue to the counter, so the unit test can pass.
         */
        test("consumer") {
          for {
            counter <- Ref.make(0)
            queue   <- Queue.bounded[Int](100)
            _       <- ZIO.foreach(1 to 100)(v => queue.offer(v)).forkDaemon
            value   <- counter.get
          } yield assertTrue(value == 5050)
        } @@ ignore +
        /**
         * EXERCISE
         *
         * Queues are fully concurrent-safe on both producer and consumer side.
         * Choose the appropriate operator to parallelize the production side so
         * all values are produced in parallel.
         */
        test("multiple producers") {
          for {
            counter <- Ref.make(0)
            queue   <- Queue.bounded[Int](100)
            _       <- ZIO.foreach(1 to 100)(v => queue.offer(v)).forkDaemon
            _       <- queue.take.flatMap(v => counter.update(_ + v)).repeatN(99)
            value   <- counter.get
          } yield assertTrue(value == 5050)
        } @@ ignore +
        /**
         * EXERCISE
         *
         * Choose the appropriate operator to parallelize the consumption side so
         * all values are consumed in parallel.
         */
        test("multiple consumers") {
          for {
            counter <- Ref.make(0)
            queue   <- Queue.bounded[Int](100)
            _       <- ZIO.foreachPar(1 to 100)(v => queue.offer(v)).forkDaemon
            _       <- queue.take.flatMap(v => counter.update(_ + v)).repeatN(99)
            value   <- counter.get
          } yield assertTrue(value == 5050)
        } @@ ignore +
        /**
         * EXERCISE
         *
         * Shutdown the queue, which will cause its sole producer to be
         * interrupted, resulting in the test succeeding.
         */
        test("shutdown") {
          for {
            done   <- Ref.make(false)
            latch  <- Promise.make[Nothing, Unit]
            queue  <- Queue.bounded[Int](100)
            _      <- (latch.succeed(()) *> queue.offer(1).forever).ensuring(done.set(true)).fork
            _      <- latch.await
            _      <- queue.takeN(100)
            isDone <- done.get.repeatWhile(_ == false).timeout(10.millis).some
          } yield assertTrue(isDone)
        } @@ ignore
    }
}

/**
 * ZIO's software transactional memory lets you create your own custom
 * lock-free, race-free, concurrent structures, for cases where there
 * are no alternatives in ZIO, or when you need to make coordinated
 * changes across many structures in a transactional way.
 */
object StmBasics extends ZIOSpecDefault {
  def spec =
    suite("StmBasics") {
      test("latch") {

        /**
         * EXERCISE
         *
         * Implement a simple concurrent latch.
         */
        final case class Latch(ref: TRef[Boolean]) {
          def await: UIO[Any]   = ZIO.unit
          def trigger: UIO[Any] = ZIO.unit
        }

        def makeLatch: UIO[Latch] = TRef.make(false).map(Latch(_)).commit

        for {
          latch  <- makeLatch
          waiter <- latch.await.fork
          _      <- Live.live(Clock.sleep(10.millis))
          first  <- waiter.poll
          _      <- latch.trigger
          _      <- Live.live(Clock.sleep(10.millis))
          second <- waiter.poll
        } yield assertTrue(first.isEmpty && second.isDefined)
      } @@ ignore +
        test("countdown latch") {

          /**
           * EXERCISE
           *
           * Implement a simple concurrent latch.
           */
          final case class CountdownLatch(ref: TRef[Int]) {
            def await: UIO[Any]     = ZIO.unit
            def countdown: UIO[Any] = ZIO.unit
          }

          def makeLatch(n: Int): UIO[CountdownLatch] = TRef.make(n).map(ref => CountdownLatch(ref)).commit

          for {
            latch  <- makeLatch(10)
            _      <- latch.countdown.repeatN(8)
            waiter <- latch.await.fork
            _      <- Live.live(Clock.sleep(10.millis))
            first  <- waiter.poll
            _      <- latch.countdown
            _      <- Live.live(Clock.sleep(10.millis))
            second <- waiter.poll
          } yield assertTrue(first.isEmpty && second.isDefined)
        } @@ ignore +
        test("permits") {

          /**
           * EXERCISE
           *
           * Implement `acquire` and `release` in a fashion the test passes.
           */
          final case class Permits(ref: TRef[Int]) {
            def acquire(howMany: Int): UIO[Unit] = ???

            def release(howMany: Int): UIO[Unit] = ???
          }

          def makePermits(max: Int): UIO[Permits] = TRef.make(max).map(Permits(_)).commit

          for {
            counter <- Ref.make(0)
            permits <- makePermits(100)
            _ <- ZIO.foreachPar(1 to 1000)(
                  _ => Random.nextIntBetween(1, 2).flatMap(n => permits.acquire(n) *> permits.release(n))
                )
            latch   <- Promise.make[Nothing, Unit]
            fiber   <- (latch.succeed(()) *> permits.acquire(101) *> counter.set(1)).forkDaemon
            _       <- latch.await
            _       <- Live.live(ZIO.sleep(1.second))
            _       <- fiber.interrupt
            count   <- counter.get
            permits <- permits.ref.get.commit
          } yield assertTrue(count == 0 && permits == 100)
        } @@ ignore
    }
}

/**
 * ZIO hubs are high-performance, asynchronous, lock-free structures backed
 * by hand-optimized ring buffers. Hubs are designed for broadcast scenarios
 * where multiple (potentially many) consumers need to access the same values
 * being published to the hub.
 */
object HubBasics extends ZIOSpecDefault {
  def spec =
    suite("HubBasics") {

      /**
       * EXERCISE
       *
       * Use the `subscribe` method from 100 fibers to pull out the same values
       * from a hub, and use those values to increment `counter`.
       *
       * Take note of the synchronization logic. Why is this logic necessary?
       */
      test("subscribe") {
        for {
          counter <- Ref.make[Int](0)
          hub     <- Hub.bounded[Int](100)
          latch   <- TRef.make(100).commit
          scount  <- Ref.make[Int](0)
          _       <- (latch.get.retryUntil(_ <= 0).commit *> ZIO.foreach(1 to 100)(hub.publish(_))).forkDaemon
          _ <- ZIO.foreachPar(1 to 100) { _ =>
                ZIO.scoped(hub.subscribe.flatMap { queue =>
                  latch.update(_ - 1).commit
                })
              }
          value <- counter.get
        } yield assertTrue(value == 505000)
      } @@ ignore
    }
}

/**
 * GRADUATION PROJECT
 *
 * To graduate from this section, you will choose and complete one of the
 * following two problems:
 *
 * 1. Implement a bulkhead pattern, which provides rate limiting to a group
 *    of services to protect other services accessing a given resource.
 *
 * 2. Implement a CircuitBreaker, which triggers on too many failures, and
 *    which (gradually?) resets after a certain amount of time.
 */
object Graduation extends ZIOSpecDefault {
  def spec =
    suite("Graduation")()
}
