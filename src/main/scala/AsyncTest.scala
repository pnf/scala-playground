import scala.concurrent._
import shapeless._

import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }
import scala.concurrent.duration._

import java.util.{ Timer, TimerTask }

import scala.util.{ Try, Success, Failure }

package core_async {

  object Timeout {
    val timer = new Timer()
    def timeout(d: Duration): Future[Unit] = timeout(d.toMillis)
    def timeout(d: Long): Future[Unit] = {
      val p = Promise[Unit]()
      val f = p.future
      val tt = new TimerTask() {
        def run {
          p.success(())
        }
      }
      timer.schedule(tt, d)
      f
    }
  }

  // to be used in conjunction with alts in lieu of pReadyForXXX.future
  class CancellablePromise[T](val p: Promise[T]) extends Promise[T] {
    val h: scala.collection.mutable.HashMap[Promise[Any], T => Unit] = new scala.collection.mutable.HashMap()

    def future: scala.concurrent.Future[T] = p.future
    def isCompleted: Boolean = p.isCompleted
    def tryComplete(result: scala.util.Try[T]): Boolean = p.tryComplete(result)

    def future(pCancel: Promise[Any])(body: T => Unit) = this.synchronized {
      pCancel.future.andThen { case _ => h.synchronized { h -= pCancel } }
      h += ((pCancel, body))
    }
    
    // Runs all bodies whose pCancel promises have not been completed.  In general, pCancel will
    // be the pNotify of an alts call, and the underlying promise is one of the pReadyForXXXX 
    p.future.map { x =>
      this.synchronized {
        h.foreach {
          (pf: (Promise[Any], T => Unit)) =>
            if (!pf._1.isCompleted) { pf._2(x) }
        }
      }
    }
  }

  object CancellablePromise {
    def apply[T] = new CancellablePromise[T](Promise[T]())
    def successful[T](result: T): Promise[T] = new CancellablePromise(Promise.fromTry(Success(result)))
  }

  // The only thing exciting about a ChanBuffer is that you pass its put/take methods
  // a promise to fulfill should that operation render the buffer no longer empty/full.
  abstract class ChanBuffer[T]() {
    def put(v: T, onNoLongerEmpty: () => Unit, onFull: () => Unit): Option[Unit]
    def take(onNoLongerFull: () => Unit, onEmpty: () => Unit): Option[T]
  }

  class NormalBuffer[T](n: Int, dropping: Boolean, sliding: Boolean) extends ChanBuffer[T] {
    val b = scala.collection.mutable.Buffer.empty[T]
    def put(v: T, onNoLongerEmpty: () => Unit, onFull: () => Unit) = this.synchronized {
      val s = b.size
      var ret: Option[Unit] = Some(Unit)
      if (s == n) {
        if (dropping) {
          b.update(n - 1, v)
        } else if (sliding) {
          b.remove(0)
          b += v
        } else {
          ret = None
        }
      } else if (s == (n - 1)) {
        b += v
        onFull()
        if (s == 0) {
          onNoLongerEmpty()
        }
      } else if (s == 0) {
        b += v
        onNoLongerEmpty()
      }
      ret
    }

    def take(onNoLongerFull: () => Unit, onEmpty: () => Unit) = this.synchronized {
      val s = b.size
      if (s > 0) {
        if (s == n) {
          onNoLongerFull()
          if (s == 1) { onEmpty() }
        }
        Some(b.remove(0))
      } else {
        None
      }
    }
  }
  
  case class CV[T](val c: Chan[T], val v: T)
  
  sealed trait ChanHolder[T] {
    def chan : Chan[T]
  }
  
  class ChanValueHolder[T](val c:Chan[T], val v:T) extends ChanHolder[T] {
    def chan = c
  }

  class Chan[T](val b: ChanBuffer[T], val name: String) extends ChanHolder[T ] {
    
    def chan = this

    private [this] var pReadyForWrite = CancellablePromise[Unit].success(Unit)
    private [this ] var pReadyForRead = CancellablePromise[Unit]

    // Extract the value in a chan-value pair, properly cast.  Note we explicitly match
    // CV[Any], because Chan is invariant.
    def unapply(cv: CV[Any]): Option[T] =
      if (cv.c == this) {
        Some(cv.v.asInstanceOf[T])
      } else None

    /** Attempt to write @v to the channel, fulfilling @pNotify if successful.
     *  If not, reschedule an attempt when pReadyForWrite fires.
     *  Do nothing if @pNotify is already completed.  
     */
    private[this] def tryWrite(v: T, pNotify: Promise[CV[T]]): Unit =
      this.synchronized {
        pNotify.synchronized {
          if (!pNotify.isCompleted) {
            b.put(v,
                  () => pReadyForRead.trySuccess(Unit),
                  () => pReadyForWrite = CancellablePromise[Unit] )
              match {
                case Some(_) => pNotify.trySuccess(CV(this,v))
                case None    => pReadyForWrite.future.onSuccess { case _ => tryWrite(v, pNotify) }
              }
          }
        }
      }

    /** Attempt to read for channel, fulfilling @pNotify with the tuple of the
     *  channel and value read if successful.
     *  If not, reschedule an attempt when pReadyForRead fires.
     *  Do nothing if @pNotify is already completed.
     */
    private[this] def tryRead(pNotify: Promise[CV[T]]): Unit =
      this.synchronized {
        pNotify.synchronized {
          if (!pNotify.isCompleted) {
            b.take(() => pReadyForWrite.trySuccess(Unit),
                   () => pReadyForRead = CancellablePromise[Unit])
              match {
                case Some(v) =>  pNotify.trySuccess(CV(this, v))
                case None =>     pReadyForRead.future.onSuccess { case _ => tryRead(pNotify) }
              }
          }
        }
      }

    /** Return a future that completes on successful write to the channel. 
     */
    def write(v: T): Future[Unit] = {
      val p = Promise[CV[T]]
      pReadyForWrite.future.onSuccess { case _ => tryWrite(v, p) }
      p.future.map(_ => Unit)
    }

    def read: Future[T] = {
      val p = Promise[CV[T]]
      pReadyForRead.future.onSuccess { case _ => tryRead(p) }
      p.future.map(_.v)
    }

    /** When pReadyForRead fires, attempt to fulfill pNotify with CV tuple.
        If pNotify fires first, de-register this callback.
    */
    def read(pNotify: Promise[CV[T]]): Unit =
      pReadyForRead.future(pNotify.asInstanceOf[Promise[Any]]) { _ => tryRead(pNotify) }

    def write(v: T, pNotify: Promise[CV[T]] ) : Unit =
      pReadyForWrite.future(pNotify.asInstanceOf[Promise[Any]]) {_ => tryWrite(v, pNotify)}

  }

  

  import java.util.UUID
  object Chan {
    def apply[T](name: String) = new Chan[T](new NormalBuffer(1, false, false), name)
    def apply[T] = new Chan[T](new NormalBuffer(1, false, false), UUID.randomUUID().toString())
    def apply[T](n: Int) = new Chan[T](new NormalBuffer(n, false, false), UUID.randomUUID.toString())
    def timeout[T](d: Duration, v: T, name: String): Chan[T] = {
      val c = Chan[T](name)
      Timeout.timeout(d) flatMap { _ => c.write(v) }
      c
    }
    def timeout[T](d: Duration, v: T): Chan[T] = timeout[T](d, v, UUID.randomUUID.toString())
    def timeout(d: Duration): Chan[Unit] = timeout(d, Unit)
    type CTuple = (Chan[T], T) forSome { type T }


    def alts(cs: ChanHolder[Any]*): Future[CV[Any]] = {
      val p = Promise[CV[Any]]
      cs.foreach { _ match {
        case c  : Chan[Any] => c.chan.read(p)
        case cv : ChanValueHolder[Any] => cv.chan.write(cv.v,p)
      }}

      p.future
    }

    implicit def ghastly[T](c: Chan[T]): Chan[Any] = c.asInstanceOf[Chan[Any]]

  }
}

import core_async._

object AsyncTest extends App {

  import Chan._

  val cListen = Chan[Int]
  val cRespond = Chan[Int]

  async {
    while (true) {
      println("Waiting")
      val i = await(cListen.read)
      println(s"Got $i")
      cRespond.write(i + 1)
    }
  }

  async {
    println("Sending 3")
    await(cListen.write(3))
    println("Sent")
    val i = await(cRespond.read)
    println(s"Got $i")
  }

  {

    val c1 = Chan.timeout(1 second, 4, "An integer")
    val c2 = Chan.timeout(1 second, "four", "A string")


    async {
      await(Chan.alts(c1, c2)) match {
        case c1(i) => println(s"${i + 1}")
        case c2(s) => println(s + " flushing")
      }
    }

  }

  Await.ready(Promise[Unit].future, Duration.Inf)

}
