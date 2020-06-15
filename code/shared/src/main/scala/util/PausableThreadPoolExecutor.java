package scalaam.util;

/*
 *  A thread pool with the ability to pause the threadpool 
 *  Adapted from https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ThreadPoolExecutor.html
 */

import java.util.concurrent.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class PausableThreadPoolExecutor extends ThreadPoolExecutor {      
  public boolean isPaused = true;
  private ReentrantLock pauseLock = new ReentrantLock();
  private Condition unpaused = pauseLock.newCondition();
  /**
   * Default constructor for a simple fixed threadpool
   */
  public PausableThreadPoolExecutor(int nThreads) {
    super(nThreads, 
          nThreads, 
          0L, 
          TimeUnit.MILLISECONDS, 
          new LinkedBlockingQueue<Runnable>());
  }

  /**
   * Executed before a task is assigned to a thread.
   */
  @Override
  protected void beforeExecute(Thread t, Runnable r) {
    super.beforeExecute(t, r);
    pauseLock.lock();
    try {
      while (isPaused) unpaused.await();
    } catch (InterruptedException ie) {
      t.interrupt();
    } finally {
      pauseLock.unlock();
    }
  }

  /**
   * Pause the threadpool. Running tasks will continue running, but new tasks
   * will not start until the threadpool is resumed.
   */
  public void pause() {
    pauseLock.lock();
    try {
      isPaused = true;
    } finally {
      pauseLock.unlock();
    }
  }

  /**
   * Resume the threadpool.
   */
  public void resume() {
    pauseLock.lock();
    try {
      isPaused = false;
      unpaused.signalAll();
    } finally {
      pauseLock.unlock();
    }
  }
}